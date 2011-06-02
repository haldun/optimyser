%%% Control the periodic running of a given set of tasks
-module(cron).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1,start_link/2,
         stop/0,
         add/3, add/4, add/1,
         remove/1,
         info/0,
         wakeup/0,
         run_early/1,
         test_jobs/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(log_info(Msg,Args),
        error_logger:info_msg("~p:~p "++Msg,
                              [?MODULE,?LINE | Args ])).
-define(log_warning(Msg,Args),
        error_logger:warning_msg("~p:~p "++Msg,
                              [?MODULE,?LINE | Args ])).
-define(log_error(Msg,Args),
        error_logger:error_msg("~p:~p "++Msg,
                              [?MODULE,?LINE | Args ])).
-define(log_debug(Msg,Args), ok). % ?log_info(Msg,Args)).

-record(state,      {jobs=[],          % all of the job records
                     running=[],       % a list of running_job records
                     wakeup_timer=none % the next-wakeup timer
                    }).
-record(job,        {task,             % fun/0 or fun/1
                     id,               % an atom identifier
                     last_run=0,       % seconds from epoch of last run
                     periocity,        % how often to sleep between
                                       % invocations, in seconds
                     last_state        % the last return of the task, when,
                                       % if task is of arity 1,
                                       % will be passed into
                                       % task/1
                    }).
-record(running_job,{id,               % the job_id
                     start_time,       % when it started (Now time)
                     pid               % its PID when it started
                    }).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
start_link() ->
  start_link([],[]).
start_link(InitialJobs) ->
  start_link(InitialJobs, []).
start_link(InitialJobs, Options) ->
  gen_server:start_link({local, ?MODULE},
                        ?MODULE,
                        _InitArgs=InitialJobs,
                        Options).

stop() ->
  gen_server:call(?MODULE,stop,infinity).

add(JobId,Task,Periocity) ->
  add(JobId,Task,Periocity,undefined).

add(JobId,Task,Periocity,InitialState) ->
  add(#job{id=JobId,
           task=Task,
           periocity=Periocity,
           last_state=InitialState}).

add(Job=#job{id=JobId,
             task=Task,
             periocity=Periocity})
  when is_atom(JobId),
       (is_function(Task,0)
        or is_function(Task,1)),
       is_number(Periocity) ->
  gen_server:cast(?MODULE,{add,Job});
%% friendly to init/1
add(JobTuple) when is_tuple(JobTuple) ->
  apply(?MODULE,add,tuple_to_list(JobTuple)).

remove(JobId) when is_atom(JobId) ->
  gen_server:call(?MODULE,{remove,JobId}).

info() ->
  gen_server:call(?MODULE,info,infinity).

wakeup() ->
  gen_server:cast(?MODULE,wakeup).

run_early(JobId) when is_atom(JobId) ->
  gen_server:cast(?MODULE,{run_early,JobId}).

%%--------------------------------------------------------------------
%%% gen_server callbacks
%%--------------------------------------------------------------------

init(InitialJobs) ->
  %% we need to be notified when our tasks complete
  erlang:process_flag(trap_exit,true),

  %% if we have any initial jobs, add them all
  lists:foreach(fun add/1, InitialJobs),

  {ok, #state{jobs=[],
              running=[]}}.

%% handle calls, casts, and infos. The choice of when to do which is
%% pretty arbitrary, based on my assumptions of when a programmer
%% expects a response

%% check for jobs that are ready, run them. don't allow multiple
%% simulaneous runs of the same job. Running this too many times isn't
%% that bad, it just wastes a little CPU. It will only run jobs when
%% they are ready
handle_cast(wakeup,
            State=#state{jobs=Jobs,
                         running=Running,
                         wakeup_timer=Timer}) ->
  Now=nnow(),

  clear_wakeup_queue(),

  %% remove the ones that are already running from those that can be
  %% run
  NotRunningJobs=
    [ Job
      || Job <- Jobs,
         not lists:any(fun(#running_job{id=RunningId}) ->
                           Job#job.id =:= RunningId
                       end, Running) ],

  %% split those that we can run from those that we can't
  {EligableToRun,NotEligableToRun}=
    lists:partition(fun(#job{last_run=LastRun,
                             periocity=Periocity}) ->
                        %% it's eligable to run if its next run-time
                        %% is in the past and it's not already running
                        LastRun+Periocity =< Now
                    end, NotRunningJobs),

  case length(EligableToRun) of
    0 ->
      ok;
    Length ->
      ?log_debug("Running ~p jobs (~p)",[Length,[ Job#job.id
                                                  || Job <- EligableToRun]])
  end,

  %% run those we can, and add them to the list of running jobs.
  NowRunning=
    lists:foldl(fun(#job{task=Task,
                         id=Id,
                         last_state=LastState},
                    AccIn) ->
                    TaskWrapper=fun() ->
                                    case Task of
                                      Task
                                      when is_function(Task,0) ->
                                        Task(),
                                        %% avoid sending messages that
                                        %% can be expensive to copy if
                                        %% the function isn't
                                        %% interested in receiving
                                        %% them
                                        exit(ok);
                                      Task
                                      when is_function(Task,1) ->
                                        exit(Task(LastState))
                                    end
                                end,
                    Pid=spawn_link(TaskWrapper),
                    [ #running_job{pid=Pid,
                                   id=Id,
                                   start_time=Now}
                      | AccIn ]
                end,
                Running,
                EligableToRun),

  %% find the amount of time that we have to wait for the next job,
  %% based on the list of jobs that weren't eligable to run (since the
  %% running ones will re-schedule themselves as they complete), and
  %% spawn an alarm to wake us up at that time
  NewTimer=schedule_wakeup(NotEligableToRun,Timer),

  {noreply, State#state{running=NowRunning,
                        wakeup_timer=NewTimer}};

handle_cast({add,Job=#job{id=JobId,
                          task=JobFunction,
                          periocity=Periocity}},
            State=#state{jobs=AllJobs})
  when is_atom(JobId),
       (is_function(JobFunction,0)
        or is_function(JobFunction,1)),
       is_number(Periocity) ->

  NewState=case lists:any(fun(#job{id=Id}) -> % check for duplicates
                              Id =:= JobId
                          end, AllJobs) of
             false ->
               %% when we're done adding it, we need to run this job
               wakeup(),
               State#state{jobs=[ Job | AllJobs ]};
             true ->
               ?log_error("Attempted to add duplicate job ~p",[JobId]),
               State
           end,
  {noreply,NewState};

handle_cast({run_early,JobId},State=#state{jobs=Jobs}) ->
  %% extract the job from the list of all jobs
  {[Job],RestJobs}=lists:partition(fun(#job{id=ElemId}) ->
                                       ElemId =:= JobId
                                   end, Jobs),
  %% make it think that it last ran a long time ago
  NewJob=Job#job{last_run=0},
  wakeup(),
  {noreply,State#state{jobs=[ NewJob
                              | RestJobs ]}}.


handle_call(info,_From,State) ->
  %% just dump out our entire state; since we may change the format of
  %% our state, this should only be used by humans, not attempted to
  %% be parsed
  {reply,
   _Reply=State,
   State};

handle_call(stop,From,State) ->
  {stop,{requested,From},ok,State};

handle_call({remove,Job},From,State)
  when is_record(Job,job) ->

  handle_call({remove,Job#job.id},From,State);
handle_call({remove,JobId},_From,State=#state{jobs=Jobs})
  when is_atom(JobId) ->

  {FoundJobs,RestJobs}=lists:partition(fun(#job{id=ElemId}) ->
                                           JobId =:= ElemId
                                       end, Jobs),
  RemovedJob=
    case FoundJobs of
      [] ->
        ?log_warning("Could not remove job ~p; not found",[JobId]),
        {not_found,JobId};
      [Job] ->
        {ok,Job}
    end,
  {reply,RemovedJob,State#state{jobs=RestJobs}}.

%% we receive 'EXIT' messages when a job dies. Note that we can't tell
%% a crashed job from a completed one, and happily pass the
%% crash-reason to the next invocation as the last_state. This could
%% be changed if it bothers you.
handle_info({'EXIT', FromPid, LastState},
            State=#state{running=RunningJobs,
                         jobs=Jobs})
  when is_pid(FromPid) ->

  %% remove it from the list of running jobs
  {[WasRunning],NewRunningJobs}=
    lists:partition(fun(#running_job{pid=Pid}) ->
                        Pid =:= FromPid
                    end,
                    RunningJobs),

  %% extract the completed job from the list of all jobs and update
  %% its timestamp, then add it back to the list
  NewJobs=case lists:partition(fun(#job{id=ElemId}) ->
                                   ElemId =:= WasRunning#running_job.id
                               end, Jobs) of
            {[Job],RestJobs} ->
              %% update the timestamp and last_state and put it back
              %% on the list
              [ Job#job{last_run=nnow(),
                        last_state=LastState}
                | RestJobs ];
            {[], RestJobs} ->
              %% the job must have been deleted from the list while it
              %% was running. Just move on with the old job-list
              RestJobs
            end,

  %% this is leaves a message in the queue, which will be processed
  %% when we're done here
  wakeup(),

  {noreply,State#state{running=NewRunningJobs,
                       jobs=NewJobs}};

handle_info(Other, State) ->
  ?log_info("Unknown message ~p",[Other]),
  {noreply,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, #state{running=[]}) ->
  ?log_info("stopped (~p)",[Reason]),
  ok;
terminate(Reason,
          #state{running=[ #running_job{id=Id,
                                        pid=Pid}
                           | Rest ] }) ->
  ?log_info("Terminating (reason: ~p); waiting on ~p; ~p jobs remaining",
            [Reason,Id,length(Rest)+1]),
  receive
    {'EXIT',Pid,_PidDeathReason} ->
      %% keep calling terminate until we're ready to die. (we discard
      %% the rest of the state here)
      terminate(Reason,#state{running=Rest})
  end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

nnow() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% takes a list of jobs and the current wakeup-timer (if any), and
%% sets a timer for when to run next
schedule_wakeup(NotEligableToRun,CurrentTimer)
  when is_list(NotEligableToRun) ->

  %% cancel any running timer we have
  case CurrentTimer of
    none -> ok;
    CurrentTimer ->
      timer:cancel(CurrentTimer)
  end,

  case NotEligableToRun of
    %% if there are no tasks to run, there's no reason to wake
    %% ourselves up
    [] -> none;
    NotEligableToRun ->
      NextRunTimes=lists:map(fun(#job{last_run=LastRun,
                                      periocity=Periocity}) ->
                                 LastRun+Periocity
                             end, NotEligableToRun),
      NextRun=lists:min(NextRunTimes),
      {ok,TRef}=timer:apply_after((NextRun-nnow())*1000+1,
                                  ?MODULE,wakeup,[]),
      TRef
  end.

%% tries to clear all of the 'wakeup' messages out of the erlang
%% message queue. This might not work in future versions of Erlang or
%% gen_server, but it's just a performance hack anyway
clear_wakeup_queue() ->
  receive
    {'$gen_cast',wakeup} ->
      clear_wakeup_queue()
  after 0 ->
      ok
  end.

test_jobs() ->
  [#job{id=ioer5,
        task=fun() ->
                 ?log_debug("ioer~p~n",[5])
             end,
        periocity=5},
   #job{id=ioer10,
        task=fun() ->
                 ?log_debug("ioer~p~n",[10])
             end,
        periocity=10},
   #job{id=ioer15,
        task=fun() ->
                 ?log_debug("ioer~p~n",[15])
             end,
        periocity=15},
   #job{id=ioer20, % job that uses state
        task=fun(LastState) ->
                 ?log_debug("ioer~p uses state (~p)~n",[20,LastState]),
                 LastState+1
             end,
        periocity=20,
        last_state=0},
   #job{id=ioer30, % job that takes a while to complete
        task=fun() ->
                 ?log_debug("ioer~p (sleeps begin)~n",[30]),
                 timer:sleep(30*1000),
                 ?log_debug("ioer~p (done sleeping)~n",[30])
             end,
        periocity=30},
   #job{id=ioer45, % job that crashes
        task=fun(State) ->
                 ?log_debug("ioer~p crashes (~p)~n",[45,State]),
                 throw(badarg)
             end,
        periocity=45},
   #job{id=ioer60, % job that removes itself after 3 invocations
        task=fun(State) ->
                 ?log_debug("ioer~p removes at state==0~p~n",[60,State]),
                 case State of
                   0 ->
                     %% kill self
                     ?log_debug("ioer~p Oh, what a world!~n",[60]),
                     remove(ioer60);
                   _ -> State-1
                 end
             end,
        periocity=60,
        last_state=3},
   #job{id=ioer60, % a job with an ID that already exists
        task=fun erlang:now/0,
        periocity=60},
   #job{id=ioer61, % a job that will try to remove a non-existant job
        task=fun() ->
                 remove(job_that_doesnt_exist)
             end,
        periocity=61}

   | [ % and a bunch of no-ops
       #job{id=list_to_atom("noop_job_" ++ integer_to_list(X)),
            task=fun erlang:now/0,
            periocity=X}
       || X <- lists:seq(10,10000) ]].
