-define(CONFIG_PATH, "optimyser.cfg").

-record(experiment, 
    {
    id, 
    name,
	user,
    status=running, 
    sections, 
    combinations=[],
	shuffled_combinations=[],
    number_of_combinations=0,
    scheme=[],
    taguchi=false,
    taguchi_array=undefined,
    amount_of_traffic=100,
    preferred_combination=nil
    }).
-record(visit, {id, count}).
-record(conversion, {id, count}).
-record(impression, {expid, count}).
-record(client, {id, apikey, is_active=true}).
-record(log, {
    id, 
    user,
    expid, 
    combination_id, 
    visitor_id, 
    visitor_ip,
    date,
    page_type
    }).
-record(plog, {
    id, 
    expid, 
    combination_id, 
    visitor_id, 
    visitor_ip,
    date,
    page_type
    }).
-record(last_selected, {expid, index=1}).
-record(config, {key, value}).
