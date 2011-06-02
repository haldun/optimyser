{application, optimyser,
 [{description, "optimyser"},
  {vsn, "0.01"},
  {modules, [
    optimyser,
    optimyser_app,
    optimyser_sup,
    optimyser_web,
    optimyser_deps
  ]},
  {registered, []},
  {mod, {optimyser_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
