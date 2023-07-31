{application, flop,
 [
  {description, "Stores network links as a database; provides CLI snippets of configured links"},
  {vsn, "0.1.0"},
  {modules, [flop_app, flop_sup, flop_cb, flop, helpers, templates, contract_checker]},
  {registered, [flop_sup]},
  {applications, [kernel, stdlib]},
  {mod, {flop_app, []}}
 ]}.
