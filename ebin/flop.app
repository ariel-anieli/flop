 {application, flop, [
 {description, "Provides CLI snippets of configured links"},
 {vsn, "0.1.0"},
 {modules, [contract_checker, flop_app, flop_cb, flop, flop_sup, helpers, templates]},
 {registered, [flop_sup]},
 {applications, [kernel, stdlib]},
 {mod, {flop_app, []}}
 ]}.
