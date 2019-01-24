%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sys,
[{description, "sys  " },
{vsn, "1.0.0" },
{modules, 
	  [sys_app,sys_sup,sys]},
{registered,[sys]},
{applications, [kernel,stdlib]},
{mod, {sys_app,[]}},
{start_phases, []}
]}.
