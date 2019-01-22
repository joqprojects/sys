%% This is the application resource file (.app file) for the 'base'
%% application.
{application, calc,
[{description, "calc  " },
{vsn, "1.0.0" },
{modules, 
	  [calc_app,calc_sup,calc,calc_lib]},
{registered,[calc]},
{applications, [kernel,stdlib]},
{mod, {calc_app,[]}},
{start_phases, []}
]}.
