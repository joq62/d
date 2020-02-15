%% This is the application resource file (.app file) for the 'base'
%% application.
{application, test_client,
[{description, "test_client" },
{vsn, "0.0.1" },
{modules, [test_client_app,test_client_sup,
	   test_client]},
{registered,[test_client]},
{applications, [kernel,stdlib]},
{mod, {test_client_app,[]}},
{start_phases, []}
]}.
