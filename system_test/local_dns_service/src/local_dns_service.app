%% This is the application resource file (.app file) for the 'base'
%% application.
{application, local_dns_service,
[{description, "local_dns_service  " },
{vsn, "1.0.0" },
{modules, 
	  [local_dns_service_app,local_dns_service_sup,local_dns_service]},
{registered,[local_dns_service]},
{applications, [kernel,stdlib]},
{mod, {local_dns_service_app,[]}},
{start_phases, []}
]}.
