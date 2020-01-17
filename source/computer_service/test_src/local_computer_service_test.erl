%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(local_computer_service_test). 
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
% -include("test_src/common_macros.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([
	]).
     
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================
init_test()->
    ok=application:set_env([{computer_service,[{computer_ip_address_port,{"localhost",40000}},
					       {dns_ip_address_port,{"localhost",50000}}
					      ]]
			   ),
    ok=application:start(pod_service),
    ok=pod_service:load_start("log_service"),
    [{"log_service",{"localhost",40000}}]=pod_service:get_all_services(),
    ok.


load_start_test()->
    ok=pod_service:load_start("adder_service"),
    [{"adder_service",{"localhost",40000}},
     {"log_service",{"localhost",40000}}]=pod_service:get_all_services(),
    ok=pod_service:load_start("divi_service"),
    [{"divi_service",_},{"adder_service",_},
     {"log_service",{"localhost",40000}}]=pod_service:get_all_services(),
    ok.
do_service_test()->
    42=adder_service:add(20,22),
    42.0=divi_service:divi(420,10),
    ok.
stop_unload_test()->
    ok=pod_service:stop_unload("adder_service"),
   [{"divi_service",{"localhost",40000}},
    {"log_service",{"localhost",40000}}]=pod_service:get_all_services(),
    
    {error,[not_loaded,"adder_service",pod_service,_Line]}=pod_service:stop_unload("adder_service"),
  
    ok=pod_service:stop_unload("divi_service"),
    ok=pod_service:stop_unload("log_service"),
    []=pod_service:get_all_services(),
    ok.

stop_pod_test()->
    application:stop(pod_service),
    application:unload(pod_service),
    ok.

%%---------------------------------------------------------------------------------
start_github_test()->
    os:cmd("rm -r adder_service"),
    ok=application:set_env([{pod_service,[{computer_ip_address_port,{"localhost",40000}},
					  {pod_ip_address_port,{"podIpAddr",50000}},
					  {dns_ip_address_port,{"localhost",50000}},
					  {source,{github,"https://github.com/joq62"}}
					 ]
			    }
			   ]),
    ok=application:start(pod_service),
    timer:sleep(1500),
    ok=pod_service:load_start("adder_service"),
    [{"adder_service",{"localhost",40000}}]=pod_service:get_all_services(),
    42=adder:add(20,22),
     ok=pod_service:stop_unload("adder_service"),
    ok.

%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

stop_test()->
  %  []=os:cmd("rm -r "++"_service"),
  %  []=os:cmd("rm -r "++"divi_service"),
%    ok=pod_service:stop_unload("divi_service"),
 %   ok=pod_service:stop_unload("adder_service"),
  %  ok=pod_service:stop_unload("log_service"),
  %  application:stop(pod_service),
    kill().

kill()->
    init:stop().
