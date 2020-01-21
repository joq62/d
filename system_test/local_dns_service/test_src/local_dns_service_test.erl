%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(local_dns_service_test). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
% -include("test_src/common_macros.hrl").
%% --------------------------------------------------------------------

-record(service_info,{id,
		      ip_address
		     }).
%% External exports
-export([
	]).
     
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================
init_test()->
    ok=application:start(local_dns_service),
    ok.


load_start_test()->
    []=local_dns_service:dns_list(),
    []=local_dns_service:get("s1_service"),
    ok.

update_get_test()->
    DnsList=[#service_info{id="s1",ip_address={"Ip1",1}},
	     #service_info{id="s2",ip_address={"Ip2",1}},
	     #service_info{id="s3",ip_address={"Ip2",1}},
	     #service_info{id="s1",ip_address={"Ip2",2}},
	     #service_info{id="s2",ip_address={"Ip1",1}}],
    ok=local_dns_service:update(DnsList),
    
    [{"Ip1",1},{"Ip2",2}]=local_dns_service:get("s1"),
    [{service_info,"s1",{"Ip1",1}},
     {service_info,"s2",{"Ip2",1}},
     {service_info,"s3",{"Ip2",1}},
     {service_info,"s1",{"Ip2",2}},
     {service_info,"s2",{"Ip1",1}}]=local_dns_service:dns_list(),
    
    NewDnsList=[#service_info{id="s2",ip_address={"Ip2",1}},
	     #service_info{id="s3",ip_address={"Ip2",1}},
	     #service_info{id="s1",ip_address={"Ip2",2}}],
    ok=local_dns_service:update(NewDnsList),

    [{"Ip2",2}]=local_dns_service:get("s1"),
    [{service_info,"s2",{"Ip2",1}},
     {service_info,"s3",{"Ip2",1}},
     {service_info,"s1",{"Ip2",2}}]=local_dns_service:dns_list(),
    ok.


stop_test()->
    application:stop(local_dns_service),
    kill().

kill()->
    init:stop().
