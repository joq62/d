%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(local_boot_service_test). 
   
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
%    ok=application:start(local_dns_service),
    ok.


vm_handler_create_test()->
    '0_vm@asus'=vm_handler:create("0_vm"),
    '1_vm@asus'=vm_handler:create("1_vm"),
    '2_vm@asus'=vm_handler:create("2_vm"),
    {error,[allready_exists,
	    "1_vm",vm_handler,_]}=vm_handler:create("1_vm"),
    ['0_vm@asus','1_vm@asus','2_vm@asus']=nodes(),
   
    ok.

vm_handler_delete_test()->
    ok=vm_handler:delete("0_vm"),
    ok=vm_handler:delete("1_vm"),
    ok=vm_handler:delete("2_vm"),
    ok=vm_handler:delete("1_vm"),
    []=nodes(),
    ok.



lib_service_start_test()->
    Vm0=vm_handler:create("0_vm"),
    ServiceId="local_dns_service",
    Type=github,
    Source="https://github.com/joq62",
    DestinationDir="0_vm",
    EnvVariables=[],
    service_handler:start(Vm0,ServiceId,Type,Source,DestinationDir,EnvVariables),
    {pong,'0_vm@asus',local_dns_service}=rpc:call(Vm0,local_dns_service,ping,[]),
    ok.

service_handler_stop_test()->
    ok=vm_handler:delete("0_vm"),
    {badrpc,nodedown}=rpc:call('0_vm@asus',local_dns_service,ping,[]),
    ok.

simulate_board_1_test()->
    Vm0=vm_handler:create("0_vm"),
    Type=github,
    Source="https://github.com/joq62",
    DestinationDir="0_vm",

    service_handler:start(Vm0,"local_dns_service",Type,Source,DestinationDir,[]),
    {pong,'0_vm@asus',local_dns_service}=rpc:call('0_vm@asus',local_dns_service,ping,[]),
    ok.
simulate_board_2_test()->
    Type=github,
    Source="https://github.com/joq62",
    DestinationDir="0_vm",
    service_handler:start('0_vm@asus',"log_service",Type,Source,DestinationDir,[]),
    {pong,'0_vm@asus',log_service}=rpc:call('0_vm@asus',log_service,ping,[]),
    ok.

simulate_board_3_test()->
    Type=github,
    Source="https://github.com/joq62",
    DestinationDir="0_vm",
    service_handler:start('0_vm@asus',"tcp_service",Type,Source,DestinationDir,[]),
    {pong,'0_vm@asus',tcp_service}=rpc:call('0_vm@asus',tcp_service,ping,[]),
    ok.
simulate_computer_stop_test()->  
    ok=vm_handler:delete("0_vm"),

    ok.

stop_test()->
  %  ok=vm_handler:delete("0_vm"),
    kill().

kill()->
    init:stop().
