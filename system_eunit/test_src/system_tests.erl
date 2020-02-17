%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Three computers 
%%% {"pod_computer_1", "localhost",40100,parallell, 40101, 10}
%%% {"pod_computer_2", "localhost" 40200,parallell, 40201, 10}
%%% {"pod_computer_3", "localhost" 40300,parallell, 40301,10}
%%% Each pod has its port number as vm name pod_40101@asus
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(system_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
-include("test_src/system_tests.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
cases_test()->
    [ets_start(),
     clean_start(),
     start_computer_pods(),
     eunit_start(),
     % Add funtional test cases 
    
   
     % cleanup and stop eunit 
     stop_computer_pods(),
     clean_stop(),
     eunit_stop()].


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,10,system}) end).



ets_start()->
    ?ETS=ets:new(?ETS,[public,set,named_table]),
    ComputerInfo=[{CId,system_test_cases:create_vm_info(CInfo)}||{CId,CInfo}<-?COMPUTER_LIST],
    [ets:insert(?ETS,{Cid,CInfo})||{Cid,CInfo}<-ComputerInfo],
    ListOfWorkers=[CInfo#computer_info.worker_info_list||{_Cid,CInfo}<-ComputerInfo],
    {ok,Host}=inet:gethostname(),
    WorkerVmList=[{"pod_"++integer_to_list(Port),list_to_atom("pod_"++integer_to_list(Port)++"@"++Host)}||{_,Port,_}<-lists:append(ListOfWorkers)],
    ets:insert(?ETS,{worker_vm_list,WorkerVmList}),
    ComputerVmList=[{CInfo#computer_info.vm_name,CInfo#computer_info.vm}||{_Cid,CInfo}<-ComputerInfo],
    ets:insert(?ETS,{computer_vm_list,ComputerVmList}).
    


clean_start()->
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    [{_,WorkerVmList}]=ets:lookup(?ETS,worker_vm_list),
    VmList=[WorkerVmList|ComputerVmList],
    [rpc:call(Vm,init,stop,[])||{_,Vm}<-VmList],
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    [pod:delete(node(),VmName)||{VmName,_}<-ComputerVmList],
    start_service(lib_service),
    check_started_service(lib_service),
    
    ok.
eunit_start()->
    [].

start_computer_pods()->
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    ?assertEqual([{ok,pod_computer_1@asus},
		  {ok,pod_computer_2@asus},
		  {ok,pod_computer_3@asus}],[pod:create(node(),VmName)||{VmName,_}<-ComputerVmList]).

clean_stop()->
    ok.

stop_computer_pods()->
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    [pod:delete(node(),VmName)||{VmName,_}<-ComputerVmList].
eunit_stop()->
    [stop_service(lib_service),
     timer:sleep(1000),
     init:stop()].

%% --------------------------------------------------------------------
%% Function:support functions
%% Description: Stop eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_service(Service)->
    ?assertEqual(ok,application:start(Service)).
check_started_service(Service)->
    ?assertMatch({pong,_,Service},Service:ping()).
stop_service(Service)->
    ?assertEqual(ok,application:stop(Service)),
    ?assertEqual(ok,application:unload(Service)).

