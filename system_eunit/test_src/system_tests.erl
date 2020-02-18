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
     eunit_start(),
     % Add funtional test cases 
     system_test_cases:start_computer_pods(),
     system_test_cases:start_worker_pods(),
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
    spawn(fun()->eunit:test({timeout,30,system}) end).



ets_start()->
    ?ETS=ets:new(?ETS,[public,set,named_table]),
    ComputerInfoList=[{CId,system_test_cases:create_vm_info(CInfo)}||{CId,CInfo}<-?COMPUTER_LIST],
    ets:insert(?ETS,{computer_info_list,ComputerInfoList}),
%    [ets:insert(?ETS,{Cid,CInfo})||{Cid,CInfo}<-ComputerInfo],
    ListOfWorkers=[CInfo#computer_info.worker_info_list||{_Cid,CInfo}<-ComputerInfoList],
    {ok,Host}=inet:gethostname(),
    WorkerVmList=[{"pod_"++integer_to_list(Port),list_to_atom("pod_"++integer_to_list(Port)++"@"++Host)}||{_,Port,_}<-lists:append(ListOfWorkers)],
    ets:insert(?ETS,{worker_vm_list,WorkerVmList}),
    ComputerVmList=[{CInfo#computer_info.vm_name,CInfo#computer_info.vm}||{_Cid,CInfo}<-ComputerInfoList],
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
		  {ok,pod_computer_3@asus}],[pod:create(node(),VmName)||{VmName,_}<-ComputerVmList]),

    ?assertEqual([ok,ok,ok],[container:create(Vm,VmName,[{{service,"lib_service"},
							  {dir,"/home/pi/erlang/d/source"}}
							])||{VmName,Vm}<-ComputerVmList]),

    ?assertEqual([{pong,pod_computer_1@asus,lib_service},
		  {pong,pod_computer_2@asus,lib_service},
		  {pong,pod_computer_3@asus,lib_service}],[rpc:call(Vm,lib_service,ping,[])||{_,Vm}<-ComputerVmList]),

    %% test if its possible to create a pod under a computer pod ..

    ?assertEqual(ok,
		 rpc:call('pod_computer_1@asus',lib_service,start_tcp_server,["localhost",40100,parallell])),

    ?assertEqual({ok,pod_c1_test@asus},tcp_client:call({"localhost",40100},{pod,create,['pod_computer_1@asus',"pod_c1_test"]})),
    ?assertEqual(ok,container:create('pod_c1_test@asus',"pod_c1_test",[{{service,"lib_service"},
							{dir,"/home/pi/erlang/d/source"}}
						      ])),
    
    
   
   

 ?assertEqual({pong,pod_c1_test@asus,lib_service},rpc:call('pod_c1_test@asus',lib_service,ping,[])),
    ok.

clean_stop()->
    ok.

stop_computer_pods()->
    timer:sleep(10000),
    {ok,stopped}=rpc:call('pod_computer_1@asus',pod,delete,['pod_computer_1@asus',"pod_c1_test"]),
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

