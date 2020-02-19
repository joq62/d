%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(system_test_cases). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
-include("test_src/system_tests.hrl").
%% --------------------------------------------------------------------
-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
% Data structures 
% ComputerName: computer_1..
% IpAddr,Port
% Mode
% WorkerStartPort 
% NumWorkers
% Source  {dir, Path
% Files to load
%

emulate_boot_service()->
    % scratch the computer
    % read config file-> computer_name.config 
    % start tcp_server
    % start computer pod
    % load [lib_service,computer_service,log_service,local_dns_service]
    % 
    % exit 
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_worker_pods()->
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    
    % Create computer pods
    ?assertEqual([{ok,pod_computer_1@asus},
		  {ok,pod_computer_2@asus},
		  {ok,pod_computer_3@asus}],[pod:create(node(),VmName)||{VmName,_}<-ComputerVmList]),

    % Load lib_service on each computer pod
    ?assertEqual([ok,ok,ok],[container:create(Vm,VmName,[{{service,"lib_service"},
							  {dir,"/home/pi/erlang/d/source"}}
							])||{VmName,Vm}<-ComputerVmList]),
    % check that lib_service is started
    ?assertEqual([{pong,pod_computer_1@asus,lib_service},
		  {pong,pod_computer_2@asus,lib_service},
		  {pong,pod_computer_3@asus,lib_service}],[rpc:call(Vm,lib_service,ping,[])||{_,Vm}<-ComputerVmList]),

    % start tcp server on each computer pod
    [{computer_info_list,ComputerInfoList}]=ets:lookup(?ETS,computer_info_list),
    ?assertEqual([ok,ok,ok],[rpc:call(CInfo#computer_info.vm,lib_service,start_tcp_server,
			     [CInfo#computer_info.ip_addr,CInfo#computer_info.port,CInfo#computer_info.mode])
		    ||{CId,CInfo}<-ComputerInfoList]),

    ?assertEqual([{pong,pod_computer_1@asus,lib_service},
		  {pong,pod_computer_2@asus,lib_service},
		  {pong,pod_computer_3@asus,lib_service}],
		 [tcp_client:call({CInfo#computer_info.ip_addr,CInfo#computer_info.port},{lib_service,ping,[]})||{CId,CInfo}<-ComputerInfoList]),

    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_computer_pods()->
    [{_,ComputerVmList}]=ets:lookup(?ETS,computer_vm_list),
    
    % Create computer pods
    ?assertEqual([{ok,pod_computer_1@asus},
		  {ok,pod_computer_2@asus},
		  {ok,pod_computer_3@asus}],[pod:create(node(),VmName)||{VmName,_}<-ComputerVmList]),

    % Load lib_service on each computer pod
    ?assertEqual([ok,ok,ok],[container:create(Vm,VmName,[{{service,"lib_service"},
							  {dir,"/home/pi/erlang/d/source"}}
							])||{VmName,Vm}<-ComputerVmList]),
    % check that lib_service is started
    ?assertEqual([{pong,pod_computer_1@asus,lib_service},
		  {pong,pod_computer_2@asus,lib_service},
		  {pong,pod_computer_3@asus,lib_service}],[rpc:call(Vm,lib_service,ping,[])||{_,Vm}<-ComputerVmList]),

   % start computer_service

    

   % start tcp server on each computer pod
    [{computer_info_list,ComputerInfoList}]=ets:lookup(?ETS,computer_info_list),
    ?assertEqual([ok,ok,ok],[rpc:call(CInfo#computer_info.vm,lib_service,start_tcp_server,
			     [CInfo#computer_info.ip_addr,CInfo#computer_info.port,CInfo#computer_info.mode])
		    ||{CId,CInfo}<-ComputerInfoList]),

    ?assertEqual([{pong,pod_computer_1@asus,lib_service},
		  {pong,pod_computer_2@asus,lib_service},
		  {pong,pod_computer_3@asus,lib_service}],
		 [tcp_client:call({CInfo#computer_info.ip_addr,CInfo#computer_info.port},{lib_service,ping,[]})||{CId,CInfo}<-ComputerInfoList]),
    
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------              
create_vm_info({VmName,Vm,IpAddr,Port,Mode,WorkerStartPort,NumWorkers})->
    WorkerInfoList=buildWorkerList(IpAddr,WorkerStartPort,Mode,NumWorkers,[]),
    #computer_info{vm_name=VmName,vm=Vm,ip_addr=IpAddr,port=Port,mode=Mode,
		   worker_info_list=WorkerInfoList}.

buildWorkerList(_IpAddrWorker,_WorkerStartPort,Mode,0,WorkerInfoList)->
    WorkerInfoList;
buildWorkerList(IpAddrWorker,WorkerStartPort,Mode,N,Acc)->
    buildWorkerList(IpAddrWorker,WorkerStartPort,Mode,N-1,[{IpAddrWorker,WorkerStartPort+N-1,Mode}|Acc]).
