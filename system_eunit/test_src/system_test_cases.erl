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
              
create_vm_info({VmName,Vm,IpAddr,Port,Mode,WorkerStartPort,NumWorkers})->
    WorkerInfoList=buildWorkerList(IpAddr,WorkerStartPort,Mode,NumWorkers,[]),
    #computer_info{vm_name=VmName,vm=Vm,ip_addr=IpAddr,port=Port,mode=Mode,
		   worker_info_list=WorkerInfoList}.

buildWorkerList(_IpAddrWorker,_WorkerStartPort,Mode,0,WorkerInfoList)->
    WorkerInfoList;
buildWorkerList(IpAddrWorker,WorkerStartPort,Mode,N,Acc)->
    buildWorkerList(IpAddrWorker,WorkerStartPort,Mode,N-1,[{IpAddrWorker,WorkerStartPort+N-1,Mode}|Acc]).
