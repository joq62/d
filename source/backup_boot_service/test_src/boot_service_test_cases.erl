%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(boot_service_test_cases). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------



-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create_config_file()->
    Info=[{vm_name,"pod_computer_1"},
	  {vm,'pod_computer_1@asus'},
	  {ip_addr,"localhost"},
	  {port,40100},
	  {mode,parallell},
	  {worker_start_port,40101},
	  {num_workers,5},
	  {source,{dir,"/home/pi/erlang/d/source"}},
	  {services_to_load,["lib_service","computer_service","log_service","local_dns_service"]},
	  {files_to_keep,["Makefile","computer_1.config","boot_service","src","ebin","test_ebin","test_src"]},
	  {master_dns,{"localhost",portGlurk}}],
    misc_lib:unconsult("computer_1.config",Info),
    ?assertEqual({ok,Info},file:consult("computer_1.config")).
    
% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
scratch_computer()->
    



% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

