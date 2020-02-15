%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(local_test_client_test). 
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
% -include("test_src/common_macros.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0
	]).
     
-compile(export_all).

 

%% ====================================================================
%% External functions
%% ====================================================================
start()->
    ok=rpc:call(node(),?MODULE,init,[],10*1000),
    ok=rpc:call(node(),?MODULE, start_test_client,[],60*1000),
    stop_test(),
    ok.

%%----------------------------------------------------------------------

init()->
    ok.
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
start_test_client()->
    ok=application:set_env([{test_client,[{ip_address_port,{"localhost",30001}},
					  {type,github},
					  {source,"https://github.com/joq62"}
					 ]
			    }]),
    ok=application:start(test_client),
    {pong,'test_client@asus',tcp_service}=tcp_service:ping(),
    {pong,'test_client@asus',test_client}=test_client:ping(),
    {state,{"localhost",30001},github,"https://github.com/joq62"}=test_client:state_info(),
    
    D=date(),
    D=tcp_service:call({"localhost",30001},{erlang,date,[]}),

    ok.


load_start_computer()->
    
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

stop_test()->
    kill().

kill()->
    init:stop().
