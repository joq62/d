%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description :pod_service
%%% Controls containers (erlang applications) and provides log service 
%%% 
%%% Key Datastructures
%%% Computer: {"localhost",ComputerPort}
%%% application:set_env(computer_service,[{pod_ip_address_port,{PodAddress,PodPort},
%%%                                       {dns_port,DnsPort},
%%%                                       {source,Type,Source}])),
%%% 
%%% ListofContainers=[{ServiceId,Vsn}]
%%%
%%% load_start(ServiceId,Vsn)->ok|{error,[Error,,,]}
%%% stop_unload(ServiceId,Vsn)-> ok|{error,[Error,,,]}
%%% getAllContainers()->[{ServiceId,Vsn},,,]
%%% 
%%%    
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pod_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{computer_ip_address_port,
	       pod_ip_address_port,
	       dns_ip_address_port,
	       source,
	       service_list
	      }).


	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
%%% load_start(ServiceId)->ok|{error,[Error,,,]}
%%% stop_unload(ServiceId)-> ok|{error,[Error,,,]}
%%% load_start(ServiceId,Vsn)->ok|{error,[Error,,,]}
%%% stop_unload(ServiceId,Vsn)-> ok|{error,[Error,,,]}
%%% getAllServices()->[{ServiceId,Vsn},,,]
%%% 

-export([ping/0]).

-export([load_start/1,stop_unload/1,
	 get_all_services/0
	 
	]).

-export([
	]).

-export([start/1,
	 stop/0
	 ]). 
 
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals

%% Gen server function

start(Args)->
   %  glurk=Args,
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
load_start(ServiceId)->
    gen_server:call(?MODULE,{load_start,ServiceId},infinity).
stop_unload(ServiceId)->
    gen_server:call(?MODULE,{stop_unload,ServiceId},infinity).

get_all_services()->
    gen_server:call(?MODULE,{get_all_services},infinity).

ping()->
    gen_server:call(?MODULE,{ping},infinity).

%%------------------ cast ---------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([{ComputerAddress,ComputerPort},{PodAddress,PodPort},
      {DnsAddress,DnsPort},{Type,Source}]) ->

    {ok, #state{computer_ip_address_port={ComputerAddress,ComputerPort},
		pod_ip_address_port={PodAddress,PodPort},
		dns_ip_address_port={DnsAddress,DnsPort},
		source={Type,Source},
		service_list=[]}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({load_start,ServiceId}, _From, State) ->
    Reply=case lists:keymember(ServiceId,1,State#state.service_list) of
	      true->
		  NewState=State,
		  {error,[already_started,ServiceId,?MODULE,?LINE]};
	      false->
		  {Type,Source}=State#state.source,
		  case lib_pod:start(ServiceId,Type,Source,State#state.dns_ip_address_port) of
		      {ok,ServiceId}->
			  NewServiceList=[{ServiceId,State#state.computer_ip_address_port}
					    |State#state.service_list],
			  NewState=State#state{service_list=NewServiceList},
			  ok;
		      {error,Err} ->
			  NewState=State,
			  {error,Err}
		  end
	  end,
									  
    {reply, Reply, NewState};
handle_call({stop_unload,ServiceId}, _From, State) ->
    Reply=case lists:keymember(ServiceId,1,State#state.service_list) of
	      false->
		  NewState=State,
		  {error,[not_loaded,ServiceId,?MODULE,?LINE]};
	      true->
		  case lib_pod:stop(ServiceId) of
		      ok->
			  NewServiceList=lists:keydelete(ServiceId,1,State#state.service_list),
			  NewState=State#state{service_list=NewServiceList},
			  ok;
		      {error,Err} ->
			  NewState=State,
			  {error,Err}
		  end
	  end,
    {reply, Reply, NewState};

handle_call({get_all_services}, _From, State) ->
     Reply=State#state.service_list,
    {reply, Reply, State};

%%----------------------------------------------------------------------
handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,?LINE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
