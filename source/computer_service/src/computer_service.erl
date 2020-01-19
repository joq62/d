%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description :computer_service
%%% Manage the Pods on the computer 
%%% Implements: local_dns
%%% Key Datastructures
%%% ComputerPort: computer_service listen port
%%% application:set_env(computer_service,[{computer_ip_address_port,{ComputerAddress,ComputerPort},
%%%                                       {pod_ports,Min,Max},
%%%                                       {dns_port,DnsPort}])),
%%% PodList=[{ErlVmId,ErlVm,[IpAddr,Port]},,,]
%%%
%%% create_pod(ErlVmId)->ok|{error,Err}
%%% delete_pod(ErlVmId)->ok|{error,Err}
%%% list_all_pods()->[{ErlVm,ErlVmId,{IpAddr,Port}}]
%%% set_pod_ipaddr(ErlVmId,{IpAddr,Port})->ok|{error,[Error,,,]}
%%% get_pod_ippaddr(ErlVmId)-> {IpAddr,Port}|{error,[Error,,,]}
%%% 
%%% LocalDnsList=[{ServiceId,IpAddr,Port}
%%% setLocalDnsList(DnsList)->ok
%%% getLocalDnsList()->DnsList
%%% getLocalDnsServiceAddresses(ServiceId)-> []|[{IpAddr,Port},,,]
%%% 
%%%    
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(computer_service). 

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
	       min_vm_port_num,
	       max_number_vms,
	       list_vms,
	       list_services
	      }).

-record(vm_info,{vm_id,
		 node,
		 ipaddress={}
		}).
	  
	  
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
%%%
%%%
%%% create_vm(ErlVmId,{IpAddr,Port})->ok|{error,Err}
%%% delete_vm(ErlVmId)->ok|{error,Err}
%%% list_all_vms()->[{ErlVm,ErlVmId,{IpAddr,Port}}]
%%% set_vm_ipaddr(ErlVmId,{IpAddr,Port})->ok|{error,[Error,,,]}
%%% get_vm_ippaddr(ErlVmId)-> {IpAddr,Port}|{error,[Error,,,]}
%%% 
%%% LocalDnsList=[{ServiceId,IpAddr,Port}
%%% set_dns_list(DnsList)->ok
%%% get_dns_list()->DnsList
%%% get_service_addresses(ServiceId)-> []|[{IpAddr,Port},,,]

-export([ping/0]).

-export([create_vm/2,delete_vm/1,
	 list_all_vms/0,
	 get_vm_ippaddr/1
	]).

-export([set_dns_list/1,get_dns_list/0,
	 get_service_addresses/1
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

start(Args)-> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%----------------------------------------------------------------------
create_pod(ErlVmId,{IpAddr,Port})->
    gen_server:call(?MODULE,{create_pod,ErlVmId,{IpAddr,Port}},infinity).
delete_pod(ErlVmId)->
    gen_server:call(?MODULE,{delete_pod,ErlVmId},infinity).
get_pod_ippaddr(ErlVmId)->
    gen_server:call(?MODULE,{get_pod_ippaddr,ErlVmId},infinity).

list_all_pods()->
    gen_server:call(?MODULE,{list_all_pods},infinity).
get_dns_list()->
    gen_server:call(?MODULE,{get_dns_list},infinity).
get_service_addresses(ServiceId)->
    gen_server:call(?MODULE,{get_service_addresses,ServiceId},infinity).

ping()->
    gen_server:call(?MODULE,{ping},infinity).

%%------------------ cast ---------------------------------------------


set_dns_list(DnsList)->
    gen_server:cast(?MODULE,{set_dns_list,DnsList}).


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
init([{ComputerAddress,ComputerPort},{MinPort,MaxPort},
      {DnsAddress,DnsPort}]) ->
    PodList=lib_computer:create_vm_list(ComputerAddress,MinPort,MaxPort)
    
    {ok, #state{computer_ip_address_port={ComputerAddress,ComputerPort},
		vm_list=[], 
		dns_ip_address_port={"localhost",DnsPort},
		dns_list=[]}}.

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
handle_call({ping}, _From, State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({create_pod,ErlVmId,{IpAddr,Port}}, _From, State) ->
    Reply={IpAddr,Port},
    {reply, Reply, State};
handle_call({delete_pod,ErlVmId}, _From, State) ->
    Reply=ErlVmId,
    {reply, Reply, State};
handle_call({list_all_pods}, _From, State) ->
     Reply=glurk,
    {reply, Reply, State};

handle_call({get_pod_ippaddr,ErlVmId}, _From, State) ->
      Reply=glurk,
    {reply, Reply, State};

handle_call({get_service_addresses,ServiceId}, _From, State) ->
      Reply=ServiceId,
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

handle_cast({set_dns_list,DnsList},State) ->
    
    {noreply, State#state{dns_list=DnsList}};

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
