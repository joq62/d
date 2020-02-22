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
	       min_vm_port,
	       max_vm_port,
	       type,
	       source,
	       list_vms,
	       list_services
	      }).

-record(vm_info,{vm_id,
		 node,
		 ipaddress={}
		}).
	  
-define(MANDATORY,["tcp_service","log_service","local_dns_service"]).

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

-export([ping/0,
	 state_info/0]).

-export([
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

get_dns_list()->
    gen_server:call(?MODULE,{get_dns_list},infinity).
get_service_addresses(ServiceId)->
    gen_server:call(?MODULE,{get_service_addresses,ServiceId},infinity).

ping()->
    gen_server:call(?MODULE,{ping},infinity).
state_info()->
    gen_server:call(?MODULE,{state_info},infinity).

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
init([]) ->
    %%- Retreive config file
    {ok,Files}=file:list_dir("."),    
    case [File||File<-Files,filename:extension(File)=:=".config"] of
	[File]->
	    {ok,[{vm_name,VmName},
		 {vm,Vm},
		 {ip_addr,ComputerIpAddr},
		 {port,ComputerPort},
		 {mode,Mode},
		 {worker_start_port,WorkerStartPort},
		 {num_workers,NumWorkers},
		 {source,Source},
		 {services_to_load,ServicesToLoad},
		 {files_to_keep,_},
		 {master_dns,DnsIpAddrAndPort}]}=file:consult(File);
	_ ->
	    {error,[no_config_file]}
    end,
    %% start needed services
    [container:create(Vm,VmName,[{{service,Service},Source}])||Service<-ServiceToLoad],

    %% Start tcp server
    ok=lib_service:start_tcp_server(ComputerIpAddr,ComputerPort,Mode),
    
    lib_computer:start_workers(ComputerIpAddr,WorkerStartPort,NumWorkers,Mode,Source)
	    

    
    {ok, #state{computer_ip_address_port={ComputerIpAddr,ComputerPort},
		min_vm_port=MinVmPort,
		max_vm_port=MaxVmPort,
		type=Type,
		source=Source,
		list_vms=VmStartInfo,
		list_services=ListOfService}
    }.

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

handle_call({state_info}, _From, State) ->
    Reply=State,
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
