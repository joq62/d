%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(local_dns_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common_macros.hrl").
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{dns_list}).

-record(service_info,{id,
		      ip_address
		     }).
%% --------------------------------------------------------------------
%% Exported functions
%% --------------------------------------------------------------------

%% dns functions 
-export([update/1,get/1,
	 dns_list/0,
	 ping/0
	]
      ).


-export([start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-------------------- Direct call -----------------------------------

%%--------------------- Server call ------------------------------------
ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

update(DnsList)-> 
    gen_server:call(?MODULE, {update,DnsList},infinity).
get(ServiceId)->
    gen_server:call(?MODULE,{get,ServiceId},infinity).  

dns_list()->
    gen_server:call(?MODULE,{dns_list},infinity).  
    
%%----------------------Server cast --------------------------------------

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
    io:format("Started Service  ~p~n",[{?MODULE}]),
   {ok, #state{dns_list=[]}}. 
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({update,DnsList},_From,State) ->
    NewState=State#state{dns_list=DnsList},
    Reply=ok,
    {reply, Reply, NewState};

handle_call({get,ServiceId}, _From, State) ->
   Reply=[Info#service_info.ip_address||Info<-State#state.dns_list,
					 ServiceId==Info#service_info.id],
   {reply, Reply, State};

handle_call({dns_list}, _From, State) ->
    Reply=State#state.dns_list,		  
    {reply, Reply, State};


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    io:format("unmatched  ~p~n",[{time(),?MODULE,?LINE,Request}]),
     Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
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
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
