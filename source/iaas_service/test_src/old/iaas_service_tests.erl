%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("test_src/common_macros.hrl").
%% --------------------------------------------------------------------

%% External exports
-export([start/0]).
%-export([test/0,
%	 init_test/0,
%	 init_start_computers_and_tcp_servers/0,
%	 add_active_passive_status/0,
%	 detect_lost_computer/0,
%	 detect_restarted_computer/0,
%	 missing_node_test/0,
%	 cleanup/0
%	]).
     
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================
-define(TIMEOUT,1000*15).
test_remove()->
    io:format("~p~n",[{?MODULE,?LINE}]),
    TestList=[init_test,
	      init_start_computers_and_tcp_servers,
	      add_active_passive_status,
	      detect_lost_computer,
	      detect_restarted_computer,
	      cleanup],
    test_support:execute(TestList,?MODULE,?TIMEOUT).	


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
   % spawn(fun()->eunit:test({timeout,60,iaas_service}) end).
    spawn(fun()->eunit:test({timeout,60,iaas_service}) end).

cases_test()->
    [start_test_glurk(),
     start_computer_and_tcp_server(),
     add_active_passive_status(),
     

     stop_test_glurk()].





start_test_glurk()->
    [start_service(lib_service),
     check_started_service(lib_service),
     start_service(iaas_service),
     check_started_service(iaas_service)].


stop_test_glurk()->
    [stop_service(lib_service),
     stop_eunit()].

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
stop_eunit()->
    cleanup(),
    timer:sleep(1000),
    init:stop().

%**************************** tcp test   ****************************
start_computer_and_tcp_server()->
    Nodes=[{"pod_computer_1","localhost",50001,sequence},
	   {"pod_computer_2","localhost",50002,sequence},
	   {"pod_computer_3","localhost",50003,sequence},
	   {"pod_computer_4","localhost",50004,sequence}
	  ],
    [start_computer_and_tcp_server(PodName,IpAddr,Port,ServerMode)||{PodName,IpAddr,Port,ServerMode}<-Nodes].

start_computer_and_tcp_server(PodName,IpAddr,Port,ServerMode)->
    {ok,Computer}=pod:create(node(),PodName),
    ?assertEqual(ok,container:create(Computer,PodName,
					[{{service,"lib_service"},
					  {dir,"/home/pi/erlang/c/source"}}
					])),    
    rpc:call(Computer,lib_service,start_tcp_server,[IpAddr,Port,ServerMode]),
    D=date(),
    {ok,Socket}=tcp_client:connect(IpAddr,Port),
    tcp_client:cast(Socket,{erlang,date,[]}),
    ?assertEqual(D,tcp_client:get_msg(Socket,1000)),
    tcp_client:disconnect(Socket).


add_active_passive_status()->

    ?assertEqual({error,no_computers_allocated},iaas_service:check_all_status()),

    iaas_service:add("localhost",50001,misc_lib:get_node_by_id("pod_computer_1"),active),
    ?assertEqual([{ok,{"localhost",50001,pod_computer_1@asus},[]}],iaas_service:check_all_status()),
    
    %----
    ?assertEqual([{"localhost",50001,pod_computer_1@asus}],iaas_service:active()),
    ?assertEqual([],iaas_service:passive()),
    ?assertEqual(active,iaas_service:status("localhost",50001,misc_lib:get_node_by_id("pod_computer_1"))),
    ?assertEqual({glurk},{"glurk",50001,misc_lib:get_node_by_id("pod_computer_1")}),
    ?assertEqual({error,[undef,glurk]},iaas_service:status("glurk",50001,misc_lib:get_node_by_id("pod_computer_1"))),

    D=date(),
    D=rpc:call(node(),tcp_client,call,[{"localhost",50001},{erlang,date,[]}],2000),
    iaas_service:add("localhost",50002,misc_lib:get_node_by_id("pod_computer_2"),active),
    iaas_service:add("localhost",50003,misc_lib:get_node_by_id("pod_computer_3"),active),
    L=iaas_service:check_all_status(),
    TestPattern=[{ok,{"localhost",50003,pod_computer_3@asus},[]},
		 {ok,{"localhost",50002,pod_computer_2@asus},[]},
		 {ok,{"localhost",50001,pod_computer_1@asus},[]}
		],
    TestL=[R||{R,_,_}<-L,R==ok],
    ok=case lists:flatlength(TestL) of
	   3->
	       ok;
	   _->
	       {"Result of call",L,"---------------","test pattern",TestPattern}
       end,

    TestL2=[R2||{_,{_,R2,_},_}<-L,
		(R2=:=50003)or(R2=:=50002)or(R2=:=50001)],
    ok=case lists:flatlength(TestL2) of
	   3->
	       ok;
	   _->
	       {"Result of call",L,"---------------","test pattern",TestPattern}
       end,	
    ok.
    
detect_lost_computer()->
    D=date(),
    D=rpc:call(node(),tcp_client,call,[{"localhost",50001},{erlang,date,[]}]),
    Computer_1=misc_lib:get_node_by_id("pod_computer_1"),
    container:delete(Computer_1,"pod_computer_1",["lib_service"]),
    {ok,stopped}=pod:delete(node(),"pod_computer_1"),
    TestPattern=[{ok,{"localhost",50003,pod_computer_3@asus},[]},
		 {ok,{"localhost",50002,pod_computer_2@asus},[]},
		 {error,{"localhost",50001,pod_computer_1@asus},[iaas,73,{error,[econnrefused]}]}],
    
    L=iaas_service:check_all_status(),
    TestL=[R||{R,_,_}<-L,R==ok],
    ok=case lists:flatlength(TestL) of
	   2->
	       ok;
	   _->
	       {"Result of call",L,"---------------","test pattern",TestPattern}
       end,
    
    %-----------
    [{"localhost",50001,pod_computer_1@asus}]=iaas_service:passive(),

    TestPattern2=[{"localhost",50002,pod_computer_2@asus},
		  {"localhost",50003,pod_computer_3@asus}],
    L2=iaas_service:active(),    
    TestL2=[R2||{_,R2,_}<-L2,
		(R2=:=50003)or(R2=:=50002)],
    ok=case lists:flatlength(TestL2) of
	   2->
	       ok;
	   _->
	       {"Result of call",L2,"---------------","test pattern",TestPattern2}
       end,
    ok.
    

detect_restarted_computer()->
    {ok,Computer_1}=pod:create(node(),"pod_computer_1"),
    ok=container:create(Computer_1,"pod_computer_1",
			[{{service,"lib_service"},
			  {dir,"/home/pi/erlang/c/source"}}
			]),    
    rpc:call(Computer_1,lib_service,start_tcp_server,["localhost",50001,sequence]),
    D=date(),
    D=rpc:call(node(),tcp_client,call,[{"localhost",50001},{erlang,date,[]}]),
    
    TestPattern=[{ok,{"localhost",50003,pod_computer_3@asus},[]},
		 {ok,{"localhost",50002,pod_computer_2@asus},[]},
		 {ok,{"localhost",50001,pod_computer_1@asus},[]}],

    L=iaas_service:check_all_status(),
    TestL=[R||{R,_,_}<-L,R==ok],
    ok=case lists:flatlength(TestL) of
	  3->
	       ok;
	   _->
	       {"Result of call",L,"---------------","test pattern",TestPattern}
       end,
    
    ok.
missing_node_test_glurk()->
    iaas_service:add("localhost",5522,node(),active),
    TestPattern1=[{error,{"localhost",5522,pod_test_1@asus},[iaas,xx,{error,[econnrefused]}]},
		  {ok,{"localhost",50003,pod_computer_3@asus},[]},
		  {ok,{"localhost",50002,pod_computer_2@asus},[]},
		  {ok,{"localhost",50001,pod_computer_1@asus},[]}],

    

    L1=iaas_service:check_all_status(),
    TestL1=[R||{R,_,_}<-L1,R==ok],
    ok=case lists:flatlength(TestL1) of
	   3->
	       ok;
	   _->
	       {"Result of call",L1,"---------------","test pattern",TestPattern1}
       end,

    iaas_service:delete("localhost",5522,node()),
    TestPattern2=[{ok,{"localhost",50003,pod_computer_3@asus},[]},
		  {ok,{"localhost",50002,pod_computer_2@asus},[]},
		  {ok,{"localhost",50001,pod_computer_1@asus},[]}],
    L2=iaas_service:check_all_status(),
    TestL2=[R||{R,_,_}<-L2,R==ok],
    ok=case lists:flatlength(TestL2) of
	   3->
	       ok;
	   _->
	       {"Result of call",L2,"---------------","test pattern",TestPattern2}
       end,
    ok.
    


    
cleanup()->
    Nodes=["pod_computer_1","pod_computer_2","pod_computer_3","pod_computer_4"],
    [pod:delete(node(),Node)||Node<-Nodes],
    ComputerList=[misc_lib:get_node_by_id(Node)||Node<-Nodes],

    
 %   Computer_1=misc_lib:get_node_by_id("pod_computer_1"),
 %   container:delete(Computer_1,"pod_computer_1",["lib_service"]),
 %   {ok,stopped}=pod:delete(node(),"pod_computer_1"),
 %   Computer_2=misc_lib:get_node_by_id("pod_computer_2"),
 %   container:delete(Computer_2,"pod_computer_2",["lib_service"]),
 %   {ok,stopped}=pod:delete(node(),"pod_computer_2"),
 %   Computer_3=misc_lib:get_node_by_id("pod_computer_3"),
 %   container:delete(Computer_3,"pod_computer_3",["lib_service"]),
 %   {ok,stopped}=pod:delete(node(),"pod_computer_3"),
 %   Computer_3=misc_lib:get_node_by_id("pod_computer_3"),
 %   container:delete(Computer_3,"pod_computer_3",["lib_service"]),
 %   {ok,stopped}=pod:delete(node(),"pod_computer_3"),

    PodDns=misc_lib:get_node_by_id("pod_dns_test"),
    container:delete(PodDns,"pod_dns_test",["dns_service"]),
    container:delete(PodDns,"pod_dns_test",["lib_service"]),
    {ok,stopped}=pod:delete(node(),"pod_dns_test"),

    ok.


%**************************************************************
