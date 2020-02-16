%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_service_test_cases). 
   
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

add_pods()->

    ?assertEqual({error,no_computers_allocated},iaas_service:check_all_status()),

    iaas_service:add("localhost",50001,misc_lib:get_node_by_id("pod_lib_1"),active),
    ?assertEqual([{ok,{"localhost",50001,pod_lib_1@asus},[]}],iaas_service:check_all_status()),
    
    %----
    ?assertEqual([{"localhost",50001,pod_lib_1@asus}],iaas_service:active()),
    ?assertEqual([],iaas_service:passive()),
    ?assertEqual(active,iaas_service:status("localhost",50001,misc_lib:get_node_by_id("pod_lib_1"))),
    ?assertEqual({"glurk",50001,pod_lib_1@asus},{"glurk",50001,misc_lib:get_node_by_id("pod_lib_1")}),
    ?assertEqual({error,[undef,"glurk",50001,pod_lib_1@asus]},iaas_service:status("glurk",50001,misc_lib:get_node_by_id("pod_lib_1"))),

    D=date(),
    D=rpc:call(node(),tcp_client,call,[{"localhost",50001},{erlang,date,[]}],2000),
    iaas_service:add("localhost",50002,misc_lib:get_node_by_id("pod_lib_2"),active),
    iaas_service:add("localhost",50003,misc_lib:get_node_by_id("pod_lib_3"),active),
    L=iaas_service:check_all_status(),
    TestPattern=[{ok,{"localhost",50003,pod_lib_3@asus},[]},
		 {ok,{"localhost",50002,pod_lib_2@asus},[]},
		 {ok,{"localhost",50001,pod_lib_1@asus},[]}
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
    Computer_1=misc_lib:get_node_by_id("pod_lib_1"),
    container:delete(Computer_1,"pod_lib_1",["lib_service"]),
    {ok,stopped}=pod:delete(node(),"pod_lib_1"),
    TestPattern=[{ok,{"localhost",50003,pod_lib_3@asus},[]},
		 {ok,{"localhost",50002,pod_lib_2@asus},[]},
		 {error,{"localhost",50001,pod_lib_1@asus},[iaas,73,{error,[econnrefused]}]}],
    
    L=iaas_service:check_all_status(),
    TestL=[R||{R,_,_}<-L,R==ok],
    ok=case lists:flatlength(TestL) of
	   2->
	       ok;
	   _->
	       {"Result of call",L,"---------------","test pattern",TestPattern}
       end,
    
    %-----------
    [{"localhost",50001,pod_lib_1@asus}]=iaas_service:passive(),

    TestPattern2=[{"localhost",50002,pod_lib_2@asus},
		  {"localhost",50003,pod_lib_3@asus}],
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
    {ok,Computer_1}=pod:create(node(),"pod_lib_1"),
    ok=container:create(Computer_1,"pod_lib_1",
			[{{service,"lib_service"},
			  {dir,"/home/pi/erlang/c/source"}}
			]),    
    rpc:call(Computer_1,lib_service,start_tcp_server,["localhost",50001,sequence]),
    D=date(),
    D=rpc:call(node(),tcp_client,call,[{"localhost",50001},{erlang,date,[]}]),
    
    TestPattern=[{ok,{"localhost",50003,pod_lib_3@asus},[]},
		 {ok,{"localhost",50002,pod_lib_2@asus},[]},
		 {ok,{"localhost",50001,pod_lib_1@asus},[]}],

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
		  {ok,{"localhost",50003,pod_lib_3@asus},[]},
		  {ok,{"localhost",50002,pod_lib_2@asus},[]},
		  {ok,{"localhost",50001,pod_lib_1@asus},[]}],

    

    L1=iaas_service:check_all_status(),
    TestL1=[R||{R,_,_}<-L1,R==ok],
    ok=case lists:flatlength(TestL1) of
	   3->
	       ok;
	   _->
	       {"Result of call",L1,"---------------","test pattern",TestPattern1}
       end,

    iaas_service:delete("localhost",5522,node()),
    TestPattern2=[{ok,{"localhost",50003,pod_lib_3@asus},[]},
		  {ok,{"localhost",50002,pod_lib_2@asus},[]},
		  {ok,{"localhost",50001,pod_lib_1@asus},[]}],
    L2=iaas_service:check_all_status(),
    TestL2=[R||{R,_,_}<-L2,R==ok],
    ok=case lists:flatlength(TestL2) of
	   3->
	       ok;
	   _->
	       {"Result of call",L2,"---------------","test pattern",TestPattern2}
       end,
    ok.
