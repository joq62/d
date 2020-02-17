-record(computer_info,{
	  vm_name,
	  vm,
	  ip_addr,
	  port,
	  mode,
	  worker_info_list    %[{ip_addr,Port,Mode},,,,]
	 }).

-define(COMPUTER_INFO_1,{"pod_computer_1",'pod_computer_1@asus',"localhost",40100,parallell,40101,5}).
-define(COMPUTER_INFO_2,{"pod_computer_2",'pod_computer_2@asus',"localhost",40200,parallell,40201,5}).
-define(COMPUTER_INFO_3,{"pod_computer_3",'pod_computer_3@asus',"localhost",40300,parallell,40301,5}).
-define(COMPUTER_LIST,[{computer_1,?COMPUTER_INFO_1},{computer_2,?COMPUTER_INFO_2},{computer_3,?COMPUTER_INFO_3}]).

-define(SOURCE,{dir,"/home/pi/erlang/d/source"}).

-define(ETS,system_ets).
