all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s node_controller_service start -sname node_controller_service
test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~ lib_service;
#	cp /home/pi/erlang/d/source/include/*.hrl ".";
	cp -r /home/pi/erlang/d/source/lib_service ".";
	erlc -D local -I /home/pi/erlang/d/source/include -o ebin src/*.erl;
	erlc -D local -I /home/pi/erlang/d/source/include -o test_ebin test_src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s computer_service_tests start -sname test_computer_service
