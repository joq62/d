all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s node_controller_service start -sname node_controller_service
test:
	rm -rf ebin/* src/*~ src/container.erl src/pod.erl src/misc_lib.erl test_ebin/* test_src/*~;
	cp /home/pi/erlang/d/source/lib_service/src/container.erl src;
	cp /home/pi/erlang/d/source/lib_service/src/pod.erl src;
	cp /home/pi/erlang/d/source/lib_service/src/misc_lib.erl src;	
	erlc -D local -I /home/pi/erlang/d/source/include -o ebin src/*.erl;
	erlc -D local -I /home/pi/erlang/d/source/include -o test_ebin test_src/*.erl;
	#cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s boot_service_tests start -sname test_boot_service
