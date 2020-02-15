all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	erl -pa ebin -s controller start -sname controller

test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~;
	cp /home/pi/erlang/d/source/include/*.hrl src;
	cp /home/pi/erlang/d/source/lib_service/src/*.app ebin;
	erlc -D local -I /home/pi/erlang/d/source/include -o ebin /home/pi/erlang/d/source/lib_service/src/*.erl;
	cp /home/pi/erlang/d/source/include/*.hrl test_src;
	erlc -D local -o ebin src/*.erl;
	erlc -D local -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin -s iaas_service_tests start -sname test_iaas_service
