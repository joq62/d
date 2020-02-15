test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~;
	pwd;
	erlc -I src -o ebin src/*.erl;
	erlc -I src -o test_ebin test_src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s unit_test_adder_service test -sname test_adder_service
