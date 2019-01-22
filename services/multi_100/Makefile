## set the paths for a default setup
all:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	cp src/*josca ebin;
	erl -pa ebin -s adder app_start -sname adder
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	cp src/*josca ebin;
	erl -pa ebin -s test_adder test -sname test_adder
