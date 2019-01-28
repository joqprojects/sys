## set the paths for a default setup
system:
	rm -rf test_ebin/* test_src/*~;
	erlc -o test_ebin test_src/*.erl;
	cp test_src/*.app test_ebin;
	erl -pa test_ebin -pa ebin/lib -s sys_lib start -sname test_system;
clean:
	rm -rf ebin/*/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/kubelet_ebin/* test/*/service_ebin/* test/*/lib_ebin/*  ;
build:
	rm -rf ebin/*/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/kubelet_ebin/* test/*/service_ebin/* test/*/lib_ebin/*  ;
#lib
	erlc -o ebin/lib kube/lib/src/*.erl;
	cp kube/lib/src/*.app ebin/lib;
	cp kube/lib/src/*.josca ebin/lib;
# dns
	erlc -o ebin/dns kube/dns/src/*.erl;
	cp kube/dns/src/*.app ebin/dns;
	cp kube/dns/src/*.josca ebin/dns;
# log
	erlc -o ebin/log kube/log/src/*.erl;
	cp kube/log/src/*.app ebin/log;
	cp kube/log/src/*.josca ebin/log;
# controller
	erlc -o ebin/controller kube/controller/src/*.erl;
	erlc -o test_ebin kube/controller/test_src/*.erl;
	cp kube/controller/src/*.app ebin/controller;
	cp kube/controller/src/*.josca ebin/controller;
#etcd
	erlc -o ebin/etcd kube/etcd/src/*.erl;
	cp kube/etcd/src/*.app ebin/etcd;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/etcd/src/* ebin/etcd;
#catalog
	erlc -o ebin/catalog kube/catalog/src/*.erl;
	cp kube/catalog/src/*.app ebin/catalog;
	cp kube/catalog/src/* ebin/catalog;
#repository
	erlc -o ebin/repository kube/repository/src/*.erl;
	cp kube/repository/src/*.app ebin/repository;
	cp kube/repository/src/* ebin/repository;
#kubelet
	erlc -o ebin/kubelet kube/kubelet/src/*.erl;
	cp kube/kubelet/src/*.app ebin/kubelet;
	cp kube/kubelet/src/* ebin/kubelet;
# Services for ebin
#adder_100
	erlc -o ebin/adder_100 services/adder_100/src/*.erl;
	cp services/adder_100/src/*.app ebin/adder_100;
	cp services/adder_100/src/*.josca ebin/adder_100;
#subtract_100
	erlc -o ebin/subtract_100 services/subtract_100/src/*.erl;
	cp services/subtract_100/src/*.app ebin/subtract_100;
	cp services/subtract_100/src/*.josca ebin/subtract_100;
#divider_100
	erlc -o ebin/divider_100 services/divider_100/src/*.erl;
	cp services/divider_100/src/*.app ebin/divider_100;
	cp services/divider_100/src/*.josca ebin/divider_100;
#multi_100
	erlc -o ebin/multi_100 services/multi_100/src/*.erl;
	cp services/multi_100/src/*.app ebin/multi_100;
	cp services/multi_100/src/*.josca ebin/multi_100;
#
#	erlc -o ebin/calc/ebin services/calc/src/*.erl;
#	cp services/calc/src/*.app ebin/calc/ebin;
#	cp services/calc/src/*.josca ebin/calc/ebin;
#	cp services/calc/src/* ebin/calc/ebin;
#	cp ebin/common/ebin/* ebin/calc/ebin;
# Workers for test
#Controller
	cp ebin/kubelet/* test/controller/kubelet_ebin;
	cp ebin/controller/* test/controller/service_ebin;
	cp ebin/lib/* test/controller/lib_ebin;
#etcd
	cp ebin/kubelet/* test/etcd/kubelet_ebin;
	cp ebin/etcd/* test/etcd/service_ebin;
#repository
	cp ebin/kubelet/* test/repository/kubelet_ebin;
	cp ebin/lib/* test/repository/lib_ebin;
	cp ebin/repository/* test/repository/service_ebin;
#catalog
	cp ebin/kubelet/* test/catalog/kubelet_ebin;
	cp ebin/lib/* test/catalog/lib_ebin;
	cp ebin/catalog/* test/catalog/service_ebin;
#dns
	cp ebin/kubelet/* test/dns/kubelet_ebin;
	cp ebin/lib/* test/dns/lib_ebin;
	cp ebin/dns/* test/dns/service_ebin;
#w10 + dns
	cp ebin/kubelet/* test/w10/kubelet_ebin;
	cp ebin/lib/* test/w10/lib_ebin;
	cp ebin/dns/* test/w10/kubelet_ebin;
#w11 + controller
	cp ebin/kubelet/* test/w11/kubelet_ebin;
	cp ebin/lib/* test/w11/lib_ebin;
	cp ebin/controller/* test/w11/kubelet_ebin;
#w100 + repository
	cp ebin/kubelet/* test/w100/kubelet_ebin;
	cp ebin/lib/* test/w100/lib_ebin;
	cp ebin/repository/* test/w100/kubelet_ebin;
#w101 + catalog
	cp ebin/kubelet/* test/w101/kubelet_ebin;
	cp ebin/lib/* test/w101/lib_ebin;
	cp ebin/catalog/* test/w101/kubelet_ebin;
#w200 
	cp ebin/kubelet/* test/w200/kubelet_ebin;
	cp ebin/lib/* test/w200/lib_ebin;
#w201 
	cp ebin/kubelet/* test/w201/kubelet_ebin;
	cp ebin/lib/* test/w201/lib_ebin;
#w202 
	cp ebin/kubelet/* test/w202/kubelet_ebin;
	cp ebin/lib/* test/w202/lib_ebin;
#system_node
	cp ebin/kubelet/* test/system_node/kubelet_ebin;
	cp ebin/lib/* test/system_node/lib_ebin;
	cp ebin/dns/* test/system_node/kubelet_ebin;
	cp ebin/repository/* test/system_node/kubelet_ebin;
	cp ebin/catalog/* test/system_node/kubelet_ebin;
#workers
	cp ebin/kubelet/* test/w1/kubelet_ebin;
	cp ebin/lib/* test/w1/lib_ebin;
	cp ebin/kubelet/* test/w2/kubelet_ebin;
	cp ebin/lib/* test/w2/lib_ebin;
	cp ebin/kubelet/* test/w3/kubelet_ebin;
	cp ebin/lib/* test/w3/lib_ebin;
	cp ebin/kubelet/* test/w4/kubelet_ebin;
	cp ebin/lib/* test/w4/lib_ebin;
# master worker
#	cp test/w1/node/ebin/* test/w_master/node/ebin;
#	cp ebin/dns/ebin/* test/w_master/node/ebin;
#	cp ebin/repo/ebin/* test/w_master/node/ebin;
#	cp ebin/vim/ebin/* test/w_master/node/ebin;
#	cp ebin/nfv_mgr/ebin/* test/w_master/node/ebin;
#	cp ebin/log/ebin/* test/w_master/node/ebin;
	echo ++  build_succeded +++++++++++
start_etcd:
	erl -pa ebin/etcd/ebin -s etcd_lib boot -sname etcd
test_oam:
	cp ebin/common/ebin/* test/test_ebin;
	erl -pa test/test_ebin -sname test_oam
dns_test:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/dns/ebin kube/dns/src/*.erl;
	erlc -o test/test_ebin kube/dns/test_src/*.erl;
	cp kube/dns/src/*.app ebin/dns/ebin;
	cp ebin/common/ebin/* ebin/dns/ebin;
	erl -pa test/test_ebin -pa ebin/dns/ebin -s test_dns test -sname test_dns -setcookie glurk
kubelet_test:
	rm -rf ebin/kubelet/* ebin/dns/*;
	rm -rf test/com_test/*/*.beam  test/com_test/*/*.app;
	erlc -o test/com_test/kubelet kube/lib/src/*.erl;
	cp  kube/lib/src/*.app  test/com_test/kubelet;
	erlc -o test/com_test/kubelet kube/kubelet/src/*.erl;
	cp  kube/kubelet/src/*.app  test/com_test/kubelet;
	erlc -o test/com_test/dns kube/dns/src/*.erl;
	cp  kube/dns/src/*.app  test/com_test/dns;
	cp test/com_test/kubelet/*.beam test/com_test/dns;
	cp test/com_test/kubelet/*.app test/com_test/dns;
	echo ++  build_succeded +++++++++++
adder_test:
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/adder/ebin services/adder/src/*.erl;
	erlc -o test/test_ebin services/adder/test_src/*.erl;
	cp services/adder/src/*.app ebin/adder/ebin;
	cp ebin/common/ebin/* ebin/adder/ebin;
	erl -pa test/test_ebin -pa ebin/adder/ebin -s test_adder test -sname test_adder -setcookie glurk
