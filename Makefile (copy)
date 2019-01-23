## set the paths for a default setup
system:
	rm -rf test_ebin/* test_src/*~;
	erlc -o test_ebin test_src/*.erl;
	erl -pa test_ebin -pa ebin/lib/ebin -s test_system test -sname test_system;
clean:
	rm -rf ebin/*/ebin/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/nfvi/*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
build:
	rm -rf ebin/*/ebin/*;
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
	rm -rf services/*/src/*~ services/*/test_src/*~ services/*/*.dump services/*~ services/*/*~;
	rm -rf test/*/kubelet_ebin/* test/*/service_ebin/* test/*/lib_ebin/*  ;
#lib
	erlc -o ebin/lib/ebin kube/lib/src/*.erl;
	cp kube/lib/src/*.app ebin/lib/ebin;
	cp kube/lib/src/*.josca ebin/lib/ebin;
# dns
	erlc -o ebin/dns/ebin kube/dns/src/*.erl;
#	erlc -o test/test_ebin services/dns/test_src/*.erl;
	cp kube/dns/src/*.app ebin/dns/ebin;
	cp kube/dns/src/*.josca ebin/dns/ebin;
# log
	erlc -o ebin/log/ebin kube/log/src/*.erl;
#	erlc -o test/test_ebin services/log/test_src/*.erl;
	cp kube/log/src/*.app ebin/log/ebin;
	cp kube/log/src/*.josca ebin/log/ebin;
# controller
	erlc -o ebin/controller/ebin kube/controller/src/*.erl;
	erlc -o test/test_ebin kube/controller/test_src/*.erl;
	cp kube/controller/src/*.app ebin/controller/ebin;
	cp kube/controller/src/*.josca ebin/controller/ebin;
#etcd
	erlc -o ebin/etcd/ebin kube/etcd/src/*.erl;
	cp kube/etcd/src/*.app ebin/etcd/ebin;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/etcd/src/* ebin/etcd/ebin;
#catalog
	erlc -o ebin/catalog/ebin kube/catalog/src/*.erl;
	cp kube/catalog/src/*.app ebin/catalog/ebin;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/catalog/src/* ebin/catalog/ebin;
#repository
	erlc -o ebin/repository/ebin kube/repository/src/*.erl;
	cp kube/repository/src/*.app ebin/repository/ebin;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/repository/src/* ebin/repository/ebin;
#kubelet
	erlc -o ebin/kubelet/ebin kube/kubelet/src/*.erl;
	cp kube/kubelet/src/*.app ebin/kubelet/ebin;
#	cp kube/kubelet/src/*.josca ebin/kubelet/ebin;
	cp kube/kubelet/src/* ebin/kubelet/ebin;
# Services for ebin
#adder_100
	erlc -o ebin/adder_100/ebin services/adder_100/src/*.erl;
	cp services/adder_100/src/*.app ebin/adder_100/ebin;
	cp services/adder_100/src/*.josca ebin/adder_100/ebin;
#subtract_100
	erlc -o ebin/subtract_100/ebin services/subtract_100/src/*.erl;
	cp services/subtract_100/src/*.app ebin/subtract_100/ebin;
	cp services/subtract_100/src/*.josca ebin/subtract_100/ebin;
#divider_100
	erlc -o ebin/divider_100/ebin services/divider_100/src/*.erl;
	cp services/divider_100/src/*.app ebin/divider_100/ebin;
	cp services/divider_100/src/*.josca ebin/divider_100/ebin;
#multi_100
	erlc -o ebin/multi_100/ebin services/multi_100/src/*.erl;
	cp services/multi_100/src/*.app ebin/multi_100/ebin;
	cp services/multi_100/src/*.josca ebin/multi_100/ebin;
#
#	erlc -o ebin/calc/ebin services/calc/src/*.erl;
#	cp services/calc/src/*.app ebin/calc/ebin;
#	cp services/calc/src/*.josca ebin/calc/ebin;
#	cp services/calc/src/* ebin/calc/ebin;
#	cp ebin/common/ebin/* ebin/calc/ebin;
# Workers for test
#Controller
	cp ebin/kubelet/ebin/* test/controller/kubelet_ebin;
	cp ebin/controller/ebin/* test/controller/service_ebin;
	cp ebin/lib/ebin/* test/controller/lib_ebin;
#etcd
	cp ebin/kubelet/ebin/* test/etcd/kubelet_ebin;
	cp ebin/etcd/ebin/* test/etcd/service_ebin;
#repository
	cp ebin/kubelet/ebin/* test/repository/kubelet_ebin;
	cp ebin/lib/ebin/* test/repository/lib_ebin;
	cp ebin/repository/ebin/* test/repository/service_ebin;
#catalog
	cp ebin/kubelet/ebin/* test/catalog/kubelet_ebin;
	cp ebin/lib/ebin/* test/catalog/lib_ebin;
	cp ebin/catalog/ebin/* test/catalog/service_ebin;
#dns
	cp ebin/kubelet/ebin/* test/dns/kubelet_ebin;
	cp ebin/lib/ebin/* test/dns/lib_ebin;
	cp ebin/dns/ebin/* test/dns/service_ebin;
#workers
	cp ebin/kubelet/ebin/* test/w1/kubelet_ebin;
	cp ebin/lib/ebin/* test/w1/lib_ebin;
	cp ebin/kubelet/ebin/* test/w2/kubelet_ebin;
	cp ebin/lib/ebin/* test/w2/lib_ebin;
	cp ebin/kubelet/ebin/* test/w3/kubelet_ebin;
	cp ebin/lib/ebin/* test/w3/lib_ebin;
	cp ebin/kubelet/ebin/* test/w4/kubelet_ebin;
	cp ebin/lib/ebin/* test/w4/lib_ebin;
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
	rm -rf ebin/*/ebin/*; 
	rm -rf kube/*/src/*~ kube/*/test_src/*~ kube/*/*.dump kube/*~ kube/*/*~;
#	rm -rf test/*//*.dump  test/*/nfvi/ebin/* test/test_ebin/* test/*/nfvi/*~;
	erlc -o ebin/common/ebin kube/common/src/*.erl;
	erlc -o test/test_ebin kube/common/test_src/*.erl;
	erlc -o ebin/kubelet/ebin kube/kubelet/src/*.erl;
	erlc -o test/test_ebin kube/kubelet/test_src/*.erl;
	cp kube/kubelet/src/*.app ebin/kubelet/ebin;
	cp ebin/common/ebin/* ebin/kubelet/ebin;
	erl -pa test/test_ebin -pa ebin/kubelet/ebin -s test_kubelet test -sname test_kubelet -setcookie glurk
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
