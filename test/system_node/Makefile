## set the paths for a default setup
all:
	erlc -o kubelet_ebin src_boot/*.erl;
	erl -pa lib_ebin -pa kubelet_ebin -pa service_ebin -s kubelet start_kubelet -sname system_node -setcookie glurk
