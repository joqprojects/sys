%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sys_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



do_test_1(0)->
    ok;

do_test_1(N) ->
    DnsInfo=#dns_info{time_stamp="not_initiaded_time_stamp",    % un_loaded, started
		      service_id = "myservice",
		      vsn = "1.0.0",
		      ip_addr="localhost",
		      port=5555},
    tcp:cast("localhost",60010,{dns,dns_register,[DnsInfo]}),
    R=tcp:call("localhost",60010,{dns,get_all_instances,[]}),
    timer:sleep(100),

    R1=l_dns_call("localhost",60010,{dns,get_all_instances,[]},N,N),
    io:format(" R1 ~p~n",[{?MODULE,?LINE,R1}]),
    timer:sleep(100),
    R2=l_dns_call("localhost",60010,{dns,get_all_instances,[]},N,1),
    io:format(" R2 ~p~n",[{?MODULE,?LINE,R2}]),
    R3=l_dns_call("localhost",60010,{dns,get_all_instances,[]},N,0),
    io:format(" R3 ~p~n",[{?MODULE,?LINE,R3}]),
    timer:sleep(1000),
    do_test_1(N-1).


l_dns_call(IpAddr,Port,{M,F,A},Send,Rec)->
    Parent=self(),
    P=spawn(fun()->tcp_call(IpAddr,Port,{M,F,A},Parent,Send,Rec,[]) end),
    receive
	{P,R}->
	    R
    end,
    R.

tcp_call(_,_,_,Parent,0,N_rec,PidList)->
    R=rec_call(PidList,N_rec,[]),
    Parent!{self(),R};
tcp_call(IpAddr,Port,{M,F,A},Parent,N_send,N_rec,Acc)->
    Parent2=self(),
    Pid=spawn(fun()->do_tcp_call(IpAddr,Port,{M,F,A},Parent2) end),
    NewAcc=[Pid|Acc],
    tcp_call(IpAddr,Port,{M,F,A},Parent,N_send-1,N_rec,NewAcc).


do_tcp_call(IpAddr,Port,{M,F,A},Parent2)->
    timer:sleep(100),
    R=tcp:call(IpAddr,Port,{M,F,A}),
    Parent2!{self(),R}.

rec_call(_,0,Acc)->
    Acc;
rec_call(PidList,Num,Acc)->
    receive
	{Pid,Result}->
	    NewAcc=[Result|Acc]
    after 2000 ->
	    NewAcc=Acc
    end,
    rec_call(PidList,Num-1,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
