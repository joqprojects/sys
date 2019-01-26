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
start()->
    application:start(sys),
    ok.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
do_a_test()->

    if_dns:call("controller",{controller,add,["mymath","1.0.0"]},{"localhost",60010}),  
    do_add(false),
    if_dns:call("controller",{controller,remove,["mymath","1.0.0"]},{"localhost",60010}), 
    if_dns:call("controller",{controller,add,["mymath","1.0.0"]},{"localhost",60010}),  
    do_add(false),
    if_dns:call("controller",{controller,remove,["mymath","1.0.0"]},{"localhost",60010}),   
    ok.

do_add(true)->
    ok;
do_add(Quit) ->
    io:format("~p~n",[{?MODULE,?LINE}]),
    R1=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},1,1),
    R2=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},2,2),
    R3=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},2,1),
    R4=l_dns_2_call("adder",latest,{adder,add,[20,22]},{"localhost",60010},2,1),
    R5=l_dns_2_call("adder",latest,{glurk,add,[20,22]},{"localhost",60010},2,1),
    R6=l_dns_2_call("glurk_2",latest,{glurk,add,[20,22]},{"localhost",60010},2,1),
    case R1 of
	[42]->
	    io:format(" Glurk ~p~n",[{?MODULE,?LINE,R1,R2,R3,R4,R5,R6}]),  
	    
	    [R11]=R1,
	    [R21,R22]=R2,
	    [R31]=R3,
	    [R41]=R4,	    
	    io:format(">>>>>>>>>>>>>>>   Success -Erika it's working now/almost  !!!!!! Kram Paps <<<<<<<<<<<<<<<<<<<<  ~n"),
	    io:format(" R11 ~p~n",[{?MODULE,?LINE,R11}]),
	    io:format(" R21,R22 ~p~n",[{?MODULE,?LINE,R21,R22}]),
	    io:format(" R31 ~p~n",[{?MODULE,?LINE,R31}]),    
	    io:format(" R41 ~p~n",[{?MODULE,?LINE,R41}]),  
	    io:format(" R5 ~p~n",[{?MODULE,?LINE,R5}]),  
	    io:format(" R6 ~p~n",[{?MODULE,?LINE,R6}]),  
	    
	    NewQuit=true;
	_->
	   % io:format(" R1 ~p~n",[{?MODULE,?LINE,R1}]),
	   % io:format(" R2 ~p~n",[{?MODULE,?LINE,R2}]),
	   % io:format(" R3 ~p~n",[{?MODULE,?LINE,R3}]),    
	    timer:sleep(20*1000),
	    NewQuit=Quit
    end,
    do_add(NewQuit).


    
l_dns_2_call(ServiceId,{M,F,A},{DnsIpAddr,DnsPort},Send,Rec)->
    Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId]}) of
	       {error,Err}->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[?MODULE,?LINE,Err]};
	       []->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId}]),
		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId]};
	       InstancesDnsInfo->
		   Parent=self(),
		   P=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,[]) end),
		   receive
		       {P,R}->
			   R
		   end
	   end,
    Result.
    
l_dns_2_call(ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,Rec)->
    Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId,Vsn]}) of
	       {error,Err}->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[?MODULE,?LINE,Err]};
	       []->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn}]),
		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
	       InstancesDnsInfo->
		   Parent=self(),
		   P=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,[]) end),
		   receive
		       {P,R}->
			   R
		   end
	   end,
    Result. 

l_tcp_2_call([],{M,F,A},Parent,Send,Rec,Acc)->
    io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',{M,F,A},Parent,Send,Rec,Acc}]),
    R={error,[?MODULE,?LINE,'no availible nodes ',{M,F,A},Parent,Send,Rec,Acc]},
    Parent!{self(),R};

l_tcp_2_call(_,_,Parent,0,N_rec,PidList)->
    R=rec_2_call(PidList,N_rec,[]),
    Parent!{self(),R};

l_tcp_2_call([DnsInfo|T],{M,F,A},Parent,Send,Rec,Acc)->
    IpAddr_Service=DnsInfo#dns_info.ip_addr,
    Port_Service=DnsInfo#dns_info.port,
 %   io:format("  ~p~n",[{?MODULE,?LINE,M,F,A,IpAddr_Service,Port_Service}]),
    Parent2=self(),
    Pid=spawn(fun()->do_tcp_2_call(IpAddr_Service,Port_Service,{M,F,A},Parent2) end),
    NewAcc=[Pid|Acc],
    l_tcp_2_call(T,{M,F,A},Parent,Send-1,Rec,NewAcc).

do_tcp_2_call(IpAddr,Port,{M,F,A},Parent2)->
%     io:format("  ~p~n",[{?MODULE,?LINE,M,F,A,IpAddr,Port}]),
%    timer:sleep(100),
    R=tcp:call(IpAddr,Port,{M,F,A}),
  %  io:format("  ~p~n",[{?MODULE,?LINE,R}]),
    Parent2!{self(),R}.

rec_2_call(_,0,Acc)->
    Acc;
rec_2_call(PidList,Num,Acc)->
    receive
	{_Pid,Result}->
	    NewAcc=[Result|Acc]
    after 2000 ->
	    NewAcc=Acc
    end,
    rec_2_call(PidList,Num-1,NewAcc).

%% ----


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
    _R=tcp:call("localhost",60010,{dns,get_all_instances,[]}),
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
	{_Pid,Result}->
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
