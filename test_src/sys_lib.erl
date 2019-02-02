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
-include("kube/include/tcp.hrl").
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
    do_a_test(1).
do_a_test(0)->
    ok;
do_a_test(N)->
    if_dns:call("controller",latest,{controller,add,["mymath","1.0.0"]},{"localhost",60010}),  
    Self=self(),
    _Pid=spawn(fun()->do_add(false,Self) end),
    receive
	{_,R}->
	    io:format("  ~p~n",[{?MODULE,?LINE,R}])
    end,
    timer:sleep(1*10),
    if_dns:call("controller",latest,{controller,remove,["mymath","1.0.0"]},{"localhost",60010}), 
    do_a_test(N-1).


do_add(true,Parent)->
    Parent!{self(),[?MODULE,?LINE,ok]};
do_add(Quit,Parent) ->
    TimeOut=5*1000,
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    R1=if_dns:call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},TimeOut),
    case R1 of
	{error,_}->
	    io:format(" R1 ~p~n",[{?MODULE,?LINE,R1}]);
	_->
	%    [R11]=R1,
	    io:format(" R1 ~w~n",[{?MODULE,?LINE,R1}])
    end,	    
    R5=if_dns:call("adder",latest,{glurk,add,[20,22]},{"localhost",60010},TimeOut),
    io:format(" R5 ~p~n",[{?MODULE,?LINE,R5}]),
    R6=if_dns:call("glurk_2",latest,{adder,add,[20,22]},{"localhost",60010},TimeOut),
    io:format(" R6 ~p~n",[{?MODULE,?LINE,R6}]),   
    R7=if_dns:call("adder",latest,{adder,add,[20,22]},{"localhost",60010},TimeOut),
    case R7 of
	{error,_}->
	    io:format(" R7 ~p~n",[{?MODULE,?LINE,R7}]);
	_->
	 %   [R71]=R7,
	    io:format(" R7 ~w~n",[{?MODULE,?LINE,R7}])
    end,
    case R1 of
	42->
	    io:format(">>>>>>>>>>>>>>>   Success -Erika it's working now/almost  !!!!!! Kram Paps <<<<<<<<<<<<<<<<<<<<  ~n"),	    
	    NewQuit=true;
	_->
	   io:format(">>>>>  ~p~n",[{date(),time()}]),
     
	    timer:sleep( 5*1000),
	    NewQuit=Quit
    end,
    do_add(NewQuit,Parent).

%%----------------- Solution
%% Observations: When calling exisitng service with undef function sometimes no response is genrated
%% Timeout is used to sort up that
%l_dns_2_call(ServiceId,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec)->    
%  l_dns_2_call(ServiceId,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec,?TIMEOUT_TCPCLIENT).  

%l_dns_2_call(ServiceId,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec,TimeOut)->
%    Diff=InitRec-Send,
 %   if	
%	Diff > 0 ->
%	    Rec=Send;
%	true->
%	    Rec=InitRec
 %   end,
  %  Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId]},TimeOut) of
%	       {error,Err}->
%		   io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
%		   {error,[?MODULE,?LINE,Err]};
%	       []->
%		   io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId}]),
%		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId]};
%	       InstancesDnsInfo->
%		   Parent=self(),
%		   Pid=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,TimeOut,[]) end),
%		   rec_result(Pid)
%	   end,
%    Result.
    
l_dns_2_call(ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec)->    
  l_dns_2_call(ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec,?TIMEOUT_TCPCLIENT).

l_dns_2_call(ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec,TimeOut)->
    Diff=InitRec-Send,
    if	
	Diff > 0 ->
	    Rec=Send;
	true->
	    Rec=InitRec
    end,
    Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId,Vsn]},TimeOut) of
	       {error,Err}->
		   {error,[?MODULE,?LINE,Err]};
	       []->
		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
	       InstancesDnsInfo->
		   Parent=self(),
		   Pid=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,TimeOut,[]) end),
		   rec_result(Pid)		   
	   end,
    Result. 

rec_result(Pid)->
    Result=receive
	       {Pid,R}->
		   R;
	       Err ->
		   rec_result(Pid)    
	   end,
    Result.
    
l_tcp_2_call([],{M,F,A},Parent,Send,Rec,TimeOut,PidList)->
    Result=rec_2_call(PidList,Rec,TimeOut,[]),
    Parent!{self(),Result};

l_tcp_2_call(_,_,Parent,0,Rec,TimeOut,PidList)->
    Result=rec_2_call(PidList,Rec,TimeOut,[]),
    Parent!{self(),Result};

l_tcp_2_call([DnsInfo|T],{M,F,A},Parent,Send,Rec,TimeOut,Acc)->
    IpAddr_Service=DnsInfo#dns_info.ip_addr,
    Port_Service=DnsInfo#dns_info.port,
    Parent2=self(),
    Pid=spawn_link(fun()->do_tcp_2_call(IpAddr_Service,Port_Service,{M,F,A},Parent2,TimeOut) end),
    NewAcc=[Pid|Acc],
    l_tcp_2_call(T,{M,F,A},Parent,Send-1,Rec,TimeOut,NewAcc).

do_tcp_2_call(IpAddr,Port,{M,F,A},Parent2,TimeOut)->
    Result=tcp:call(IpAddr,Port,{M,F,A},TimeOut), 
    Parent2!{self(),Result}.

rec_2_call(_PidList,0,_,Acc)->
    Acc;
rec_2_call(PidList,Num,TimeOut,Acc)->
    receive
	{_Pid,Result}->
	    NewAcc=[Result|Acc]
    after TimeOut ->
	    NewAcc=Acc
    end,
    rec_2_call(PidList,Num-1,TimeOut,NewAcc).

%% ****************************************************************************************************'


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
