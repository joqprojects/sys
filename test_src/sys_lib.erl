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
    do_a_test(1).
do_a_test(0)->
    ok;
do_a_test(N)->
    if_dns:call("controller",{controller,add,["mymath","1.0.0"]},{"localhost",60010}),  
    Self=self(),
    _Pid=spawn(fun()->do_add(false,Self) end),
    receive
	{_,R}->
	    io:format("  ~p~n",[{?MODULE,?LINE,R}])
    end,
    if_dns:call("controller",{controller,remove,["mymath","1.0.0"]},{"localhost",60010}), 
    do_a_test(N-1).


do_add(true,Parent)->
    Parent!{self(),[?MODULE,?LINE,ok]};
do_add(Quit,Parent) ->
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    R1=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},1,1),
    case R1 of
	{error,_}->
	    io:format(" R1 ~p~n",[{?MODULE,?LINE,R1}]);
	_->
	%    [R11]=R1,
	    io:format(" R1 ~w~n",[{?MODULE,?LINE,R1}])
    end,	    
    R2=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},2,2),
    case R2 of
	{error,_}->
	    io:format(" R2 ~p~n",[{?MODULE,?LINE,R2}]);
	_->
	   % [R21,R22]=R2,
	    io:format(" R2 ~w~n",[{?MODULE,?LINE,R2}])
    end,
    R3=l_dns_2_call("adder","1.0.0",{adder,add,[20,22]},{"localhost",60010},2,1),
    case R3 of
	{error,_}->
	    io:format(" R3 ~p~n",[{?MODULE,?LINE,R3}]);
	_->
	   % [R31]=R3,
	    io:format(" R3 ~w~n",[{?MODULE,?LINE,R3}])
    end,
    R4=l_dns_2_call("adder",latest,{adder,add,[20,22]},{"localhost",60010},2,1),
    case R4 of
	{error,_}->
	    io:format(" R4 ~p~n",[{?MODULE,?LINE,R4}]);
	_->
	    %[R41]=R4,
	    io:format(" R4 ~w~n",[{?MODULE,?LINE,R4}])
    end,
    R5=l_dns_2_call("adder",latest,{glurk,add,[20,22]},{"localhost",60010},2,1),
    io:format(" R5 ~p~n",[{?MODULE,?LINE,R5}]),
    R6=l_dns_2_call("glurk_2",latest,{adder,add,[20,22]},{"localhost",60010},2,1),
    io:format(" R6 ~p~n",[{?MODULE,?LINE,R6}]),   
    R7=l_dns_2_call("adder",latest,{adder,add,[20,22]},{"localhost",60010},5,1),
    case R7 of
	{error,_}->
	    io:format(" R7 ~p~n",[{?MODULE,?LINE,R7}]);
	_->
	 %   [R71]=R7,
	    io:format(" R7 ~w~n",[{?MODULE,?LINE,R7}])
    end,
    R8=l_dns_2_call("adder",latest,{adder,add,[20,22]},{"localhost",60010},2,5),
    case R8 of
	{error,_}->
	    io:format(" R8 ~p~n",[{?MODULE,?LINE,R8}]);
	
	_->
	   % [R81,R82]=R8,
	    io:format(" R8 ~w~n",[{?MODULE,?LINE,R8}])
    end,
    case R1 of
	[42]->
	    io:format(">>>>>>>>>>>>>>>   Success -Erika it's working now/almost  !!!!!! Kram Paps <<<<<<<<<<<<<<<<<<<<  ~n"),	    
	    NewQuit=true;
	_->
	   io:format(">>>>>  ~p~n",[{date(),time()}]),
     
	    timer:sleep( 5*1000),
	    NewQuit=Quit
    end,
    do_add(NewQuit,Parent).


    
l_dns_2_call(ServiceId,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec)->
    Diff=InitRec-Send,
    if	
	Diff > 0 ->
	    Rec=Send;
	true->
	    Rec=InitRec
    end,
    Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId]}) of
	       {error,Err}->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[?MODULE,?LINE,Err]};
	       []->
		   io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId}]),
		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId]};
	       InstancesDnsInfo->
	%	   io:format("  ~p~n",[{?MODULE,?LINE,InstancesDnsInfo}]),
		   Parent=self(),
		   P=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,[]) end),
		   R=receive
			 {P,R}->
			     R
		     after 15*1000->
			     R={error,[?MODULE,?LINE,'timeout',ServiceId]}
		     end
	   end,
    Result.
    
l_dns_2_call(ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec)->
    Diff=InitRec-Send,
    if	
	Diff > 0 ->
	    Rec=Send;
	true->
	    Rec=InitRec
    end,
    Result=case tcp:call(DnsIpAddr,DnsPort,{dns,get_instances,[ServiceId,Vsn]}) of
	       {error,Err}->
		%   io:format(" Error ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[?MODULE,?LINE,Err]};
	       []->
		 %  io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn}]),
		   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
	       InstancesDnsInfo->
	%	   io:format("  ~p~n",[{?MODULE,?LINE,InstancesDnsInfo}]),
		   Parent=self(),
		   P=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,[]) end),
		   R=receive
			 {P,R}->
			     R
		     after 15*1000->
			     R={error,[?MODULE,?LINE,'timeout',ServiceId,Vsn]}
		     end
	   end,
    Result. 

l_tcp_2_call([],{M,F,A},Parent,Send,Rec,PidList)->
    R=rec_2_call(PidList,Rec,[]),
    %Parent!{self(),R};
  %  io:format(" Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',{M,F,A},Parent,Send,Rec,PidList}]),
  %  R={error,[?MODULE,?LINE,'no availible nodes ',{M,F,A},Parent,Send,Rec,Acc]},
    Parent!{self(),R};

l_tcp_2_call(_,_,Parent,0,N_rec,PidList)->
    R=rec_2_call(PidList,N_rec,[]),
    Parent!{self(),R};

l_tcp_2_call([DnsInfo|T],{M,F,A},Parent,Send,Rec,Acc)->
    IpAddr_Service=DnsInfo#dns_info.ip_addr,
    Port_Service=DnsInfo#dns_info.port,
 %   io:format("  ~p~n",[{?MODULE,?LINE,M,F,A,IpAddr_Service,Port_Service}]),
    Parent2=self(),
    Pid=spawn_link(fun()->do_tcp_2_call(IpAddr_Service,Port_Service,{M,F,A},Parent2) end),
    NewAcc=[Pid|Acc],
 %   io:format("~p~n",[{?MODULE,?LINE,T,{M,F,A},Parent,Send,Rec,Acc}]),
 %   Instances=lists:join(T,[DnsInfo]),
    l_tcp_2_call(T,{M,F,A},Parent,Send-1,Rec,NewAcc).

do_tcp_2_call(IpAddr,Port,{M,F,A},Parent2)->
%     io:format("  ~p~n",[{?MODULE,?LINE,M,F,A,IpAddr,Port}]),
%    timer:sleep(100),
    R=tcp:call(IpAddr,Port,{M,F,A},5*1000),
  %  io:format("  ~p~n",[{?MODULE,?LINE,R}]),
    Parent2!{self(),R}.

rec_2_call(PidList,0,Acc)->
   % [erlang:exit(Pid,die)||Pid<-PidList],
    Acc;
rec_2_call(PidList,Num,Acc)->
    receive
	{_Pid,Result}->
	    NewAcc=[Result|Acc]
    after 6*1000 ->
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
