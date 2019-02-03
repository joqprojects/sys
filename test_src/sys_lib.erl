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
demo(N)->
    if_dns:call("controller",latest,{controller,add,["mymath","1.0.0"]},{"80.216.3.159",60000}),  
    demo_loop(N).

demo_loop(N)->
    TimeOut=5*1000,
    case if_dns:call("dns",latest,{dns,get_instances,["adder","1.0.0"]},{"80.216.3.159",60000},TimeOut) of
	{error,_}=X ->
	    NewN=N,
	    io:format("Error no service availible = ~p~n",[{X}]);
	Instances->
	    Addresses=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-Instances],
	    case tcp:test_call(Addresses,{adder,add,[N,1000]},TimeOut) of
		noresult->
		    NewN=N,
		    io:format("Error no service availible ~n");
		R->
		    NewN=N+1,
		    io:format("N+1000 = ~p~n",[R])
	    end
    end,	   
    timer:sleep(3000),
    demo_loop(NewN).
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

do_c_test()->
    do_c_test(1).
do_c_test(0)->
    ok;
do_c_test(N)->
    if_dns:call("controller",latest,{controller,add,["mymath","1.0.0"]},{"80.216.3.159",60000}),  
    c_loop(N),
    if_dns:call("controller",latest,{controller,remove,["mymath","1.0.0"]},{"80.216.3.159",60000}), 
    do_c_test(0).

c_loop(0)->
    ok;
c_loop(N)->
    Self=self(),
    _Pid=spawn(fun()->do_add(false,Self) end),
    receive
	{_,R}->
	    io:format("  ~p~n",[{?MODULE,?LINE,R}])
    end,
    timer:sleep(2*1000),
    c_loop(N-1).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



do_b_test()->
    do_b_test(1).
do_b_test(0)->
    ok;
do_b_test(N)->
    if_dns:call("controller",latest,{controller,add,["mymath","1.0.0"]},{"80.216.3.159",60000}),  
    Self=self(),
    _Pid=spawn(fun()->do_add(false,Self) end),
    receive
	{_,R}->
	    io:format("  ~p~n",[{?MODULE,?LINE,R}])
    end,
    timer:sleep(1*10),
    if_dns:call("controller",latest,{controller,remove,["mymath","1.0.0"]},{"80.216.3.159",60000}), 
 %   timer:sleep(1*1000),
    do_b_test(N-1).


do_add(true,Parent)->
    Parent!{self(),[?MODULE,?LINE,ok]};
do_add(Quit,Parent) ->
    TimeOut=5*1000,
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    Instances=if_dns:call("dns",latest,{dns,get_instances,["adder","1.0.0"]},{"80.216.3.159",60000},TimeOut),
    io:format("Instances ~p~n",[{?MODULE,?LINE,Instances}]),
    case Instances of
	{error,Err}->
	    R1={error,Err};
	_->
	    Addresses1=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-Instances],
	    io:format("Addresses ~p~n",[{?MODULE,?LINE,Addresses1}]),
	    R1=tcp:test_call(Addresses1,{adder,add,[20,22]},TimeOut)
						%  R1=if_dns:call("adder","1.0.0",{adder,add,[20,22]},{"80.216.3.159",60000},TimeOut),
    end,
    case R1 of
	{error,_}->
	    io:format(" R1 ~p~n",[{?MODULE,?LINE,R1}]);
	_->
	%    [R11]=R1,
	    io:format(" R1 ~w~n",[{?MODULE,?LINE,R1}])
    end,
    R5=case if_dns:call("dns",latest,{dns,get_instances,["adder","1.0.0"]},{"80.216.3.159",60000},TimeOut) of
	   {error,_}=X ->
	       X;
	   _->
	       Addresses=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-Instances],
	       tcp:test_call(Addresses,{adder,glurk,[20,22]},TimeOut)
       end,	    
    io:format(" R5 ~p~n",[{?MODULE,?LINE,R5}]),
    R6=case if_dns:call("dns",latest,{dns,get_instances,["glurk_2","1.0.0"]},{"80.216.3.159",60000},TimeOut) of
	   {error,Err}=Y->
	       Y;
	   _->
	       Addresses2=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-Instances],
	       tcp:test_call(Addresses2,{adder,add,[20,22]},TimeOut)
       end,	
    io:format(" R6 ~p~n",[{?MODULE,?LINE,R6}]),   
    R7=case if_dns:call("dns",latest,{dns,get_instances,["adder","1.0.0"]},{"80.216.3.159",60000},TimeOut) of
	   {error,Err}=Z->
	       Z;
	   _->
	       Addresses=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-Instances],
	       tcp:test_call(Addresses,{adder,add,[glurk,22]},TimeOut)
       end,	
    io:format(" R7 ~p~n",[{?MODULE,?LINE,R7}]),   

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
