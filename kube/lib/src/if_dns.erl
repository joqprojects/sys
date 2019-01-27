%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(if_dns).

% 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/include/dns.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/dns_data.hrl").
%% --------------------------------------------------------------------



%% External exports
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

call(ServiceId,{M,F,A},{DnsIp,DnsPort})->
 %    io:format("old,calll need to be changed ~p~n",[{?MODULE,?LINE,ServiceId,M,F,A}]),
    Vsn=latest,
    TimeOut=?TIMEOUT_TCPCLIENT,
    Send=1,
    InitRec=1,
    call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,InitRec,TimeOut).


call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,InitRec)->    
 % io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,M,F,A}]),
    call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,InitRec,?TIMEOUT_TCPCLIENT).

call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,InitRec,TimeOut)->
  %  io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,InitRec,TimeOut}]),
    Diff=InitRec-Send,
    if	
	Diff > 0 ->
	    Rec=Send;
	true->
	    Rec=InitRec
    end,
    Result=case ServiceId of
	      "dns"->
		  tcp:call(DnsIp,DnsPort,{M,F,A},TimeOut);
	      _->
		  case tcp:call(DnsIp,DnsPort,{dns,get_instances,[ServiceId,Vsn]},TimeOut) of
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err]};
		      []->
			  {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
		      InstancesDnsInfo->
			  Parent=self(),
			  Pid=spawn(fun()->l_tcp_2_call(InstancesDnsInfo,{M,F,A},Parent,Send,Rec,TimeOut,[]) end),
			  rec_result(Pid)		   
		  end
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


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

glurk_call(ServiceStr,{M,F,A},{DnsIp,DnsPort},SenderInfo)->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A},SenderInfo]),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:call(DnsIp,DnsPort,{M,F,A},SenderInfo);
	      _->
		  Instances=tcp:call(DnsIp,DnsPort,{dns,get_instances,[ServiceStr]},SenderInfo),
		  case Instances of
		      []->
			  {error,[?MODULE,?LINE,'no service found',ServiceStr,SenderInfo]};
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err,SenderInfo]};
		      %->
		      [DnsInfo|_]->
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:call(IpAddr,Port,{M,F,A},SenderInfo)
		  end
	  end,
    Reply.

glurk_call(ServiceStr,{M,F,A},{DnsIp,DnsPort})->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Reply=case ServiceStr of
	      "dns"->
		  rpc:call(node(),tcp,call,[DnsIp,DnsPort,{M,F,A}]);
	      _->
		  Instances=rpc:call(node(),tcp,call,[DnsIp,DnsPort,{dns,get_instances,[ServiceStr]}]),
		  case Instances of
		      []->
			  {error,[?MODULE,?LINE,'no service found',ServiceStr]};
		      {error,Err}->
			  {error,[?MODULE,?LINE,Err]};
		      %->
		      [DnsInfo|_]->
			  IpAddr=DnsInfo#dns_info.ip_addr,
			  Port=DnsInfo#dns_info.port,
			  tcp:call(IpAddr,Port,{M,F,A})
		  end
	  end,
    Reply.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

