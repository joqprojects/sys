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
    Rec=1,
    call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,Rec,TimeOut).


call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,Rec)->    
 % io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,M,F,A}]),
    call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,Rec,?TIMEOUT_TCPCLIENT).

call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send,Rec,TimeOut)->
  %  io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,Vsn,{M,F,A},{DnsIpAddr,DnsPort},Send,Rec,TimeOut}]),
    Result=case ServiceId of
	       "dns"->
		   PidList=tcp_call([{DnsIp,DnsPort}],{M,F,A},Send,TimeOut,[]),
		   tcp_rec(PidList,Rec,TimeOut,[]);
	       _->
		   PidList=tcp_call([{DnsIp,DnsPort}],{dns,get_instances,[ServiceId,Vsn]},1,TimeOut,[]),
		   case tcp_rec(PidList,1,TimeOut,[]) of
		       [{error,Err}]->
			   {error,[?MODULE,?LINE,Err]};
		       []->
			  {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
		       [InstancesDnsInfo]->
			   IpAddresses_Ports=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-InstancesDnsInfo],
			   Diff=Rec-Send,
			  ListResults=if	
					  Diff>0 ->
					      Rec=Send,
					      PidList=tcp_call(IpAddresses_Ports,{M,F,A},Send,TimeOut,[]),
					      tcp_rec(PidList,Rec,TimeOut,[]);
					  Diff =:=0 ->
					      PidList=tcp_call(IpAddresses_Ports,{M,F,A},Send,TimeOut,[]),
					      tcp_rec(PidList,Rec,TimeOut,[]);
					  Diff<0 -> 
					      PidList=tcp_call(IpAddresses_Ports,{M,F,A},Send,TimeOut,[]),
					      tcp_rec(PidList,Rec,TimeOut,[])
				      end,
			  ListResults
		  end
	   end,
    Result. 

tcp_call(_,_,0,_,PidList)->
    PidList;
tcp_call([{IpAddr,Port}|T],{M,F,A},Send,TimeOut,Acc) ->
    ClientPid=self(),
    Pid=spawn(tcp,call,[ClientPid,IpAddr,Port,{M,F,A},TimeOut]),
    NewAcc=[Pid|Acc],
    NewT=[T|{IpAddr,Port}],
    tcp_call(NewT,{M,F,A},Send-1,TimeOut,NewAcc). 

tcp_rec(PidList,0,TimeOut,ListResults)->
    clean_up(PidList,TimeOut),
    ListResults;
tcp_rec([],_,_,ListResults) ->
    ListResults;
tcp_rec(PidList,Rec,TimeOut,Acc)->
    receive
	{Pid,result,Result}->
	    NewPidList=lists:delete(Pid,PidList),
	    NewAcc=[Result|Acc],
	    io:format("~p~n",[{?MODULE,?LINE,Result,NewPidList}])
    after TimeOut+2000->
	    NewPidList=PidList,
	    NewAcc=[{error,[?MODULE,?LINE,'timeout']}|Acc]
    end,
    tcp_rec(NewPidList,Rec-1,TimeOut,NewAcc).
	    

clean_up(PidList,TimeOut)->
    ClientPid=self(),
    [Pid!{ClientPid,close}||Pid<-PidList],
    empty_queue(false,TimeOut).

empty_queue(true,_)->
    ok;
empty_queue(false,TimeOut)->
    receive
	{_,_}->
	    Quit=false
    after TimeOut ->
	    Quit=true
    end,
    empty_queue(Quit,TimeOut).


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
cast(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},Send)->
    Result=case ServiceId of
	"dns"->
		   tcp:cast(DnsIp,DnsPort,{M,F,A});
	       _->
		   PidList=tcp_call([{DnsIp,DnsPort}],{dns,get_instances,[ServiceId,Vsn]},1,?TIMEOUT_TCPCLIENT,[]),
		   io:format("~p~n",[{?MODULE,?LINE,PidList}]),
		   InstancesGlurk=tcp_rec(PidList,1,?TIMEOUT_TCPCLIENT,[]),
		   io:format("~p~n",[{?MODULE,?LINE,InstancesGlurk}]),
		   case InstancesGlurk of
		       [{error,Err}]->
			   {error,[?MODULE,?LINE,Err]};
		       [[]]->
			   io:format("Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn}]),
			  {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
		       [InstancesDnsInfo]->
			   io:format("~p~n",[{?MODULE,?LINE,InstancesDnsInfo}]),
			   IpAddresses_Ports=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-InstancesDnsInfo],
			   tcp_cast(IpAddresses_Ports,{M,F,A},Send,[]);		   
		       Err ->
			   io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
			   {error,[?MODULE,?LINE,Err]}
		   end
	   end,
    Result. 


tcp_cast(_,_,0,CastResult)->
    CastResult;
tcp_cast([{IpAddr,Port}|T],{M,F,A},Send,Acc) ->
    R=tcp:cast(IpAddr,Port,{M,F,A}),
    NewAcc=[R|Acc],
    NewT=[T|{IpAddr,Port}],
    tcp_cast(NewT,{M,F,A},Send-1,NewAcc). 
