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

call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort})->    
 % io:format(" ~p~n",[{?MODULE,?LINE,ServiceId,M,F,A}]),
    call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},?TIMEOUT_TCPCLIENT).

call(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort},TimeOut)->
    Result=case ServiceId of
	       "dns"->
		   tcp:test_call([{DnsIp,DnsPort}],{M,F,A},TimeOut);
	       _->
		   case tcp:test_call([{DnsIp,DnsPort}],{dns,get_instances,[ServiceId,Vsn]}) of
		       {error,Err}->
			   io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
			   {error,[?MODULE,?LINE,Err]};
		       []->
			   %io:format("Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn}]),
			   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
		       {badrpc,Err}->
			   io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
			   {error,[?MODULE,?LINE,Err]};
		       InstancesDnsInfo->
			   Addresses=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-InstancesDnsInfo],
			   tcp:test_call(Addresses,{M,F,A},TimeOut)
		 
		  end
	   end,
    Result. 

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
cast(ServiceId,Vsn,{M,F,A},{DnsIp,DnsPort})->
    Result=case ServiceId of
	"dns"->
		   tcp:test_cast([{DnsIp,DnsPort}],{M,F,A});
	       _->
		   case tcp:test_call([{DnsIp,DnsPort}],{dns,get_instances,[ServiceId,Vsn]}) of
		       {error,Err}->
			   {error,[?MODULE,?LINE,Err]};
		       []->
			   io:format("Error ~p~n",[{?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn}]),
			   {error,[?MODULE,?LINE,'no availible nodes ',ServiceId,Vsn]};
		       {badrpc,Err}->
			   io:format("Error ~p~n",[{?MODULE,?LINE,Err}]),
			  {error,[?MODULE,?LINE,Err]};
		       InstancesDnsInfo->
			   Addresses=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port}||DnsInfo<-InstancesDnsInfo],
			   tcp:test_cast(Addresses,{M,F,A})
		   end
	   end,
    Result. 
