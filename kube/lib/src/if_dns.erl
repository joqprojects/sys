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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

call(ServiceStr,M,F,A,SenderInfo)->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A},SenderInfo]),
    Reply=case ServiceStr of
	      "dns"->
		  tcp:call(?DNS_IP,?DNS_PORT,{M,F,A},SenderInfo);
	      _->
		  Instances=tcp:call(?DNS_IP,?DNS_PORT,{dns,get_instances,[ServiceStr]},SenderInfo),
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

call(ServiceStr,M,F,A)->
   % io:format(" ~p~n",[{?MODULE,?LINE,ServiceStr,M,F,A}]),
    Reply=case ServiceStr of
	      "dns"->
		  rpc:call(node(),tcp,call,[?DNS_IP,?DNS_PORT,{M,F,A}]);
	      _->
		  Instances=rpc:call(node(),tcp,call,[?DNS_IP,?DNS_PORT,{dns,get_instances,[ServiceStr]}]),
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

