%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/controller/src/controller_local.hrl").

-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/kubelet_data.hrl").
%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
stop_services([],DnsList,_State)->
    io:format("ok  ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    DnsList;
stop_services([{ServiceId,Vsn}|T],DnsList,State)->
    io:format("ServiceId,Vsn, Tail ~p~n",[{time(),?MODULE,?LINE,ServiceId,Vsn,T}]),
    io:format("DnsList,Vsn ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
%    ListWithIp=[{IpAddr,Port,ServiceId,Vsn}||#dns_info{service_id=X_Id,
%							      vsn=X_Vsn,
%							      ip_addr=IpAddr,
%							      port=Port}<-DnsList,
%						    {ServiceId,Vsn}=:={X_Id,X_Vsn}],

    ListWithIp=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port,DnsInfo#dns_info.service_id,DnsInfo#dns_info.port,DnsInfo}||DnsInfo<-DnsList,
														    {ServiceId,Vsn}=:={DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}],
    
   io:format("ListWithIp,Vsn ~p~n",[{time(),?MODULE,?LINE,ListWithIp}]),

    R1= [{X_IpAddr,X_Port,X_ServiceId,X_Vsn,rpc:call(node(),tcp,call,[X_IpAddr,X_Port,{kubelet,stop_service,[ServiceId]}])}||{X_IpAddr,X_Port,X_ServiceId,X_Vsn,_DnsInfo}<-ListWithIp],
    io:format("result stop_service ~p~n",[{?MODULE,?LINE,R1}]),
 %   {dns,DnsIpAddr,DnsPort}=State#state.dns_addr,
  %  R2=[{rpc:cast(node(),if_dns,call,["controller",latest,{controller,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort},1,0])}])}||{IpAddr,Port,_,_,DnsInfo}<-ListWithIp],
   
 %   R2=[{rpc:cast(node(),if_dns,call,["controller",{controller,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort}]),
  %    rpc:cast(node(),if_dns,call,["dns",{dns,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort}]),
   %   rpc:cast(node(),tcp,call,[IpAddr,Port,{dns,de_dns_register,[DnsInfo]}])}||{IpAddr,Port,_,_,DnsInfo}<-ListWithIp],
%    io:format("result stop_service ~p~n",[{?MODULE,?LINE,ServiceId,Vsn,R2}]),
 %   io:format(" ~p~n",[{?MODULE,?LINE,T}]),
    NewDnsList=[Y_DnsInfo||Y_DnsInfo<-DnsList,
					   false==({ServiceId,Vsn}=:={Y_DnsInfo#dns_info.service_id,Y_DnsInfo#dns_info.vsn})],

    stop_services(T,NewDnsList,State).
						  
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
needed_services(ApplicationList,State)->
    needed_services(ApplicationList,State,[]).

needed_services([],_,NeededServices)->
    NeededServices;
needed_services([{{_AppId,_Vsn},JoscaFile}|T],State,Acc)->
    {dependencies,ServiceList}=lists:keyfind(dependencies,1,JoscaFile),
    NewAcc=check_services(ServiceList,State,Acc),
    needed_services(T,State,NewAcc).

check_services([],_,Acc)->
    Acc;
check_services([{Id,Vsn}|T],State,Acc) ->
    NewAcc=case josca:start_order(Id,Vsn,State) of
	       {error,Err}->
		   io:format("error~p~n",[{?MODULE,?LINE,Err}]),
		   Acc;
	       Services ->
		   case lists:member({Id,Vsn},Acc) of
		       true->
			   Acc;
		       false->
			   lists:append(Services,Acc)
		   end
	   end,
    check_services(T,State,NewAcc).

missing_services(NeededServices,DnsList)->
    AvailibleServices=[{DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}||DnsInfo<-DnsList],
    [{Id,Vsn}||{Id,Vsn}<-NeededServices, 
	       lists:member({Id,Vsn},AvailibleServices)=:=false].

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

start_services([],_Nodes,_)->
    ok;
start_services([{ServiceId,Vsn}|T],Nodes,State)->
 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,Vsn,Nodes}]),
    {dns,DnsIp,DnsPort}=State#state.dns_addr,
    case if_dns:call("catalog",latest,{catalog,read,[ServiceId,Vsn]},{DnsIp,DnsPort},1,1) of
	{error,Err}->
	    io:format("~p~n",[{?MODULE,?LINE,'error',Err}]),
	    {error,[?MODULE,?LINE,Err]};
	[{ok,_,JoscaInfo}]->
%	    io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
	    {zone,WantedZone}=lists:keyfind(zone,1,JoscaInfo),
	    {needed_capabilities,WantedCapabilities}=lists:keyfind(needed_capabilities,1,JoscaInfo),
	    NodesFullfilledNeeds=get_nodes_fullfills_needs(WantedZone,WantedCapabilities,Nodes),
	 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,WantedZone,WantedCapabilities,'=>>',NodesFullfilledNeeds}]),
	    case NodesFullfilledNeeds of
		[]->
		    io:format("~p~n",[{?MODULE,?LINE,'error no availible nodes'}]);
		NodesFullfilledNeeds->
		    R=schedule_start(ServiceId,Vsn,NodesFullfilledNeeds,?NUM_APPLICATIONS),
		    io:format("~p~n",[{?MODULE,?LINE,'Service start result =',R,ServiceId,Vsn}])
	    end;
	Err ->
	    io:format("~p~n",[{?MODULE,?LINE,'error',Err}]),
	     {error,[?MODULE,?LINE,Err]}
	    
    end,
    start_services(T,Nodes,State).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
schedule_start(ServiceId,Vsn,NodesFullfilledNeeds,NumApps)->
%    io:format("~p~n",[{?MODULE,?LINE,ServiceId,Vsn,NodesFullfilledNeeds,NumApps}]),
    NumNodes=lists:flatlength(NodesFullfilledNeeds),
%    io:format("NumNodes ~p~n",[{?MODULE,?LINE,NumNodes}]),
    Result=case NumNodes of
	       0->
		   io:format("Error ~p~n",[{?MODULE,?LINE,'No nodes are availible for the service ',ServiceId,Vsn}]),
		   {error,[?MODULE,?LINE,'No nodes are availible for the service ',ServiceId,Vsn]};
	       NumNodes ->
		   do_start(NodesFullfilledNeeds,?NUM_APPLICATIONS,ServiceId,Vsn,[])
    end,
    Result.

do_start([],_,_,_,StartResult)-> % Less nodes then NodesFullfilledNeeds
    StartResult;
do_start(_,0,_,_,StartResult)->
      StartResult;
do_start([KubeleteInfo|T],NumApps,ServicesId,Vsn,Acc)->
  %  io:format(" ~p~n",[{?MODULE,?LINE,KubeleteInfo,NumApps,ServicesId,Vsn,Acc}]),
    IpAddr=KubeleteInfo#kubelet_info.ip_addr,
    Port=KubeleteInfo#kubelet_info.port,
    R=tcp:call(IpAddr,Port,{kubelet,start_service,[ServicesId,Vsn]}),
 %   io:format(" ~p~n",[{?MODULE,?LINE,R}]),
    NewAcc=[R|Acc],
    do_start(T,NumApps-1,ServicesId,Vsn,NewAcc).

%schedule_start(ServicesId,Vsn,NodesFullfilledNeeds)->
 %   [KubeleteInfo|_]=NodesFullfilledNeeds,
  %  IpAddr=KubeleteInfo#kubelet_info.ip_addr,
   % Port=KubeleteInfo#kubelet_info.port,

 %   R=rpc:cast(node(),tcp,call,[IpAddr,Port,{kubelet,start_service,[ServicesId,Vsn]}]),
  %  R.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_nodes_fullfills_needs(WantedZone,WantedCapabilities,AvailibleNodes)->
    % Which nodes is in needed zone
    Workers=[X_Node||X_Node<-AvailibleNodes,
		     X_Node#kubelet_info.node_type=:=worker_node],
%    io:format("Workers ~p~n",[{?MODULE,?LINE,Workers}]), 
    RightZone = case WantedZone of
		    []->
			Workers;
		    Zone ->
		%	io:format("Zone=  ~p~n",[{?MODULE,?LINE,Zone}]), 
			[Node||Node<-Workers,
				Node#kubelet_info.zone=:=Zone]
		end,
   % io:format("RightZone  ~p~n",[{?MODULE,?LINE,RightZone}]),    
    NodesFullfilledNeeds=case WantedCapabilities of
			     []->
				 RightZone;
			     WantedCapabilities->
				 [Node||Node<-RightZone,
					check_capbility(WantedCapabilities,Node)]
			 end,
    
    NodesFullfilledNeeds.


check_capbility(WantedCapabilities,Node)->
    check_capbility(WantedCapabilities,Node,false).
    
check_capbility([],_,Boolean)->
    Boolean;
check_capbility([WCap|T],Node,_)->    
    case lists:member(WCap,Node#kubelet_info.capabilities) of
	false->
	    Tail=[],  % Stop searching
	    R=false;  % Failed
	true->
	    Tail=T,   % Continue search
	    R=true    % Succeded 
    end,
    check_capbility(Tail,Node,R).    
	   
				
    % Which nodes in needed zone has the right capabilities



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
