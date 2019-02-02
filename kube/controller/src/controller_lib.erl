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
campaign(State)->
   io:format("campaign start ~p~n",[{?MODULE,?LINE,time()}]),
    NeededServices=controller_lib:needed_services(State#state.application_list,State),
  %  io:format("NeededServices ~p~n",[{?MODULE,?LINE,NeededServices}]),
    {dns,DnsIp,DnsPort}=State#state.dns_addr,
   % io:format("   {dns,DnsIp,DnsPort} ~p~n",[{?MODULE,?LINE,   {dns,DnsIp,DnsPort}}]),
    AvailableServices=if_dns:call("dns",latest,{dns,get_all_instances,[]},{DnsIp,DnsPort}),
    
						%    AvailableServices=glurk,
   % io:format("AvailableServices ~p~n",[{?MODULE,?LINE,AvailableServices}]),
    MissingServices=controller_lib:missing_services(NeededServices,AvailableServices),
    %start missing services
    case MissingServices of
	[]->
	    io:format("System is in preferred state  ~p~n",[{date(),time()}]),
	    io:format("Availible services  ~p~n",[{NeededServices}]);
	MissingServices->
	    io:format("MissingServices ~p~n",[{date(),time(),MissingServices}])
    end,
    StartResult=controller_lib:start_services(MissingServices,State#state.node_list,State,[]),

    %Stop surplus services
    %keep system services repo, catalog, controller
    L1=keep_system_services(["repo","controller","catalog"],AvailableServices),
  %  io:format("L1 ~p~n",[{?MODULE,?LINE,time(),L1,NeededServices}]),
    SurplusServices=controller_lib:surplus_services(NeededServices,L1),
    io:format("SurplusServices ~p~n",[{?MODULE,?LINE,time(),SurplusServices}]),
    StopResult=controller_lib:stop_services(SurplusServices,AvailableServices,State),
  %  io:format("StopResult ~p~n",[{?MODULE,?LINE,time(),StopResult}]),
    io:format("campaign stop ~p~n",[{?MODULE,?LINE,time()}]),
    ok.

keep_system_services([],WorkerService)->
    [{DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}||DnsInfo<-WorkerService];
keep_system_services([ServiceId|T],Acc)->
    NewAcc=[DnsInfo||DnsInfo<-Acc,
		     false==(DnsInfo#dns_info.service_id==ServiceId)],
    keep_system_services(T,NewAcc). 


surplus_services([],SurplusServices)->
    SurplusServices;
surplus_services([X_DnsInfo|T],Acc)->
    
    NewAcc=[DnsInfo||DnsInfo<-Acc,
		     false==({DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}==
				 {X_DnsInfo#dns_info.service_id,X_DnsInfo#dns_info.vsn})],
    surplus_services(T,NewAcc).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
dns_register(DnsInfo,State)->
    Service=DnsInfo#dns_info.service_id,
    Ip=DnsInfo#dns_info.ip_addr,
    P=DnsInfo#dns_info.port,
 %   io:format("~p~n",[{time(),Service,Ip,P,?MODULE,?LINE}]),
    TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    
    X1=[X||X<-State#state.dns_list,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList=[NewDnsInfo|X1],
    NewState=State#state{dns_list=NewDnsList},
    NewState.


de_dns_register(DnsInfo,State)->
      #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    NewDnsList=[X||X<-State#state.dns_list,
		   false==({IpAddr,Port,ServiceId,Vsn}=={X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    
    NewState=State#state{dns_list=NewDnsList},
    NewState.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
node_register(KubeletInfo, State) ->
    Service=KubeletInfo#kubelet_info.service_id,
    Ip=KubeletInfo#kubelet_info.ip_addr,
    P=KubeletInfo#kubelet_info.port,
  %  io:format("~p~n",[{time(),Service,Ip,P,?MODULE,?LINE}]),

    TimeStamp=erlang:now(),
    NewKubeletInfo=KubeletInfo#kubelet_info{time_stamp=TimeStamp},
    #kubelet_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn,
		  max_workers=_MaxWorkers,zone=_Zone,capabilities=_Capabilities,
		  node_type=_
		 }=KubeletInfo,
%    io:format("~p~n",[{?MODULE,?LINE,State#state.node_list}]),
    X1=[X||X<-State#state.node_list,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#kubelet_info.ip_addr,X#kubelet_info.port,X#kubelet_info.service_id,X#kubelet_info.vsn})],
    NewKubeletList=[NewKubeletInfo|X1],
   % io:format("~p~n",[{?MODULE,?LINE,NewKubeletList}]),
    NewState=State#state{node_list=NewKubeletList},
    NewState.

de_node_register(KubeletInfo, State) ->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=KubeletInfo,
    NewKubeletList=[X||X<-State#state.node_list,
		       false==({IpAddr,Port,ServiceId,Vsn}==
				   {X#kubelet_info.ip_addr,X#kubelet_info.port,X#kubelet_info.service_id,X#kubelet_info.vsn})],
    NewState=State#state{node_list=NewKubeletList},
    NewState.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_services([],DnsList,_State)->
  %  io:format("ok  ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
    DnsList;
stop_services([{ServiceId,Vsn}|T],DnsList,State)->
   % io:format("ServiceId,Vsn, Tail ~p~n",[{time(),?MODULE,?LINE,ServiceId,Vsn,T}]),
   % io:format("DnsList,Vsn ~p~n",[{time(),?MODULE,?LINE,DnsList}]),
%    ListWithIp=[{IpAddr,Port,ServiceId,Vsn}||#dns_info{service_id=X_Id,
%							      vsn=X_Vsn,
%							      ip_addr=IpAddr,
%							      port=Port}<-DnsList,
%						    {ServiceId,Vsn}=:={X_Id,X_Vsn}],

    ListWithIp=[{DnsInfo#dns_info.ip_addr,DnsInfo#dns_info.port,
		 DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn,DnsInfo}||DnsInfo<-DnsList,
									    {ServiceId,Vsn}=:={DnsInfo#dns_info.service_id,DnsInfo#dns_info.vsn}],
    
 %  io:format("ListWithIp,Vsn ~p~n",[{time(),?MODULE,?LINE,ListWithIp}]),

    R1= [{X_IpAddr,X_Port,X_ServiceId,X_Vsn,tcp:call(X_IpAddr,X_Port,{kubelet,stop_service,[X_ServiceId,X_Vsn]})}||{X_IpAddr,X_Port,X_ServiceId,X_Vsn,_DnsInfo}<-ListWithIp],
   % io:format("result stop_service ~p~n",[{?MODULE,?LINE,R1}]),
 %   {dns,DnsIpAddr,DnsPort}=State#state.dns_addr,
  %  R2=[{rpc:cast(node(),if_dns,call,["controller",latest,{controller,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort},1,0])}])}||{IpAddr,Port,_,_,DnsInfo}<-ListWithIp],
   
 %   R2=[{rpc:cast(node(),if_dns,call,["controller",{controller,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort}]),
  %    rpc:cast(node(),if_dns,call,["dns",{dns,de_dns_register,[DnsInfo]},{DnsIpAddr,DnsPort}]),
   %   rpc:cast(node(),tcp,call,[IpAddr,Port,{dns,de_dns_register,[DnsInfo]}])}||{IpAddr,Port,_,_,DnsInfo}<-ListWithIp],
%    io:format("result stop_service ~p~n",[{?MODULE,?LINE,ServiceId,Vsn,R2}]),
 %   io:format(" ~p~n",[{?MODULE,?LINE,T}]),
    NewDnsList=[Y_DnsInfo||Y_DnsInfo<-DnsList,
					   false==({ServiceId,Vsn}=:={Y_DnsInfo#dns_info.service_id,Y_DnsInfo#dns_info.vsn})],
  %  io:format(" NewDnsList ~p~n",[{?MODULE,?LINE,NewDnsList}]),
    stop_services(T,NewDnsList,State).
						  
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
needed_services(ApplicationList,State)->
 %   io:format("ApplicationList ~p~n",[{?MODULE,?LINE,time(),ApplicationList}]),
    needed_services(ApplicationList,State,[]).

needed_services([],_,NeededServices)->
  %   io:format(" NeededServices ~p~n",[{?MODULE,?LINE,time(),NeededServices}]),
    NeededServices;
needed_services([{{_AppId,_Vsn},JoscaFile}|T],State,Acc)->
  %  io:format(" JoscaFile ~p~n",[{?MODULE,?LINE,time(),JoscaFile}]),

    {dependencies,ServiceList}=lists:keyfind(dependencies,1,JoscaFile),
    NewAcc=check_services(ServiceList,State,Acc),
 %   io:format(" JoscaFile ~p~n",[{?MODULE,?LINE,time(),JoscaFile}]),
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

start_services([],_Nodes,_,StartResult)->
    StartResult;
start_services([{ServiceId,Vsn}|T],Nodes,State,Acc)->
 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,Vsn,Nodes}]),
    {dns,DnsIp,DnsPort}=State#state.dns_addr,
    case if_dns:call("catalog",latest,{catalog,read,[ServiceId,Vsn]},{DnsIp,DnsPort}) of
	{error,Err}->
	    NewAcc=[{error,Err}|Acc],
	    io:format("~p~n",[{?MODULE,?LINE,'error',Err}]),
	    {error,[?MODULE,?LINE,Err]};
	{ok,_,JoscaInfo}->
%	    io:format("~p~n",[{?MODULE,?LINE,JoscaInfo}]),
	    {zone,WantedZone}=lists:keyfind(zone,1,JoscaInfo),
	    {needed_capabilities,WantedCapabilities}=lists:keyfind(needed_capabilities,1,JoscaInfo),
	    NodesFullfilledNeeds=get_nodes_fullfills_needs(WantedZone,WantedCapabilities,Nodes),
	 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,WantedZone,WantedCapabilities,'=>>',NodesFullfilledNeeds}]),
	    case NodesFullfilledNeeds of
		[]->
		    NewAcc=[{error,['error no availible nodes',ServiceId,Vsn]}|Acc],
		    io:format("~p~n",[{?MODULE,?LINE,'error no availible nodes'}]);
		NodesFullfilledNeeds->
		    R=schedule_start(ServiceId,Vsn,NodesFullfilledNeeds,?NUM_APPLICATIONS),
		     NewAcc=[R|Acc],
		    io:format("~p~n",[{?MODULE,?LINE,'Service start result =',R,ServiceId,Vsn}])
	    end;
	Err ->
	    NewAcc=[{error,Err}|Acc],
	    io:format("~p~n",[{?MODULE,?LINE,'error',Err}]),
	     {error,[?MODULE,?LINE,Err]}
	    
    end,
    start_services(T,Nodes,State,NewAcc).

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
