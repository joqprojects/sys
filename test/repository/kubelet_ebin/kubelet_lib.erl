%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(kubelet_lib).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube/kubelet/src/kubelet_local.hrl").

-include("kube/include/trace_debug.hrl").
-include("kube/include/kubelet_data.hrl").
-include("kube/include/dns_data.hrl").
-include("kube/include/repository_data.hrl").
%% --------------------------------------------------------------------
-define(NUM_TRIES_START_SERVICE,10).
-define(INTERVAL_START_SERVICE,1000).


%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
dns_register(DnsInfo, DnsList) ->
    TimeStamp=erlang:now(),
    NewDnsInfo=DnsInfo#dns_info{time_stamp=TimeStamp},
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    
    X1=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList=[NewDnsInfo|X1],
    NewDnsList.

de_dns_register(DnsInfo,DnsList)->
    #dns_info{time_stamp=_,ip_addr=IpAddr,port=Port,service_id=ServiceId,vsn=Vsn}=DnsInfo,
    NewDnsList=[X||X<-DnsList,false==({IpAddr,Port,ServiceId,Vsn}==
				  {X#dns_info.ip_addr,X#dns_info.port,X#dns_info.service_id,X#dns_info.vsn})],
    NewDnsList.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start_app(ServiceId,VsnInput,NodeIp,NodePort,State)->
    Module=list_to_atom(ServiceId),
    {dns,DnsIp,DnsPort}=State#state.dns_addr, 
    {ok,Artifact}=load_appfiles(ServiceId,VsnInput,NodeIp,NodePort,{DnsIp,DnsPort}),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={_,_},
	      modules=_
	     }=Artifact,
    ok=application:set_env(Module,ip_addr,NodeIp),
    ok=application:set_env(Module,port,NodePort),
    ok=application:set_env(Module,service_id,ServiceId),
    ok=application:set_env(Module,vsn,Vsn),
    ok=application:set_env(Module,dns_ip_addr,DnsIp),
    ok=application:set_env(Module,dns_port,DnsPort),
    R=application:start(Module).    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start_pre_loaded_apps(PreLoadApps,NodeIp,NodePort,{DnsIp,DnsPort})->
  %  io:format("~p~n",[{?MODULE,?LINE,PreLoadApps}]),
    load_start_apps(PreLoadApps,NodeIp,NodePort,[],{DnsIp,DnsPort}).
load_start_apps([],_,_,StartResult,_)->
    StartResult;
load_start_apps([catalog|T],NodeIp,NodePort,Acc,{DnsIp,DnsPort}) -> %Has to be pre loaded
    ok=application:set_env(catalog,dns_ip_addr,DnsIp),
    ok=application:set_env(catalog,dns_port,DnsPort),

    ok=application:set_env(catalog,ip_addr,NodeIp),
    ok=application:set_env(catalog,port,NodePort),
    ok=application:set_env(catalog,service_id,"catalog"),
    EbinDir=?KUBELET_EBIN,
    Appfile=filename:join(EbinDir,"catalog.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(catalog,vsn,Vsn),
    R=application:start(catalog),
    NewAcc=[{"catalog",Vsn,R}|Acc],
  load_start_apps(T,NodeIp,NodePort,NewAcc,{DnsIp,DnsPort});

load_start_apps([repo|T],NodeIp,NodePort,Acc,{DnsIp,DnsPort}) -> %Has to be pre loaded
 %    io:format(" ~p~n",[{?MODULE, ?LINE,NodeIp,NodePort,Acc,State}]),
 
   ok=application:set_env(repo,dns_ip_addr,DnsIp),
    ok=application:set_env(repo,dns_port,DnsPort),

    ok=application:set_env(repo,ip_addr,NodeIp),
    ok=application:set_env(repo,port,NodePort),
    ok=application:set_env(repo,service_id,"repo"),
    EbinDir=?KUBELET_EBIN,
   Appfile=filename:join(EbinDir,"repo.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(repo,vsn,Vsn),

    R=application:start(repo),
    io:format("start_result ~p~n",[{?MODULE, ?LINE,R}]),
    NewAcc=[{"repo",Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc,{DnsIp,DnsPort});

load_start_apps([dns|T],NodeIp,NodePort,Acc,{DnsIp,DnsPort}) -> %Has to be pre loaded
    ok=application:set_env(dns,ip_addr,NodeIp),
    ok=application:set_env(dns,port,NodePort),
    ok=application:set_env(dns,service_id,"dns"),
    ok=application:set_env(dns,dns_ip_addr,DnsIp),
    ok=application:set_env(dns,dns_port,DnsPort),
    EbinDir=?KUBELET_EBIN,
  %  EbinDir=".",
    Appfile=filename:join(EbinDir,"dns.app"),
    {ok,[{application,_,Info}]}=file:consult(Appfile),
    {vsn,Vsn}=lists:keyfind(vsn,1,Info),
    ok=application:set_env(dns,vsn,Vsn),
    R=application:start(dns),
    NewAcc=[{"dns",Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc,{DnsIp,DnsPort});
    
load_start_apps([Module|T],NodeIp,NodePort,Acc,{DnsIp,DnsPort}) ->
  % io:format("~p~n",[{?MODULE,?LINE,Module,NodeIp,NodePort}]),
    {ok,Artifact}=load_appfiles(atom_to_list(Module),latest,NodeIp,NodePort,{DnsIp,DnsPort}),
    io:format("~p~n",[{?MODULE,?LINE}]),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={_,_},
	      modules=_
	     }=Artifact,

    ok=application:set_env(Module,dns_ip_addr,DnsIp),
    ok=application:set_env(Module,dns_port,DnsPort),
    ok=application:set_env(Module,ip_addr,NodeIp),
    ok=application:set_env(Module,port,NodePort),
    ok=application:set_env(Module,service_id,ServiceId),
    ok=application:set_env(Module,vsn,Vsn),
    R=application:start(Module),
       io:format("~p~n",[{?MODULE,?LINE,R}]),
    NewAcc=[{ServiceId,Vsn,R}|Acc],
    load_start_apps(T,NodeIp,NodePort,NewAcc,{DnsIp,DnsPort}).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_unload_app(DnsInfo,State)->
    #dns_info{service_id=ServiceId,vsn=Vsn}=DnsInfo,
    {dns,DnsIp,DnsPort}=State#state.dns_addr, 
    Artifact=if_dns:call("repo",{repo,read_artifact,[ServiceId,Vsn]},{DnsIp,DnsPort}),    
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,_},
	      modules=Modules
	     }=Artifact,
    Ebin=case ServiceId of
	     "lib"->
		 "lib_ebin";
	     "kubelet"->
		 "kubelet_ebin";
	     "repo"->
		 "kubelet_ebin";
	     "catalog"->
		 "kubelet_ebin";
	     "controller"->
		 "kubelet_ebin";
	     _->
		 ?SERVICE_EBIN
	 end,   
    Module=list_to_atom(ServiceId),
    application:stop(Module),
    application:unload(Module),

    Appfile=filename:join(Ebin,AppFileBaseName),
    ok=file:delete(Appfile),
    DeleteResult=[file:delete(filename:join(Ebin,ModuleName))||{ModuleName,_}<-Modules], 
    rpc:cast(node(),if_dns,call,["dns",{dns,de_dns_register,[DnsInfo]},{DnsIp,DnsPort}]),
    rpc:cast(node(),if_dns,call,["controller",{dns,de_dns_register,[DnsInfo]},{DnsIp,DnsPort}]),
    
    Reply=case [Y||Y<-DeleteResult,false=={Y=:=ok}] of
	      []->
		  ok;
	      Err ->
		  {error,[?MODULE,?LINE,'error deleting modules',Err]}
	  end,
   Reply.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
zone()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(zone,1,I) of
	  {zone,Z}->
	      Z;
	  false ->
	      []
      end,
    R.

capabilities()->
    {ok,I}=file:consult("kubelet.config"),
    R=case lists:keyfind(capabilities,1,I) of
	  {capabilities,C}->
	      C;
	  false ->
	      []
      end,
    R.


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_appfiles(ServiceId,VsnInput,NodeIp,NodePort,{DnsIp,DnsPort})->  % VsnInput Can be latest !!!

    Ebin=case ServiceId of
	     "lib"->
		 "lib_ebin";
	     "kubelet"->
		 "kubelet_ebin";
	     "repo"->
		 "kubelet_ebin";
	     "catalog"->
		 "kubelet_ebin";
	     "controller"->
		 "kubelet_ebin";
	     _->
		 ?SERVICE_EBIN
      end,   
 %   io:format("~p~n",[{?MODULE,?LINE,ServiceId,VsnInput}]),
    [Artifact]=if_dns:call("repo",latest,{repo,read_artifact,[ServiceId,VsnInput]},{DnsIp,DnsPort},1,1),
 %   io:format("~p~n",[{?MODULE,?LINE,Artifact}]),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
  %  io:format("~p~n",[{?MODULE,?LINE}]),
    Appfile=filename:join(Ebin,AppFileBaseName),
    ok=file:write_file(Appfile,AppBinary),
   % io:format("~p~n",[{?MODULE,?LINE}]),
    [file:write_file(filename:join(Ebin,ModuleName),Bin)||{ModuleName,Bin}<-Modules],
   % io:format("~p~n",[{?MODULE,?LINE}]),
    {ok,Artifact}.
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start(ServiceId)->
    application:start(list_to_atom(ServiceId)).

upgrade(ServiceId,Vsn,NodeIp,NodePort,{DnsIp,DnsPort})->
    Artifact=load_appfiles(ServiceId,Vsn,NodeIp,NodePort,{DnsIp,DnsPort}),
    #artifact{service_id=ServiceId,
	      vsn=Vsn,
	      appfile={AppFileBaseName,AppBinary},
	      modules=Modules
	     }=Artifact,
    GenServerModule=list_to_atom(ServiceId),  
    ModulesToPurge=[Module||Module<-Modules,false==(GenServerModule==Module)],
    update_modules(ModulesToPurge),
    update_server(GenServerModule),
    ok.

update_server(GenServerModule)->
    ok= sys:suspend(GenServerModule),
    false=code:purge(GenServerModule),
    {module,GenServerModule}=code:load_file(GenServerModule),
    ok= sys:change_code(GenServerModule,GenServerModule,"0",[]),
    sys:resume(GenServerModule).

update_modules([])->
    ok;
update_modules([Module|T]) ->
    code:purge(Module),
    code:load_file(Module),
    update_modules(T).


