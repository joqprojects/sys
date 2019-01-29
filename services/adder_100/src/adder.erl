%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(adder).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("services/adder_100/src/adder_local.hrl").

-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
-include("kube/include/data.hrl").
-include("kube/include/dns_data.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------




-export([add/2,crash/0
	]).

-export([start/0,
	 stop/0,
	 heart_beat/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------

heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).


add(A,B)->
    gen_server:call(?MODULE, {add,A,B},infinity).

crash()->
    gen_server:call(?MODULE, {crash},infinity).

%%-----------------------------------------------------------------------

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
% dict:fetch(oam_rpi3,D1).
% [{brd_ip_port,"80.216.90.159"},
% {port,6001},
% {worker_ip_port,"80.216.90.159"},
%  {port,6002}]
%
%% --------------------------------------------------------------------
init([]) ->
    % Kubelete sets the env variables when starting the application!
    % Updates when changed
    % Glurk ta bort app_start
    {ok,MyIp}=application:get_env(ip_addr),
    {ok,Port}=application:get_env(port),
    {ok,ServiceId}=application:get_env(service_id),
    {ok,Vsn}=application:get_env(vsn),
    {ok,DnsIp}=application:get_env(dns_ip_addr),
    {ok,DnsPort}=application:get_env(dns_port),

    DnsInfo=#dns_info{time_stamp="not_initiaded_time_stamp",
			service_id = ServiceId,
			vsn = Vsn,
			ip_addr=MyIp,
			port=Port
		       },
    rpc:cast(node(),if_dns,call,["dns",latest,{dns,dns_register,[DnsInfo]},{DnsIp,DnsPort},1,0]),
    rpc:cast(node(),if_dns,call,["controller",latest,{controller,dns_register,[DnsInfo]},{DnsIp,DnsPort},1,0]),
    rpc:cast(node(),kubelet,dns_register,[DnsInfo]),
    spawn(fun()-> local_heart_beat(?HEARTBEAT_INTERVAL) end), 
     io:format("Service ~p~n",[{?MODULE, 'started ',?LINE}]),
    {ok, #state{dns_info=DnsInfo,dns_addr={dns,DnsIp,DnsPort}}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({add,A,B}, _From, State) ->
    Reply=rpc:call(node(),adder_lib,add,[A,B]),
    {reply, Reply, State};

handle_call({crash}, _From, State) ->
    A=0,
    Reply=1/A,
    {reply, Reply, State};

handle_call({heart_beat}, _From, State) ->
    DnsInfo=State#state.dns_info,
    {dns,DnsIp,DnsPort}=State#state.dns_addr,
    rpc:cast(node(),if_dns,call,["dns",latest,{dns,dns_register,[DnsInfo]},{DnsIp,DnsPort},1,0]),
    rpc:cast(node(),if_dns,call,["controller",latest,{controller,dns_register,[DnsInfo]},{DnsIp,DnsPort},1,0]),
    rpc:cast(node(),kubelet,dns_register,[DnsInfo]),
   % if_dns:call("contoller",controller,controller_register,[DnsInfo]),
    Reply=ok,
   {reply, Reply, State};
    


handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    DnsInfo=State#state.dns_info,
    if_log:call(DnsInfo,notification,[?MODULE,?LINE,'unmatched_signal',Request,From]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    DnsInfo=State#state.dns_info,
 %   if_log:call(DnsInfo,notification,[?MODULE,?LINE,'unmatched_signal',Msg]),
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({tcp_closed,_Port}, State) ->
  %  io:format("unmatched signal ~p~n",[{?MODULE,?LINE,tcp,Port,binary_to_term(Bin)}]),
    {noreply, State};

handle_info({tcp,_Port,_Bin}, State) ->
  %  io:format("unmatched signal ~p~n",[{?MODULE,?LINE,tcp,Port,binary_to_term(Bin)}]),
    {noreply, State};


handle_info(Info, State) ->
%  DnsInfo=State#state.dns_info,
%    if_log:call(DnsInfo,notification,[?MODULE,?LINE,'unmatched_signal',Info]),
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
local_heart_beat(Interval)->
%    io:format(" ~p~n",[{?MODULE,?LINE}]),
    timer:sleep(100),
    ?MODULE:heart_beat(),
    timer:sleep(Interval),

    spawn(fun()-> local_heart_beat(Interval) end).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

