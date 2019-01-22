%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_calc).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("../include/tcp.hrl").
-include("../include/dns.hrl").
%% --------------------------------------------------------------------
-export([]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: Application
%% Description:
%% Returns: non
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% 1. Initial set up
%% --------------------------------------------------------------------
start_test()->
    calc:app_start(?CALC_IP,?CALC_PORT,"calc","1.0.0").

%% Build and release a service and application josca
calc_1_test()->
 ok= case tcp:call(?DNS_IP,?DNS_PORT,[dns,get_instances,["calc"]]) of
	 [[{public_ip,CalcIp},{public_port,CalcPort}]|_]->
	     42=tcp:call(CalcIp,CalcPort,[calc,add,[20,22]]),
	     -2=tcp:call(CalcIp,CalcPort,[calc,subt,[20,22]]),
	     5.0 =tcp:call(CalcIp,CalcPort,[calc,divi,[100,20]]),
	     {badrpc,Err}=tcp:call(CalcIp,CalcPort,[calc,divi,[22,0]]),
	     ok;
	 Err ->	%no log available or dns down
	     error
     end,
    ok.


stop_test()->    
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
    
