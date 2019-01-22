%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_adder).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("eunit/include/eunit.hrl").
-include("kube/include/tcp.hrl").
-include("kube/include/dns.hrl").
%% --------------------------------------------------------------------
-compile(export_all).
%-export([test]).

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
    ok=application:set_env(adder,ip_addr,?ADDER_IP),
    ok=application:set_env(adder,port,?ADDER_PORT),
    ok=application:set_env(adder,service_id,"adder"),
    ok=application:set_env(adder,vsn,"1.0.0"),
    ok=application:load(adder),
    ok=application:start(adder),
    ok.
%% Build and release a service and application josca
calc_1_test()->
    42=adder:add(20,22),
    ok.

crash_test_glurk()->
    adder:crash(),
    ok.

calc_2_test()->
    42=adder:add(20,22),
    ok.

stop_test()->    
    ok=application:stop(adder),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
