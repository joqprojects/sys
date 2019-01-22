%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_divider).
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
    ok=application:set_env(divider,ip_addr,?DIVIDER_IP),
    ok=application:set_env(divider,port,?DIVIDER_PORT),
    ok=application:set_env(divider,service_id,"divider"),
    ok=application:set_env(divider,vsn,"1.0.0"),
    ok=application:load(divider),
    ok=application:start(divider),
    ok.
%% Build and release a service and application josca
calc_1_test()->
    42=divider:add(420,10),
    ok.

crash_test_glurk()->
    divider:crash(),
    ok.

calc_2_test()->
    42=divider:divi(420,10),
    ok.

stop_test()->    
    ok=application:stop(divider),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
