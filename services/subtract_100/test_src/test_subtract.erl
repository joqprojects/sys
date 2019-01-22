%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_subtract).
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
    ok=application:set_env(subtract,ip_addr,?SUBTRACT_IP),
    ok=application:set_env(subtract,port,?SUBTRACT_PORT),
    ok=application:set_env(subtract,service_id,"subtract"),
    ok=application:set_env(subtract,vsn,"1.0.0"),
    ok=application:load(subtract),
    ok=application:start(subtract),
    ok.
%% Build and release a service and application josca

calc_2_test()->
    42=subtract:sub(72,20),
    ok.

stop_test()->    
    ok=application:stop(subtract),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
