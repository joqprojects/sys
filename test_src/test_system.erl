%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_system).
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
add_app_test()->
    ok=if_dns:call("controller",controller,add,["app_adder","1.0.0"]),
    [{{"app_adder","1.0.0"},_}]=if_dns:call("controller",controller,get_all_applications,[]),    
    ok.

campaign_1_test()->
    if_dns:call("controller",controller,campaign,[]),
    ok.

add_mymath_test()->
    ok=if_dns:call("controller",controller,add,["mymath","1.0.0"]),
  %  [{{"app_adder","1.0.0"},_}]=if_dns:call("controller",controller,get_all_applications,[]),    
    ok.
campaign_2_test()->
    if_dns:call("controller",controller,campaign,[]),
    ok.
stop_test()->
    if_dns:call("controller",controller,remove,["app_adder","1.0.0"]),
    if_dns:call("controller",controller,remove,["mymath","1.0.0"]),
    spawn(fun()->kill_session() end),
    ok.
kill_session()->
    timer:sleep(1000),
    erlang:halt(),
    ok.
  
    
