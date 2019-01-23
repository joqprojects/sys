%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sys).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
%%  -include("").
%-include_lib("eunit/include/eunit.hrl").
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
a()->
    if_dns:call("controller",{controller,add,["mymath","1.0.0"]},{"localhost",60010}),  
    do_calc(false),
    if_dns:call("controller",{controller,remove,["mymath","1.0.0"]},{"localhost",60010}),  
    ok.

add()->
    if_dns:call([{service,"controller","1.0.0"},{mfa,controller,add,["mymath","1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
    ok.
add(AppId)->
    if_dns:call([{service,"controller","1.0.0"},{mfa,controller,add,[AppId,"1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
    ok.

remove()->
     if_dns:call([{service,"controller","1.0.0"},{mfa,controller,remove,["mymath","1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
    ok.

remove(AppId)->
     if_dns:call([{service,"controller","1.0.0"},{mfa,controller,remove,[AppId,"1.0.0"]},{dns,"localhost",60010},{num_to_send,1},{num_to_rec,1},{timeout,5*1000}]),  
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
  
    
do_calc(true)->
    ok;
do_calc(false) ->
    R1=if_dns:call("adder",{adder,add,[20,20]},{"localhost",60010}),
    R2=if_dns:call("divider",{divider,divi,[410,10]},{"localhost",60010}),
    R3=if_dns:call("multi",{multi,mul,[6,7]},{"localhost",60010}),
    R4=if_dns:call("subtract",{subtract,sub,[63,20]},{"localhost",60010}),
    case {R1,R2,R3,R4} of
	{40,41.0,42,43}->
	 io:format("Test succeded ~p~n",[{?MODULE,?LINE,R1,R2,R3,R4}]),
	    NewQuit=true;
	 _->
	    io:format("Error ~p~n",[{R1,R2,R3,R4}]),    
	    timer:sleep(20*1000),
	    NewQuit=false
    end,
    
    do_calc(NewQuit).

