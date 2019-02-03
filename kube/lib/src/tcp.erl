%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(tcp).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-include("kube/include/trace_debug.hrl").
-include("kube/include/tcp.hrl").
-include("certificate/cert.hrl").

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([test_call/2,test_call/3,test_cast/2,
	 server_seq/1,server_parallel/1,par_connect/1]).

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
test_call(Addresses,{os,cmd,A})->
    test_send_call(Addresses,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],?TIMEOUT_TCPCLIENT,noresult);
test_call(Addresses,{M,F,A})->
    test_send_call(Addresses,[call,{M,F,A},?KEY_MSG],?TIMEOUT_TCPCLIENT,noresult).

test_call(Addresses,{os,cmd,A},TimeOut)->
    test_send_call(Addresses,[call,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],TimeOut,noresult);
test_call(Addresses,{M,F,A},TimeOut)->
    test_send_call(Addresses,[call,{M,F,A},?KEY_MSG],TimeOut,noresult).

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
test_send_call([],_,_,Reply)->
   % io:format("test_call Reply ~p~n",[{?MODULE,?LINE,Reply}]),
    Reply;
%test_send_call([{IpAddr,Port}|T],Msg,TimeOut,Result)->
 %   io:format(" test_call ~p~n",[{?MODULE,?LINE,IpAddr,Port,Msg,TimeOut,Result}]);
%test_send_call(Glurk,Msg,TimeOut,Result)->
test_send_call([{IpAddr,Port}|T],Msg,TimeOut,Result)->
  %  io:format("test_call ~p~n",[{?MODULE,?LINE,{IpAddr,Port},Msg,TimeOut,Result}]),
    case gen_tcp:connect(IpAddr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	    case gen_tcp:send(Socket,term_to_binary(Msg)) of
		ok->
		    receive
			{tcp,Socket,Bin}->
			    NewResult=binary_to_term(Bin),
			    gen_tcp:close(Socket),
			    Retry=false;
			{error,Err} ->
			    io:format("send error ~p~n",[{?MODULE,?LINE,Err,IpAddr,Port,Msg}]),
			    NewResult={error,[?MODULE,?LINE,Err]},
			    gen_tcp:close(Socket),
			    Retry=true
		    after TimeOut ->
			    io:format("send error ~p~n",[{?MODULE,?LINE,time_out,IpAddr,Port,Msg}]),
			    NewResult={error,[?MODULE,?LINE,tcp_timeout,IpAddr,Port,Msg]},
			    gen_tcp:close(Socket),
			    Retry=true
		    end;
		{error,Err}->
		    Retry=true,
		    NewResult={error,[?MODULE,?LINE,Err,IpAddr,Port,Msg]}
	    end;
	{error,Err}->
	    Retry=true,
	    NewResult={error,[?MODULE,?LINE,Err,IpAddr,Port,Msg]}
    end,
    case Retry of
	true->
	    Reply=test_send_call(T,Msg,TimeOut,NewResult);
	false ->
	    Reply=NewResult
    end,
    Reply.
    
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

   
test_cast(Addresses,{os,cmd,A})->
    test_send_cast(Addresses,[cast,{?KEY_M_OS_CMD,?KEY_F_OS_CMD,A},?KEY_MSG],noresult);
test_cast(Addresses,{M,F,A})->
    test_send_cast(Addresses,[cast,{M,F,A},?KEY_MSG],noresult).

test_send_cast([],_,Reply)->
  %  io:format("test_call Reply ~p~n",[{?MODULE,?LINE,Reply}]),
    Reply;
test_send_cast([{IpAddr,Port}|T],Msg=[cast,{M,F,A},?KEY_MSG],Result)->
 %    io:format("test_call ~p~n",[{?MODULE,?LINE,{IpAddr,Port},Msg,Result}]),
    case gen_tcp:connect(IpAddr,Port,?CLIENT_SETUP) of
	{ok,Socket}->
	    case gen_tcp:send(Socket,term_to_binary(Msg)) of
		ok->
		    NewResult=ok,
		    Retry=false,
		    gen_tcp:close(Socket);
		{error,Err}->
		    Retry=true,
		    NewResult={error,[?MODULE,?LINE,Err,IpAddr,Port,Msg]}
	    end;
	{error,Err}->
	    
	    Retry=true,
	    NewResult={error,[?MODULE,?LINE,Err,IpAddr,Port,Msg]}
    end,
    case Retry of
	true->
	    Reply=test_send_cast(T,Msg,NewResult);
	false ->
	    Reply=NewResult
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------

% Receive part
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_seq(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),  
    seq_loop(LSock).

seq_loop(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    single(Socket),
    seq_loop(LSock).

%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
server_parallel(Port)->
    {ok, LSock}=gen_tcp:listen(Port,?SERVER_SETUP),
    spawn(fun()-> par_connect(LSock) end),
    receive
	wait_for_ever->
	    ok
    end.

par_connect(LSock)->
    {ok,Socket}=gen_tcp:accept(LSock),
    spawn(fun()-> par_connect(LSock) end),
    single(Socket).
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
single(Socket)->
    receive
	{tcp, Socket, RawData}->
	    case binary_to_term(RawData) of
		[{call,{M,F,A}},?KEY_MSG]->
		    R=rpc:call(node(),M,F,A),
		    io:format("~p~n",[{?MODULE,?LINE,R}]),
		    Reply=case R of
			      {badrpc,Err}->
				  {error,[?MODULE,?LINE,R]};
			      R->
				  R
			  end,
		    gen_tcp:send(Socket,term_to_binary(Reply)),
		    single(Socket);
		[{cast,{M,F,A}},?KEY_MSG]->
			%   io:format(" ~p~n",[{?MODULE,?LINE,{cast,{M,F,A}}}]),
		    _A=rpc:cast(node(),M,F,A),
		    gen_tcp:close(Socket);
		%  io:format("Error ~p~n",[{?MODULE,?LINE,A}]);
		Err->
		    io:format("Error ~p~n",[{?MODULE,?LINE,Err}])
	    end;
	{tcp_closed,Socket} ->
	    exit
    end.
