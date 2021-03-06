%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dfs_action).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([regular/2,regular1/2,dir/2]).


%% ====================================================================
%% External functions
%% ====================================================================


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
regular(_Next_Fullname,Acc)->
   % io:format(" ~p~n",[{?MODULE,?LINE,Acc}]),
    case Acc of
	[]->
	    Acc1=[1];
	[X]->
	    Acc1=[X+1]
    end,
    {ok,Acc1}.

dir(_Next_Fullname,Acc)->
    case Acc of
	[]->
	    Acc1=[1];
	[X]->
	    Acc1=[X+1]
    end,
    {ok,Acc1}.


regular1(Next_Fullname,Acc)->
    case filename:extension(Next_Fullname) of
	".JPG"->
	    Acc1=[{regular,Next_Fullname}|Acc];
	_->
	    Acc1=Acc
    end,
    {ok,Acc1}.
