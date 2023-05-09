%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a  
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used 
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than 
%%% one rr_balancer. This is by design, not by accident. 
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_statem_car).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/1, start/2, start_link/2, stop/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).
%%API Calls
-export([off/1,park/1, reverse/1, neutral/1, drive/1, brake_applied/1]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

start(Statem_name) ->
    gen_statem:start({local,Statem_name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).


%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private
init(Initial_state) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    % io:format("~p ~n", [Initial_state]),
    {ok, Initial_state, []}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)

off(Car) -> io:format("accessed off"),gen_statem:call(Car, off).
% on(Car) -> gen_statem:call(Car, on).
park(Car) -> io:format("accessed park"), gen_statem:call(Car, park).
reverse(Car) -> gen_statem:call(Car, reverse).
neutral(Car) -> gen_statem:call(Car, neutral).
drive(Car) -> gen_statem:call(Car, drive).
brake_applied(Car) -> gen_statem:call(Car, brake_applied).

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
% handle_event({call,From}, park, off,{Statem_name,State_data}) ->
    % io:format("new state: ~p Currentstate: ~p State name:  State Data: ~n", [park, off]),
    % {next_state, park, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, off, [], []) ->
    % io:format("new state: ~p Currentstate: ~p State name: ~p State Data~p ~n", [off, ready, Statem_name,State_data]),
    {next_state, off, [], [{reply, From, off}]};
handle_event({call,From}, park, off, []) ->
    {next_state, park, [], [{reply,From, parked}]};
handle_event({call,From}, brake_applied, park, []) ->
    {next_state, {park, brake_applied}, [], [{reply,From, brake_applied}]};
handle_event({call,From}, reverse, {park,brake_applied}, []) ->
    {next_state, {reverse, brake_applied}, [], [{reply,From,reverse}]};
handle_event({call,From}, neutral, reverse, []) ->
    {next_state, neutral, [], [{reply,From,neutral}]};
handle_event({call,From}, drive, neutral, []) ->
    {next_state, drive, [], [{reply,From,drive}]};
handle_event({call,From}, neutral, drive, []) ->
    {next_state, neutral, [], [{reply,From,neutral}]};
handle_event({call,From}, reverse, neutral, []) ->
    {next_state, reverse, [], [{reply,From,reverse}]};
handle_event({call,From}, brake_applied, reverse, []) ->
    {next_state, {reverse, brake_applied}, [], [{reply,From,brake_applied}]};
handle_event({call,From}, park, {reverse, brake_applied}, []) ->
    {next_state, {park, brake_applied}, [], [{reply,From,parked}]};
handle_event({call,From}, off, park, []) ->
    {next_state, off, [], [{reply,From,off}]};
handle_event({call,From}, off, neutral, []) ->
    {next_state, off, [], [{reply,From,off}]};
handle_event({call,From}, NewState, CurrentState, Data) ->
    io:format("New State: ~p Current State: ~p State Data:~p ~n", [NewState, CurrentState, Data]),
    {next_state, error,Data,[{reply,From,CurrentState}]}.



%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
handle_event_test_() -> 
    [   
        % ?_assert(false),
        ?_assertEqual({next_state, off, [],[{reply,somewhere,off}]}, handle_event({call, somewhere}, off, [], [] )),
        ?_assertEqual({next_state, park, [],[{reply,somewhere,parked}]}, handle_event({call, somewhere}, park, off, [] )),
        ?_assertEqual({next_state, {park, brake_applied}, [],[{reply,somewhere,brake_applied}]}, handle_event({call, somewhere}, brake_applied, park, [] )),
        ?_assertEqual({next_state, {reverse, brake_applied}, [],[{reply,somewhere,reverse}]}, handle_event({call, somewhere}, reverse, {park, brake_applied}, [] )),
        ?_assertEqual({next_state, neutral, [],[{reply,somewhere,neutral}]}, handle_event({call, somewhere}, neutral, reverse, [] )),
        ?_assertEqual({next_state, drive, [],[{reply,somewhere,drive}]}, handle_event({call, somewhere}, drive, neutral, [] )),
        ?_assertEqual({next_state, neutral, [],[{reply,somewhere,neutral}]}, handle_event({call, somewhere}, neutral, drive, [] )),
        ?_assertEqual({next_state, reverse, [],[{reply,somewhere,reverse}]}, handle_event({call, somewhere}, reverse, neutral, [] )),
        ?_assertEqual({next_state, {reverse, brake_applied}, [],[{reply,somewhere,brake_applied}]}, handle_event({call, somewhere}, brake_applied, reverse, [] )),
        ?_assertEqual({next_state, {park, brake_applied}, [],[{reply,somewhere,parked}]}, handle_event({call, somewhere}, park, {reverse, brake_applied}, [] )),
        ?_assertEqual({next_state, off, [],[{reply,somewhere,off}]}, handle_event({call, somewhere}, off, park, [] )),
        ?_assertEqual({next_state, off, [],[{reply,somewhere,off}]}, handle_event({call, somewhere}, off, neutral, [] ))
    ]. 
-endif.
