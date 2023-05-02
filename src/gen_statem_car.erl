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
-module(gen_statem_template).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/2,stop/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).


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
init(_Worker_ids) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok, {off, {park}}, {}}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)

off(Car) -> gen_statem:call(Car, off).
on(Car) -> gen_statem:call(Car, on).
park(Car) -> gen_statem:call(Car, park).
reverse(Car) -> gen_statem:call(Car, reverse).
neutral(Car) -> gen_statem:call(Car, neutral).
drive(Car) -> gen_statem:call(Car, drive).
brake_applied(Car) -> gen_statem:call(Car, brake_applied).

%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
handle_event({call,From}, park, {off, {park}},{Statem_name,State_data}) ->
    {next_state, park, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, brake_applied, park,{Statem_name,State_data}) ->
    {next_state, {park, brake_applied}, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, reverse, {park, brake_applied},{Statem_name,State_data}) ->
    {next_state, {reverse, brake_applied}, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, neutral, reverse,{Statem_name,State_data}) ->
    {next_state, neutral, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, drive, neutral, {Statem_name,State_data}) ->
    {next_state, drive, {Statem_name,State_data}, [{reply,From,Statem_name}]};
handle_event({call,From}, next, ready,{Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, ready,{Statem_name,State_data},[{reply,From,Statem_name}]}.



%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
%%
%% Unit tests go here. 
handle_event_test() -> 
    [
        ?_assertEqual({off, park}, handle_event({call, somewhere}, off, ready, {[],[]} )),
        ?_assertEqual({on, park}, handle_event({call, somewhere}, on, off, {[],[]} )),
        ?_assertEqual(park, handle_event({call, somewhere}, park, Any, {[],[]} )),
        ?_assertEqual({park, applied_brake}, handle_event({call, somewhere}, brake_applied, park, {[],[]} )),
        ?_assertEqual({park, rev}, handle_event({call, somewhere}, gas_applied, park, {[],[]} )),
        ?_assertEqual({park, steering_applied}, handle_event({call, somewhere}, steering_applied, park, {[],[]} )),
        ?_assertEqual({reverse, brake_applied}, handle_event({call, somewhere}, reverse, {park, brake_applied}, {[],[]} )),
        ?_assertEqual({reverse, brake_applied}, handle_event({call, somewhere}, {reverse, brake_applied}, reverse, {[],[]} )),
        ?_assertEqual({reverse, gas_applied}, handle_event({call, somewhere}, {reverse, gas_applied}, reverse, {[],[]} )),
        ?_assertEqual({reverse, gas_applied}, handle_event({call, somewhere}, {reverse, gas_applied}, reverse, {[],[]} )),
        ]. 
-endif.
