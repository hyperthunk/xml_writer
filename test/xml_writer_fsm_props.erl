%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(xml_writer_fsm_props).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("xml_writer.hrl").

-compile(export_all).

%%
%% fsm transitions
%%

waiting_for_input(#writer{}=W) ->
     [{history,
        {call, xml_writer, write_value, [?MODULE, binary()]}},
      {element_started,
        {call, xml_writer, start_element, [?MODULE, binary()]}}].

element_started(W) ->
    [{waiting_for_input, {call, xml_writer, end_element, [?MODULE]}}].

initial_state() -> waiting_for_input.

initial_state_data() -> #writer{}.

next_state_data(waiting_for_input, element_started, S,
                Result, {call, xml_writer, start_element, [_, Elem]}=Call) ->
    S#writer{ stack=[#stack_frame{ local_name=Elem }|S#writer.stack] };
next_state_data(From, Target, StateData, Result, {call, _, _, _}=Call) ->
    ct:pal("From = ~p, Target = ~p, StateData = ~p, Result = ~p, Call = ~p~n",
            [From, Target, StateData, Result, Call]),
    StateData.

%precondition(waiting_for_input, _, W, {call, _, write_value, _}) ->
%    true; % length(W#writer.stack) > 0.
%precondition(waiting_for_input, stopping, W, {_, _, _, _}) ->
%    false;
%precondition(stopping, stopping, _, _) ->
%    true.

%precondition(Today, _, S, {call,_,hungry,[]}) ->
%    case Today of
%    cheese_day ->
%        S#storage.cheese > 0;
%    lettuce_day ->
%        S#storage.lettuce > 0;
%    grapes_day ->
%        S#storage.grapes > 0
%    end;

%precondition(waiting_for_input, element_started,
%                S, {call, _, start_element, _}) ->
%    length(S#writer.stack) > 0;
precondition(_From, _Target, _StateData, {call ,_,_,_}) ->
    true.

postcondition(waiting_for_input, _, #writer{stack=[]},
                {call, _, write_value, _}, Result) ->
    Result == {error, no_root_node};
postcondition(element_started, waiting_for_input,
                W, {call, _, _, _}=Call, Result) ->
%    ct:pal("W = ~p, Call = ~p, Result = ~p~n",
%                [W,Call, Result]),
    length(W#writer.stack) > 0;
postcondition(_, _, _, _, _) ->
    true.

%weight(_Today, _Tomorrow, {call,_,new_day,_}) -> 1;
%weight(_Today, _Today, {call,_,hungry,_}) -> 3;
%weight(_Today, _Today, {call,_,buy,_}) -> 2.

prop_all_state_transitions_are_valid() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        begin
        Writer = xml_writer:new(?MODULE, fun io:format/2),
        {History, State, Result} =
            proper_fsm:run_commands(?MODULE, Cmds),
        xml_writer:close(Writer),
        ?WHENFAIL(
           io:format("History: ~w\nState: ~w\nResult: ~w\n",
                 [History, State, Result]),
           aggregate(zip(proper_fsm:state_names(History),
                 command_names(Cmds)),
                 Result =:= ok))
        end).
