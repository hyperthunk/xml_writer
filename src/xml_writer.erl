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
-module(xml_writer).
-behaviour(gen_fsm).

%% API exports
-export([new/1, new/2, close/1]).
-export([write_value/2, start_element/2, end_element/1]).

%% gen_fsm state-name exports
-export([waiting_for_input/3, element_started/3]).

%% gen_fsm behaviour exports
-export([init/1,
         handle_sync_event/4,
         terminate/3,
         handle_event/3,
         handle_info/3,
         code_change/4]).

-include("xml_writer.hrl").

%%
%% API
%%

-spec new(name(), write_function()) -> writer().
new(Name, WriteFun) ->
    {ok, _Pid} = gen_fsm:start({local, Name}, ?MODULE, [WriteFun], []),
    Name.

-spec new(write_function()) -> writer().
new(WriteFun) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [WriteFun], []),
    Pid.

- spec close(writer()) -> term().
close(Writer) ->
    gen_fsm:sync_send_all_state_event(Writer, stop).

-spec write_value(writer(), iodata()) -> 'ok' | {'error', term()}.
write_value(Writer, Value) ->
    gen_fsm:sync_send_event(Writer, {write_value, Value}).

-spec start_element(writer(), element_name()) -> ok.
start_element(Writer, ElementName) ->
    gen_fsm:sync_send_event(Writer, {start_element, ElementName}).

-spec end_element(writer()) -> 'ok'.
end_element(Writer) ->
    gen_fsm:sync_send_event(Writer, end_element).

%%
%% gen_fsm callbacks
%%

init([Writer]) ->
    {ok, waiting_for_input, #writer{ write=Writer }}.

waiting_for_input({write_value, _}, From, W=#writer{ stack=[] }) ->
    {reply, {error, no_root_node}, waiting_for_input, W};
waiting_for_input({start_element, ElementName},
                    From, W=#writer{ stack=Stack }) ->
    gen_fsm:reply(From, ok),
    {next_state, element_started,
        W#writer{ stack=[#stack_frame{ local_name=ElementName }|Stack] }}.

element_started(end_element, _From, Writer) ->
    {reply, ok, waiting_for_input, pop(Writer)}.

handle_sync_event(stop, _From, _StateName, _StateData) ->
    {stop, normal, ok, []}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%
%% Internal API
%%

pop(Writer=#writer{ stack=[] }) ->
    Writer;
pop(Writer=#writer{ stack=[_|Rest] }) ->
    Writer#writer{ stack=Rest }.

