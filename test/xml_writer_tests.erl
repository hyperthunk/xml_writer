%% -----------------------------------------------------------------------------
%%
%% xml_writer: test suites
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
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
-module(xml_writer_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(EQC(P),
    case code:lib_dir(eqc, include) of
        {error, bad_name} ->
            proper:quickcheck(P,
                [verbose, {on_output, fun ct:pal/2}, {numtests, 50}]);
        _ ->
            eqc:check(P)
    end).

has_inner_text_value(V) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual, [{encoding, latin1}])) of
            {#xmlElement{content=[#xmlText{value=V}|_]},_} -> true;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

has_value_in_node(Node, Value) ->
    ?MATCHER(match_named_node_value(Node, Value),
        {Node, Value}, io_lib:format("A node named ~p, "
            "containing a value matching ~p~n", [Node, Value])).

match_named_node_value(Node, Value) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{name=Name,
                content=[#xmlText{value=ActualValue}|_]},_} ->
                list_to_atom(Node) == Name andalso
                atom_to_list(Value) == (ActualValue ++ "foo");
            _Other -> false
        end
    end.

match_node_value(Expected) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{content=[#xmlText{value=Expected}|_]},_} -> true;
            _Other -> false
        end
    end.

basic_serialisation_test_() ->
    {"Sanity check basic serialisation doesn't produce invalid XML",
     setup,
     fun() ->
         truncate_output()
     end,
     {with, xml_writer:new(fun store_xml/1),
         [fun simple_binary_serialisation/1]}}.
      %% fun simple_atom_serialisation/1 ]}.

store_xml(Xml) ->
    file:write_file(test_data(), Xml, [append]).

test_data() ->
    filename:join(rebar_utils:get_cwd(), "output.xml").

output() ->
    {ok, Bin} = file:read_file(test_data()), 
    binary_to_list(Bin).

truncate_output() ->
    FName = test_data(),
    {ok, IoDevice} = file:open(FName, [write]),
    try 
        ok = file:truncate(IoDevice)
    after
        file:close(IoDevice)
    end,
    FName.

simple_atom_serialisation(Writer) ->
    ?assert(?EQC(?FORALL(Value, atom(),
        ?IMPLIES(length(atom_to_list(Value)) > 5,
        begin
            xml_writer:with_element("foo", Writer, fun(W) ->
                xml_writer:write_value(Value, W)
            end),
            ?assertThat(output(), has_inner_text_value(Value))
        end)))).

simple_binary_serialisation(Writer) ->
    ?assert(?EQC(?FORALL(Value, list(integer(64, 255)),
        ?IMPLIES(length(Value) > 1,
        begin
            try
                xml_writer:with_element("foo", Writer, fun(W) ->
                    xml_writer:write_value(Value, W)
                end),
                assert_that(output(), has_inner_text_value(Value))
            after
                truncate_output()
            end
        end)))).
