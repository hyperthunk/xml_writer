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
                [verbose, {on_output, fun ct:pal/2}, {numtests, 200}]);
        _ ->
            eqc:check(P)
    end).

namespace_handling_test_() ->
    {"Basic namespace handling",
         [{"Tags should be put into their proper namespace",
            fun() ->
                FileId = tmp_file_id(),
                NS = "ns1",
                NSUri = "http://foo.bar.baz/schemas/2011",
                Writer = xml_writer:new(store_xml(FileId)),
                WithNS = xml_writer:add_namespace(NS, NSUri, Writer),
                xml_writer:write_node(NS, "foobar", WithNS),
                XMLNode = output(FileId),
                ?assertThat(XMLNode, is_in_namespace(NS, NSUri, "foobar"))
                end
            }]}.

%basic_serialisation_test_() ->
%    {"A slow test",
%        {timeout, 60, [{"10s Wait", fun slow/0}]}}.

%slow() ->
%    slow_thing(10000).

%slow_thing(Slowness) ->
%    timer:sleep(Slowness).

basic_serialisation_test_() ->
    {"Sanity check basic serialisation doesn't produce invalid XML",
    [{"Simple serialisation of a (deep) iolist",
      timeout, 120, ?_assert(?EQC(?FORALL(IoData, iodata(),
          ?IMPLIES(length(IoData) > 0,
              enforce(fun has_iodata_content/2, tmp_file_id(), IoData)))))},
     {"Simple serialisation of a binary",
        ?_assert(?EQC(?FORALL(Value, a_to_z(),
            ?IMPLIES(length(Value) > 1,
                enforce(fun inner_text_value/2, tmp_file_id(), Value)))))},
      {"Simple serialisation of an atom",
        ?_assert(?EQC(?FORALL(Value, a_to_z(),
            ?IMPLIES(length(Value) > 1,
                enforce(fun has_named_value_foo/2, "foo", 
                        tmp_file_id(), Value)))))}
    ]}.

%%
%% Custom Hamcrest Matchers
%%

is_in_namespace(NS, NSUri, NodeName) ->
    UriAsAtom = list_to_atom(NSUri),
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual, [{encoding, latin1}])) of
            {#xmlElement{
                expanded_name=ExName,
                nsinfo={NS, NodeName},
                namespace=#xmlNamespace{nodes = [{NS,UriAsAtom}]}
            },_} ->
                ExName == list_to_atom(NS ++ ":" ++ NodeName);
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

has_inner_text_value(V) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual, [{encoding, latin1}])) of
            {#xmlElement{content=[#xmlText{value=V}|_]},_} -> true;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

has_content_inside(_Node) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{
                content=[#xmlText{value=Value}|_]},_} ->
                length(Value) > 0;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

match_named_node_value(Node, Value) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{name=Node,
                content=[#xmlText{value=Value}|_]},_} ->
                true;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

%%
%% Utilities
%%

tmp_file_id() ->
    Val = case get(current) of
        undefined ->
            put(current, random:uniform(1000000)),
            get(current);
        Other ->
            NextVal = Other + 1,
            put(current, NextVal), NextVal
    end,
    integer_to_list(Val).

store_xml(FileId) ->
    fun(Xml) ->
        case file:write_file(test_data(FileId), Xml, [append]) of
            ok -> ok;
            Other ->
                ct:pal("Unable to write: ~p~n", [Other])
        end
    end.

test_data(FileId) ->
    filename:join([rebar_utils:get_cwd(),
        string:join(["output", FileId, "xml"], ".")]).

output(FileId) ->
    {ok, Bin} = file:read_file(test_data(FileId)),
    binary_to_list(Bin).

%%
%% PropEr Type Definitions
%%

a_to_z() ->
    %% NB: make sure xmerl (which we're using in the matchers)
    %% doesn't fall on it's backside complaining about encodings and so on
    %% TODO: this is far too conservative, so we'll need to broaden it later
    non_empty(list(integer(97, 122))).

node_data() ->
    union([a_to_z(), noshrink(non_empty(utf8_binary()))]).

iodata() ->
    %% TODO: relax the matchers so we can remove the noshrink constraints
    union([noshrink(non_empty(list(noshrink(non_empty(list(node_data())))))),
        noshrink(non_empty(list(node_data())))]).

utf8_binary() ->
  ?LET(L, list(a_to_z()),
    unicode:characters_to_binary(L, utf8)).

%%
%% Abstract Test Functions
%%

has_iodata_content(FileId, _) ->
    assert_that(output(FileId), has_content_inside(iodata)).

has_named_value_foo(FileId, Value) ->
    assert_that(output(FileId), match_named_node_value(foo, Value)).

inner_text_value(FileId, Value) ->
    assert_that(output(FileId), has_inner_text_value(Value)).

enforce(Enforcement, FileId, Value) ->
    enforce(Enforcement, "data", FileId, Value).

enforce(Enforcement, Elem, FileId, Value) ->
    %% why the bloody hell PropEr isn't enforcing the size constraint I do not know. <o>|<0>
    if length(Value) > 0 ->
        Writer = xml_writer:new(store_xml(FileId)),
        xml_writer:with_element(Elem, Writer, fun(W) ->
           xml_writer:write_value(Value, W)
        end),
        Enforcement(FileId, Value)
    end.
