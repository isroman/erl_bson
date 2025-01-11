-module(boolean_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/boolean.json

%% True
decode_true_test_() ->
	Bson = <<9,0,0,0,8,98,0,1,0>>,
	Json = #{<<"b">> => true},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

%% False
decode_false_test_() ->
	Bson = <<9,0,0,0,8,98,0,0,0>>,
	Json = #{<<"b">> => false},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

%% Invalid boolean value of 2
decode_invalid_boolean_test_() ->
	Bson = <<9,0,0,0,8,98,0,2,0>>,
	?_assertError(invalid_boolean, erl_bson:decode(Bson)).

%% Invalid boolean value of -1
decode_invalid_boolean2_test_() ->
	Bson = <<9,0,0,0,8,98,0,255,0>>,
	?_assertError(invalid_boolean, erl_bson:decode(Bson)).

