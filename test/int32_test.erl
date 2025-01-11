-module(int32_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/int32.json

decode_min_int32_test_() ->
	Bson = <<12,0,0,0,16,105,0,0,0,0,128,0>>,
	Json = #{<<"i">> => #{<<"$numberInt">> => <<"-2147483648">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_max_int32_test_() ->
	Bson = <<12,0,0,0,16,105,0,255,255,255,127,0>>,
	Json = #{<<"i">> => #{<<"$numberInt">> => <<"2147483647">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_minus_one_int32_test_() ->
	Bson = <<12,0,0,0,16,105,0,255,255,255,255,0>>,
	Json = #{<<"i">> => #{<<"$numberInt">> => <<"-1">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_zero_int32_test_() ->
	Bson = <<12,0,0,0,16,105,0,0,0,0,0,0>>,
	Json = #{<<"i">> => #{<<"$numberInt">> => <<"0">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_one_int32_test_() ->
	Bson = <<12,0,0,0,16,105,0,1,0,0,0,0>>,
	Json = #{<<"i">> => #{<<"$numberInt">> => <<"1">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_invalid_int32_test_() ->
	Bson = <<9,0,0,0,16,97,0,5,0>>,
	?_assertError(invalid_int32, erl_bson:decode(Bson)).

