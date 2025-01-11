-module(int64_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/int64.json

decode_min_int64_test_() ->
	Bson = <<16,0,0,0,18,97,0,0,0,0,0,0,0,0,128,0>>,
	Json = #{<<"a">> => #{<<"$numberLong">> => <<"-9223372036854775808">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_max_int64_test_() ->
	Bson = <<16,0,0,0,18,97,0,255,255,255,255,255,255,255,127,0>>,
	Json = #{<<"a">> => #{<<"$numberLong">> => <<"9223372036854775807">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_minus_one_int64_test_() ->
	Bson = <<16,0,0,0,18,97,0,255,255,255,255,255,255,255,255,0>>,
	Json = #{<<"a">> => #{<<"$numberLong">> => <<"-1">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).
	
decode_zero_int64_test_() ->
	Bson = <<16,0,0,0,18,97,0,0,0,0,0,0,0,0,0,0>>,
	Json = #{<<"a">> => #{<<"$numberLong">> => <<"0">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_one_int64_test_() ->
	Bson = <<16,0,0,0,18,97,0,1,0,0,0,0,0,0,0,0>>,
	Json = #{<<"a">> => #{<<"$numberLong">> => <<"1">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_invalid_int64_test_() ->
	Bson = <<12,0,0,0,18,97,0,18,52,86,120,0>>,
	?_assertError(invalid_int64, erl_bson:decode(Bson)).

