-module(timestamp_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/timestamp.json

%% Timestamp: (123456789, 42)
decode_timestamp_test_() ->
	Bson = <<16,0,0,0,17,97,0,42,0,0,0,21,205,91,7,0>>,
	Json = #{<<"a">> => #{<<"$timestamp">> => #{<<"i">> => 42, <<"t">> => 123456789}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_timestamp_with_high_order_bit_test_() ->
	Bson = <<16,0,0,0,17,97,0,255,255,255,255,255,255,255,255,0>>,
	Json = #{<<"a">> => #{<<"$timestamp">> => #{<<"i">> => 4294967295, <<"t">> => 4294967295}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_timestamp_with_high_order_bit2_test_() ->
	Bson = <<16,0,0,0,17,97,0,0,40,107,238,0,40,107,238,0>>,
	Json = #{<<"a">> => #{<<"$timestamp">> => #{<<"i">> => 4000000000, <<"t">> => 4000000000}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_truncated_timestamp_test_() ->
	Bson = <<15,0,0,0,17,97,0,42,0,0,0,21,205,91,0>>,
	?_assertError(invalid_timestamp, erl_bson:decode(Bson)).

