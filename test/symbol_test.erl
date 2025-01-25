-module(symbol_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/symbol.json

decode_empty_symbol_test_() ->
	Bson = <<13,0,0,0,14,97,0,1,0,0,0,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<""/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_single_character_symbol_test_() ->
	Bson = <<14,0,0,0,14,97,0,2,0,0,0,98,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<"b"/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_multi_character_symbol_test_() ->
	Bson = <<25,0,0,0,14,97,0,13,0,0,0,97,98,97,98,97,98,97,98,97,98,97,98,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<"abababababab"/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_two_byte_utf8_symbol_test_() ->
	Bson = <<25,0,0,0,14,97,0,13,0,0,0,195,169,195,169,195,169,195,169,195,169,195,169,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<"éééééé"/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_three_byte_utf8_symbol_test_() ->
	Bson = <<25,0,0,0,14,97,0,13,0,0,0,226,152,134,226,152,134,226,152,134,226,152,134,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<"☆☆☆☆"/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_embedded_nulls_symbol_test_() ->
	Bson = <<25,0,0,0,14,97,0,13,0,0,0,97,98,0,98,97,98,0,98,97,98,97,98,0,0>>,
	Json = #{<<"a">> => #{<<"$symbol">> => <<"ab\x{00}bab\x{00}babab"/utf8-little>>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_bad_symbol_length_test_() ->
	Bson = <<12,0,0,0,14,97,0,0,0,0,0,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

decode_bad_symbol_length2_test_() ->
	Bson = <<12,0,0,0,14,97,0,255,255,255,255,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

decode_bad_symbol_length3_test_() ->
	Bson = <<16,0,0,0,14,97,0,5,0,0,0,98,0,98,0,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

decode_bad_symbol_length4_test_() ->
	Bson = <<18,0,0,0,14,0,255,255,255,0,102,111,111,98,97,114,0,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

decode_not_null_terminated_symbol_test_() ->
	Bson = <<16,0,0,0,14,97,0,4,0,0,0,97,98,99,255,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

decode_empty_symbol_but_extra_null_test_() ->
	Bson = <<14,0,0,0,14,97,0,1,0,0,0,0,0,0>>,
	?_assertError(invalid_bson, erl_bson:decode(Bson)).

decode_invalid_utf8_symbol_test_() ->
	Bson = <<14,0,0,0,14,97,0,2,0,0,0,233,0,0>>,
	?_assertError(invalid_symbol, erl_bson:decode(Bson)).

