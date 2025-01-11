-module(oid_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/oid.json

%% All zeroes
decode_object_id_test_() ->
	Bson = <<20,0,0,0,7,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	Json = #{<<"a">> => #{<<"$oid">> => <<"000000000000000000000000">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_object_id2_test_() ->
	Bson = <<20,0,0,0,7,97,0,255,255,255,255,255,255,255,255,255,255,255,255,0>>,
	Json = #{<<"a">> => #{<<"$oid">> => <<"ffffffffffffffffffffffff">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_object_id3_test_() ->
	Bson = <<20,0,0,0,7,97,0,86,225,252,114,224,201,23,233,196,113,65,97,0>>,
	Json = #{<<"a">> => #{<<"$oid">> => <<"56e1fc72e0c917e9c4714161">>}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_invalid_object_id_test_() ->
	Bson = <<18,0,0,0,7,97,0,86,225,252,114,224,201,23,233,196,113>>,
	?_assertError(invalid_bson, erl_bson:decode(Bson)).

