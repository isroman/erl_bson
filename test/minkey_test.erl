-module(minkey_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/minkey.json

%% Minkey
decode_minkey_test_() ->
	Bson = <<8,0,0,0,255,97,0,0>>,
	Json = #{<<"a">> => #{<<"$minKey">> => 1}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

