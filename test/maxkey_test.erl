-module(maxkey_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/maxkey.json

%% Maxkey
decode_maxkey_test_() ->
	Bson = <<8,0,0,0,127,97,0,0>>,
	Json = #{<<"a">> => #{<<"$maxKey">> => 1}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

