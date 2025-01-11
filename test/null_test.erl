-module(null_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/null.json

%% Null type
decode_null_test_() ->
	Bson = <<8,0,0,0,10,97,0,0>>,
	Json = #{<<"a">> => null},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

