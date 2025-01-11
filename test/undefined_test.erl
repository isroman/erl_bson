-module(undefined_test).

-include_lib("eunit/include/eunit.hrl").

%% Look up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/undefined.json

%% Undefined type (deprecated)
decode_undefined_test_() ->
	Bson = <<8,0,0,0,6,97,0,0>>,
	Json = #{<<"a">> => #{<<"$undefined">> => true}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

