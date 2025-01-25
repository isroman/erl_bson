-module(datetime_test).

-include_lib("eunit/include/eunit.hrl").

%% Looku up for json tests here:
%% https://github.com/mongodb/specifications/blob/master/source/bson-corpus/tests/datetime.json

%% 1970-01-01T00:00:00Z
decode_epoch_test_() ->
	Bson = <<16,0,0,0,9,97,0,0,0,0,0,0,0,0,0,0>>,
	Json = #{<<"a">> => #{<<"$date">> => #{<<"$numberLong">> => <<"0">>}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).	

%% 2012-12-24T12:15:30.501Z
decode_positive_datetime_test_() ->
	Bson = <<16,0,0,0,9,97,0,197,216,214,204,59,1,0,0,0>>,
	Json = #{<<"a">> => #{<<"$date">> => #{<<"$numberLong">> => <<"1356351330501">>}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_negative_datetime_test_() ->
	Bson = <<16,0,0,0,9,97,0,195,60,231,185,189,255,255,255,0>>,
	Json = #{<<"a">> => #{<<"$date">> => #{<<"$numberLong">> => <<"-284643869501">>}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_y10k_datetime_test_() ->
	Bson = <<16,0,0,0,9,97,0,0,220,31,210,119,230,0,0,0>>,
	Json = #{<<"a">> => #{<<"$date">> => #{<<"$numberLong">> => <<"253402300800000">>}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

%% 2012-12-24T12:15:30.001Z
decode_datetime_with_leading_zero_ms_test_() ->
	Bson = <<16,0,0,0,9,97,0,209,214,214,204,59,1,0,0,0>>,
	Json = #{<<"a">> => #{<<"$date">> => #{<<"$numberLong">> => <<"1356351330001">>}}},
	?_assertEqual({Json, <<>>}, erl_bson:decode(Bson)).

decode_truncated_datetime_test_() ->
	Bson = <<12,0,0,0,9,97,0,18,52,86,120,0>>,
	?_assertError(invalid_datetime, erl_bson:decode(Bson)).

