-module(erl_bson).

-export([
	encode/1,
	decode/1
]).

-define(UNDEFINED, 6).
-define(OBJECT_ID, 7).
-define(BOOLEAN, 8).
-define(UTC_DATETIME, 9).
-define(NULL, 10).
-define(SYMBOL, 14).
-define(INTEGER_32_BIT, 16).
-define(TIMESTAMP, 17).
-define(INTEGER_64_BIT, 18).
-define(MIN_KEY, -1).
-define(MAX_KEY, 127).

%%%============================================================
%%% API
%%%============================================================

encode(_Value) ->
	not_implemented.

decode(<<Size:4/little-signed-integer-unit:8, Document:(Size - 5)/binary, 0:1/little-unsigned-integer-unit:8, Rest/bitstring>>) ->
	{decode_document(Document, #{}), Rest};
decode(_Binary) ->
	error(invalid_bson).

decode_document(<<>>, DecodedDocument) ->
	DecodedDocument;
% Datatypes without values
decode_document(<<?UNDEFINED:1/little-signed-integer-unit:8, RestDocument/binary>>, DecodedDocument) ->
	{Key, RestDocument2} = decode_cstring(RestDocument),
	decode_document(RestDocument2, maps:put(Key, #{<<"$undefined">> => true}, DecodedDocument));
decode_document(<<?NULL:1/little-signed-integer-unit:8, RestDocument/binary>>, DecodedDocument) ->
	{Key, RestDocument2} = decode_cstring(RestDocument),
	decode_document(RestDocument2, maps:put(Key, null, DecodedDocument));
decode_document(<<?MIN_KEY:1/little-signed-integer-unit:8, RestDocument/binary>>, DecodedDocument) ->
	{Key, RestDocument2} = decode_cstring(RestDocument),
	decode_document(RestDocument2, maps:put(Key, #{<<"$minKey">> => 1}, DecodedDocument));
decode_document(<<?MAX_KEY:1/little-signed-integer-unit:8, RestDocument/binary>>, DecodedDocument) ->
	{Key, RestDocument2} = decode_cstring(RestDocument),
	decode_document(RestDocument2, maps:put(Key, #{<<"$maxKey">> => 1}, DecodedDocument));
% General case
decode_document(<<Type:1/little-signed-integer-unit:8, RestDocument/binary>>, DecodedDocument) ->
	{Key, RestDocument2} = decode_cstring(RestDocument),
	{Value, RestDocument3} = decode_value(Type, RestDocument2),
	decode_document(RestDocument3, maps:put(Key, Value, DecodedDocument)).

decode_value(?OBJECT_ID, <<ObjectId:12/little-binary, Rest/binary>>) ->
	{#{<<"$oid">> => binary:encode_hex(ObjectId, lowercase)}, Rest};

decode_value(?BOOLEAN, <<0:1/little-unsigned-integer-unit:8, Rest/binary>>) ->
	{false, Rest};
decode_value(?BOOLEAN, <<1:1/little-unsigned-integer-unit:8, Rest/binary>>) ->
	{true, Rest};
decode_value(?BOOLEAN, _InvalidBinary) ->
	error(invalid_boolean);

decode_value(?UTC_DATETIME, <<DateTime:8/little-signed-integer-unit:8, Rest/binary>>) ->
	{#{<<"$date">> => #{<<"$numberLong">> => integer_to_binary(DateTime)}}, Rest};
decode_value(?UTC_DATETIME, _InvalidBinary) ->
	error(invalid_datetime);

decode_value(?SYMBOL, <<Size:4/little-signed-integer-unit:8, String:(Size - 1)/binary, 0:1/little-unsigned-integer-unit:8, Rest/binary>>) ->
	case unicode:characters_to_binary(String, utf8) of
		{error, _Binary, _RestData} ->
			error(invalid_symbol);
		{incomplete, _Binary, _RestData} ->
			error(invalid_symbol);
		String ->
			{#{<<"$symbol">> => String}, Rest}
	end;
decode_value(?SYMBOL, _InvalidBinary) ->
	error(invalid_symbol);

decode_value(?INTEGER_32_BIT, <<Integer:4/little-signed-integer-unit:8, Rest/binary>>) ->
	{#{<<"$numberInt">> => integer_to_binary(Integer)}, Rest};
decode_value(?INTEGER_32_BIT, _InvalidBinary) ->
	error(invalid_int32);

decode_value(?TIMESTAMP, <<Increment:4/little-unsigned-integer-unit:8, Timestamp:4/little-unsigned-integer-unit:8, Rest/binary>>) ->
	{#{<<"$timestamp">> => #{<<"i">> => Increment, <<"t">> => Timestamp}}, Rest};
decode_value(?TIMESTAMP, _InvalidBinary) ->
	error(invalid_timestamp);

decode_value(?INTEGER_64_BIT, <<Integer:8/little-signed-integer-unit:8, Rest/binary>>) ->
	{#{<<"$numberLong">> => integer_to_binary(Integer)}, Rest};
decode_value(?INTEGER_64_BIT, _InvalidBinary) ->
	error(invalid_int64);

% Error
decode_value(_Type, _Binary) ->
	not_implemented.

decode_cstring(Binary) ->
	decode_cstring(Binary, <<>>).

decode_cstring(<<0:1/little-unsigned-integer-unit:8, Rest/binary>>, CString) ->
	{CString, Rest};
decode_cstring(<<Ch/utf8-little, Rest/binary>>, CString) ->
	decode_cstring(Rest, <<CString/binary, Ch/utf8-little>>).

%%%============================================================
%%% Internal functions
%%%============================================================

