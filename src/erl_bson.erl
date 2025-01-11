-module(erl_bson).

-export([
	encode/1,
	decode/1
]).

-define(UNDEFINED, 6).
-define(NULL, 10).
-define(MIN_KEY, -1).
-define(MAX_KEY, 127).

%%%============================================================
%%% API
%%%============================================================

encode(_Value) ->
	not_implemented.

decode(<<Size:4/little-signed-integer-unit:8, Document:(Size - 5)/binary, 0:1/little-unsigned-integer-unit:8, Rest/bitstring>>) ->
	%io:format("Doc size: ~p~n", [Size]),
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

% ObjectId
decode_value(7, <<ObjectId:12/little-binary, Rest/binary>>) ->
	{#{<<"$oid">> => binary:encode_hex(ObjectId, lowercase)}, Rest};
% Boolean
decode_value(8, <<0:1/little-unsigned-integer-unit:8, Rest/binary>>) ->
	{false, Rest};
decode_value(8, <<1:1/little-unsigned-integer-unit:8, Rest/binary>>) ->
	{true, Rest};
decode_value(8, _InvalidBinary) ->
	error(invalid_boolean);
% 32-bit Integer
decode_value(16, <<Integer:4/little-signed-integer-unit:8, Rest/binary>>) ->
	{#{<<"$numberInt">> => integer_to_binary(Integer)}, Rest};
decode_value(16, _InvalidBinary) ->
	error(invalid_int32);
% 64-bit Integer
decode_value(18, <<Integer:8/little-signed-integer-unit:8, Rest/binary>>) ->
	{#{<<"$numberLong">> => integer_to_binary(Integer)}, Rest};
decode_value(18, _InvalidBinary) ->
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

