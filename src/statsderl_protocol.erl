-module(statsderl_protocol).
-include("statsderl.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    encode/1,
    format_tags/1,
    join/2
]).

%% public
-spec encode(operation()) -> iodata().

encode({counter, Key, Value, SampleRate, Tags}) ->
    [Key, <<":">>, format_value(Value), <<"|c">>,
        format_sample_rate(SampleRate)] ++ format_tags(Tags);
encode({gauge, Key, Value, Tags}) ->
    [Key, <<":">>, format_value(Value), <<"|g">>] ++ format_tags(Tags);
encode({gauge_decrement, Key, Value, Tags}) ->
    [Key, <<":-">>, format_value(Value), <<"|g">>] ++ format_tags(Tags);
encode({gauge_increment, Key, Value, Tags}) ->
    [Key, <<":+">>, format_value(Value), <<"|g">>] ++ format_tags(Tags);
encode({timing, Key, Value, Tags}) ->
    [Key, <<":">>, format_value(Value), <<"|ms">>] ++ format_tags(Tags).

%% private
format_sample_rate(SampleRate) when SampleRate >= 1 ->
    <<>>;
format_sample_rate(SampleRate) ->
    [<<"|@">>, float_to_list(SampleRate, [compact, {decimals, 6}])].

format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]).

format_tags(Map) when is_map(Map) ->
    case maps:size(Map) of
        N when N > 0 ->
            [_|Fold] = maps:fold(fun(K, V, Acc) -> BK = to_binary(K), BV = to_binary(V), [<<",">>,<<BK/binary,":",BV/binary>>] ++ Acc end, [], Map),
            [<<"#">> | Fold];
        _ -> []
    end;
format_tags(_) ->
    [].

join([], _Separator) ->
  <<>>;
join([S], _separator) ->
  S;
join(L, Separator) ->
  iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
  Acc;
join([S | Rest], Separator, []) ->
  join(Rest, Separator, [S]);
join([S | Rest], Separator, Acc) ->
  join(Rest, Separator, [S, Separator | Acc]).

to_binary(V) when is_list(V) ->
  list_to_binary(V);
to_binary(V) when is_atom(V) ->
  atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
  list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
  V.