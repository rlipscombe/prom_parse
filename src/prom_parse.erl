-module(prom_parse).
-export([
    string/1,
    line/1
]).

string(Body) when is_binary(Body) ->
    parse_lines(string:split(Body, "\n", all)).

line(Line) when is_binary(Line) ->
    do_parse_line(Line).

parse_lines(Lines) ->
    lists:filtermap(fun parse_line/1, Lines).

parse_line(<<"#", _/binary>>) ->
    false;
parse_line(<<>>) ->
    false;
parse_line(Line) ->
    {true, do_parse_line(Line)}.

do_parse_line(Line) ->
    [Name, Rest] = string:split(Line, " "),
    parse_rest(Name, Rest).

% Yes, I should probably write a proper parser using leex, etc., but it's not worth the effort.
parse_rest(Name, <<"{", Rest/binary>>) ->
    [Labels, Value] = string:split(Rest, "}"),
    {Name, parse_labels(Labels), parse_value(string:trim(Value))};
parse_rest(Name, Value) ->
    {Name, #{}, parse_value(Value)}.

parse_labels(Labels) ->
    lists:foldl(
        fun(Label, Acc) ->
            [Key, Value] = string:split(Label, "="),
            Acc#{Key => string:trim(Value, both, [$"])}
        end,
        #{},
        string:split(Labels, ",", all)
    ).

parse_value(Value) ->
    try
        binary_to_float(Value)
    catch
        error:badarg ->
            binary_to_integer(Value)
    end.
