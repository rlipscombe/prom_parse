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
    parse_name(Line).

parse_name(<<Ch, Rest/binary>>) when
    (Ch >= $a andalso Ch =< $z) orelse (Ch >= $A andalso Ch =< $Z) orelse Ch == $_ orelse Ch == $:
->
    parse_name_1(Rest, [Ch]).

parse_name_1(<<Ch, Rest/binary>>, Name) when
    (Ch >= $a andalso Ch =< $z) orelse (Ch >= $A andalso Ch =< $Z) orelse
        (Ch >= $0 andalso Ch =< $9) orelse Ch == $_ orelse Ch == $:
->
    parse_name_1(Rest, [Ch | Name]);
parse_name_1(Rest, Name) ->
    parse_name_2(Rest, list_to_binary(lists:reverse(Name))).

parse_name_2(<<" ", Rest/binary>>, Name) ->
    parse_name_2(Rest, Name);
parse_name_2(Rest, Name) ->
    parse_labels(Rest, Name, #{}).

parse_labels(<<"{", Rest/binary>>, Name, Labels) ->
    parse_labels_1(Rest, Name, Labels);
parse_labels(Rest, Name, Labels) ->
    parse_value(Rest, Name, Labels).

parse_labels_1(<<Ch, Rest/binary>>, Name, Labels) when
    (Ch >= $a andalso Ch =< $z) orelse (Ch >= $A andalso Ch =< $Z) orelse Ch == $_
->
    parse_labels_2(Rest, [Ch], Name, Labels);
parse_labels_1(<<",", Rest/binary>>, Name, Labels) ->
    parse_labels_1(Rest, Name, Labels);
parse_labels_1(<<"}", Rest/binary>>, Name, Labels) ->
    parse_value(Rest, Name, Labels).

parse_labels_2(<<Ch, Rest/binary>>, Label, Name, Labels) when
    (Ch >= $a andalso Ch =< $z) orelse (Ch >= $A andalso Ch =< $Z) orelse
        (Ch >= $0 andalso Ch =< $9) orelse Ch == $_
->
    parse_labels_2(Rest, [Ch | Label], Name, Labels);
parse_labels_2(<<"=", Rest/binary>>, Label, Name, Labels) ->
    parse_labels_3(Rest, list_to_binary(lists:reverse(Label)), Name, Labels).

parse_labels_3(<<$", Rest/binary>>, Label, Name, Labels) ->
    parse_label_value(Rest, [], Label, Name, Labels).

parse_label_value(<<$", Rest/binary>>, Value, Label, Name, Labels) ->
    parse_labels_1(Rest, Name, Labels#{Label => list_to_binary(lists:reverse(Value))});
parse_label_value(<<Ch, Rest/binary>>, Value, Label, Name, Labels) ->
    parse_label_value(Rest, [Ch | Value], Label, Name, Labels).

parse_value(<<" ", Rest/binary>>, Name, Labels) ->
    parse_value(Rest, Name, Labels);
parse_value(Rest, Name, Labels) ->
    {Name, Labels, binary_to_number(Rest)}.

binary_to_number(Value) ->
    try
        binary_to_float(Value)
    catch
        error:badarg ->
            binary_to_integer(Value)
    end.
