-module(prom_parse_tests).
-include_lib("eunit/include/eunit.hrl").

no_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_memory_ets_tables">>, #{}, 56},
        prom_parse:line(<<"erlang_vm_memory_ets_tables 56">>)
    ).

space_single_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_memory_system_bytes_total">>, #{<<"usage">> => <<"binary">>}, 228192},
        prom_parse:line(<<"erlang_vm_memory_system_bytes_total {usage=\"binary\"} 228192">>)
    ).

single_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_memory_system_bytes_total">>, #{<<"usage">> => <<"binary">>}, 228192},
        prom_parse:line(<<"erlang_vm_memory_system_bytes_total{usage=\"binary\"} 228192">>)
    ).

multiple_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_allocators">>,
            #{
                <<"alloc">> => <<"binary_alloc">>,
                <<"instance_no">> => <<"6">>,
                <<"kind">> => <<"mbcs">>,
                <<"usage">> => <<"carriers_size">>
            },
            32768},
        prom_parse:line(
            <<"erlang_vm_allocators{alloc=\"binary_alloc\",instance_no=\"6\",kind=\"mbcs\",usage=\"carriers_size\"} 32768">>
        )
    ).

multiple_label_commas_test() ->
    % Labels where the values contain commas, motivating the hand-written parser, rather than regexes/split.
    ?assertEqual(
        {<<"foo">>,
            #{
                <<"bar">> => <<"baz_1,baz_2">>,
                <<"gargle">> => <<"blaarg">>,
                <<"mingo">> => <<"zonko">>,
                <<"quux">> => <<"wurdle">>
            },
            3},
        prom_parse:line(
            <<"foo {bar=\"baz_1,baz_2\",quux=\"wurdle\",gargle=\"blaarg\",mingo=\"zonko\"} 3">>
        )
    ).
