-module(prom_parse_tests).
-include_lib("eunit/include/eunit.hrl").

no_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_memory_ets_tables">>, #{}, 56},
        prom_parse:line(<<"erlang_vm_memory_ets_tables 56">>)
    ).

single_label_test() ->
    ?assertEqual(
        {<<"erlang_vm_memory_system_bytes_total{usage=\"binary\"}">>, #{}, 228192},
        prom_parse:line(<<"erlang_vm_memory_system_bytes_total{usage=\"binary\"} 228192">>)
    ).
