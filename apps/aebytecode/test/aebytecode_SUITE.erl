-module(aebytecode_SUITE).

%% common_test exports
-export([ all/0 ]).

%% test case exports
-export([ roundtrip_identy/1 ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [ roundtrip_identy ].

roundtrip_identy(_Cfg) ->
    CodeDir = code:lib_dir(aebytecode, test),
    FileName = filename:join(CodeDir, "asm_code/identity.aesm"),
    Code = aeb_asm:file(FileName, []),
    ct:log("Code ~p:~n~s~n", [FileName, aeb_disassemble:format(Code, fun io:format/2)]),
    ok.
