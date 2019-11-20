%% VM versions
-define(VM_NO_VM,           16#00).
-define(VM_AEVM_SOPHIA_1,   16#01).
-define(VM_AEVM_SOLIDITY_1, 16#02).
-define(VM_AEVM_SOPHIA_2,   16#03).
-define(VM_AEVM_SOPHIA_3,   16#04).
-define(VM_FATE_SOPHIA_1,   16#05).
-define(VM_AEVM_SOPHIA_4,   16#06).
-define(VM_FATE_SOPHIA_2,   16#07).

%% ABI versions
-define(ABI_NO_VM,         16#00).
-define(ABI_AEVM_SOPHIA_1, 16#01).
-define(ABI_SOLIDITY_1,    16#02).
-define(ABI_FATE_SOPHIA_1, 16#03).

-define(IS_AEVM_SOPHIA(___VM_VERSION___),
        (___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_4 orelse
         ___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_3 orelse
         ___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_2 orelse
         ___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_1)).

-define(IS_FATE_SOPHIA(___VM_VERSION___),
        (___VM_VERSION___ =:= ?VM_FATE_SOPHIA_2 orelse
         ___VM_VERSION___ =:= ?VM_FATE_SOPHIA_1)).

-ifdef(TEST).
-define(VM_AEVM_SOLIDITY_1_enabled, true).
-else.
-define(VM_AEVM_SOLIDITY_1_enabled, false).
-endif.
