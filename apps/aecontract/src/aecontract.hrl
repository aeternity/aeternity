%% VM versions
-define(VM_NO_VM,           16#00).
-define(VM_AEVM_SOPHIA_1,   16#01).
-define(VM_AEVM_SOLIDITY_1, 16#02).
-define(VM_AEVM_SOPHIA_2,   16#03).

%% ABI versions
-define(ABI_NO_VM,      16#00).
-define(ABI_SOPHIA_1,   16#01).
-define(ABI_SOLIDITY_1, 16#02).

-define(CURRENT_VM_SOPHIA,  ?VM_AEVM_SOPHIA_2).
-define(CURRENT_ABI_SOPHIA, ?ABI_SOPHIA_1).

-define(IS_VM_SOPHIA(___VM_VERSION___), (___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_2 orelse ___VM_VERSION___ =:= ?VM_AEVM_SOPHIA_1)).

-ifdef(TEST).
-define(VM_AEVM_SOLIDITY_1_enabled, true).
-else.
-define(VM_AEVM_SOLIDITY_1_enabled, false).
-endif.

