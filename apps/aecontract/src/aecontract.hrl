-define(AEVM_NO_VM,           16#00).
-define(AEVM_01_Sophia_01,    16#01).
-define(AEVM_01_Solidity_01,  16#02).
-define(AEVM_02_Sophia_01,    16#03).

-define(CURRENT_AEVM_SOPHIA, ?AEVM_02_Sophia_01).

-define(IS_AEVM_SOPHIA(___VM_VERSION___), (___VM_VERSION___ =:= ?AEVM_02_Sophia_01 orelse ___VM_VERSION___ =:= ?AEVM_01_Sophia_01)).

-ifdef(TEST).
-define(AEVM_01_Solidity_01_enabled, true).
-else.
-define(AEVM_01_Solidity_01_enabled, false).
-endif.
