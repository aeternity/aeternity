
%% AEVM opcodes
-define(           'STOP', 16#00).
-define(            'ADD', 16#01).
-define(            'MUL', 16#02).
-define(            'SUB', 16#03).
-define(            'DIV', 16#04).
-define(           'SDIV', 16#05).
-define(            'MOD', 16#06).
-define(           'SMOD', 16#07).
-define(         'ADDMOD', 16#08).
-define(         'MULMOD', 16#09).
-define(            'EXP', 16#0a).
-define(     'SIGNEXTEND', 16#0b).

-define(             'LT', 16#10).
-define(             'GT', 16#11).
-define(            'SLT', 16#12).
-define(            'SGT', 16#13).
-define(             'EQ', 16#14).
-define(         'ISZERO', 16#15).
-define(            'AND', 16#16).
-define(             'OR', 16#17).
-define(            'XOR', 16#18).
-define(            'NOT', 16#19).
-define(           'BYTE', 16#1a).

-define(           'SHA3', 16#20).

-define(        'ADDRESS', 16#30).
-define(        'BALANCE', 16#31).
-define(         'ORIGIN', 16#32).
-define(         'CALLER', 16#33).
-define(      'CALLVALUE', 16#34).
-define(   'CALLDATALOAD', 16#35).
-define(   'CALLDATASIZE', 16#36).
-define(   'CALLDATACOPY', 16#37).
-define(       'CODESIZE', 16#38).
-define(       'CODECOPY', 16#39).
-define(       'GASPRICE', 16#3a).
-define(    'EXTCODESIZE', 16#3b).
-define(    'EXTCODECOPY', 16#3c).
-define( 'RETURNDATASIZE', 16#3d).
-define( 'RETURNDATACOPY', 16#3e).

-define(      'BLOCKHASH', 16#40).
-define(       'COINBASE', 16#41).
-define(      'TIMESTAMP', 16#42).
-define(         'NUMBER', 16#43).
-define(     'DIFFICULTY', 16#44).
-define(       'GASLIMIT', 16#45).

-define(            'POP', 16#50).
-define(          'MLOAD', 16#51).
-define(         'MSTORE', 16#52).
-define(        'MSTORE8', 16#53).
-define(          'SLOAD', 16#54).
-define(         'SSTORE', 16#55).
-define(           'JUMP', 16#56).
-define(          'JUMPI', 16#57).
-define(             'PC', 16#58).
-define(          'MSIZE', 16#59).
-define(            'GAS', 16#5a).
-define(       'JUMPDEST', 16#5b).

-define(          'PUSH1', 16#60).
-define(          'PUSH2', 16#61).
-define(          'PUSH3', 16#62).
-define(          'PUSH4', 16#63).
-define(          'PUSH5', 16#64).
-define(          'PUSH6', 16#65).
-define(          'PUSH7', 16#66).
-define(          'PUSH8', 16#67).
-define(          'PUSH9', 16#68).
-define(         'PUSH10', 16#69).
-define(         'PUSH11', 16#6a).
-define(         'PUSH12', 16#6b).
-define(         'PUSH13', 16#6c).
-define(         'PUSH14', 16#6d).
-define(         'PUSH15', 16#6e).
-define(         'PUSH16', 16#6f).
-define(         'PUSH17', 16#70).
-define(         'PUSH18', 16#71).
-define(         'PUSH19', 16#72).
-define(         'PUSH20', 16#73).
-define(         'PUSH21', 16#74).
-define(         'PUSH22', 16#75).
-define(         'PUSH23', 16#76).
-define(         'PUSH24', 16#77).
-define(         'PUSH25', 16#78).
-define(         'PUSH26', 16#79).
-define(         'PUSH27', 16#7a).
-define(         'PUSH28', 16#7b).
-define(         'PUSH29', 16#7c).
-define(         'PUSH30', 16#7d).
-define(         'PUSH31', 16#7e).
-define(         'PUSH32', 16#7f).
-define(           'DUP1', 16#80).
-define(           'DUP2', 16#81).
-define(           'DUP3', 16#82).
-define(           'DUP4', 16#83).
-define(           'DUP5', 16#84).
-define(           'DUP6', 16#85).
-define(           'DUP7', 16#86).
-define(           'DUP8', 16#87).
-define(           'DUP9', 16#88).
-define(          'DUP10', 16#89).
-define(          'DUP11', 16#8a).
-define(          'DUP12', 16#8b).
-define(          'DUP13', 16#8c).
-define(          'DUP14', 16#8d).
-define(          'DUP15', 16#8e).
-define(          'DUP16', 16#8f).
-define(          'SWAP1', 16#90).
-define(          'SWAP2', 16#91).
-define(          'SWAP3', 16#92).
-define(          'SWAP4', 16#93).
-define(          'SWAP5', 16#94).
-define(          'SWAP6', 16#95).
-define(          'SWAP7', 16#96).
-define(          'SWAP8', 16#97).
-define(          'SWAP9', 16#98).
-define(         'SWAP10', 16#99).
-define(         'SWAP11', 16#9a).
-define(         'SWAP12', 16#9b).
-define(         'SWAP13', 16#9c).
-define(         'SWAP14', 16#9d).
-define(         'SWAP15', 16#9e).
-define(         'SWAP16', 16#9f).
-define(           'LOG0', 16#a0).
-define(           'LOG1', 16#a1).
-define(           'LOG2', 16#a2).
-define(           'LOG3', 16#a3).
-define(           'LOG4', 16#a4).

-define(         'CREATE', 16#f0).
-define(           'CALL', 16#f1).
-define(       'CALLCODE', 16#f2).
-define(         'RETURN', 16#f3).
-define(   'DELEGATECALL', 16#f4).
-define(   'CALLBLACKBOX', 16#f5).

-define(     'STATICCALL', 16#fa).

-define(         'REVERT', 16#fd).
-define(        'INVALID', 16#fe).
-define(        'SUICIDE', 16#ff).

-define(        'COMMENT', comment).

%% Transactions are implemented as contract calls to address zero, with the
%% first argument encoding the transaction type according to the below.
-define(PRIM_CALL_SPEND, 1).

