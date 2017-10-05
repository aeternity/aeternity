
%% From the yellow pages

%% Nothing paid for operations of the set Wzero.
-define(GZERO, 0).

%% Amount of gas to pay for operations of the set Wbase.
-define(GBASE, 2).

%% Amount of gas to pay for operations of the set Wverylow.
-define(GVERYLOW, 3).

%% Amount of gas to pay for operations of the set Wlow.
-define(GLOW, 5).

%% Amount of gas to pay for operations of the set Wmid.
-define(GMID, 8).

%% Amount of gas to pay for operations of the set Whigh.
-define(GHIGH, 10).

%% Amount of gas to pay for operations of the set Wextcode.
-define(GEXTCODE, 700).

%% Amount of gas to pay for a BALANCE operation.
-define(GBALANCE, 400).

%% Paid for a SLOAD operation.
-define(GSLOAD, 200).

%% Paid for a JUMPDEST operation.
-define(GJUMPDEST, 1).

%% Paid for an SSTORE operation when the storage value is set to
%% non-zero from zero.
-define(GSSET, 20000).

%% Paid for an SSTORE operation when the storage value’s zeroness
%% remains unchanged or is set to zero.
-define(GSRESET, 5000).

%% Refund given (added into refund counter) when the storage value is
%% set to zero from non-zero.
-define(RSCLEAR, 15000).

%% Refund given (added into refund counter) for self-destructing an
%% account.
-define(RSELFDESTRUCT, 24000).

%% Amount of gas to pay for a SELFDESTRUCT operation.
-define(GSELFDESTRUCT, 5000).

%% Paid for a CREATE operation.
-define(GCREATE, 32000).

%% Paid per byte for a CREATE operation to succeed in placing code
%% into state.
-define(GCODEDEPOSIT, 200).

%% Paid for a CALL operation.
-define(GCALL, 700).

%% Paid for a non-zero value transfer as part of the CALL operation.
-define(GCALLVALUE, 9000).

%% A stipend for the called contract subtracted from Gcallvalue for a
%% non-zero value transfer.
-define(GCALLSTIPEND, 2300).

%% Paid for a CALL or SELFDESTRUCT operation which creates an account.
-define(GNEWACCOUNT, 25000).

%% Partial payment for an EXP operation.
-define(GEXP, 10).

%% Partial payment when multiplied by dlog256(exponent)e for the EXP
%% operation.
-define(GEXPBYTE, 50).

%% Paid for every additional word when expanding memory.
-define(GMEMORY, 3).

%% Paid by all contract-creating transactions after the Homestead
%% transition.
-define(GTXCREATE, 32000).

%% Paid for every zero byte of data or code for a transaction.
-define(GTXDATAZERO, 4).

%% Paid for every non-zero byte of data or code for a transaction.
-define(GTXDATANONZERO, 68).

%% Paid for every transaction.
-define(GTRANSACTION, 21000).

%% Partial payment for a LOG operation.
-define(GLOG, 375).

%% Paid for each byte in a LOG operation’s data.
-define(GLOGDATA, 8).

%% Paid for each topic of a LOG operation.
-define(GLOGTOPIC, 375).

%% Paid for each SHA3 operation.
-define(GSHA3, 30).

%% Paid for each word (rounded up) for input data to a SHA3 operation.
-define(GSHA3WORD, 6).

%% Partial payment for *COPY operations, multiplied by words copied,
%% rounded up.
-define(GCOPY, 3).

%% Payment for BLOCKHASH operation.
-define(GBLOCKHASH, 20).
