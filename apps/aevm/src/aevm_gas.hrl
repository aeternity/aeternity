
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

%% Amount of gas to pay for operations of the set Wextcodesize.
-define(GEXTCODESIZE, 20). %% From go implementation

%% Amount of gas to pay for operations of the set Wextcodecopy.
-define(GEXTCODECOPY, 20). %% From go implementation

%% Amount of gas to pay for a BALANCE operation.
-define(GBALANCE, 20). %% From the go implementation. 400 in yellowpages

%% Paid for a SLOAD operation.
-define(GSLOAD, 50). %% From the go implementation. 200 in yellowpaper

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

%% From the go implementation
%% -define(GSUICIDE, 0).

%% Paid for a CREATE operation.
-define(GCREATE, 32000).

%% Paid per byte for a CREATE operation to succeed in placing code
%% into state.
-define(GCODEDEPOSIT, 200).

%% Paid for a CALL operation.
-define(GCALL, 40). %% From the go implementation. 700 from the yellowpaper

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
-define(GEXPBYTE, 10). %% From the go implementation. 50 from the yellowpages

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

%% From https://github.com/ethereum/go-ethereum/blob/master/params/gas_table.go
%% package params

%% type GasTable struct {
%% 	ExtcodeSize uint64
%% 	ExtcodeCopy uint64
%% 	Balance     uint64
%% 	SLoad       uint64
%% 	Calls       uint64
%% 	Suicide     uint64

%% 	ExpByte uint64

%% 	// CreateBySuicide occurs when the
%% 	// refunded account is one that does
%% 	// not exist. This logic is similar
%% 	// to call. May be left nil. Nil means
%% 	// not charged.
%% 	CreateBySuicide uint64
%% }
%%
%% var (
%% 	// GasTableHomestead contain the gas prices for
%% 	// the homestead phase.
%% 	GasTableHomestead = GasTable{
%% 		ExtcodeSize: 20,
%% 		ExtcodeCopy: 20,
%% 		Balance:     20,
%% 		SLoad:       50,
%% 		Calls:       40,
%% 		Suicide:     0,
%% 		ExpByte:     10,
%% 	}

%% 	// GasTableHomestead contain the gas re-prices for
%% 	// the homestead phase.
%% 	GasTableEIP150 = GasTable{
%% 		ExtcodeSize: 700,
%% 		ExtcodeCopy: 700,
%% 		Balance:     400,
%% 		SLoad:       200,
%% 		Calls:       700,
%% 		Suicide:     5000,
%% 		ExpByte:     10,

%% 		CreateBySuicide: 25000,
%% 	}

%% 	GasTableEIP158 = GasTable{
%% 		ExtcodeSize: 700,
%% 		ExtcodeCopy: 700,
%% 		Balance:     400,
%% 		SLoad:       200,
%% 		Calls:       700,
%% 		Suicide:     5000,
%% 		ExpByte:     50,

%% 		CreateBySuicide: 25000,
%% 	}
%% )
