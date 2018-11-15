%%% File        : aeo_oracles_mock.erl
%%% Author      : Hans Svensson
%%% Description :
%%% Created     : 19 Dec 2017 by Hans Svensson
-module(aeo_oracles_mock).

-compile(export_all).
-compile(nowarn_export_all).

%% aec_tx_sign
data(S) -> S.
verify(_) -> ok.
serialize_to_binary(X) -> term_to_binary(X).

%% aec_target
verify(_, _) -> ok.
