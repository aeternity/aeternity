-module(aec_governance_utils).

%% API
-export([state_gas/2]).

state_gas(_GasPerKeyBlock = {Part, Whole}, NKeyBlocks)
  when is_integer(Whole), Whole > 0,
       is_integer(Part), Part >= 0,
       is_integer(NKeyBlocks), NKeyBlocks >= 0 ->
    Tmp = NKeyBlocks * Part,
    (Tmp + (Whole - 1)) div Whole. %% Ceiling of the fraction.
