-module(aestratum_share).

%% TODO: eunit

-export([new/3,
         user/1,
         miner_nonce/1,
         pow/1,
         validity/1,
         created/1,
         status/1
        ]).

-export([set_validity/2,
         is_duplicate/3
        ]).

-export_type([share/0,
              validity/0
             ]).

-type user()           :: aestratum_user_register:user().

-type miner_nonce()    :: aestratum_nonce:part_nonce().

-type pow()            :: aestratum_miner:pow().

-type validity()       :: undefined
                        | valid_block
                        | valid_share
                        | user_not_found
                        | invalid_miner_nonce
                        | duplicate_share
                        | invalid_solution
                        | high_target_share
                        | max_solve_time_exceeded
                        | job_not_found.

-type created()        :: aestratum_utils:timestamp().

-record(share, {
          user         :: user(),
          miner_nonce  :: miner_nonce(),
          pow          :: pow(),
          validity     :: validity(),
          created      :: created()
         }).

-opaque share()        :: #share{}.

-spec new(user(), miner_nonce(), pow()) -> share().
new(User, MinerNonce, Pow) ->
    #share{user = User,
           miner_nonce = MinerNonce,
           pow = Pow,
           created = aestratum_utils:timestamp()}.

-spec user(share()) -> user().
user(#share{user = User}) ->
    User.

-spec miner_nonce(share()) -> miner_nonce().
miner_nonce(#share{miner_nonce = MinerNonce}) ->
    MinerNonce.

-spec pow(share()) -> pow().
pow(#share{pow = Pow}) ->
    Pow.

-spec validity(share()) -> validity().
validity(#share{validity = Validity}) ->
    Validity.

-spec created(share()) -> created().
created(#share{created = Created}) ->
    Created.

-spec status(share()) -> map().
status(#share{user = User,
              miner_nonce = MinerNonce,
              pow = Pow,
              validity = Validity,
              created = Created}) ->
    #{user => User,
      miner_nonce => aestratum_nonce:to_hex(MinerNonce),
      pow => Pow,
      validity => Validity,
      created => Created}.

-spec set_validity(validity(), share()) -> share().
set_validity(Validity, #share{} = Share) ->
    Share#share{validity = Validity}.

-spec is_duplicate(miner_nonce(), pow(), share()) -> boolean().
is_duplicate(MinerNonce, Pow, #share{miner_nonce = MinerNonce, pow = Pow}) ->
    true;
is_duplicate(_MinerNonce, _Pow, _Share) ->
    false.

