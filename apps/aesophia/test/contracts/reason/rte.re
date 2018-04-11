
/* Primitive types */
type address     = string;
type uint        = int;
exception Abort;

type env =
  { mutable _caller : address
  };

let env = { _caller: "" };

/* Builtin functions */
let caller() : address = env._caller;
let abort() = raise(Abort);

let call(who, fn) = {
  env._caller = who;
  let res = try(fn()) { | e => env._caller = ""; raise(e) };
  env._caller = "";
  res
};

/* Library functions */
let require(x) = x ? () : abort();

/* -- Managing contract state ------------------------------------------------ */

type state_rep('s) = { mutable state : option('s) };

let newStateRep() : state_rep('s) = { state: None };

exception UninitializedState;
exception ReinitializedState;

let getState(rep) =
  switch(rep.state) {
  | None => raise(UninitializedState)
  | Some(s) => s
  };

let setState(rep, s) =
  switch(rep.state) {
  | None    => rep.state = Some(s)
  | Some(_) => raise(ReinitializedState)
  };

module type Contract = {
  type state;
  type args;
  let init : args => state;
  let stateRep : state_rep(state);
};

module Setup = (C : Contract) => {
  let init(creator, args) =
    setState(C.stateRep, call(creator, () => C.init(args)));
  let reset() = C.stateRep.state = None;
};

