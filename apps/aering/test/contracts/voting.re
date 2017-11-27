
open Rte;

/*-- Types ------------------------------------------------------------------*/

/* Not so nice */
module AddrKey = { type t = address; let compare = Pervasives.compare };
module AddrMap = Map.Make(AddrKey);
type addr_map('a) = AddrMap.t('a);

type proposal =
  { name: string
  , mutable voteCount: uint
  };

type voter =
  { mutable weight: int
  , mutable delegate: option(address)
  , mutable vote: option(int)
  };

type state =
  { chairPerson: address
  , mutable voters: addr_map(voter)
  , proposals: list(proposal)
  };

/* Initialization */
type args = list(string);
let init(proposalNames: args): state =
  { chairPerson: caller(),
    voters:      AddrMap.empty,
    proposals:   List.map((name) => {name: name, voteCount: 0}, proposalNames)
  };

/* Boilerplate */
let stateRep = newStateRep();
let state()  = getState(stateRep);

let initVoter() = { weight: 1, delegate: None, vote: None };

let giveRightToVote(voter: address) = {
  require(caller() == state().chairPerson);
  require(!AddrMap.mem(voter, state().voters));
  state().voters = AddrMap.add(voter, initVoter(), state().voters);
  ()
};

let rec delegateChain(delegate: address) = {
  require(delegate != caller()); /* Delegation loop! */
  let voter = AddrMap.find(delegate, state().voters);
  switch(voter.delegate) {
  | None    => delegate;
  | Some(d) => delegateChain(d)
  }
};

let addVote(candidate, weight) = {
  let proposal = List.nth(state().proposals, candidate);
  proposal.voteCount = proposal.voteCount + weight;
};

let delegateVote(delegateTo: address, weight: uint) = {
  let voter = AddrMap.find(delegateTo, state().voters);
  switch(voter.vote) {
  | Some(vote) => addVote(vote, weight)
  | None => voter.weight = voter.weight + weight
  }
};

let delegate(delegateTo: address) = {
  require(delegateTo != caller());
  let voter = AddrMap.find(caller(), state().voters);
  require(voter.vote == None);
  let finalDelegate = delegateChain(delegateTo);
  voter.vote = Some(0); /* Hm... */
  voter.delegate = Some(finalDelegate);
  delegateVote(finalDelegate, voter.weight)
};

let vote(candidate: uint) = {
  let voter = AddrMap.find(caller(), state().voters);
  require(voter.vote == None);
  voter.vote = Some(candidate);
  addVote(candidate, voter.weight);
};

let rec winningProposal'(current, rest) =
  switch(rest) {
  | []         => current;
  | [p, ...ps] => winningProposal'(p.voteCount > current.voteCount ? p : current, ps)
  };

/* const */
let winningProposal() : proposal = {
  switch(state().proposals) {
  | [] => abort()
  | [p, ...ps] => winningProposal'(p, ps)
  }
};

/* const */
let winnerName() = winningProposal().name;

