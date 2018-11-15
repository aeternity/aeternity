
open Rte;
open Voting;

let creator = "0x123";
let voter1  = "0x1001";
let voter2  = "0x1002";
let voter3  = "0x1003";
let other   = "0xffff";

module SetupVote = Setup(Voting);
open Voting;

let print_tally() = {
  let tally = call(other, () => currentTally());
  List.map(((name, count)) => Printf.printf("%s: %d\n", name, count), tally);
  let winner = call(other, () => winnerName());
  Printf.printf("Winner: %s\n", winner);
};

Printf.printf("Delegate before vote\n");
SetupVote.init(creator, ["Cake", "Beer"]);
call(creator, () => giveRightToVote(voter1));
call(creator, () => giveRightToVote(voter2));
call(creator, () => giveRightToVote(voter3));
call(voter3,  () => delegate(voter1));
call(voter1,  () => vote(1));
call(voter2,  () => vote(0));
print_tally();

SetupVote.reset();

Printf.printf("Delegate after vote\n");
SetupVote.init(creator, ["Cake", "Beer"]);
call(creator, () => giveRightToVote(voter1));
call(creator, () => giveRightToVote(voter2));
call(creator, () => giveRightToVote(voter3));
call(voter1,  () => vote(1));
call(voter3,  () => delegate(voter1));
call(voter2,  () => vote(0));
print_tally();

