
open Rte;

let creator = "0x123";
let voter1  = "0x1001";
let voter2  = "0x1002";
let other   = "0xffff";

module SetupVote = Setup(Voting);
open Voting;

SetupVote.init(creator, ["Cake", "Beer"]);
call(creator, () => giveRightToVote(voter1));
call(creator, () => giveRightToVote(voter2));
call(voter1,  () => vote(1));
call(voter2,  () => vote(0));
let winner = call(other, () => winnerName());
print_string(winner ++ "\n");
