(* ::Package:: *)

(* ::Text:: *)
(*Written December 7th, 2023.*)

(*Import*)

day = 7;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Import[inputPath,"Table"];


(*Part 1*)

cards = {"A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"};
lexOrder = Thread[# -> Range[Length[#]] &@cards];
orderBy[{hand_, bet_}] :=
  Module[{tally = Tally[hand]},
   {Length[tally], Sort[tally[[;; , 2]]], hand /. lexOrder}
   ];

sorted = SortBy[input, orderBy];
Sum[sorted[[-i, 2]]*i, {i, Length[sorted]}]

(* Part 2 *)

cards = {"A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"};
lexOrder = Thread[# -> Range[Length[#]] &@cards];

bestVersion[hand_] :=
  If[
   Length[DeleteDuplicates[hand]] == 1, 
   hand,
   hand /. ("J" -> Sort[Commonest[DeleteCases[hand, "J"]], SortBy[# /. lexOrder] &][[1]])];
orderBy[{hand_, bet_}] :=
  Module[{tally = Tally[bestVersion[hand]]},
   {Length[tally], Sort[tally[[;; , 2]]], (hand /. lexOrder)}
   ];

sorted = SortBy[
   input,
   orderBy[#] &];
Sum[sorted[[-i, 2]]*i, {i, Length[sorted], 1, -1}]
