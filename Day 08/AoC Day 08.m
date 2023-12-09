(* ::Package:: *)

(* ::Text:: *)
(*Written December 8th, 2023.*)

(*Import*)

day = 8;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = toExpression[
   StringSplit[#, Alternatives[" = (", ", ", ")"]] & /@ 
    StringSplit[Import[inputPath], "\n"]];
    
(*Setup*)

directions = Characters[input[[1, 1]]] /. {"L" -> 1, "R" -> 2};
(map[#[[1]]] = #[[2 ;;]]) & /@ input[[3 ;;]];
nextState[{p_, c_}] := {map[p][[directions[[c]]]], Mod[c + 1, Length[directions], 1]};
findZ[pos_] :=
  Module[{state = {pos, 1}, count = 0},
   While[
   Characters[state[[1]]][[-1]] != "Z",
   count += 1; state = nextState[state]];
   count];

(*Part 1*)

findZ["AAA"]

(* Part 2 *)

LCM @@ (findZ /@ Select[input[[3 ;;, 1]], Characters[#][[-1]] == "A" &])
