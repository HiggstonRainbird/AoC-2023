(* ::Package:: *)

(* ::Text:: *)
(*Written December 6th, 2023.*)

(*Import*)

day = 6;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Import[inputPath,"Table"];


(*Part 1*)

winConditions[{time_, distance_}] :=
 Ceiling[time/2 + 1/2 Sqrt[-4 distance + time^2]] - 
  Floor[time/2 - 1/2 Sqrt[-4 distance + time^2]] + 1;

Times @@ (winConditions /@ Transpose[input[[;; , 2 ;;]]])

(* Part 2 *)

fromDigits[nums_] := Fold[10^Ceiling[Log10[#2]]*#1 + #2 &, 0, nums];
newInput = fromDigits /@ input[[;; , 2 ;;]];
winConditions[newInput]
