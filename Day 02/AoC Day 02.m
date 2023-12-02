(* ::Package:: *)

(* ::Text:: *)
(*Written December 2nd, 2023.*)

(*Import*)

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input=StringSplit[Import[inputPath],"\n\n"];
input = StringSplit[#, " " | ", "] & /@ StringSplit[#, Alternatives[": ", "; "]] & /@ StringSplit[Import[inputPath], "\n"];

(*Part 1*)

assoc =
  Association @@ # & /@
   Table[#[[1, 2]] -> Max[ToExpression[#[[;; , 1]]]] & /@
     GatherBy[
      Flatten[#[[1]] -> #[[2]] & /@ Partition[#, 2] & /@ 
        input[[row, 2 ;;]]], Last],
    {row, Length[input]}];

Position[assoc, _?(#["red"] <= 12 && #["green"] <= 13 && #["blue"] <= 14 &)] // Flatten // Total

(*Part 2*)

Total[Times @@ Transpose[Values /@ assoc]]
