(* ::Package:: *)

(* ::Text:: *)
(*Written December 9th, 2023.*)

(*Import*)

day = 9;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
toExpression[inputText_] :=
  Map[
   If[! IntegerQ[#] \[And] 
      StringMatchQ[#, 
       Alternatives["+", "-", ""] ~~ DigitCharacter ..], 
     ToExpression[#], #] &,
   inputText,
   {Depth[inputText] - 1, Depth[inputText]}];
input = toExpression[Import[inputPath, "Table"]];
    
(*Part 1*)

Total[Table[FindSequenceFunction[s, Length[s] + 1], {s, input}]]

(* Part 2 *)

Total[Table[FindSequenceFunction[s, 0], {s, input}]]
