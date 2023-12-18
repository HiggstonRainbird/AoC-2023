(* ::Package:: *)

(* ::Text:: *)
(*Written December 18th, 2023.*)

(*Import*)

day = 18;
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

directions = 
  Thread[{"R", "D", "L", "U"} -> {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}];
instructions = {#[[2]], #[[1]] /. directions} & /@ input;

trench = FoldList[#1 + Times @@ #2 &, {0, 0}, instructions];
(Area[Polygon[trench]] + Perimeter[Polygon[trench]]/2 + 1)

(* Part 2 *)

directions = 
  Thread[{0, 1, 2, 3} -> {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}];
instructions = {
     FromDigits[
      toExpression[#[[3 ;; 7]]] /. 
       Thread[CharacterRange["a", "f"] -> Range[10, 15]], 16],
     ToExpression[#[[8]]] /. directions} & /@
   
   Characters[input[[;; , 3]]];

trench = FoldList[#1 + Times @@ #2 &, {0, 0}, instructions];
(Area[Polygon[trench]] + Perimeter[Polygon[trench]]/2 + 1)