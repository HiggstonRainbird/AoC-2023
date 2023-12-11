(* ::Package:: *)

(* ::Text:: *)
(*Written December 11th, 2023.*)

(*Import*)

day = 11;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Characters /@ Import[inputPath, "List"];

(*Setup*)

expandUniverse[factor_, array_] :=
 Module[{sp, emptyRows, emptyColumns, oldPos, newPos, subs},
  oldPos = Position[array, "#"];
  sp = SparseArray[# -> 1 & /@ oldPos];
  emptyRows = Flatten[Position[Total /@ sp, 0]];
  emptyColumns = Flatten[Position[Total /@ Transpose[sp], 0]];
  newPos =
   Table[
    pos + (factor - 1)*{
      Count[emptyRows, _?(# < pos[[1]] &)], 
      Count[emptyColumns, _?(# < pos[[2]] &)]},
    {pos, oldPos}];
  subs = Subsets[newPos, {2}];
  Total[ManhattanDistance @@ # & /@ subs]]

(*Part 1*)

expandUniverse[2, input]

(* Part 2 *)

expandUniverse[10^6, input]
