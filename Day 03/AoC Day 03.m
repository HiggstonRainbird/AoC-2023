(* ::Package:: *)

(* ::Text:: *)
(*Written December 3rd, 2023.*)

(*Import*)

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Import[inputPath,"List"];


(*Part 1*)

part[mat_, lis_] := 
  If[Depth[lis] == 1, Part[mat, Sequence @@ lis], 
   Table[Part[mat, Sequence @@ l], {l, lis}]];
ClearAll@neighborsD;
neighborsD[list_, i_, j_] := neighborsD[list, i, j] =
   Select[
    {i, j} + # & /@ {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 
       1}, {1, -1}, {1, 0}, {1, 1}},
    1 <= #[[1]] <= Length[list] \[And] 
      1 <= #[[2]] <= Length[list[[i]]] &];

integerPositions = 
  Table[StringPosition[input[[row]], DigitCharacter .., 
    Overlaps -> False], {row, Length[input]}];
symbolPositions = 
  Table[#[[1]] & /@ 
    StringPosition[input[[row]], 
     Except[Alternatives[DigitCharacter, "."]]], {row, Length[input]}];
table = Characters /@ input;

integerNeighbors = Table[
   Union[Flatten[
     neighborsD[table, row, #] & /@ 
      Range @@ integerPositions[[row, i]], 1]],
   {row, Length[integerPositions]},
   {i, Length[integerPositions[[row]]]}];
symbolAdjacentQ[row_, range_] :=
  Module[{neighborList},
   neighborList = 
    Union[Flatten[neighborsD[table, row, #] & /@ Range @@ range, 1]];
   Length[
     Complement[table[[#[[1]], #[[2]]]] & /@ neighborList,
      Join[ToString /@ Range[0, 9], {"."}]]] >= 1];

Table[
   ToExpression[StringTake[input[[row]], #]] & /@
    
    Select[integerPositions[[row]], symbolAdjacentQ[row, #] &],
   {row, Length[input]}] // Flatten // Total

(*Part 2*)

starPositions = 
  Flatten[Table[{row, #} & /@ (#[[1]] & /@ 
       StringPosition[input[[row]], "*"]), {row, Length[input]}], 1];

ClearAll@integerReplacements;
integerReplacements[{x_, y_}] := Nothing;
(integerReplacements[#[[1]]] = #[[2]]) & /@
  Flatten[
   Table[{{row, #}, {row, 
        integerPositions[[row, i, 1]]}} & /@ (Range @@ 
       integerPositions[[row, i]]),
    {row, Length[integerPositions]}, {i, 
     Length[integerPositions[[row]]]}],
   2];

ClearAll@integerStarts;
Do[
 integerStarts[{row, integerPositions[[row, i, 1]]}] = 
  ToExpression[StringTake[input[[row]], integerPositions[[row, i]]]],
 {row, Length[integerPositions]},
 {i, Length[integerPositions[[row]]]}];
 
Total[Times @@ # & /@ 
  Select[Table[
    integerStarts /@ 
     DeleteDuplicates[
      integerReplacements /@ neighborsD[table, Sequence @@ s]], {s, 
     starPositions}], Length[#] == 2 &]]
