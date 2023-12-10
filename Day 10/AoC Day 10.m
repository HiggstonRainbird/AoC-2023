(* ::Package:: *)

(* ::Text:: *)
(*Written December 10th, 2023.*)

(*Import*)

day = 10;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Characters /@ Import[inputPath, "List"];

(*Part 1*)

neighbors[list_, {i_, j_}] :=
  Select[
   {i, j} + # & /@ {{-1, 0}, {1, 0}, {0, -1}, {0, 1}},
   1 <= #[[1]] <= Length[list] \[And] 
     1 <= #[[2]] <= Length[list[[i]]] &];
ClearAll@possibleNeighbors;
possibleNeighbors[list_, {i_, j_}] := possibleNeighbors[list, {i, j}] =
   Intersection[
    Which[
     list[[i, j]] == "|", {i, j} + # & /@ {{-1, 0}, {1, 0}},
     list[[i, j]] == "-", {i, j} + # & /@ {{0, -1}, {0, 1}},
     list[[i, j]] == "L", {i, j} + # & /@ {{-1, 0}, {0, 1}},
     list[[i, j]] == "J", {i, j} + # & /@ {{-1, 0}, {0, -1}},
     list[[i, j]] == "7", {i, j} + # & /@ {{1, 0}, {0, -1}},
     list[[i, j]] == "F", {i, j} + # & /@ {{1, 0}, {0, 1}},
     list[[i, j]] == ".", {},
     list[[i, j]] == "S", neighbors[list, {i, j}]
     ],
    neighbors[list, {i, j}]];
realNeighbors[list_, {i_, j_}] :=
  Select[
   possibleNeighbors[list, {i, j}],
   MemberQ[possibleNeighbors[list, #], {i, j}] &];
g = Graph[
   Flatten[
    Table[
     ToString[{i, j}] \[UndirectedEdge] ToString[#] & /@ 
      realNeighbors[input, {i, j}],
     {i, Length[input]}, {j, Length[input[[i]]]}]]];
s = ToString[FirstPosition[input, "S"]];
part1 = Select[GraphDistance[g, s], # != Infinity &] // Max

(* Part 2 *)

loopGraph = NeighborhoodGraph[g, s, part1];
loop = ToExpression /@ VertexList[loopGraph];

ClearAll@filled;
filled[pos_] := False;
(filled[#] = True) & /@ loop;

newNeighbors[list_, {i_, j_}] := 
  Select[neighbors[list, {i, j}], ! filled[#] &];
g2 = Graph[
   Flatten[Table[
     ToString[{i, j}] \[UndirectedEdge] ToString[#] & /@ 
      newNeighbors[input, {i, j}], {i, Length[input]}, {j, 
      Length[input[[i]]]}]]];
outside = 
  ToExpression /@ VertexList[NeighborhoodGraph[g2, "{1, 1}", 100000]];

toCheck = Complement[
   Flatten[Table[{i, j}, {i, Length[input]}, {j, Length[input[[i]]]}],
     1],
   loop,
   outside];

p = Polygon[
   ToExpression[FindHamiltonianCycle[loopGraph][[1, ;; , 1]]]];
r = Region[p];
img = Show[
   Region[
    Polygon[ToExpression[
      FindHamiltonianCycle[loopGraph][[1, ;; -1, 1]]]]],
   Graphics[{Black, Point /@ toCheck}]
   ];

Length[toCheck] -
 (Length[ComponentMeasurements[
     ColorNegate[Binarize[Show[img, ImageSize -> 1000]]], "Count"]] - 
   1)
