(* ::Package:: *)

(* ::Text:: *)
(*Written December 14th, 2023.*)

(*Import*)

day = 14;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Characters /@ Import[inputPath, "List"];

(*Part 1*)

tilt[map_, dir_] :=
  Module[
   { newMap = map, pos, tmp},
   pos = Which[
     dir === {-1, 0}, Position[newMap, "O"],
     dir === {1, 0}, Reverse[Position[newMap, "O"]],
     dir === {0, -1}, SortBy[Position[newMap, "O"], Last],
     dir === {0, 1}, ReverseSortBy[Position[newMap, "O"], Last]
     ];
   newMap = newMap /. "O" -> ".";
   Do[tmp = p;
    While[
     Min[tmp] > 0 \[And] tmp[[1]] < Length[map] + 1 \[And] 
      tmp[[2]] < Length[map[[tmp[[1]]]]] + 1 \[And]
      newMap[[tmp[[1]], tmp[[2]]]] == ".",
     tmp += dir;
     ];
    tmp -= dir;
    newMap[[tmp[[1]], tmp[[2]]]] = "O",
    {p, pos}];
   newMap
   ];

load[map_] := Total[Length[map] + 1 - Position[map, "O"][[;; , 1]]];

load[tilt[input, {-1, 0}]]

(* Part 2 *)

cycle[map_] := Fold[tilt, map, {{-1, 0}, {0, -1}, {1, 0}, {0, 1}}];

ClearAll@seen; seen[s_] := -1;
ClearAll@index; index[n_] := {};

newMap = input;
count = 0;
While[seen[newMap] == -1,
  seen[newMap] = count;
  index[count] = newMap;
  newMap = cycle[newMap];
  count += 1];
  
finalMap = index[seen[newMap] + Mod[10^9 - seen[newMap], count - seen[newMap]]];
load[finalMap]