(* ::Package:: *)

(* ::Text:: *)
(*Written December 17th, 2023.*)

(*Import*)

day = 17;
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

input = toExpression[
   Characters /@ StringSplit[Import[inputPath, "String"], "\n"]];

(*Part 1*)

tallyGather[tallies_List] := {#[[1, 1]], Min[#[[;; , 2]]]} & /@ 
   GatherBy[Flatten[tallies, 1], First];
directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
path = {{{{1, 1}, 1, 0}, 0}, {{{1, 1}, 2, 0}, 0}};
nextPos[{{oldPos_, dir_, steps_}, heat_}] :=
  
  Module[{new = {}, pos},
   pos = oldPos + directions[[dir]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]] \[And]
     steps < 3,
    AppendTo[
     new, {{pos, dir, steps + 1}, heat + input[[Sequence @@ pos]]}]
    ];
   pos = oldPos + directions[[Mod[dir + 1, 4, 1]]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]],
    AppendTo[
     new, {{pos, Mod[dir + 1, 4, 1], 1}, 
      heat + input[[Sequence @@ pos]]}]
    ];
   pos = oldPos + directions[[Mod[dir - 1, 4, 1]]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]],
    AppendTo[
     new, {{pos, Mod[dir - 1, 4, 1], 1}, 
      heat + input[[Sequence @@ pos]]}]
    ];
   new
   ];

ClearAll@seen;
seen[p_] := Infinity;
final = Infinity;
count = 0;
While[
  Length[path] >= 1,
  count += 1;
  globalWatch = {Length[path], count};
  path = Select[tallyGather[nextPos /@ path], seen[#[[1]]] > #[[2]] &];
  (seen[#[[1]]] = Min[seen[#[[1]]], #[[2]]]) & /@ path;
  Do[If[p[[1, 1]] === Dimensions[input], 
    final = Min[final, p[[2]]]], {p, path}];
  ];
final

(* Part 2 *)

tallyGather[tallies_List] := {#[[1, 1]], Min[#[[;; , 2]]]} & /@ 
   GatherBy[Flatten[tallies, 1], First];
directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
path = {{{{1, 1}, 1, 0}, 0}, {{{1, 1}, 2, 0}, 0}};
nextPos[{{oldPos_, dir_, steps_}, heat_}] :=
  
  Module[{new = {}, pos},
   pos = oldPos + directions[[dir]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]] \[And]
     steps < 10,
    AppendTo[
     new, {{pos, dir, steps + 1}, heat + input[[Sequence @@ pos]]}]
    ];
   pos = oldPos + directions[[Mod[dir + 1, 4, 1]]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]] \[And]
     steps >= 4,
    AppendTo[
     new, {{pos, Mod[dir + 1, 4, 1], 1}, 
      heat + input[[Sequence @@ pos]]}]
    ];
   pos = oldPos + directions[[Mod[dir - 1, 4, 1]]];
   If[Min[pos] >= 1 \[And] pos[[1]] <= Length[input] \[And] 
     pos[[2]] <= Length[input[[pos[[1]]]]] \[And]
     steps >= 4,
    AppendTo[
     new, {{pos, Mod[dir - 1, 4, 1], 1}, 
      heat + input[[Sequence @@ pos]]}]
    ];
   new
   ];

ClearAll@seen;
seen[p_] := Infinity;
final = Infinity;
count = 0;
While[
  Length[path] >= 1,
  count += 1;
  globalWatch = {Length[path], count};
  path = Select[tallyGather[nextPos /@ path], seen[#[[1]]] > #[[2]] &];
  (seen[#[[1]]] = Min[seen[#[[1]]], #[[2]]]) & /@ path;
  Do[If[p[[1, 1]] === Dimensions[input], 
    final = Min[final, p[[2]]]], {p, path}];
  ];
final