(* ::Package:: *)

(* ::Text:: *)
(*Written December 23rd, 2023.*)

(*Import*)

day = 23;

session = 
  Import[FileNameJoin[{NotebookDirectory[], "..", "Session.txt"}]];
SetCookies[<|"Domain" -> ".adventofcode.com", "Path" -> "/", 
   "Name" -> "session", "Content" -> session|>];

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Characters /@ Import[inputPath, "List"];

(* Setup *)

importGraph[inputText_] :=
  
  Module[{board, viewBoard, doors, keys, style, g, position},
   board = 
    Flatten[Table[{i, j} -> inputText[[i, j]], {i, 
       Length[inputText]}, {j, Length[inputText[[i]]]}]];
   viewBoard = SparseArray[board];
   
   doors = 
    Sort[viewBoard[[#[[1]], #[[2]]]] -> # & /@ 
      Position[inputText, _?(UpperCaseQ[#] &), {2}, Heads -> False]];
   keys = 
    Join[{"@" -> position}, 
     Sort[viewBoard[[#[[1]], #[[2]]]] -> # & /@ 
       Position[inputText, _?(LowerCaseQ[#] &), {2}, Heads -> False]]];
   
   style = {Background -> GrayLevel[0], 
     BaseStyle -> {Directive[White, EdgeForm[], Opacity[1]]}, 
     VertexShapeFunction -> (Rectangle[#1 + .5, #1 - .5] &), 
     EdgeShapeFunction -> (Rectangle[#1[[1]] + .5, #1[[2]] - .5] &)};
   
   g = Graph[Union[Flatten[Table[
         If[
          StringMatchQ[viewBoard[[i, j]], Except[{"#"}]],
          {If[
            i + 1 <= Length[viewBoard] \[And] 
             StringMatchQ[viewBoard[[i + 1, j]], Except[{"#"}]], 
            ToString[{i, j}] \[UndirectedEdge] ToString[{i + 1, j}]],  
           If[j + 1 <= Length[viewBoard[[i]]] \[And] 
             StringMatchQ[viewBoard[[i, j + 1]], Except[{"#"}]], 
            ToString[{i, j}] \[UndirectedEdge] ToString[{i, j + 1}]]}
          ], {i, Length[inputText]}, {j, Length[inputText[[i]]]}]]][[
      2 ;;]]]
   ];

chars = {">", "v", "<", "^"};
directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
{start, end} = ToString /@ {First[#], Last[#]} &@Position[input, "."];

(*Part 1*)

tempGraph = importGraph[input];
toRemove =
  Join[
   Flatten[
    Table[ToString[pos] \[UndirectedEdge] ToString[pos + d], {c, 
      Length[chars]}, {d, directions}, {pos, 
      Position[input, chars[[c]]]}], 2],
   Reverse /@ 
    Flatten[Table[
      ToString[pos] \[UndirectedEdge] ToString[pos + d], {c, 
       Length[chars]}, {d, directions}, {pos, 
       Position[input, chars[[c]]]}], 2]];
toAdd =
  Union@Join[
    Flatten[Table[
      If[input[[Sequence @@ (pos + d)]] != "#", 
       ToString[pos + d] -> ToString[pos], Nothing],
      {c, Length[chars]}, {d, directions}, {pos, 
       Position[input, chars[[c]]]}], 2],
    Flatten[
     Table[DirectedEdge[ToString[pos], 
       ToString[pos + directions[[c]]]], {c, Length[chars]}, {pos, 
       Position[input, chars[[c]]]}], 1]];

tempGraph = 
  EdgeDelete[tempGraph, Intersection[EdgeList[tempGraph], toRemove]];
tempGraph = EdgeAdd[tempGraph, toAdd];

Max[Length[#] - 1 & /@ FindPath[tempGraph, start, end, {0, \[Infinity]}, All]]

(* Part 2 *)

tempGraph = importGraph[input];

pointsOfInterest = 
  Join[{start, end}, 
   Select[VertexList[tempGraph], VertexDegree[tempGraph, #] > 2 &]];
newEdges =
  Flatten[Table[
    tmp = tempGraph;
    tmp = VertexDelete[tempGraph, 
      Complement[pointsOfInterest, pointsOfInterest[[{p1, p2}]]]];
    If[
       Length[#] == 0,
       Nothing,
       {Sort[
         pointsOfInterest[[p1]] \[UndirectedEdge] 
          pointsOfInterest[[p2]]], Length[#[[1]]] - 1}
       ] &@
     MaximalBy[
      FindPath[tmp, pointsOfInterest[[p1]], pointsOfInterest[[p2]], 
       All, All], Length],
    {p1, Length[pointsOfInterest]},
    {p2, p1 + 1, Length[pointsOfInterest]}], 1];

actualDistance[{w1_, w2_}] := 
  Sort[w1 \[UndirectedEdge] w2] /. 
   Thread[newEdges[[;; , 1]] -> newEdges[[;; , 2]]];
compressedGraph = 
  Graph[Transpose[newEdges][[1]], 
   EdgeWeight -> Transpose[newEdges][[2]], VertexLabels -> Automatic, 
   EdgeLabels -> "EdgeWeight"];

path = FindPath[compressedGraph, start, end, {0, \[Infinity]}];
i = 1;

While[Length[path] > 0,
  max = Total[actualDistance /@ Partition[path[[1]], 2, 1]];
  path = FindPath[compressedGraph, start, end, {max + 1, \[Infinity]}];
  i += 1;
  globalWatch = {max, i};
  ];
max