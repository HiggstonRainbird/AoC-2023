(* ::Package:: *)

(* ::Text:: *)
(*Written December 22nd, 2023.*)

(*Import*)

day = 22;
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
   Partition[StringSplit[#, "," | "~"], 3] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(*Setup*)

cubes = SortBy[input, #[[1, -1]] &];

ClearAll@filled;
filled[{x_, y_, z_}] := \[Infinity];
filled[{x_, y_, z_ /; z < 1}] := -1;

newCubes = CreateDataStructure["DynamicArray"];
Do[
  newC = cubes[[c]];
  While[
   tmp = (# - {0, 0, 1}) & /@ newC;
   AllTrue[Tuples[Range @@@ Transpose[tmp]], 
    filled[#] == \[Infinity] &],
   newC = tmp];
  Do[filled[t] = c, {t, Tuples[Range @@@ Transpose[newC]]}];
  newCubes["Append", newC];
  , {c, Length[cubes]}];

g = Graph[
   Union[Flatten[
     Table[
      # -> c & /@ Select[Table[
         filled[t - {0, 0, 1}],
         {t, Tuples[Range @@@ Transpose[newCubes["Part", c]]]}],
        # != \[Infinity] \[And] # != c &],
      {c, newCubes["Length"]}]]]];

(* Part 1 *)

Count[
 VertexList[g],
 _?(AllTrue[
     VertexList[VertexOutComponentGraph[g, #, {1}]],
     VertexInDegree[g, #] > 1 &] &)]

(* Part 2 *)

Sum[
  VertexCount[g]-1-
  VertexCount[FixedPoint[
    VertexDelete[#,_?(Function[x,VertexInDegree[#,x]==0\[And]x!=-1])]&,
    VertexDelete[g,v]]],
  {v,VertexList[g][[2;;]]}]