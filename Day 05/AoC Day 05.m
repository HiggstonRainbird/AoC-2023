(* ::Package:: *)

(* ::Text:: *)
(*Written December 5th, 2023.*)

(*Import*)

day = 4;
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

input = StringSplit[Import[inputPath], "\n\n"];
input = toExpression[
   StringSplit[#, " "] & /@ StringSplit[#, "\n"] & /@ input];

(*Part 1*)

seedData = Association @@ (#[[1, 1]] -> Sort[#[[2 ;;]]] & /@ input);
seedData["seeds:"] = input[[1, 1, 2 ;;]];
seedData[" "] = {};

ClearAll@nextKey; ClearAll@finalKey;
nextKey[{conv_, num_}] :=
  {Keys[seedData][[
    Position[Keys[seedData], conv][[1, 1]] + 1]],
   (#[[1]] - #[[2]] + num) &@
    SelectFirst[
     seedData[conv], #[[3]] > num - #[[2]] >= 0 &, {0, 0, 0}]};
finalKey[num_] :=
  
  Nest[nextKey, {"seed-to-soil", num}, Length[Keys[seedData]] - 2];
Min[Table[finalKey[s], {s, seedData["seeds:"]}][[;; , -1]]]

(* Part 2 *)

initialIntervals = 
  Sort[Interval[{#[[1]], #[[1]] + #[[2]] - 1}] & /@ 
    Partition[seedData["seeds:"], 2]];

minRange = MinimalBy[
 Table[# -> finalKey[#][[-1]] &@
   RandomInteger[RandomChoice[initialIntervals][[1]]], {i, 
   100000}], Last];

a = minRange[[1]];
b = minRange[[2]];
While[True,
 a -= 1;
 b2 = finalKey[a][[-1]];
 down = (b2 < b);
 If[! down, Return[{a + 1, b}], b = b2]
 ]
