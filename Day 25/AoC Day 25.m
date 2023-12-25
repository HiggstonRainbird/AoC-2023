(* ::Package:: *)

(* ::Text:: *)
(*Written December 25th, 2023.*)

(*Import*)

day = 25th;

session = 
  Import[FileNameJoin[{NotebookDirectory[], "..", "Session.txt"}]];
SetCookies[<|"Domain" -> ".adventofcode.com", "Path" -> "/", 
   "Name" -> "session", "Content" -> session|>];

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = toExpression[StringSplit[#, ":" | " "] & /@ StringSplit[Import[inputPath], "\n"]];

(*Part 1*)

g = Flatten[Table[#[[1]] \[UndirectedEdge] n, {n, #[[3 ;;]]}] & /@ input];
Times @@ (Length /@ FindMinimumCut[g][[2]])