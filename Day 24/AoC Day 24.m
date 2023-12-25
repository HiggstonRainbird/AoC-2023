(* ::Package:: *)

(* ::Text:: *)
(*Written December 24th, 2023.*)

(*Import*)

day = 24;

session = 
  Import[FileNameJoin[{NotebookDirectory[], "..", "Session.txt"}]];
SetCookies[<|"Domain" -> ".adventofcode.com", "Path" -> "/", 
   "Name" -> "session", "Content" -> session|>];

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
   StringSplit[#, ("," ~~ 
         WhitespaceCharacter ...) | (WhitespaceCharacter ... ~~ "@" ~~
          WhitespaceCharacter ...)] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(*Part 1*)

intersection[{{x1_, y1_, z1_, vx1_, vy1_, vz1_}, {x2_, y2_, z2_, vx2_,
      vy2_, vz2_}}] :=
  Module[{k1, k2, p},
   k1 = -((-vy2 x1 + vy2 x2 + vx2 y1 - vx2 y2)/(vx2 vy1 - vx1 vy2));
   k2 = -((-vy1 x1 + vy1 x2 + vx1 y1 - vx1 y2)/(vx2 vy1 - vx1 vy2));
   p = {k1 vx1 + x1, k1 vy1 + y1};
   {p, k1, k2}];

within[point_, min_, max_] := 
  If[Min[point] >= min \[And] Max[point] <= max, True, False];

pairs = Subsets[input, {2}];
sum = 0;
min = 200000000000000;
max = 400000000000000;
futureQ[pointx_, 
   linex_] := ((-pointx + linex[[1]])/(linex[[1]] - linex[[2]]) > 0);

Quiet@Count[
  intersection /@ 
   pairs, _?(within[#[[1]], min, max] \[And] #[[2]] > 
       0 \[And] #[[3]] > 0 &)]

(* Part 2 *)

x + y + z /. Solve[
  And @@ 
  Table[
    {x, y, z} + Subscript[k, i] {vx, vy, vz} == 
    input[[i, 1 ;; 3]] + Subscript[k, i]*input[[i, 4 ;; 6]], 
  {i, 3}],
  {x, y, z, vx, vy, vz}~Join~Table[Subscript[k, i], {i, 3}]
][[1]]