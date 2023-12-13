(* ::Package:: *)

(* ::Text:: *)
(*Written December 13th, 2023.*)

(*Import*)

day = 12;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Characters[StringSplit[StringSplit[Import[inputPath], "\n\n"], "\n"]];

(*Setup*)

allMirrors[mat_] :=
  Table[
   StringJoin /@ 
    {mat[[Max[2 n + 1 - Length[mat], 1] ;; n]], 
     mat[[Min[2 n, Length[mat]] ;; n + 1 ;; -1]]},
   {n, Length[mat] - 1}];

reflectionNum[mat_, diff_] :=
 Module[
  {hor = HammingDistance @@@ allMirrors[mat], 
   ver = HammingDistance @@@ allMirrors[Transpose[mat]]},
  If[MemberQ[hor, diff], 100 FirstPosition[hor, diff][[1]], 
   FirstPosition[ver, diff][[1]]]]

(*Part 1*)

Total[reflectionNum[#, 0] & /@ input]

(* Part 2 *)

Total[reflectionNum[#, 1] & /@ input]