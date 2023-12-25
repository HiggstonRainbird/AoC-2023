(* ::Package:: *)

(* ::Text:: *)
(*Written December 19th, 2023.*)

(*Import*)

day = 19;
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

input = Import[inputPath, "List"];

(* Part 1 *)

split = Position[input, ""][[1, 1]];
rules = input[[;; split - 1]];
parts = input[[split + 1 ;;]];
fullRules = Association @@ Table[
    tmp = Quiet@StringCases[
        rule,
        part : LetterCharacter .. ~~ "{" ~~ 
          r : (LetterCharacter .. ~~ Alternatives["<", ">", "="] ~~ 
              DigitCharacter .. ~~ ":" ~~ LetterCharacter .. ~~ 
              ",") .. ~~ sent : LetterCharacter .. ~~ "}" -> {part, r,
           sent}][[1]];
    tmp[[1]] -> Join[
      StringCases[#, (comp : (LetterCharacter .. ~~ 
                Alternatives["<", ">", "="] ~~ DigitCharacter ..) ~~ 
             ":" ~~ dest : 
              LetterCharacter ..) ->
           {ToExpression[comp], 
            dest}][[1]] & /@ StringSplit[tmp[[2]], ","],
      {{True, tmp[[3]]}}],
    {rule, rules}];
fullParts = 
  Table[Association @@ 
    ToExpression[
     StringReplace[
      StringSplit[p, ","], {"=" -> "->", 
       Alternatives["{", "}"] -> ""}]], {p, parts}];

ClearAll@evaluatePart;
evaluatePart[p_, rule_] :=
  If[rule == "A" \[Or] rule == "R",
   Return[rule],
   Do[If[ToExpression[r[[1]]] /. p, 
     Return[evaluatePart[p, r[[2]]]]], {r, fullRules[rule]}]];
Select[Table[{Total[Values[p]], evaluatePart[p, "in"]}, {p, 
     fullParts}], #[[2]] == "A" &][[;; , 1]] // Total

(* Part 2 *)

split = Position[input, ""][[1, 1]];
rules = input[[;; split - 1]];
parts = input[[split + 1 ;;]];
fullRules = Association @@ Table[
    tmp = Quiet@StringCases[
        rule,
        part : LetterCharacter .. ~~ "{" ~~ 
          r : (LetterCharacter .. ~~ Alternatives["<", ">", "="] ~~ 
              DigitCharacter .. ~~ ":" ~~ LetterCharacter .. ~~ 
              ",") .. ~~ sent : LetterCharacter .. ~~ "}" -> {part, r,
           sent}][[1]];
    tmp[[1]] -> Join[
      StringCases[#, (comp : (LetterCharacter .. ~~ 
                Alternatives["<", ">", "="] ~~ DigitCharacter ..) ~~ 
             ":" ~~ dest : 
              LetterCharacter ..) :>
           {Evaluate[
             ToExpression[comp]], dest}][[1]] & /@ 
       StringSplit[tmp[[2]], ","],
      {{True, tmp[[3]]}}],
    {rule, rules}];

ClearAll@evaluatePart;
evaluatePart[rule_] := evaluatePart[rule] =
   Module[{func},
    func = PiecewiseExpand[Piecewise[
       Table[{evaluatePart[r[[2]]], 
         Reduce[And @@ {r[[1]], 1 <= x <= 4000, 1 <= m <= 4000, 
            1 <= a <= 4000, 1 <= s <= 4000}]},
        {r, fullRules[rule]}]]]
    ];
evaluatePart["A"] = 1; evaluatePart["R"] = 0;

nodes = Select[
   Reverse[TopologicalSort[
     Graph[Flatten[
       Table[v -> #[[2]] & /@ fullRules[v], {v, 
         Keys[fullRules]}]]]]], # != "A" \[And] # != "R" &];
Do[
 globalWatch = {n, Length[nodes], ByteCount[evaluatePart[nodes[[n]]]],
     ByteCount[DownValues[evaluatePart]]};
 , {n, Length[nodes]}]

Sum[evaluatePart["in"], {x, 1, 4000}, {m, 1, 4000}, {a, 1, 4000}, {s, 1, 4000},]

(* Takes ~1900 seconds, would take more than fifty times longer without the trailing comma. *)