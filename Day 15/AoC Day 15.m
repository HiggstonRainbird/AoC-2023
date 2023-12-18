(* ::Package:: *)

(* ::Text:: *)
(*Written December 15th, 2023.*)

(*Import*)

day = 15;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = StringSplit[Import[inputPath, "List"], ","][[1]];

(*Part 1*)

hash[s_] := 
 Fold[Mod[(#1 + ToCharacterCode[#2][[1]])*17, 256] &, 0, 
  Characters[s]];

Total[hash /@ input]

(* Part 2 *)

boxes = Table[{}, {i, 256}];

Do[
  lens = StringCases[line, LetterCharacter ..][[1]];
  newList = boxes[[hash[lens] + 1]];
  Which[
   StringContainsQ[line, "-"],
   newList = DeleteCases[newList, {lens, _}],
   
   StringContainsQ[line, "="] \[And] 
    MemberQ[newList[[;; , 1]], lens],
   num = ToExpression[StringCases[line, DigitCharacter ..][[1]]];
   newList = newList /. {lens, _} -> {lens, num},
   
   StringContainsQ[line, "="] \[And] ! 
     MemberQ[newList[[;; , 1]], lens],
   num = ToExpression[StringCases[line, DigitCharacter ..][[1]]];
   newList = AppendTo[newList, {lens, num}]
   ];
  boxes[[hash[lens] + 1]] = newList;
  , {line, input}];

Sum[i*Range[Length[#]] . # &@boxes[[i, ;; , 2]], {i, Length[boxes]}]