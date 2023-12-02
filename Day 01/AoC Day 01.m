(* ::Package:: *)

(* ::Text:: *)
(*Written December 1st, 2023.*)

(*Import*)

inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input=StringSplit[Import[inputPath],"\n\n"];
input=toExpression[StringSplit[#,"\n"]&/@input];


(*Part 1*)

Total[FromDigits[(ToExpression /@ StringCases[#, DigitCharacter])[[{1, -1}]]] & /@ input]


(*Part 2*)

digits = IntegerName /@ Range[1, 9];
replacement = Thread[digits -> Range[1, 9]];
Total[
 FromDigits[ToExpression /@ (
       StringCases[#, Alternatives[digits, DigitCharacter],
         Overlaps -> All] /. replacement)[[{1, -1}]]]
   & /@ input]
