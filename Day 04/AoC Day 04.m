(* ::Package:: *)

(* ::Text:: *)
(*Written December 4th, 2023.*)

(*Import*)

day = 4;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];
input = Import[inputPath,"List"];


(*Part 1*)

cards = input[[;; , 3 ;; 12]];
winning = input[[;; , 14 ;;]];

Total[Floor[2^(Length[Intersection @@ #] - 1)] & /@ 
  Transpose[{cards, winning}]]

(* Part 2 *)

cardQueue = Table[1, {row, Length[input]}];
Do[
  cardQueue[[i + c]] += cardQueue[[c]],
  {c, Length[input]},
  {i, Length[Intersection[cards[[c]], winning[[c]]]]}];
Total[cardQueue]
