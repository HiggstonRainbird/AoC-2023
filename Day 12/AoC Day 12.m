(* ::Package:: *)

(* ::Text:: *)
(*Written December 12th, 2023.*)

(*Import*)

day = 12;
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

input = {Characters[#[[1]]], 
    toExpression[
     StringCases[#[[2]], DigitCharacter ..]]} & /@ (StringSplit[#, 
      " "] & /@ Import[inputPath, "List"]);

(*Setup*)

tallyGather[tallies_List] := {#[[1, 1]], Total[#[[;; , 2]]]} & /@ 
   GatherBy[Flatten[tallies, 1], First];

nextState[{{list_, curr_}, count_}, char_] :=
  Module[{newState, newList},
   newList = list;
   Which[
    char == "#" \[And] curr == ".", AppendTo[newList, 1],
    char == "#" \[And] curr == "#", newList[[-1]] += 1
    ];
   newState = {{newList, char}, count}
   ];

possibilities[conf_, goal_] :=
 Module[{running = {{{{}, "."}, 1}}},
  Do[
   If[conf[[i]] == "?",
    running = {nextState[#, "."] & /@ running,
      nextState[#, "#"] & /@ running},
    running = {nextState[#, conf[[i]]] & /@ running}];
   running = tallyGather[running];
   running =
    Select[running,
     Length[#[[1, 1]]] <= Length[goal] \[And]
       (Length[#[[1, 1]]] == 0 \[Or]
         And @@ 
          Thread[#[[1, 1]] <= goal[[;; Length[#[[1, 1]]]]]]) \[And]
       (Length[#[[1, 1]]] <= 1 \[Or]
         And @@ 
          Thread[#[[1, 1, ;; -2]] == 
            goal[[;; Length[#[[1, 1]]] - 1]]]) &
     ],
   {i, Length[conf]}];
  Select[running, #[[1, 1]] === goal &]
]

(*Part 1*)

Sum[
 Total[possibilities[input[[i, 1]], input[[i, 2]]][[;; , 2]]],
 {i, Length[input]}]

(* Part 2 *)

Sum[
 Total[possibilities[
   Flatten[Riffle[Table[input[[i, 1]], {j, 5}], "?"]],
   Flatten[Table[input[[i, 2]], {j, 5}]]][[;; , 2]]],
 {i, Length[input]}]