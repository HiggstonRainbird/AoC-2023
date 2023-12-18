(* ::Package:: *)

(* ::Text:: *)
(*Written December 16th, 2023.*)

(*Import*)

day = 16;
inputPath = 
  FileNameJoin[{NotebookDirectory[], 
    "Day" <> ToString[day] <> "Input.txt"}];

input = Flatten[Characters /@ Import[inputPath, "Table"], 1];

(*Part 1*)

directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
beam = {{{1, 1}, 1}};

changeDirection[{pos_, dir_}] :=
  Which[
   input[[Sequence @@ pos]] == "." \[Or]
    (input[[Sequence @@ pos]] == "-" \[And] Mod[dir, 2] == 1) \[Or]
    (input[[Sequence @@ pos]] == "|" \[And] Mod[dir, 2] == 0),
   {{pos, dir}},
   
   input[[Sequence @@ pos]] == "-" \[Or] 
    input[[Sequence @@ pos]] == "|",
   {{pos, Mod[dir + 1, 4, 1]}, {pos, Mod[dir - 1, 4, 1]}},
   
   input[[Sequence @@ pos]] == "\\",
   {{pos, dir /. {1 -> 2, 2 -> 1, 3 -> 4, 4 -> 3}}},
   
   input[[Sequence @@ pos]] == "/",
   {{pos, dir /. {1 -> 4, 2 -> 3, 3 -> 2, 4 -> 1}}}
   ];
moveBeam[{oldPos_, dir_}] :=
  
  Module[{pos = oldPos + directions[[dir]]},
   If[
    Min[pos] <= 0 \[Or] pos[[1]] > Length[input] \[Or] 
     pos[[2]] > Length[input[[pos[[1]]]]],
    Nothing,
    {pos, dir}]
   ];

seen = input*0;
seen[[1, 1]] = 1;
count = 1;
While[
  Length[beam] > 0,
  beam = DeleteDuplicates[
    moveBeam /@ Flatten[changeDirection /@ beam, 1]];
  Table[(seen[[Sequence @@ b[[1]]]] = 1), {b, beam}];
  globalWatch = {count, Total[Total[seen]]};
  count += 1;
  ];

globalWatch

(* Part 2 *)

directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
beam = {{{1, 1}, 1}};

changeDirection[{pos_, dir_}] :=
  Which[
   input[[Sequence @@ pos]] == "." \[Or]
    (input[[Sequence @@ pos]] == "-" \[And] Mod[dir, 2] == 1) \[Or]
    (input[[Sequence @@ pos]] == "|" \[And] Mod[dir, 2] == 0),
   {{pos, dir}},
   
   input[[Sequence @@ pos]] == "-" \[Or] 
    input[[Sequence @@ pos]] == "|",
   {{pos, Mod[dir + 1, 4, 1]}, {pos, Mod[dir - 1, 4, 1]}},
   
   input[[Sequence @@ pos]] == "\\",
   {{pos, dir /. {1 -> 2, 2 -> 1, 3 -> 4, 4 -> 3}}},
   
   input[[Sequence @@ pos]] == "/",
   {{pos, dir /. {1 -> 4, 2 -> 3, 3 -> 2, 4 -> 1}}}
   ];
moveBeam[{oldPos_, dir_}] :=
  
  Module[{pos = oldPos + directions[[dir]]},
   If[
    Min[pos] <= 0 \[Or] pos[[1]] > Length[input] \[Or] 
     pos[[2]] > Length[input[[pos[[1]]]]],
    Nothing,
    {pos, dir}]
   ];

energizeTotal[start_] := Module[
   {energized, seen, beam = {start}, count = 0},
   
   energized = input*0;
   energized[[Sequence @@ start[[1]]]] = 1;
   seen[b_] := False;
   
   While[
    Length[beam] > 0,
    beam = moveBeam /@ Flatten[changeDirection /@ beam, 1];
    Do[seen[b] = True; 
     energized[[Sequence @@ b[[1]]]] += 1, {b, beam}];
    globalWatch = {start, count, Total[Total[energized]]};
    count += 1;
    ];
   Total[Total[energized]]
   ];

toCheck = Join[
   {#, 1} & /@ Tuples[{Range[Dimensions[input][[1]]], {1}}],
   {#, 2} & /@ Tuples[{{1}, Range[Dimensions[input][[1]]]}],
   {#, 3} & /@ 
    Tuples[{Range[
       Dimensions[input][[1]]], {Dimensions[input][[1]]}}],
   {#, 4} & /@ 
    Tuples[{{Dimensions[input][[1]]}, Range[Dimensions[input][[1]]]}]];

Max[ Table[energizeTotal[s], {s, toCheck}]]