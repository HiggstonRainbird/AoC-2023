(* ::Package:: *)

(* ::Text:: *)
(*Written December 20th, 2023.*)

(*Import*)

day = 20;
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
   StringSplit[#, " -> " | ", "] & /@ 
    StringSplit[Import[inputPath], "\n"]];

(* Part 1*)

parseLine[line_] :=
  Module[{first = Characters[line[[1]]]},
   Which[
    first[[1]] == "%",
    StringJoin[first[[2 ;;]]] -> <|"Type" -> "%", "State" -> "Low", 
      "Destination" -> line[[2 ;;]]|>,
    
    first[[1]] == "&",
    StringJoin[first[[2 ;;]]] -> <|"Type" -> "&", "State" -> <||>, 
      "Destination" -> line[[2 ;;]]|>,
    
    first[[1]] == "b",
    StringJoin[first] -> <|"Type" -> "Broadcast", 
      "Destination" -> line[[2 ;;]]|>
    ]];
flip = {"Low" -> "High", "High" -> "Low"};

state = Association @@ (parseLine /@ input);
graph = Flatten[Thread[# -> state[#]["Destination"]] & /@ Keys[state]];
conjunctions = Keys[Select[state, #["Type"] == "&" &]];
Do[state[c]["State"] = 
   Association @@ 
    Table[in -> "Low", {in, 
      Select[graph, #[[2]] == c &][[;; , 1]]}], {c, conjunctions}];
count = <|"Low" -> 0, "High" -> 0|>;
ClearAll@hasOutputs; 
hasOutputs[node_] := False; Do[
 hasOutputs[node] = True, {node, Keys[state]}]

newPulses[node_, type_, origin_ : Null] :=
  Module[{dest},
   count[type] = count[type] + 1;
   
   If[! hasOutputs[node], Return[{}, Module]];
   
   dest = state[node]["Destination"];
   Which[
    state[node]["Type"] == "Broadcast",
    Table[queue[d, "Low", node], {d, dest}],
    
    state[node]["Type"] == "%" \[And] type == "Low",
    state[node]["State"] = state[node]["State"] /. flip;
    Table[queue[d, state[node]["State"], node], {d, dest}],
    
    state[node]["Type"] == "%" \[And] type == "High",
    {},
    
    state[node]["Type"] == "&",
    state[node]["State"][origin] = type;
    Table[
     queue[d, 
      If[Count[Values[state[node, "State"]], "Low"] == 0, "Low", 
       "High"], node], {d, dest}],
    
    True,
    Print["ERROR!"]; Print[{node, type, origin}]; Print[state]; 
    Pause[5];
    ]
   ];

deque = CreateDataStructure["Deque"];
Do[
 deque["PushBack", queue["broadcaster", "Low", Null]];
 While[
  deque["Length"] != 0,
  p = deque["PopFront"];
  new = newPulses @@ p;
  Do[deque["PushBack", n], {n, new}];
  ],
{i, 1000}];

count["High"]*count["Low"]

(* Part 2 *)

state = Association @@ (parseLine /@ input);
graph = Flatten[Thread[# -> state[#]["Destination"]] & /@ Keys[state]];
conjunctions = Keys[Select[state, #["Type"] == "&" &]];
Do[state[c]["State"] = 
   Association @@ 
    Table[in -> "Low", {in, 
      Select[graph, #[[2]] == c &][[;; , 1]]}], {c, conjunctions}];
count = <|"Low" -> 0, "High" -> 0|>;
ClearAll@hasOutputs; 
hasOutputs[node_] := False; Do[
 hasOutputs[node] = True, {node, Keys[state]}]

newPulses[node_, type_, origin_ : Null] :=
  Module[{dest},
   count[type] = count[type] + 1;
   
   Do[If[node == n \[And] type == "Low", Print[{n, globalWatch}]; 
     Pause[1]], {n, Keys[state["ls", "State"]]}];
   If[! hasOutputs[node], Return[{}, Module]];
   
   dest = state[node]["Destination"];
   Which[
    state[node]["Type"] == "Broadcast",
    Table[queue[d, "Low", node], {d, dest}],
    
    state[node]["Type"] == "%" \[And] type == "Low",
    state[node]["State"] = state[node]["State"] /. flip;
    Table[queue[d, state[node]["State"], node], {d, dest}],
    
    state[node]["Type"] == "%" \[And] type == "High",
    {},
    
    state[node]["Type"] == "&",
    state[node]["State"][origin] = type;
    Table[
     queue[d, 
      If[Count[Values[state[node, "State"]], "Low"] == 0, "Low", 
       "High"], node], {d, dest}]
    ]
   ];

deque = CreateDataStructure["Deque"];
Do[
  deque["PushBack", queue["broadcaster", "Low", Null]];
  While[
   deque["Length"] != 0,
   p = deque["PopFront"];
   new = newPulses @@ p;
   Do[deque["PushBack", n], {n, new}];
   ],
  {i, 5000}];

(* Answer was obtained by taking the LCM of the four results printed.*)