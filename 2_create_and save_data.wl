(*** read solution and set variables according to previous file ***)
Get[NotebookDirectory[] <> "solutions.mx"]
dim = 2;
lengthIncpf = 200;
timeSimulation = 300;
pRange = {{-4.95, 4.95}, {-4.95, 4.95}}; 
pRange = {pRange[[1]]*16/9, pRange[[2]]}; (* to have 16/9 ratio *)

(* load solutions into (xs,ys) *)
MapThread[
  Set, {Table[{xs[i], ys[i]}, {i, 1/dim lengthIncpf}], 
   Table[({x[i], y[i]} /. sol)[[1]], {i, 1/dim lengthIncpf}]}];
   

(*** set option for video output ***)

(* quality=5120; fontSize=100; (* 8K resolution *) *)
(* quality=2560; fontSize=50; (* for 4K *) *)
quality = 1280; fontSize = 25; (* for 1080p *)


optionsPlot = {
   PlotStyle -> PointSize[0.007],
   AspectRatio -> 9/16,
   Axes -> False,
   Background -> Black,
   ImageSize -> quality};
   
   
(* data from solutions, not fine-tuned, for tests *)
(*
data = Table[ListPlot[
    Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
    PlotStyle -> PointSize[0.007],
    AspectRatio -> 9/16,
    Axes -> False,
    Background -> Black,
    ImageSize -> 1/5*quality,
    PlotRange -> 2 pRange,
    Epilog -> 
     Inset[Style["t  =" <> ToString[PaddedForm[N[t], {5, 2}]] , White,
        fontSize], {0, 23}]
    ], {t, 0, timeSimulation, 0.05 (*step-size: 
    reduce this and later increase frameRate for smoother \
animations*)}];

Beep[]
Beep[]
ListAnimate[data]
*)



(* fine-tuned data from solutions *)
data = Join[
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
     optionsPlot,
     PlotRange -> 5 pRange,
     Epilog -> 
      Inset[Style["t  =" <> ToString[PaddedForm[N[t], {5, 2}]] , 
        White, fontSize], {0, 23}]
     ], {t, 0, 60, 0.05}],
   
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}} /. t -> 60, {i, 
       1/dim lengthvariablesFunc}],
     PlotRange -> zoom pRange, optionsPlot, 
     Epilog -> 
      Inset[Style[
        "t  = 60" <> "  Pause and zooming out " <> 
         ToString[PaddedForm[N[1/(zoom /5)], {4, 2}]] <> " X" , White,
         fontSize], zoom/5 {0, 23}]
     ], {zoom, 5, 10, 0.05}],
   
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
     PlotRange -> 10 pRange, optionsPlot,
     Epilog -> 
      Inset[Style["t  =" <> ToString[PaddedForm[N[t], {5, 2}]] , 
        White, fontSize], 2 {0, 23}]
     ], {t, 60, 150, 0.05}],
   
   Table[Show[
     ListPlot[
      Table[{{xs[i][t], ys[i][t]}} /. t -> 150, {i, 
        1/dim lengthvariablesFunc}],
      PlotRange -> zoom pRange, optionsPlot, 
      Epilog -> 
       Inset[Style[
         "t  = 150" <> "  Pause and zooming out " <> 
          ToString[PaddedForm[N[10/zoom], {4, 2}]] <> " X" , White, 
         fontSize], zoom/5 {0, 23}]
      ],
     Graphics[{EdgeForm[Directive[Thick, Blue]], Transparent, 
       Rectangle[10 pRange[[;; , 1]], 10 pRange[[;; , 2]]]}]
     ]
    , {zoom, 10, 20, 0.05}],
   
   Table[Show[
     ListPlot[
      Table[{{xs[i][t], ys[i][t]}} /. t -> 150, {i, 
        1/dim lengthvariablesFunc}],
      PlotRange -> 20 pRange + {-33, 23} shift, optionsPlot,
      Epilog -> 
       Inset[Style[
         "t  = 150" <> "  Pause and shift (-33,23) x" <> 
          ToString[PaddedForm[N[shift], {3, 2}]] , White, fontSize], 
        4 {0, 23} + {-33, 23} shift]
      ],
     Graphics[{EdgeForm[Directive[Thick, Blue]], Transparent, 
       Rectangle[10 pRange[[;; , 1]] + {-33, 22} shift, 
        10 pRange[[;; , 2]] + {-33, 22} shift]}]
     ]
    , {shift, 0, 1, 0.02}],
   
   Table[Show[
     ListPlot[
      Table[{{xs[i][t], ys[i][t]}} /. t -> 150, {i, 
        1/dim lengthvariablesFunc}],
      PlotRange -> zoom pRange + {-33, 23}, optionsPlot, 
      Epilog -> 
       Inset[Style[
         "t  = 150" <> "  Pause and zooming in " <> 
          ToString[PaddedForm[N[10/zoom ], {4, 2}]] <> " X" , White, 
         fontSize], zoom/5 {0, 23} + {-33, 23}]
      ],
     Graphics[{EdgeForm[Directive[Thick, Blue]], Transparent, 
       Rectangle[10 pRange[[;; , 1]] + {-33, 22}, 
        10 pRange[[;; , 2]] + {-33, 22}]}]
     ]
    , {zoom, 20, 10, -0.05}],
   
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
     PlotRange -> 10 pRange + {-33, 23}, optionsPlot,
     Epilog -> 
      Inset[Style["t  =" <> ToString[PaddedForm[N[t], {5, 2}]] , 
        White, fontSize], 2 {0, 23} + {-33, 23}]
     ], {t, 150, timeSimulation, 0.05}],
   
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
     PlotRange -> 5 pRange, optionsPlot,
     Epilog -> 
      Inset[Style[
        "t  =" <> ToString[PaddedForm[N[t], {6, 3}]] <> 
         "  Speed 0.1 X" , White, fontSize], {0, 23}]
     ], {t, 0, 13, 0.005}],
   
   Table[ListPlot[
     Table[{{xs[i][t], ys[i][t]}}, {i, 1/dim lengthvariablesFunc}],
     PlotRange -> 5 pRange, optionsPlot,
     Epilog -> 
      Inset[Style[
        "t  =" <> ToString[PaddedForm[N[t], {6, 3}]] <> 
         "  Speed 0.4 X" , White, fontSize], {0, 23}]
     ], {t, 13, 60, 0.02}]
   ];

(* save data into a file *)
DumpSave[NotebookDirectory[] <> "data4K.mx", data];

Beep[]
