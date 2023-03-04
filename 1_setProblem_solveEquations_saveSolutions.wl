(*** SET THE PROBLEM **)

 (* initial positions of masses*)
pos = Flatten[Table[{i, j}, {i, -4.5, 4.5}, {j, -4.5, 4.5}], 1] ; 

(* other possible initial positions*)
 (* pos=Table[100*{ Cos[t]+RandomReal[{-0.7,0.7}], \
Sin[t]+RandomReal[{-0.7,0.7}]},{t,0,2\[Pi],2\[Pi] 1/(100-1)}]; *)
(* pos = \
{#[[1]]+RandomReal[{-0.1,0.1}],#[[2]]+RandomReal[{-0.1,0.1}]}&/@(\
Flatten[Table[{i,j},{i,-4.5,4.5},{j,-4.5,4.5}],1]); *)
(* pos = \
{#[[1]]+RandomReal[{-5,5}],#[[2]]+RandomReal[{-5,5}]}&/@ConstantArray[\
0,{100,2}]; *)

(* constants*)
dim = 2;
numpt = Length@pos;
Print["# points: ", numpt ]
GMm = 1
a0 = 10^-2;


(* set each mass = 1 *)
Table[m[i], {i, numpt}];
(* to randomise: Set[#,1+RandomReal[]]&/@%; *)
 Set[#, 1] & /@ %;


(* variables *)
variablesFunc = Flatten[Table[{x[i], y[i]}, {i, numpt}]];
lengthvariablesFunc = Length@variablesFunc;
variables = Apply[#, {t}] & /@ variablesFunc;

(* functions *)
(* functions - distance *)
distq[a_, b_] := (a[[1]] - b[[1]])^2 + (a[[2]] - b[[2]])^2 + a0
dist[a_, b_] := \[Sqrt]((a[[1]] - b[[1]])^2 + (a[[2]] - b[[2]])^2 + a0)
distC[a_, b_] := \[Sqrt]((a[[1]] - b[[1]])^2 + (a[[2]] - b[[2]])^2)

(* functions - gravitational potential *)
Uf[a_, b_] := -GMm 1/dist[a, b]

dirNormalised[v1_, v2_] := (v2 - v1)/distC[v1, v2]

(* functions - gravitational force between 2 bodies *)
forceGrav[v1_, v2_] := 
 Abs[distC[v1, v2]/(dist[v1, v2])^3] dirNormalised[v1, 
   v2] (* force = derivative of the potential; D[-GMm/Sqrt[r^2+a],r] \
= (GMm r)/(a+r^2)^(3/2) *)

(* functions - tot gravitational force between 1 body and all the \
others *)
 forceGravTot[v_] := 
 Total@Table[forceGrav[v, i], {i, Select[pos, distC[#, v] > 0 &]}] 

(* centre of mass *)
rCM = Total[pos]/Length[pos];

(*
(* functions - velocities *)

velocityDirection[v_]:=1/Norm[#]{-#[[2]],#[[1]]}&@(forceGravTot[v])
velocity[v_]:=velocityModule[v]*velocityDirection[v]
*)

(* functions - velocities *)
velocityModule[v_] := Norm[forceGravTot[v]]/Sqrt[dist[rCM, v]]
(* velocityModule[v_]:=Norm[forceGravTot[v]] *)
(* velocityModule[v_]:=\[Sqrt](Norm[forceGravTot[v]]*distC[v,rCM]) *)

velocityDirection[v_] := {-#[[2]], #[[1]]} &[1/Norm[rCM - v] (rCM - v)]
velocity[v_] := velocityModule[v]*velocityDirection[v]



(* numpt positions and numpt velocities *)
conditions = N@Flatten[{pos,
     Table[velocity[pos[[i]]], {i, Length@pos}]}];

(* plot range *)
pRange = Block[{xRange, yRange, lengthXRange, lengthYRange},
   xRange = MinMax[pos[[;; , 1]]];
   yRange = MinMax[pos[[;; , 2]]];
   lengthXRange = #[[2]] - #[[1]] &@xRange;
   lengthYRange = #[[2]] - #[[1]] &@yRange;
   If[lengthXRange > lengthYRange,
    {{1/2 Total[xRange] - 11/20 lengthXRange, 
      1/2 Total[xRange] + 11/20 lengthXRange},
     {1/2 Total[yRange] - 11/20 lengthXRange, 
      1/2 Total[yRange] + 11/20 lengthXRange}},
    {{1/2 Total[xRange] - 11/20 lengthYRange, 
      1/2 Total[xRange] + 11/20 lengthYRange},
     {1/2 Total[yRange] - 11/20 lengthYRange, 
      1/2 Total[yRange] + 11/20 lengthYRange}}]
   ];


(* show points and velocities *)
(* Show[
ListPlot[Take[Partition[conditions,2],numpt],PlotRange\[Rule]pRange],
ListVectorPlot[Table[{{pos[[i,1]],pos[[i,2]]},{pos[[i,2]],-pos[[i,1]]}\
1.35((dist[pos[[i]],{0,0}])\[ExponentialE]^(-(dist[pos[[i]],{0,0}])/2)\
)},{i,numpt}],VectorPoints->Table[{pos[[i,1]],pos[[i,2]]},{i,numpt}],\
PlotRange\[Rule]pRange],
AspectRatio\[Rule]1] *)

Show[
 ListVectorPlot[Table[{pos[[i]], velocity[pos[[i]]]}, {i, numpt}], 
  VectorPoints -> Table[pos[[i]], {i, numpt}], PlotRange -> pRange],
 ListPlot[Take[Partition[conditions, 2], numpt], 
  PlotRange -> pRange],
 AspectRatio -> (pRange[[2, 2]] - pRange[[2, 1]])/(pRange[[1, 2]] - 
     pRange[[1, 1]])]



(*** SOLVE THE PROBLEM **)

(* kinetic energy *)
T = 1/2 Sum[
    m[i] ( D[x[i][t], t]^2 + D[y[i][t], t]^2), {i, 
     1/dim lengthvariablesFunc}];
(* potential energy *)
U = Sum[
   Uf[{x[i][t], y[i][t]}, {x[j][t], y[j][t]}],
   {j, 2, 1/dim lengthvariablesFunc}, {i, 1, j - 1}];

(* Lagrangian *)
L = T - U;

(* Euler-Lagrange equations *)
eq = D[D[L, D[#, t]], t] - D[L, #] == 0 & /@ variables;

(* initial conditions *)
D[#, t] & /@ variables;
var = Join[variables, %];

condiz = Thread[(var /. t -> 0) == conditions];

equaz = Join[eq, condiz];




(* time length of the simulation *)
timeSimulation = 10;

(* solve 6*numpt Euler-Lgrange differential equations *)
AbsoluteTiming[
  sol = NDSolve[equaz, variablesFunc, {t, 0, timeSimulation}, 
     Method -> {"EquationSimplification" -> "Residual"}];][[1]]
Beep[]

(* save the solution into a file *)
DumpSave[NotebookDirectory[] <> "solutions.mx", sol]
