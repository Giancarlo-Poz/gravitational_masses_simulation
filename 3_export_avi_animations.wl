(*** CREATE ANIMATION ***)

(* read data from the previously created file *)
Get[NotebookDirectory[] <> "data4K.mx"]

(* export animation into a single avi file *)
(*
AbsoluteTiming@Export[NotebookDirectory[]<>"test1.avi",data[[;;60]],\
FrameRate->60]
Beep[]
*)

(* for big simulations, to avoid memory overloads and crashes, create \
small avi files to be joined later*)
step = 50;
frameRate = 60;

lData = Length@data;
nSteps = Floor[lData/step];
nStepsTot = If[lData > step*nSteps, nSteps + 1, nSteps];
nDigits = Length@IntegerDigits[nStepsTot];

j = 1;
now = Now;
Do[
  Print["step ", j, "/", nStepsTot, 
   "   simulation" <> IntegerString[j, 10, nDigits] <> 
    ".avi   data=", {i, i + step - 1}];
  Export[
   NotebookDirectory[] <> "simulation" <> 
    IntegerString[j, 10, nDigits] <> ".avi", 
   data[[i ;; i + step - 1]], FrameRate -> frameRate];
  
  elapsedTime = Now - now;
  remainingTime = (lData/(j*step) - 1)*elapsedTime;
  remainingHours = 
   Floor[1/3600 QuantityMagnitude[
      UnitConvert[remainingTime, "Seconds"]]];
  remainingMinutes = 
   Floor[1/60 (QuantityMagnitude[
        UnitConvert[remainingTime, "Seconds"]] - 3600*remainingHours)];
  finish = Now + remainingTime;
  
  Print[elapsedTime, " elapsed. Estimated ", remainingHours, "h ", 
   remainingMinutes, "m remaining, finishing at ", finish];
  Print[];
  
  j = j + 1,
  {i, 1, step*nSteps, step}];

If[lData > step*nSteps,
  Print["step ", j, "/", nStepsTot, 
   "   simulation" <> IntegerString[j, 10, nDigits] <> 
    ".avi   data=", {step*nSteps + 1, lData}];
   Export[
   NotebookDirectory[] <> "simulation" <> 
    IntegerString[j, 10, nDigits] <> ".avi", 
   data[[step*nSteps + 1 ;;]], FrameRate -> frameRate] ;
  Print["All done!"]
  ];

Beep[]
