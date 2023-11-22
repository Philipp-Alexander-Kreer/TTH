(* ::Package:: *)

(* ::Text:: *)
(*Get the package.*)


current = DirectoryName[If[$FrontEnd === Null, $InputFileName, NotebookFileName[]]];
Get[FileNameJoin[{current, "TTH.wl"}]];


(* ::Text:: *)
(*(Optional) You can set the options for the package using TTHOptions. Available options include*)
(*	"NF" - the number of massless quarks, 5 by default*)
(*	"NC" - the number of colors, 3 by default*)
(*	"yt" - the Yukawa coupling, 82979727/120018599 (~0.691) by default*)
(*	"\[Alpha]S" - the strong coupling, 59/500 (~0.118) by default*)
(*	"PrecisionGoal" - precision goal for the final results, 6 by default*)
(*	"SilentMode" - silent mode (won't print any message during the computations), False by default*)


TTHOptions["NF" -> 5, "NC" -> 3, "yt" -> 82979727/120018599, "\[Alpha]S" -> 59/500, "PrecisionGoal" -> 6, "SilentMode" -> False]


(* ::Text:: *)
(*Set the kinematics. Must use rational numbers.*)


RKin = {s12 -> 1000000, s13 -> -145373486921728/229528275, s14 -> -14409943129571/66409125, s23 -> -116741427517764/644673025, s24 -> -111825705173728/214903925, s34 -> 203738282402836/296389125, mt2 -> 749956/25};


(* ::Text:: *)
(*Compute the counter term and the amplitude.*)


counter = TTHUVCounter[RKin];
{t0,amplitude} = TTHAmplitudeLoopTree[RKin]//AbsoluteTiming;


counter
amplitude


t0
