(* ::Package:: *)

(* ::Subsection:: *)
(*Unzip the directory input_files.zip*)


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*Set the kinematics. Must use rational numbers.*)


RKin = {s12 -> 1000000, s13 -> -145373486921728/229528275, s14 -> -14409943129571/66409125, s23 -> -116741427517764/644673025, s24 -> -111825705173728/214903925, s34 -> 203738282402836/296389125, mt2 -> 749956/25};


RKinPaper = {s12 -> 1.`13.*^6, s13 -> -326469.8212801153463505084`13., s14 -> -42714.0928044675752609834`13., s23 -> -278075.5131108924984170146`13., s24 -> -431515.623767060410965933`13., s34 -> 216900.0509625359537671261`13., 
 mt2 -> 30625.0000000000603207807`13.};
RKinPaper = RKinPaper //N 
RKinPaper[[;;,2]]=Rationalize[RKinPaper[[;;,2]],10^-16];


RKinPaper


(* ::Subsection:: *)
(*Compute the counter term and the amplitude.*)


counter = TTHUVCounter[RKinPaper];
{t0,amplitude} = TTHAmplitudeLoopTree[RKinPaper]//AbsoluteTiming;


Print["Evaluation time: ",t0]
TTHPackageResult = amplitude-counter;
Print["Renormalized TTHPackage result: ", ReferencePointPaper]

ReferencePointPaper = (\[Minus] 0.75348873/\[Epsilon]^2 + 1.3691456/\[Epsilon] + 0.8261367 \[Minus] 4.9282871 \[Epsilon] + 1.581737 \[Epsilon]^2)*10^-7//Expand;
AgreementQ = 0===(ReferencePointPaper-TTHPackageResult//Chop);
Print["Agreement with paper reference point: ", AgreementQ]



