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


FourVectorSquared[FourVector_]:=Module[{},
If[ListQ[FourVector] && Length[FourVector]== 4, Nothing[],
Print["Input is no four vector of the form {#1,#2,#3,#4}"]; Abort[]];
Return[FourVector[[1]]^2-FourVector[[2]]^2-FourVector[[3]]^2-FourVector[[4]]^2];
]

Options[ToKinInput]={AllIncomingQ -> False};
ToKinInput[InputMomentaArray_,optn:OptionsPattern[{AllIncomingQ -> False}]] := Module[
{RKin,c, p1, p2, p3, p4, p5},

{p1,p2,p3,p4,p5}= If[OptionValue[AllIncomingQ],InputMomentaArray,InputMomentaArray*{1,1,-1,-1,-1}];

RKin = {s12,s13,s14,s23,s24,s34,mt2}->(FourVectorSquared/@{p1+p2,p1+p3,p1+p4,p2+p3,p2+p4,p3+p4,p3}) //Thread;

Return[RKin]

]


InputMomenta = {{500., 0., 0., 500.}, {500., 0., 0., -500.}, {332.89766719550397, 97.6631429012027, -264.71166403668974, 
  -24.197154084611423}, {267.739858285764, 13.089564983094487, -55.64743824939586, 194.40076548129642}, 
 {399.3624745187321, -110.7527078842972, 320.3591022860856, -170.20361139668498}};

RKinPaper = ToKinInput[InputMomenta] //Rationalize[#,10^-16]&


(* ::Subsection:: *)
(*Compute the counter term and the amplitude.*)


tree=TTHAmplitudeTreeTree[RKinPaper];
counter = TTHUVCounter[RKinPaper];
{t0,amplitude} = TTHAmplitudeLoopTree[RKinPaper]//AbsoluteTiming;


TTHOptions[]


Coefficient[amplitude,\[Epsilon],-2]/tree*"\[Alpha]S"/4/Pi /. TTHOptions[]


Print["Evaluation time: ",t0]
TTHPackageResult = amplitude-counter;
Print["Renormalized TTHPackage result: ", TTHPackageResult]

ReferencePointPaper = (\[Minus] 0.75348873/\[Epsilon]^2 + 1.3691456/\[Epsilon] + 0.8261367 \[Minus] 4.9282871 \[Epsilon] + 1.581737 \[Epsilon]^2)*10^-7//Expand;
AgreementQ = 0===(ReferencePointPaper-TTHPackageResult//Chop);
Print["Agreement with paper reference point: ", AgreementQ]

