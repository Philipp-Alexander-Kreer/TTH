(* ::Package:: *)

BeginPackage["TTH`"];


TTHOptions::usage = "TTHOptions[opt] sets the options for the package.";
TTHPrint::usage = "TTHPrint[A] prints A if silent mode is off.";
TTHInitialize::usage = "TTHInitialize[] initializes the package.";
TTHUVCounter::usage = "TTHUVCounter[kinematics] computes the UV counter term.";
TTHAmplitudeLoopTree::usage = "TTHAmplitudeLoopTree[kinematics] computes the one-loop*tree amplitude.";


Begin["`Private`"];


ep = Symbol["Global`\[Epsilon]"];
$CTX = $Context;
$Current = DirectoryName[$InputFileName];
$InputPath = FileNameJoin[{$Current, "input_files"}];
$AMFlowPath = FileNameJoin[{$Current, "one_loop_amflow"}];


Options[TTHOptions] = {"NF" -> 5, "NC" -> 3, "yt" -> 82979727/120018599, "\[Alpha]S" -> 59/500, "PrecisionGoal" -> 6, "SilentMode" -> False};
TTHOptions[opt___]:=Block[{},
If[MemberQ[Keys[{opt}], "NF"], $NF = "NF"/.{opt}];
If[MemberQ[Keys[{opt}], "NC"], $NC = "NC"/.{opt}];
If[MemberQ[Keys[{opt}], "yt"], $yt = "yt"/.{opt}];
If[MemberQ[Keys[{opt}], "\[Alpha]S"], $\[Alpha]S = "\[Alpha]S"/.{opt}];
If[MemberQ[Keys[{opt}], "PrecisionGoal"],
$PrecisionGoal = "PrecisionGoal"/.{opt};
$WorkingPrecision = ($PrecisionGoal+3)*5;
$EpsList = 10^-($PrecisionGoal+3)+10^-($PrecisionGoal+5)Range[5]];
If[MemberQ[Keys[{opt}], "SilentMode"], $SilentMode = "SilentMode"/.{opt}];

PrintOptions[TTHOptions, $CTX];
];


TTHPrint[a___]:=If[$SilentMode=!=True, Print[a], Hold[a]];
PrintOptions[func_, ctx_]:=TTHPrint[func -> Thread[Keys[Options[func]] -> ToExpression[ctx<>"$"<>#&/@Keys[Options[func]]]]];


FitEps[rule_,leading_]:=Fit[Transpose[{Keys[rule], Values[rule]}], Power[ep, leading+Range[0, Length[rule]-1]], ep];


TTHInitialize[]:=Block[{time1,time2},
TTHPrint["TTHInitialize: Initializing. This may take a few minutes."];
time1 = AbsoluteTime[];
IBPMatrix = Get[FileNameJoin[{$InputPath, "IBPMatrix_dotted.m"}]];
KinematicMatrices = Get[FileNameJoin[{$InputPath, "KinematicMatrices.m"}]];
HelicityProjectorEven = Get[FileNameJoin[{$InputPath, "HelicityProjectorEven.m"}]];
HelicityProjectorOdd = Get[FileNameJoin[{$InputPath, "HelicityProjectorOdd.m"}]];
HelicityFormFactorsTree = Get[FileNameJoin[{$InputPath, "HelicityFormFactorsTree.m"}]];
HelicityFormFactorsUVCT = Get[FileNameJoin[{$InputPath, "HelicityFormFactorsUVCT.m"}]];
ContractionMasterFormula = Get[FileNameJoin[{$InputPath, "ContractionMasterFormula.m"}]];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["TTHInitialize: Initialized. Time used: `1`s."][Ceiling[time2-time1]]];
];


TTHDynamicEvaluationAMFIntegrals[KinematicReplacements_, epslist_, Digits_]:=Block[{},
Put[{KinematicReplacements, epslist, Digits}, FileNameJoin[{$AMFlowPath, "config"}]];
Run[First[$CommandLine]<> " -script "<>FileNameJoin[{$AMFlowPath, "calc.wl"}]];
Quiet[Get[FileNameJoin[{$AMFlowPath, "results"}]]]
];


NEvalHelicityFormFactors[RKinematics_]:=Block[{$MinPrecision=$WorkingPrecision,$MaxPrecision=$WorkingPrecision,time1,time2,rule,ibp,kinmat,peven, podd,ints,allints,allkin,ffactors},
TTHPrint["NEvalHelicityFormFactors: substituting the kinematics into the coefficients."];
time1 = AbsoluteTime[];
rule = Thread[Keys[RKinematics] -> N[Values[RKinematics], $WorkingPrecision]];
ibp = IBPMatrix/.rule;
kinmat = KinematicMatrices/.rule;
peven = HelicityProjectorEven/.rule;
podd = HelicityProjectorOdd/.rule;

ibp = ibp/.ep -> #&/@$EpsList;
kinmat = kinmat/.ep -> #&/@$EpsList;
peven = peven/.ep -> #&/@$EpsList;
podd = podd/.ep -> #&/@$EpsList;
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["NEvalHelicityFormFactors: substituted. Time used: `1`s."][Ceiling[time2-time1]]];

TTHPrint["NEvalHelicityFormFactors: computing master integrals using one-loop-amflow."];
time1 = AbsoluteTime[];
ints = TTHDynamicEvaluationAMFIntegrals[RKinematics, $EpsList, $WorkingPrecision];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["NEvalHelicityFormFactors: computed. Time used: `1`s."][Ceiling[time2-time1]]];

TTHPrint["NEvalHelicityFormFactors: doing matrix multiplications."];
time1 = AbsoluteTime[];
allints = Table[ibp[[i]] . ints[[i]], {i, Length[$EpsList]}];
allkin =  Table[kinmat[[i]] . allints[[i]], {i, Length[$EpsList]}];
ffactors = Table[{peven[[i]] . #&/@allkin[[i]], podd[[i]] . #&/@allkin[[i]]}, {i, Length[$EpsList]}];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["NEvalHelicityFormFactors: done. Time used: `1`s."][Ceiling[time2-time1]]];

ffactors
];


NEvalHelicityFormFactorsTree[RKinematics_]:=HelicityFormFactorsTree/.RKinematics;


InitRulesContractionTreeLoop[ColVecLoopEven_,ColVecLoopOdd_,ColVecTreeEven_,ColVecTreeOdd_]:=Block[{PartA,PartB,PartC,RFOddLoop,RFEvenLoop,RFEvenTree,RFOddTree},
PartA = Thread[Symbol["Global`FTATBtr5Loop"]/@Range[8] -> ColVecLoopOdd[[1]]];
PartB = Thread[Symbol["Global`FTBTAtr5Loop"]/@Range[8] -> ColVecLoopOdd[[2]]];
PartC = Thread[Symbol["Global`DeltaABtr5Loop"]/@Range[8] -> ColVecLoopOdd[[3]]];
RFOddLoop = Flatten[{PartA, PartB, PartC}];

PartA = Thread[Symbol["Global`FTATBLoop"]/@Range[8] -> ColVecLoopEven[[1]]];
PartB = Thread[Symbol["Global`FTBTALoop"]/@Range[8] -> ColVecLoopEven[[2]]];
PartC = Thread[Symbol["Global`DeltaABLoop"]/@Range[8] -> ColVecLoopEven[[3]]];
RFEvenLoop = Flatten[{PartA, PartB, PartC}];

PartA = Thread[Symbol["Global`FTATBtr5"]/@Range[8] -> ColVecTreeOdd[[1]]];
PartB = Thread[Symbol["Global`FTBTAtr5"]/@Range[8] -> ColVecTreeOdd[[2]]];
RFOddTree = Flatten[{PartA, PartB}];

PartA = Thread[Symbol["Global`FTATB"]/@Range[8] -> ColVecTreeEven[[1]]];
PartB = Thread[Symbol["Global`FTBTA"]/@Range[8] -> ColVecTreeEven[[2]]];
RFEvenTree = Flatten[{PartA, PartB}];

Flatten[{RFEvenLoop, RFOddLoop, RFEvenTree, RFOddTree}]
];


(*Works only for tree * oneloop*)
NContractLoopWithTree[RKinematics_, HelicityFormFactorsOneLoop_, HelicityFormFactorsTree_]:=Block[{ColVectorEven1Loop,ColVectorOdd1Loop,RAnalyticToNumeric,ContractedResult},
ColVectorEven1Loop = {0, 0, HelicityFormFactorsOneLoop[[1, 9]]};
ColVectorEven1Loop[[1]] = Total[HelicityFormFactorsOneLoop[[1, ;;4]]*{$NF, $NC^-1, 1, $NC}];
ColVectorEven1Loop[[2]] = Total[HelicityFormFactorsOneLoop[[1, 5;;8]]*{$NF, $NC^-1, 1, $NC}];
ColVectorOdd1Loop = {0, 0, HelicityFormFactorsOneLoop[[2, 9]]};
ColVectorOdd1Loop[[1]] = Total[HelicityFormFactorsOneLoop[[2, ;;4]]*{$NF, $NC^-1, 1, $NC}];
ColVectorOdd1Loop[[2]] = Total[HelicityFormFactorsOneLoop[[2, 5;;8]]*{$NF, $NC^-1, 1, $NC}];

RAnalyticToNumeric = InitRulesContractionTreeLoop[ColVectorEven1Loop, ColVectorOdd1Loop, HelicityFormFactorsTree[[1]], HelicityFormFactorsTree[[2]]];
ContractedResult = ContractionMasterFormula/.Symbol["Global`tr5"] -> 0/.Symbol["Global`SUN"] -> $NC/.RKinematics/.RAnalyticToNumeric;
ContractedResult
];


ReconstructNormalization[ContractedAmplitude_]:=Block[{OLNormalization},
(*1/64 average color,  average of gluon helicities 1/4, tree yt but loop mt/2 -> 1/2, 1/(2\[Pi])^D -> 1/16/\[Pi]^2, (4\[Pi])^3 from gs^6 -> \[Alpha]S^3*)
OLNormalization = $yt^2*$\[Alpha]S^3*\[Pi]/32;
OLNormalization*2*Re[ContractedAmplitude]
];


TTHAmplitudeLoopTree[RKinematics_]:=N[Block[{mt2value,Rkin,$MinPrecision=$WorkingPrecision,$MaxPrecision=$WorkingPrecision,time1,time2,ffactors,ffactorstree,amp},
TTHPrint["TTHAmplitudeLoopTree: computing form factors at one loop level."];
mt2value = Symbol["mt2"]/.RKinematics;
Rkin = Thread[Keys[RKinematics] -> Values[RKinematics]/mt2value];
time1 = AbsoluteTime[];
ffactors = NEvalHelicityFormFactors[Rkin];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["TTHAmplitudeLoopTree: computed. Time used: `1`s."][Ceiling[time2-time1]]];

TTHPrint["TTHAmplitudeLoopTree: computing form factors at tree level."];
time1 = AbsoluteTime[];
ffactorstree = NEvalHelicityFormFactorsTree[Rkin];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["TTHAmplitudeLoopTree: computed. Time used: `1`s."][Ceiling[time2-time1]]];

TTHPrint["TTHAmplitudeLoopTree: contracting."];
time1 = AbsoluteTime[];
amp = Table[mt2value^-1*ReconstructNormalization[1/(Gamma[1+$EpsList[[i]]])*NContractLoopWithTree[Rkin, ffactors[[i]], ffactorstree]], {i, Length[$EpsList]}];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["TTHAmplitudeLoopTree: contracted. Time used: `1`s."][Ceiling[time2-time1]]];

FitEps[Thread[$EpsList -> amp], -2]
], $PrecisionGoal];


TTHUVCounter[RKinematics_]:=N[Block[{mt2value,Rkin,$MinPrecision=$WorkingPrecision,$MaxPrecision=$WorkingPrecision,time1,time2,NUVCT,NTree,TreeLevelContractedUVCT},
TTHPrint["TTHUVCounter: computing counter term."];
mt2value = Symbol["mt2"]/.RKinematics;
Rkin = Thread[Keys[RKinematics] -> Values[RKinematics]/mt2value];
time1 = AbsoluteTime[];
NUVCT = HelicityFormFactorsUVCT/.Rkin;
NUVCT = N[NUVCT/.ep->#&/@$EpsList, $WorkingPrecision];
NTree = NEvalHelicityFormFactorsTree[Rkin];
TreeLevelContractedUVCT = Table[mt2value^-1*ReconstructNormalization[NContractLoopWithTree[Rkin, NUVCT[[i]], NTree]], {i, Length[$EpsList]}];
time2 = AbsoluteTime[];
TTHPrint[StringTemplate["TTHUVCounter: computed. Time used: `1`s."][Ceiling[time2-time1]]];

FitEps[Thread[$EpsList -> TreeLevelContractedUVCT], -2]
], $PrecisionGoal];


End[];


EndPackage[];


TTHPrint["TTH: a package to calculate one-loop amplitude for ttH production in QCD."];
TTHPrint["Authors: Federico Buccioni, Philipp Alexander Kreer, Xiao Liu, Lorenzo Tancredi"];
TTHInitialize[];
TTHOptions@@Options[TTHOptions];
