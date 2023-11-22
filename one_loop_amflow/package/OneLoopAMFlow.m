(* ::Package:: *)

BeginPackage["OneLoopAMFlow`", {"DESolver`"}];


eps::usage = "The dimensional regulator eps = (4-D)/2.";
Term::usage = "Head of the integrals. A integral should look like Term[family, D0, {n1, n2, n3, ...}].";
Momenta::usage = "Momenta[fam] returns the momenta of the propagators.";
Masses::usage = "Masses[fam] returns the masses of the propagators.";
Replacements::usage = "Definition of kinematics.";
SymmetryRelations::usage = "Symmetry relations among integrals after introducing eta (partial, and can be {}).";
Masters::usage = "List of master integrals.";
Numerics::usage = "Phase space point under which you would like to evaluate the integrals.";
NThread::usage = "The number of threads you wish to use.";


GetFam::usage = "GetFam[term] gets the name of family.";
GetDim::usage = "GetDim[term] gets the space-time dimension.";
GetPow::usage = "GetPow[term] gets the power index.";
GetTotalPow::usage = "GetTotalPow[term] gets the total power.";
GetProp::usage = "GetProp[term] gets the number of propagators.";
GetTerms::usage = "GetTerms[exp] gets the integrals involved in exp.";
GenerateDEQ::usage = "GenerateDEQ[exp] generates the derivatives with respect to eta.";
TermOrderedQ::usage = "TermOrderedQ[t1, t2] judges whether the two integrals are ordered correctly.";
DEQSystem::usage = "DEQSystem[ints] generates the system of differential equations for the given list of integrals.";
GenerateBC::usage = "GenerateBC[int] generates the boundary condition.";


EvaluateMasters0::usage = "EvaluateMasters0[eps0, wkpre, xorder] evaluates the master integrals at eps=eps0.";
GenerateNumericalConfig::usage = "GenerateNumericalConfig[goal, order] gives the suggested numerical configuration {epslist, working precision, truncated order}.";
EvaluateMasters::usage = "EvaluateMasters[goal, order] evaluates the masters up to given epsilon order and precision goal.";


Begin["`Private`"];


GetFam[int_Term]:=int[[1]];
GetDim[int_Term]:=int[[2]];
GetPow[int_Term]:=int[[3]];
GetTotalPow[int_Term]:=Total[GetPow[int]];
GetProp[int_Term]:=Length[Select[GetPow[int], #>0&]];
GetTerms[exp_]:=DeleteDuplicates[Cases[exp, _Term, Infinity]];


(*generates the differential equation for a linear combination of integrals*)
GenerateDEQ[exp_]:=Block[{terms,coe},
terms = GetTerms[exp];
coe = Coefficient[exp, terms];
If[Collect[exp-coe . terms, _Term, Together]=!=0, Print["GenerateDEQ: unexpected combination" -> exp]; Abort[]];
D[coe, eta] . terms+coe . GenerateDEQ/@terms
];
GenerateDEQ[int_Term]:=Block[{fam,dim,power,nonzero,RN,SN,z0,sol,Ceta,zeta,subint,deq,first,rel},
{fam, dim, power} = {GetFam[int], GetDim[int], GetPow[int]};
nonzero = Flatten[Position[power, _?(#>0&)]];
If[nonzero==={}, Return[0]];

(*RN and SN before introducing eta*)
RN = Table[Expand[(Momenta[fam][[i]]-Momenta[fam][[j]])^2-Masses[fam][[i]]-Masses[fam][[j]]]/.Replacements/.Numerics, {i, nonzero}, {j, nonzero}];
SN = Table[Which[i===j===1, 0, i===1, 1, j===1, 1, True, RN[[i-1, j-1]]], {i, Length@RN+1}, {j, Length@RN+1}];

Which[Det[SN]=!=0,
(*set z0 = 1 and solve for C and zi. We have sol:={-C, z1, z2, ...}*)
z0 = 1;
sol = Inverse[SN] . Prepend[ConstantArray[0, Length@RN], z0];
(*after introducing eta, we have the transformation: det(SNeta) = det(SN), det(RNeta) = det(RN)+2*eta*det(SN), zieta = zi, Ceta = C-2*z0*eta*)
Ceta = -sol[[1]]-2*z0*eta;
zeta = sol[[2;;]];
(*DEQ*)
(*for one loop: Iprime(D) \[Rule] -I(D-2)*)
subint = Term[fam, dim-2, power-UnitVector[Length[power], #]]&/@nonzero;
deq = -Ceta^-1*((dim-1-Total[power])*z0*int+zeta . subint),

True,
(*by definition, Det[SN]===0, we must have z0 -> 0, so sol is at least one-dimension*)
sol = NullSpace[SN][[1]];

If[sol[[1]]=!=0, 
(*if C=!=0, we can still obtain a differential equation*)
subint = Term[fam, dim-2, power-UnitVector[Length[power], #]]&/@nonzero;
deq = sol[[1]]^-1*sol[[2;;]] . subint,

(*if C===0, we can only obtain a reduction relation*)
first = FirstPosition[sol[[2;;]], _?(#=!=0&), Heads -> False][[1]];
subint = Term[fam, dim, power+UnitVector[Length[power], nonzero[[first]]]-UnitVector[Length[power], #]]&/@nonzero;
deq = GenerateDEQ[int/.Solve[sol[[2;;]] . subint==0, int][[1]]]]
];

deq/.SymmetryRelations
];


(*judge whether two integrals are ordered correctly*)
TermOrderedQ[t1_Term, t2_Term]:=Which[
GetProp[t1]>GetProp[t2], True,
GetProp[t1]<GetProp[t2], False,

GetTotalPow[t1]>GetTotalPow[t2], True,
GetTotalPow[t1]<GetTotalPow[t2], False,

True, OrderedQ[t1,t2]
];


DEQSystem[ints_List]:=Block[{extra,current,system,ints2,posi,deqs},
If[ints==={}, Print["DEQSystem: no desired integrals."]; Abort[]];
If[!AllTrue[ints, Head[#]===Term&], Print["DEQSystem: undefined integrals" -> ints]; Abort[]];
extra = DeleteDuplicates[ints];
current = {};
system = {};

While[extra=!={},
current = Join[current, extra];
system = Join[system, GenerateDEQ/@extra];
extra = Complement[GetTerms[system], current];
];

ints2 = Reverse[Sort[current, TermOrderedQ]];
posi = Flatten[ints2/.PositionIndex[current]];
deqs = Coefficient[#, ints2]&/@system[[posi]];
If[AnyTrue[Collect[system[[posi]]-deqs . ints2, _Term, Together], #=!=0&], Print["DEQSystem: bad DEQ system."]; Abort[]];

{ints2, deqs}
];


GenerateBC[int_Term]:={GetDim[int]*1/2-GetTotalPow[int] -> (-1)^GetTotalPow[int]*Gamma[GetTotalPow[int]-GetDim[int]*1/2]*Gamma[GetTotalPow[int]]^-1};


EvaluateMasters0[eps0_,wkpre_,xorder_]:=Block[{dim,max,shift,masters,ints,deqs,bc,run,sol,
DESolver`Private`WorkingPre,DESolver`Private`ChopPre,DESolver`Private`SilentMode,DESolver`Private`RationalizePre,
DESolver`Private`XOrder,DESolver`Private`ExtraXOrder,DESolver`Private`LearnXOrder,DESolver`Private`TestXOrder,
DESolver`Private`RunRadius,DESolver`Private`RunLength,DESolver`Private`RunCandidate,DESolver`Private`RunDirection,
$MinPrecision,$MaxPrecision,t1,t2},
Print["EvaluateMasters0: started."];
Print[StringTemplate["EvaluateMasters0: evaluating masters at eps = `1` with working precision = `2` and truncated order = `3`."][
ToString[eps0, InputForm], ToString[wkpre, InputForm], ToString[xorder, InputForm]]];
t1 = AbsoluteTime[];
If[Masters==={}, sol = {},
dim = Expand[1/2*GetDim/@Masters-GetTotalPow/@Masters];
max = Max[Expand[dim-dim[[1]]]]+dim[[1]];
shift = dim-max;
If[!AllTrue[shift, IntegerQ], Print["EvaluateMasters0: space-time dimension of masters are not well defined."]; Abort[]];

masters = Masters;
masters[[All, 2]] = masters[[All, 2]]-2*shift/.D -> 4-2eps0;
masters = masters/.SymmetryRelations;

SetGlobalOptions["WorkingPre" -> wkpre, "ChopPre" -> 20, "SilentMode" -> True, "RationalizePre" -> 20];
SetExpansionOptions["XOrder" -> xorder, "ExtraXOrder" -> 20, "LearnXOrder" -> -1, "TestXOrder" -> 5];
SetRunningOptions["RunRadius" -> 2, "RunLength" -> 200, "RunCandidate" -> 10, "RunDirection" -> -I];

{ints, deqs} = DEQSystem[masters];
bc = GenerateBC/@ints;
bc[[All,1,2]] = N[bc[[All,1,2]], wkpre];
Print["EvaluateMasters0: DEQ system constructed with "<>ToString[Length[ints], InputForm]<>" integrals."];

LoadSystem[$InternalSystem, deqs, bc, Infinity];
run = RunEta[GetPoles[DE[$InternalSystem]]];
Print["EvaluateMasters0: expanding at the infinity..."];
InfToRegular[$InternalSystem, run[[1]]];
Print["EvaluateMasters0: expanding at regular points..."];
RegularRun[$InternalSystem, run];
Print["EvaluateMasters0: expanding at zero..."];
SolveAsyExp[$InternalSystem];
sol = MapThread[PickZeroRuleSShift, {masters/.Thread[ints -> AsyExp[$InternalSystem]], -shift}];
ClearSystem[$InternalSystem]
];

t2 = AbsoluteTime[];
Print["EvaluateMasters0: finished in "<>ToString[Ceiling[t2-t1], InputForm]<>"s."];
sol
];


PickZeroRuleSShift[rules_, shift_]:=Block[{key},
key = Select[Keys[rules], IntegerQ];
If[Length[key]===0, Return[0]];
If[Length[key] > 1, Print["PickZeroRuleSShift: uncombined regions encountered."]; Abort[]];
key = key[[1]];
If[key > shift, Return[0]];
(-1)^shift*Factorial[shift]*(key/.rules)[[1, -key+1+shift]]
];


GenerateNumericalConfig[goal_, order_]:=Block[{number,eps0,epslist,singlepre,wkpre,xorder},
number = 2*order+1;
eps0 = Power[10, -2*goal*number^-1-1];
eps0 = Rationalize[N[eps0, MachinePrecision], eps0/100];
epslist = eps0+Range[number]*eps0/100;
singlepre = 2*goal+number;
wkpre = 2*singlepre;
xorder = 3*singlepre;
{epslist, wkpre, xorder}
];


Options[EvaluateMasters] = {"Dimension" -> 4};
EvaluateMasters[goal_, order_, OptionsPattern[]]:=Block[{time1,epslist0,epslist,wkpre,xorder,$D0,leading,sol,time2},
Print["EvaluateMasters: started."];
time1 = AbsoluteTime[];
{epslist0, wkpre, xorder} = GenerateNumericalConfig[goal, order];
$D0 = OptionValue["Dimension"];
epslist = epslist0+(4-$D0)*1/2;
Print[StringTemplate["EvaluateMasters: masters will be evaluated at `1` different eps values `2`."][ToString[Length[epslist]], ToString[epslist, InputForm]]];
Print[StringTemplate["EvaluateMasters: working precision is `1` and truncated order is `2`."][ToString[wkpre], ToString[xorder]]];
Which[
!IntegerQ[NThread] || NThread<=1, 
sol = EvaluateMasters0[#, wkpre, xorder]&/@epslist,
True,
If[NThread>=Length[Kernels[]], LaunchKernels[NThread-Length[Kernels[]]], CloseKernels[]; LaunchKernels[NThread]];
sol = ParallelTable[EvaluateMasters0[epslist[[i]], wkpre, xorder], {i, Length[epslist]}, DistributedContexts -> All, Method -> "FinestGrained"]];
leading = -2;
sol = (FitEps[Thread[epslist0 -> #], leading]&/@Transpose[sol]) . Power[eps, Range[leading, leading+Length[epslist]-1]];
sol = N[Normal[Series[sol, {eps, 0, leading+order}]], goal];
time2 = AbsoluteTime[];
Print["EvaluateMasters: finished in "<>ToString[Ceiling[time2-time1], InputForm]<>"s."];
sol
];


End[];


EndPackage[];
