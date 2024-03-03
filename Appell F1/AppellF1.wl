(* ::Package:: *)

BeginPackage["F1`"]


Print["AppellF1.wl v1.0\n","Authors : Souvik Bera & Tanay Pathak\n",
"Last modified : 03.03.2024"];


F1::usage="The command gives the numerical value of the Appell F1.
 F1[a, b1, b2, c, x, y, precision, terms, verbose-> True]";
F1ROC::usage="The command gives the region of convergence (ROC) of the analytic continuation along with the given point
 F1ROC[{x,y}, analytic_continuation_number,{plot range}]";
F1findall::usage="The command finds all the analytic continuations that are valid for the given point
 F1findall[{x,y}]";
F1evaluate::usage="The command gives the value of F1 at a given point and Pochhammer parameters with a chosen analytic continuation
 F1evaluate[series_number,{a, b1, b2, c, x, y}, precision, terms]";
F1expose::usage="The command exposes the region of convergence (ROC) and the expression of the analytic continuation of F1[a, b1, b2, c, x, y , m , n]
 F1expose[series_number]";


Begin["`Private`"]


Off[General::infy,General::indet,General::munfl,General::stop ];
ParallelEvaluate[Off[General::infy,General::indet,General::munfl,General::stop ]];


F1expose[list_]:=Module[{i},
If[list>28,Print["Total number of ACs is 28"];Abort[];];
ClearAll[Global`x,Global`y,Global`a,Global`b1,Global`b2,Global`c,Global`m,Global`n];
Return[F1seriesifelse[{Global`a,Global`b1,Global`b2,Global`c,Global`x,Global`y},Global`m,Global`n][[list]]];
];



F1findall[{x0_?NumberQ, y0_?NumberQ}]:=Module[{test,pos,roc,a, b1, b2, c, x, y,m,n},
roc[{x_,y_}]=F1seriesifelse[{a, b1, b2, c, x, y},m,n][[All,1]];
test = roc[Rationalize/@{x0,y0}];
pos=Position[test,True];
Return[Flatten[pos]];


]



F1evaluate::singular="F1 is singular";
F1evaluate[list_/;(IntegerQ[list]&&list>0), para_List, p_,terms_]:=Module[
{m,a, b1, b2, c, x, y,result,p0,roc,acs,selectedseries,eps,peps,m1,
a0,b0,c0,d0,x0,y0},
$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];

p0= 10 p;
peps= 5p;

If[Length[para]=!=6,Abort[];];
If[Or@@((IntegerQ[#]&&#<=0)&/@(para[[-3;;-3]])),
Message[F1evaluate::singular];Abort[];];



(* If x=0 or y=0*)
{a0,b0,c0,d0,x0,y0}=para;
If[x0===0.0||x0===0,result=Hypergeometric2F1[a0,c0,d0,y0];Goto[end];];
If[y0===0.0||y0===0,result=Hypergeometric2F1[a0,b0,d0,x0];Goto[end];];

If[x0===1||x0===1., If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = Hypergeometric2F1[a0,b0,d0,1] Hypergeometric2F1[a0,c0,-b0+d0,y0];Goto[end];];
If[y0===1||y0===1., If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = Hypergeometric2F1[a0,b0,-c0+d0,x0] Hypergeometric2F1[a0,c0,d0,1];Goto[end];];

(*reduction formula*)
If[x0===y0, If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = Hypergeometric2F1[a0, b0 + c0, d0, x0];Goto[end];];


{a,b1,b2,c,x,y}=SetPrecision[para,p0];

(*Log conditions*)
If[IntegerQ[para[[1]]],a=para[[1]]-eps/2];
If[IntegerQ[para[[2]]],b1=para[[2]]-eps/3];
If[IntegerQ[para[[3]]],b2=para[[3]]+eps/5];

If[IntegerQ[-para[[1]]+para[[4]]],c=para[[4]]-eps/7];
If[IntegerQ[para[[1]]-para[[2]]],a=para[[1]]-eps/2];
If[IntegerQ[para[[1]]-para[[3]]],a=para[[1]]-eps/2];
If[IntegerQ[-para[[2]]+para[[4]]],b1=para[[2]]-eps/3];
If[IntegerQ[-para[[3]]+para[[4]]],b2=para[[3]]+eps/5];

If[IntegerQ[para[[1]]-para[[2]]-para[[3]]],a=para[[1]]-eps/2];
If[IntegerQ[-para[[2]]-para[[3]]+para[[4]]],c=para[[4]]-eps/7];
If[IntegerQ[para[[1]]+para[[2]]-para[[4]]],c=para[[4]]-eps/7];
If[IntegerQ[para[[1]]+para[[3]]-para[[4]]],c=para[[4]]-eps/7];

If[IntegerQ[para[[1]]+para[[2]]+para[[3]]-para[[4]]],c=para[[4]]-eps/7];




 eps= SetPrecision[ 10^(-p) I,peps];
 result=0;


roc[{x_,y_}]=F1seriesas2F1ifelse[{a, b1, b2, c, x, y},m][[All,1]];






If[F1seriesas2F1ifelse[{a,b1,b2,c,Rationalize[x],Rationalize[y]},m][[list,1]],Nothing[];,
Print["The point does not lie in ", list];Abort[];];


selectedseries[m_]=F1seriesas2F1ifelse[{a, b1, b2, c, x, y},m][[list,2]];

DistributeDefinitions[terms,p,p0,selectedseries,m];
result = N[ParallelSum[selectedseries[m],{m,0,terms}],p0];



Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]]


F1ROC[point___List,list_/;(IntegerQ[list]&&list>0),range_List]:=Module[{i,m,a, b1, b2, c, x, y,roc},

roc[{x_,y_}]=F1seriesas2F1ifelse[{a, b1, b2, c, x, y},m][[list,1]];
Show[ListPlot[{point},PlotStyle->Directive[PointSize[Medium],Red],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}}],
RegionPlot[roc[{x,y}],{x,range[[1]]-1,range[[2]]+1},{y,range[[1]]-1,range[[2]]+1},PlotPoints->100],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}},AspectRatio-> 1]
]


Options[F1]={verbose->False};
F1::poch="Pochhammer parameters do not obey the constraint";
F1::val="The point is not covered";
F1::singular="F1 is singular";
F1[args___] := F1core[F1,args];

F1core[F1,a0_,b0_,c0_,d0_,x0_,y0_,p_,terms_,OptionsPattern[F1]]:=Module[
{a,b1,b2,c,x1,y1,x,y,m,n,m1,aa,bb1,bb2,cc,xx,yy,
i,test1,test2,pos1,pos2,result,p0,seriesselect,
parameters,acs, roc,eachser,
eachserlist,convrate1,selectedseries,eps,peps,para},

$MaxExtraPrecision=1000;
(*p0=p+10;*)
m1=100;
p0= 10 p;
peps= 5 p;

If[Or@@((IntegerQ[#]&&#<=0)&/@{d0}),Message[F1::singular];Abort[];];






(* If x=0 or y=0*)

If[x0===0.0||x0===0,result=Hypergeometric2F1[a0,c0,d0,y0];Goto[end];];
If[y0===0.0||y0===0,result=Hypergeometric2F1[a0,b0,d0,x0];Goto[end];];
If[x0===y0, result = Hypergeometric2F1[a0, b0 + c0, d0, x0];Goto[end];];
If[x0===1||x0===1., result = Hypergeometric2F1[a0,b0,d0,1] Hypergeometric2F1[a0,c0,-b0+d0,y0];Goto[end];];
If[y0===1||y0===1., result = Hypergeometric2F1[a0,b0,-c0+d0,x0] Hypergeometric2F1[a0,c0,d0,1];Goto[end];];


{a,b1,b2,c,x,y}=SetPrecision[{a0,b0,c0,d0,x0,y0},p0];

(*Log conditions*)
If[IntegerQ[a0],a=a0-eps/2];
If[IntegerQ[b0],b1=b0-eps/3];
If[IntegerQ[c0],b2=c0+eps/5];

If[IntegerQ[-a0+d0],c=d0-eps/7];
If[IntegerQ[a0-b0],a=a0-eps/2];
If[IntegerQ[a0-c0],a=a0-eps/2];
If[IntegerQ[-b0+d0],b1=b0-eps/3];
If[IntegerQ[-c0+d0],b2=c0+eps/5];

If[IntegerQ[a0-b0-c0],a=a0-eps/2];
If[IntegerQ[-b0-c0+d0],c=d0-eps/7];
If[IntegerQ[a0+b0-d0],c=d0-eps/7];
If[IntegerQ[a0+c0-d0],c=d0-eps/7];

If[IntegerQ[a0+b0+c0-d0],c=d0-eps/7];



parameters= SetPrecision[{a,b1,b2,c,x,y},p0];
 eps= SetPrecision[ 10^(-p) I,peps];
 result=0;

Off[General::infy,General::indet];
(* check the region *)

roc[{xx_,yy_}]=F1seriesas2F1ifelse[{a, b1, b2, c, xx, yy},m][[All,1]];
(*acs[{a_, b1_, b2_, c_,x_,y_},m_]=F1seriesas2F1ifelse[{a, b1, b2, c, x, y},m][[All,2]];*)

test1 = roc[Rationalize/@(parameters[[-2;;-1]])];

(*Print[test1];*)

PrintTemporary["Finding a proper AC..."];

pos1=Flatten[Position[test1,True]];

If[pos1==={},Message[F1::val];Abort[];];

If[OptionValue[verbose],Print["valid series : ",pos1]];

test2=Table[{Max[Norm[#]&/@(Abs[{Simplify[(#1/.{m-> m1+ 1})/(#1)/.{m-> m1}]}
/.{aa-> a,bb1-> b1,bb2-> b2,cc-> c,xx-> x,yy-> y}]&/@If[Head[#]===Plus,List@@#,{#}]&@Expand[
F1seriesas2F1ifelse[{aa,bb1,bb2,cc,xx,yy},m][[pos1[[i]],2]]])],pos1[[i]]},{i,Length[pos1]}];




seriesselect=SortBy[test2,First];
If[OptionValue[verbose],Print["convergence rates :",{N[#[[1]],10],#[[2]]}&/@seriesselect]];

pos2=seriesselect[[1]][[2]];





If[OptionValue[verbose],Print["selected series : ",pos2]];

PrintTemporary["Evaluating sum..."];


selectedseries[m_]=F1seriesas2F1ifelse[{a, b1, b2, c, x, y},m][[pos2,2]]; 
DistributeDefinitions[terms,m,selectedseries,p0];
result = N[ParallelSum[selectedseries[m],{m,0,terms}],p0];



Label[end];
Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]

]



(* ::Section:: *)
(*Appell F1 ACs : A total of 28 ACs*)


F1seriesifelse[{a_,b1_,b2_,c_,x_,y_},m_,n_]={{Abs[x]<1&&Abs[y]<1,(x^m y^n Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Pochhammer[c,m+n])},{Abs[1-x]<1&&Abs[y]<1&&Abs[1-x]+Abs[y]<1,((1-x)^m y^n Gamma[c] Gamma[-a-b1+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m] Pochhammer[-b1+c,n])+((1-x)^(-a-b1+c+m) y^n Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[-a+c,m] Pochhammer[-b1+c,m+n])/(m! n! Gamma[a] Gamma[b1] Pochhammer[-b1+c,n] Pochhammer[1-a-b1+c,m])},{Abs[1-y]<1&&Abs[x]<1&&Abs[x]+Abs[1-y]<1,(x^m (1-y)^n Gamma[c] Gamma[-a-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,n] Pochhammer[-b2+c,m])+(x^m (1-y)^(-a-b2+c+n) Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[-a+c,n] Pochhammer[-b2+c,m+n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[-b2+c,m] Pochhammer[1-a-b2+c,n])},{Abs[y]<1&&Abs[1-x]>1+Abs[y]&&Abs[y/(-1+x)]<1,((1/(1-x))^m (1-x)^-b1 (-y)^n Gamma[a-b1] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m-n] Pochhammer[-b1+c,n])+((1/(1-x))^m (1-x)^-a (y/(-1+x))^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[-b1+c,n])},{Abs[x]<1&&Abs[1-y]>1+Abs[x]&&Abs[x/(-1+y)]<1,((-x)^m (1/(1-y))^n (1-y)^-b2 Gamma[a-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,-m+n] Pochhammer[-b2+c,m])+((1/(1-y))^n (1-y)^-a (x/(-1+y))^m Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b2+c,m+n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[-b2+c,m])},{1/Abs[1-x]<1&&1+1/Abs[y]<1/Abs[y/(1-x)]&&1/Abs[y]<1&&Abs[y/(1-x)]<1&&Abs[y/(-1+x)]<1&&1/Abs[1-x]+Abs[y/(-1+x)]<1,((1/(1-x))^m (1-x)^-b1 (1/y)^n (-y)^-b2 Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((1-x)^-b1 (-(1/y))^n (-y)^(-a+b1) (y/(-1+x))^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m-n] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m-n] Pochhammer[-a+c,m-n])+((1/(1-x))^m (1-x)^-a (y/(-1+x))^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[-b1+c,n])},{1/Abs[1-y]<1&&1+1/Abs[x]<1/Abs[x/(1-y)]&&1/Abs[x]<1&&Abs[x/(1-y)]<1&&Abs[x/(-1+y)]<1&&1/Abs[1-y]+Abs[x/(-1+y)]<1,((1/x)^m (-x)^-b1 (1/(1-y))^n (1-y)^-b2 Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-(1/x))^m (-x)^(-a+b2) (1-y)^-b2 (x/(-1+y))^n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[b2,n] Pochhammer[-a+b1+b2,-m+n] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b2,-m+n] Pochhammer[-a+c,-m+n])+((1/(1-y))^n (1-y)^-a (x/(-1+y))^m Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b2+c,m+n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[-b2+c,m])},{Abs[-1+x]<1&&Abs[(-1+x)/y]<1&&Abs[(-1+x)/y]+1/Abs[y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+Abs[-1+x]),((1-x)^m (-y)^(-a-m) y^-n Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[1+a+b1-c,m+n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[1+a+b1-c,m])+((1-x)^m (-y)^(-b2-m) y^(m-n) Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,-m+n] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^(-a-b1+c+m) (-y)^-b2 y^-n Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2-c,-m+n] Pochhammer[1-a-b1+c,m])},{Abs[-1+y]<1&&Abs[(-1+y)/x]<1&&1/Abs[x]+Abs[(-1+y)/x]<1&&1/Abs[x]<1&&1/Abs[x]<1/(1+Abs[-1+y]),((-x)^(-a-n) x^-m (1-y)^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[1+a+b2-c,m+n])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[1+a+b2-c,n])+((-x)^(-b1-n) x^(-m+n) (1-y)^n Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-n] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a-b2+c+n) Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[1+b1+b2-c,m-n] Pochhammer[1-a-b2+c,n])},{1/Abs[1-x]<1&&Abs[(1-x)/y]<1&&Abs[(1-x)/y]<1/(1+1/Abs[1-x])&&Abs[(-1+x)/y]<1&&1/Abs[y]<1&&Abs[(-1+x)/y]+1/Abs[y]<1,((1/y)^n ((-1+x)/y)^m (-y)^-a Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[1+a+b1-c,m+n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[1+a+b1-c,m])+((1/(1-x))^m (1-x)^-b1 (1/y)^n (-y)^-b2 Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((1/(1-x))^m (1-x)^(-a+b2) ((1-x)/y)^n (-y)^-b2 Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-b1-b2+c,m-n])/(m! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-n])},{1/Abs[1-y]<1&&Abs[(1-y)/x]<1&&Abs[(1-y)/x]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/x]<1&&1/Abs[x]<1&&1/Abs[x]+Abs[(-1+y)/x]<1,((1/x)^m (-x)^-a ((-1+y)/x)^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[1+a+b2-c,m+n])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[1+a+b2-c,n])+((1/x)^m (-x)^-b1 (1/(1-y))^n (1-y)^-b2 Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((1-y)/x)^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m] Pochhammer[-b1-b2+c,-m+n])/(m! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n])},{Abs[x]>1&&Abs[y]<1&&Abs[y/x]<1,((-1)^n (-x)^(-a-n) x^-m y^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[1+a-c,m])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n])+((-x)^-b1 x^-m y^n Gamma[a-b1] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n])/(m! n! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m-n])},{Abs[y]>1&&Abs[x]<1&&Abs[x/y]<1,((-1)^m x^m (-y)^(-a-m) y^-n Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[1+a-c,n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n])+(x^m (-y)^-b2 y^-n Gamma[a-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n])/(m! n! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,-m+n])},{1/Abs[x]<1&&1/Abs[y]<1&&Abs[y/x]<1,((-1)^n (-x)^(-a-n) x^-m y^n Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[1+a-c,m])/(m! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n])+((-x)^-b1 x^-m (-y)^(-a+b1) y^(m-n) Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[1+a-c,n])/(m! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n])+((-x)^-b1 x^-m (-y)^-b2 y^-n Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m+n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])},{1/Abs[y]<1&&1/Abs[x]<1&&Abs[x/y]<1,((-x)^(-a+b2) x^(-m+n) (-y)^-b2 y^-n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[1+a-c,m])/(m! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-n])+((-1)^m x^m (-y)^(-a-m) y^-n Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[1+a-c,n])/(m! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n])+((-x)^-b1 x^-m (-y)^-b2 y^-n Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m+n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-a (y/(-1+y))^n Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[-a+c,m] Pochhammer[-b2+c,m+n])/(m! n! Pochhammer[c,m+n] Pochhammer[-b2+c,m])},{Abs[(-1+x)/x]<1&&Abs[y/(-1+y)]<1&&Abs[(-1+x)/x]<1&&Abs[y/(-1+y)]<1,((1-x)^-b1 (-((-1+x)/x))^b1 ((-1+x)/x)^m (1-y)^-a (y/(-1+y))^n Gamma[c] Gamma[-a-b1+c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[1+b1-c,m-n] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m] Pochhammer[1+b1+b2-c,m-n])+((1-x)^-b1 (-((-1+x)/x))^(-a+c) ((-1+x)/x)^m (1-y)^-a (y/(-1+y))^n Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m-n] Pochhammer[a,n] Pochhammer[1-a+b2,m] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[b1] Pochhammer[1-a+b2,m-n] Pochhammer[1-a-b1+c,m])},{Abs[(-1+y)/y]<1&&Abs[x/(-1+x)]<1&&Abs[(-1+y)/y]<1&&Abs[x/(-1+x)]<1,((1-x)^-a (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n Gamma[c] Gamma[-a-b2+c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,n] Pochhammer[1+b1+b2-c,-m+n])+((1-x)^-a (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) ((-1+y)/y)^n Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,-m+n] Pochhammer[a,m] Pochhammer[1-a+b1,n] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[1-a+b1,-m+n] Pochhammer[1-a-b2+c,n])},{Abs[(-1+x)/x]<1&&Abs[(-1+y)/y]<1&&Abs[((-1+x) y)/(x (-1+y))]<1,((1-x)^-b1 (-((-1+x)/x))^b1 ((-1+x)/x)^m (1-y)^-a (-((-1+y)/y))^a ((-1+y)/y)^n Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[1+a+b1-c,m+n] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1-c,m] Pochhammer[1+a+b1+b2-c,m+n])+((-1)^m (1-x)^-b1 (-((-1+x)/x))^(-a+c) ((-1+x)/x)^m (1-y)^-a (-((-1+y)/y))^(a-b2) ((-1+y)/y)^n (-(y/(-1+y)))^m Gamma[a+b1-c] Gamma[c] Pochhammer[1-b2,n] Pochhammer[a-b2,-m+n] Pochhammer[1-a+b2,m] Pochhammer[-a+c,m])/(m! n! Gamma[a] Gamma[b1] Pochhammer[1-b2,-m+n] Pochhammer[1-a-b1+c,m])+((-1)^m (1-x)^-b1 (-((-1+x)/x))^b1 ((-1+x)/x)^m (1-y)^-a (-((-1+y)/y))^(-b1-b2+c) ((-1+y)/y)^n (-(y/(-1+y)))^m Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[1+b1+b2-c,m] Pochhammer[-b1-b2+c,-m+n])/(m! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,m] Pochhammer[1-a-b1-b2+c,-m+n])},{Abs[(-1+y)/y]<1&&Abs[(-1+x)/x]<1&&Abs[(x (-1+y))/((-1+x) y)]<1,((1-x)^-a (-((-1+x)/x))^a ((-1+x)/x)^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[1+a+b2-c,m+n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b2-c,n] Pochhammer[1+a+b1+b2-c,m+n])+((-1)^n (1-x)^-a (-((-1+x)/x))^(a-b1) ((-1+x)/x)^m (-(x/(-1+x)))^n (1-y)^-b2 (-((-1+y)/y))^(-a+c) ((-1+y)/y)^n Gamma[a+b2-c] Gamma[c] Pochhammer[1-b1,m] Pochhammer[a-b1,m-n] Pochhammer[1-a+b1,n] Pochhammer[-a+c,n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[1-b1,m-n] Pochhammer[1-a-b2+c,n])+((-1)^n (1-x)^-a (-((-1+x)/x))^(-b1-b2+c) ((-1+x)/x)^m (-(x/(-1+x)))^n (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-b1-b2+c,m-n])/(m! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2-c,n] Pochhammer[1-a-b1-b2+c,m-n])},{Abs[y]>1&&Abs[(x-y)/(-1+x)]<1&&Abs[(x-y)/((-1+x) y)]<1,((-1)^m (1-x)^-b1 (1-y)^(-a-b2+c) ((x-y)/(-1+x))^m (-y)^(a-c-m) y^-n Gamma[a-b1-b2] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((1-x)^-b1 (1-y)^(-a-b2+c) ((x-y)/(-1+x))^m (-y)^(b1+b2-c) y^-n Gamma[-a+b1+b2] Gamma[c] Pochhammer[b1,m] Pochhammer[1-b1-b2,-m+n] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[b1+b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n])},{Abs[x]>1&&Abs[(-x+y)/(-1+y)]<1&&Abs[(-x+y)/(x (-1+y))]<1,((-1)^n (1-x)^(-a-b1+c) (-x)^(a-c-n) x^-m (1-y)^-b2 ((-x+y)/(-1+y))^n Gamma[a-b1-b2] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((1-x)^(-a-b1+c) (-x)^(b1+b2-c) x^-m (1-y)^-b2 ((-x+y)/(-1+y))^n Gamma[-a+b1+b2] Gamma[c] Pochhammer[1-b1-b2,m-n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[b1+b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-n])},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^m (1-y)^n Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m+n])+((1-x)^-b1 (1-y)^(-a-b2+c+n) ((-1+y)/(-1+x))^m Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n])+((1-x)^(-a-b1-b2+c+m-n) (1-y)^n Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,m-n])},{Abs[1-y]<1&&Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((1-x)^m (1-y)^n Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m+n])+((1-x)^(-a-b1+c+m) (1-y)^-b2 ((-1+x)/(-1+y))^n Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n])+((1-x)^m (1-y)^(-a-b1-b2+c-m+n) Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[-a-b1+c,-m+n] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,-m+n])},{Abs[1-x]>1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^-m (1/(-1+x))^b1 (1-y)^n Gamma[a-b1] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-n])/(m! n! Gamma[a] Gamma[1+a+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-n])+((1-x)^(-a-b1-b2+c-m) (1/(-1+x))^(-a-b2+c) (1-y)^n Gamma[a-b1] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-n])/(m! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-n])+((-1)^n (1-x)^-m (1/(-1+x))^a (-1+x)^-n (1-y)^n Gamma[-a+b1] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[b1] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1,m+n])+((-1)^n (1-x)^(-a-b1-b2+c-m) (1/(-1+x))^(-b1-b2+c) (-1+x)^-n (1-y)^n Gamma[-a+b1] Gamma[a+b1+b2-c] Gamma[c] Gamma[1-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n])+((1-x)^-b1 (1-y)^(-a-b2+c+n) ((-1+y)/(-1+x))^m Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n])},{Abs[1-y]>1&&Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((1-x)^m (1-y)^-n (1/(-1+y))^b2 Gamma[a-b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a-b1+c,-m+n])/(m! n! Gamma[a] Gamma[1+a+b1-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,-m+n])+((1-x)^m (1-y)^(-a-b1-b2+c-n) (1/(-1+y))^(-a-b1+c) Gamma[a-b2] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a-b1+c,-m+n])/(m! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,-m+n])+((1-x)^(-a-b1+c+m) (1-y)^-b2 ((-1+x)/(-1+y))^n Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n])+((-1)^m (1-x)^m (1-y)^-n (1/(-1+y))^a (-1+y)^-m Gamma[-a+b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b2,m+n])+((-1)^m (1-x)^m (1-y)^(-a-b1-b2+c-n) (1/(-1+y))^(-b1-b2+c) (-1+y)^-m Gamma[-a+b2] Gamma[a+b1+b2-c] Gamma[c] Gamma[1-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n])},{1/Abs[1-x]<1&&1/Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((-1)^m (1-x)^-b1 (1-y)^(-a-b2+c-n) (1/(-1+y))^(-a+c) (-1+y)^-m ((-1+y)/(-1+x))^m Gamma[a-b1-b2] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-1)^m (1/(-1+x))^b1 (1-y)^-n (1/(-1+y))^b2 (-1+y)^-m ((-1+y)/(-1+x))^m Gamma[a-b1-b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-1)^m (1-x)^(-a-b1-b2+c) (1/(-1+x))^(-a-b2+c) (1-y)^-n (1/(-1+y))^b2 (-1+y)^-m ((-1+y)/(-1+x))^m Gamma[a-b1-b2] Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-1)^n (1-x)^-m (1/(-1+x))^a (-1+x)^-n (1-y)^n Gamma[-a+b1] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[b1] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1,m+n])+((-1)^n (1-x)^(-a-b1-b2+c-m) (1/(-1+x))^(-b1-b2+c) (-1+x)^-n (1-y)^n Gamma[-a+b1] Gamma[a+b1+b2-c] Gamma[c] Gamma[1-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n])+((1-x)^-b1 (1-y)^(-a-b2+c-n) (1/(-1+y))^(-b1-b2+c) ((-1+y)/(-1+x))^m Gamma[-a+b1+b2] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[1-a+b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n])+((1/(-1+x))^b1 (1-y)^-n (1/(-1+y))^(a-b1) ((-1+y)/(-1+x))^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1-b2,-m+n])+((1-x)^(-a-b1-b2+c) (1/(-1+x))^(-a-b2+c) (1-y)^-n (1/(-1+y))^(a-b1) ((-1+y)/(-1+x))^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1-b2,-m+n])},{1/Abs[1-y]<1&&1/Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((-1)^n (1-x)^(-a-b1+c-m) (1/(-1+x))^(-a+c) (-1+x)^-n (1-y)^-b2 ((-1+x)/(-1+y))^n Gamma[a-b1-b2] Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-1)^n (1-x)^-m (1/(-1+x))^b1 (-1+x)^-n (1/(-1+y))^b2 ((-1+x)/(-1+y))^n Gamma[a-b1-b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((-1)^n (1-x)^-m (1/(-1+x))^b1 (-1+x)^-n (1-y)^(-a-b1-b2+c) (1/(-1+y))^(-a-b1+c) ((-1+x)/(-1+y))^n Gamma[a-b1-b2] Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+n])/(m! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n])+((1-x)^(-a-b1+c-m) (1/(-1+x))^(-b1-b2+c) (1-y)^-b2 ((-1+x)/(-1+y))^n Gamma[-a+b1+b2] Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Gamma[1-a+b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-n])+((1-x)^-m (1/(-1+x))^(a-b2) (1/(-1+y))^b2 ((-1+x)/(-1+y))^n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1-b2,m-n])+((1-x)^-m (1/(-1+x))^(a-b2) (1-y)^(-a-b1-b2+c) (1/(-1+y))^(-a-b1+c) ((-1+x)/(-1+y))^n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[-b1-b2+c,m])/(m! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1-b2,m-n])+((-1)^m (1-x)^m (1-y)^-n (1/(-1+y))^a (-1+y)^-m Gamma[-a+b2] Gamma[1+a+b1+b2-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b2,m+n])+((-1)^m (1-x)^m (1-y)^(-a-b1-b2+c-n) (1/(-1+y))^(-b1-b2+c) (-1+y)^-m Gamma[-a+b2] Gamma[a+b1+b2-c] Gamma[c] Gamma[1-a-b1-b2+c] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2+c,n])/(m! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n])}};


F1seriesas2F1ifelse[{a_,b1_,b2_,c_,x_,y_},m_]={{Abs[x]<1&&Abs[y]<1,(y^m HypergeometricPFQ[{b1,a+m},{c+m},x] Pochhammer[a,m] Pochhammer[b2,m])/(m! Pochhammer[c,m])},{Abs[1-x]<1&&Abs[y]<1&&Abs[1-x]+Abs[y]<1,((1-x)^m Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b2,a+m},{-b1+c},y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m])+((1-x)^(-a-b1+c+m) Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b2,-b1+c+m},{-b1+c},y] Pochhammer[-a+c,m] Pochhammer[-b1+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[1-y]<1&&Abs[x]<1&&Abs[x]+Abs[1-y]<1,(x^m (1-y)^(-a-b2+c) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{-a+c,-b2+c+m},{1-a-b2+c},1-y] Pochhammer[b1,m])/(m! Gamma[a] Gamma[b2])+(x^m Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b2,a+m},{1+a+b2-c},1-y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[-a+c] Gamma[-b2+c] Pochhammer[-b2+c,m])},{Abs[y]<1&&Abs[1-x]>1+Abs[y]&&Abs[y/(-1+x)]<1,((1-x)^-a (y/(-1+x))^m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,-b1+c+m},{1+a-b1+m},1/(1-x)] Pochhammer[a,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^-m (1-x)^-b1 (-y)^m Gamma[a-b1] Gamma[c] HypergeometricPFQ[{b1,-a+c},{1-a+b1-m},1/(1-x)] Pochhammer[a-b1,m] Pochhammer[b2,m])/(m! Gamma[a] Gamma[-b1+c] Pochhammer[-b1+c,m])},{Abs[x]<1&&Abs[1-y]>1+Abs[x]&&Abs[x/(-1+y)]<1,((1-y)^-a (x/(-1+y))^m Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,-b2+c+m},{1+a-b2+m},1/(1-y)] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^-m (-x)^m (1-y)^-b2 Gamma[a-b2] Gamma[c] HypergeometricPFQ[{b2,-a+c},{1-a+b2-m},1/(1-y)] Pochhammer[b1,m] Pochhammer[a-b2,m])/(m! Gamma[a] Gamma[-b2+c] Pochhammer[-b2+c,m])},{1/Abs[1-x]<1&&1+1/Abs[y]<1/Abs[y/(1-x)]&&1/Abs[y]<1&&Abs[y/(1-x)]<1&&Abs[y/(-1+x)]<1&&1/Abs[1-x]+Abs[y/(-1+x)]<1,((1-x)^-a (y/(-1+x))^m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,-b1+c+m},{1+a-b1+m},1/(1-x)] Pochhammer[a,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((1-x)^-b1 (-y)^(-a+b1) (y/(-1+x))^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{a-b1-m,1+a-c-m},{1+a-b1-b2-m},1/y] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m])+((1/(1-x))^m (1-x)^-b1 (-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c},{1-a+b1+b2+m},1/y] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{1/Abs[1-y]<1&&1+1/Abs[x]<1/Abs[x/(1-y)]&&1/Abs[x]<1&&Abs[x/(1-y)]<1&&Abs[x/(-1+y)]<1&&1/Abs[1-y]+Abs[x/(-1+y)]<1,((1-y)^-a (x/(-1+y))^m Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,-b2+c+m},{1+a-b2+m},1/(1-y)] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-x)^(-a+b2) (1-y)^-b2 (x/(-1+y))^m Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{a-b2-m,1+a-c-m},{1+a-b1-b2-m},1/x] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b2,m])+((-x)^-b1 (1/(1-y))^m (1-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2-c},{1-a+b1+b2+m},1/x] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[-1+x]<1&&Abs[(-1+x)/y]<1&&Abs[(-1+x)/y]+1/Abs[y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+Abs[-1+x]),((1-x)^m (-y)^(-a-m) Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1-c+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^-m (1-x)^m (-y)^(-b2-m) y^m Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b2,1+b1+b2-c},{1-a+b2-m},1/y] Pochhammer[b1,m] Pochhammer[a-b2,m])/(m! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1-c,m])+((1-x)^(-a-b1+c+m) (-y)^-b2 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c},{1+b1+b2-c-m},1/y] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[-1+y]<1&&Abs[(-1+y)/x]<1&&1/Abs[x]+Abs[(-1+y)/x]<1&&1/Abs[x]<1&&1/Abs[x]<1/(1+Abs[-1+y]),((-x)^(-a-m) (1-y)^m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2-c+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^-m (-x)^(-b1-m) x^m (1-y)^m Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b1,1+b1+b2-c},{1-a+b1-m},1/x] Pochhammer[a-b1,m] Pochhammer[b2,m])/(m! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b2-c,m])+((-x)^-b1 (1-y)^(-a-b2+c+m) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2-c},{1+b1+b2-c-m},1/x] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])},{1/Abs[1-x]<1&&Abs[(1-x)/y]<1&&Abs[(1-x)/y]<1/(1+1/Abs[1-x])&&Abs[(-1+x)/y]<1&&1/Abs[y]<1&&Abs[(-1+x)/y]+1/Abs[y]<1,(((-1+x)/y)^m (-y)^-a Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1-c+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^m (1-x)^(-a+b2) ((1-x)/y)^m (-y)^-b2 Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{a-b2-m,-b1-b2+c-m},{1+a-b1-b2-m},1/(1-x)] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b2,m])+((1/(1-x))^m (1-x)^-b1 (-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c},{1-a+b1+b2+m},1/y] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{1/Abs[1-y]<1&&Abs[(1-y)/x]<1&&Abs[(1-y)/x]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/x]<1&&1/Abs[x]<1&&1/Abs[x]+Abs[(-1+y)/x]<1,((-x)^-a ((-1+y)/x)^m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2-c+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^m (-x)^-b1 (1-y)^(-a+b1) ((1-y)/x)^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{a-b1-m,-b1-b2+c-m},{1+a-b1-b2-m},1/(1-y)] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m])+((-x)^-b1 (1/(1-y))^m (1-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2-c},{1-a+b1+b2+m},1/x] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[x]>1&&Abs[y]<1&&Abs[y/x]<1,((-x)^-a x^-m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b2,a+m},{1+a-b1+m},y/x] Pochhammer[a,m] Pochhammer[1+a-c,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 x^-m Gamma[a-b1] Gamma[c] HypergeometricPFQ[{b2,a-b1-m},{-b1+c-m},y] Pochhammer[b1,m] Pochhammer[1+b1-c,m])/(m! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m])},{Abs[y]>1&&Abs[x]<1&&Abs[x/y]<1,((-1)^m x^m (-y)^(-a-m) Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+(x^m (-y)^-b2 Gamma[a-b2] Gamma[c] HypergeometricPFQ[{b2,1+b2-c-m},{1-a+b2-m},1/y] Pochhammer[b1,m] Pochhammer[a-b2,m])/(m! Gamma[a] Gamma[-b2+c] Pochhammer[-b2+c,m])},{1/Abs[x]<1&&1/Abs[y]<1&&Abs[y/x]<1,((-x)^-b1 x^-m (-y)^(-a+b1) y^m Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a-b1-m},{1+a-b1-b2-m},1/y] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m])+((-x)^-a x^-m Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b2,a+m},{1+a-b1+m},y/x] Pochhammer[a,m] Pochhammer[1+a-c,m])/(m! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 x^-m (-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c+m},{1-a+b1+b2+m},1/y] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{1/Abs[y]<1&&1/Abs[x]<1&&Abs[x/y]<1,((-1)^m x^m (-y)^(-a-m) Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-x)^(-a+b2) x^-m (-y)^-b2 Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b2,-a+b1+b2-m},{1-a+b2-m},x/y] Pochhammer[a-b2,m] Pochhammer[1+a-c,m])/(m! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m])+((-x)^-b1 x^-m (-y)^-b2 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c+m},{1-a+b1+b2+m},1/y] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-a HypergeometricPFQ[{a,-b2+c+m},{c+m},y/(-1+y)] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Pochhammer[c,m])},{Abs[(-1+x)/x]<1&&Abs[y/(-1+y)]<1&&Abs[(-1+x)/x]<1&&Abs[y/(-1+y)]<1,((-1+1/x)^b1 (1-x)^-b1 ((-1+x)/x)^m (1-y)^-a Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{a,-b1-b2+c-m},{-b1+c-m},y/(-1+y)] Pochhammer[b1,m] Pochhammer[1+b1-c,m])/(m! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m])+((-1+1/x)^(-a+c) (1-x)^-b1 ((-1+x)/x)^m (1-y)^-a Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{a,a-b2-m},{a-m},y/(-1+y)] Pochhammer[1-a,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[(-1+y)/y]<1&&Abs[x/(-1+x)]<1&&Abs[(-1+y)/y]<1&&Abs[x/(-1+x)]<1,((1-x)^-a (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^m Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{a,-b1-b2+c-m},{-b2+c-m},x/(-1+x)] Pochhammer[b2,m] Pochhammer[1+b2-c,m])/(m! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,m])+((1-x)^-a (-1+1/y)^(-a+c) (1-y)^-b2 ((-1+y)/y)^m Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{a,a-b1-m},{a-m},x/(-1+x)] Pochhammer[1-a,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])},{Abs[(-1+x)/x]<1&&Abs[(-1+y)/y]<1&&Abs[((-1+x) y)/(x (-1+y))]<1,((-1)^m (-1+1/x)^b1 (1-x)^-b1 ((-1+x)/x)^m (-1+1/y)^(-b1-b2+c) (1-y)^-a (-(y/(-1+y)))^m Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{1-b2,-b1-b2+c-m},{1-a-b1-b2+c-m},(-1+y)/y] Pochhammer[b1,m] Pochhammer[a+b1+b2-c,m])/(m! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,m])+((-1+1/x)^b1 (1-x)^-b1 ((-1+x)/x)^m (-1+1/y)^a (1-y)^-a Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{a,1+a+b1-c+m},{1+a+b1+b2-c+m},(-1+y)/y] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((-1)^m (-1+1/x)^(-a+c) (1-x)^-b1 ((-1+x)/x)^m (-1+1/y)^(a-b2) (1-y)^-a (-(y/(-1+y)))^m Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{1-b2,a-b2-m},{1-b2-m},(-1+y)/y] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[(-1+y)/y]<1&&Abs[(-1+x)/x]<1&&Abs[(x (-1+y))/((-1+x) y)]<1,((-1)^m (-1+1/x)^(-b1-b2+c) (1-x)^-a (-(x/(-1+x)))^m (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^m Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{1-b1,-b1-b2+c-m},{1-a-b1-b2+c-m},(-1+x)/x] Pochhammer[b2,m] Pochhammer[a+b1+b2-c,m])/(m! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2-c,m])+((-1+1/x)^a (1-x)^-a (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^m Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{a,1+a+b2-c+m},{1+a+b1+b2-c+m},(-1+x)/x] Pochhammer[b2,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((-1)^m (-1+1/x)^(a-b1) (1-x)^-a (-(x/(-1+x)))^m (-1+1/y)^(-a+c) (1-y)^-b2 ((-1+y)/y)^m Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{1-b1,a-b1-m},{1-b1-m},(-1+x)/x] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])},{Abs[y]>1&&Abs[(x-y)/(-1+x)]<1&&Abs[(x-y)/((-1+x) y)]<1,((1-x)^-b1 (1-y)^(-a-b2+c) ((x-y)/(-1+x))^m (-y)^(b1+b2-c) Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{-b1-b2+c,1-b1-b2-m},{1+a-b1-b2-m},1/y] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[b1+b2] Gamma[-a+c] Pochhammer[b1+b2,m])+((-1)^m (1-x)^-b1 (1-y)^(-a-b2+c) ((x-y)/(-1+x))^m (-y)^(a-c-m) Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a+b1+b2+m},1/y] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[x]>1&&Abs[(-x+y)/(-1+y)]<1&&Abs[(-x+y)/(x (-1+y))]<1,((1-x)^(-a-b1+c) (-x)^(b1+b2-c) (1-y)^-b2 ((-x+y)/(-1+y))^m Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{-b1-b2+c,1-b1-b2-m},{1+a-b1-b2-m},1/x] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[b1+b2] Gamma[-a+c] Pochhammer[b1+b2,m])+((-1)^m (1-x)^(-a-b1+c) (-x)^(a-c-m) (1-y)^-b2 ((-x+y)/(-1+y))^m Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a+b1+b2+m},1/x] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^(-a-b1-b2+c-m) (1-y)^m Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{-b1-b2+c,-a-b2+c-m},{1-a-b1-b2+c-m},1-x] Pochhammer[b2,m] Pochhammer[a+b1+b2-c,m])/(m! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2-c,m])+((1-y)^m Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b1,a+m},{1+a+b1+b2-c+m},1-x] Pochhammer[a,m] Pochhammer[b2,m])/(m! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((1-x)^-b1 (1-y)^(-a-b2+c+m) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b1,-a+c+m},{1-a-b2+c+m},(-1+y)/(-1+x)] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])},{Abs[1-y]<1&&Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((1-x)^m (1-y)^(-a-b1-b2+c-m) Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{-b1-b2+c,-a-b1+c-m},{1-a-b1-b2+c-m},1-y] Pochhammer[b1,m] Pochhammer[a+b1+b2-c,m])/(m! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,m])+((1-x)^m Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b2,a+m},{1+a+b1+b2-c+m},1-y] Pochhammer[a,m] Pochhammer[b1,m])/(m! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((1-x)^(-a-b1+c+m) (1-y)^-b2 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b2,-a+c+m},{1-a-b1+c+m},(-1+x)/(-1+y)] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[1-x]>1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^(-a-b1-b2-m) (1/(-1+x))^(-a-b2) Gamma[a-b1] Gamma[c] ((1-x)^(a+b1+b2) (1/(-1+x))^(a+b1+b2) Gamma[1-b1] Gamma[b1] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-x)^c (1/(-1+x))^c Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b2,a-b1-m},{1+a+b2-c-m},1-y] Pochhammer[b1,m] Pochhammer[-a-b2+c,m])/(m! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[1+a+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m])+((1-x)^-b1 (1-y)^(-a-b2+c) ((-1+y)/(-1+x))^m Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{-b1-b2+c,-a+c+m},{1-a-b2+c+m},1-y] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((1-x)^(-a-b1-b2-m) (1/(-1+x))^(-b1-b2) Gamma[-a+b1] Gamma[c] ((1-x)^(a+b1+b2) (1/(-1+x))^(a+b1+b2) Gamma[1-a] Gamma[a] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-x)^c (1/(-1+x))^c Gamma[1+b1+b2-c] Gamma[a+b1+b2-c] Gamma[-b1-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b2,a+m},{1+a-b1+m},(-1+y)/(-1+x)] Pochhammer[a,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1,m])},{Abs[1-y]>1&&Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((1-y)^(-a-b1-b2-m) (1/(-1+y))^(-a-b1) Gamma[a-b2] Gamma[c] ((1-y)^(a+b1+b2) (1/(-1+y))^(a+b1+b2) Gamma[1-b2] Gamma[b2] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-y)^c (1/(-1+y))^c Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b1,a-b2-m},{1+a+b1-c-m},1-x] Pochhammer[b2,m] Pochhammer[-a-b1+c,m])/(m! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[1+a+b1-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,m])+((1-x)^(-a-b1+c) (1-y)^-b2 ((-1+x)/(-1+y))^m Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{-b1-b2+c,-a+c+m},{1-a-b1+c+m},1-x] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((1-y)^(-a-b1-b2-m) (1/(-1+y))^(-b1-b2) Gamma[-a+b2] Gamma[c] ((1-y)^(a+b1+b2) (1/(-1+y))^(a+b1+b2) Gamma[1-a] Gamma[a] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-y)^c (1/(-1+y))^c Gamma[1+b1+b2-c] Gamma[a+b1+b2-c] Gamma[-b1-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b1,a+m},{1+a-b2+m},(-1+x)/(-1+y)] Pochhammer[a,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b2,m])},{1/Abs[1-x]<1&&1/Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^(-a-b1-b2) (1/(-1+x))^(-a-b2) (1-y)^(-a-b2) (1/(-1+y))^(-b1-b2) ((-1+y)/(-1+x))^m Gamma[-a+b1+b2] Gamma[c] ((1-x)^(a+b2) (1/(-1+x))^(a+b2) Gamma[1-b1] Gamma[b1] ((1-y)^c (1/(-1+y))^c Gamma[a+b2-c] Gamma[1+b1+b2-c] Gamma[1-a-b2+c] Gamma[-b1-b2+c]+(1-x)^b1 (1/(-1+x))^b1 (1-y)^(a+b2) (1/(-1+y))^(a+b2) Gamma[a-b1] Gamma[1-a+b1] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c])+(1-x)^c (1/(-1+x))^c (1-y)^(a+b2) (1/(-1+y))^(a+b2) Gamma[a-b1] Gamma[1-a+b1] Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{-b1-b2+c,a-b1-m},{1+a-b1-b2-m},1/(1-y)] Pochhammer[b1,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[1-a+b1] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m])+((-1)^m (1-x)^(-a-b1-b2) (1/(-1+x))^(-a-b2) (1-y)^(-a-b2) (1/(-1+y))^-a (-1+y)^-m ((-1+y)/(-1+x))^m Gamma[a-b1-b2] Gamma[c] ((1-x)^(a+b2) (1/(-1+x))^(a+b2) Gamma[1-b1] Gamma[b1] ((1-y)^c (1/(-1+y))^c Gamma[1+a-c] Gamma[a+b2-c] Gamma[-a+c] Gamma[1-a-b2+c]+(1-x)^b1 (1/(-1+x))^b1 (1-y)^(a+b2) (1/(-1+y))^(a+b2) Gamma[1-b2] Gamma[b2] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c])+(1-x)^c (1/(-1+x))^c (1-y)^(a+b2) (1/(-1+y))^(a+b2) Gamma[1-b2] Gamma[b2] Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[-a-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b2,-a+c+m},{1-a+b1+b2+m},1/(1-y)] Pochhammer[b1,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])+((1-x)^(-a-b1-b2-m) (1/(-1+x))^(-b1-b2) Gamma[-a+b1] Gamma[c] ((1-x)^(a+b1+b2) (1/(-1+x))^(a+b1+b2) Gamma[1-a] Gamma[a] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-x)^c (1/(-1+x))^c Gamma[1+b1+b2-c] Gamma[a+b1+b2-c] Gamma[-b1-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b2,a+m},{1+a-b1+m},(-1+y)/(-1+x)] Pochhammer[a,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b1,m])},{1/Abs[1-y]<1&&1/Abs[1-x]<1&&Abs[(-1+x)/(-1+y)]<1,((1-x)^(-a-b1) (1/(-1+x))^(-b1-b2) (1-y)^(-a-b1-b2) (1/(-1+y))^(-a-b1) ((-1+x)/(-1+y))^m Gamma[-a+b1+b2] Gamma[c] ((1-y)^(a+b1) (1/(-1+y))^(a+b1) Gamma[1-b2] Gamma[b2] ((1-x)^c (1/(-1+x))^c Gamma[a+b1-c] Gamma[1+b1+b2-c] Gamma[1-a-b1+c] Gamma[-b1-b2+c]+(1-x)^(a+b1) (1/(-1+x))^(a+b1) (1-y)^b2 (1/(-1+y))^b2 Gamma[a-b2] Gamma[1-a+b2] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c])+(1-x)^(a+b1) (1/(-1+x))^(a+b1) (1-y)^c (1/(-1+y))^c Gamma[a-b2] Gamma[1-a+b2] Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{-b1-b2+c,a-b2-m},{1+a-b1-b2-m},1/(1-x)] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m])/(m! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1-a+b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,m])+((-1)^m (1-x)^(-a-b1) (1/(-1+x))^-a (-1+x)^-m (1-y)^(-a-b1-b2) (1/(-1+y))^(-a-b1) ((-1+x)/(-1+y))^m Gamma[a-b1-b2] Gamma[c] ((1-y)^(a+b1) (1/(-1+y))^(a+b1) Gamma[1-b2] Gamma[b2] ((1-x)^c (1/(-1+x))^c Gamma[1+a-c] Gamma[a+b1-c] Gamma[-a+c] Gamma[1-a-b1+c]+(1-x)^(a+b1) (1/(-1+x))^(a+b1) (1-y)^b2 (1/(-1+y))^b2 Gamma[1-b1] Gamma[b1] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c])+(1-x)^(a+b1) (1/(-1+x))^(a+b1) (1-y)^c (1/(-1+y))^c Gamma[1-b1] Gamma[b1] Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[-a-b1+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b1,-a+c+m},{1-a+b1+b2+m},1/(1-x)] Pochhammer[b2,m] Pochhammer[-a+c,m])/(m! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])+((1-y)^(-a-b1-b2-m) (1/(-1+y))^(-b1-b2) Gamma[-a+b2] Gamma[c] ((1-y)^(a+b1+b2) (1/(-1+y))^(a+b1+b2) Gamma[1-a] Gamma[a] Gamma[1+a+b1+b2-c] Gamma[-a-b1-b2+c]+(1-y)^c (1/(-1+y))^c Gamma[1+b1+b2-c] Gamma[a+b1+b2-c] Gamma[-b1-b2+c] Gamma[1-a-b1-b2+c]) HypergeometricPFQ[{b1,a+m},{1+a-b2+m},(-1+x)/(-1+y)] Pochhammer[a,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[1+b1+b2-c] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a-b2,m])}};


(* ::Section:: *)
(*End package*)


End[]


EndPackage[]
