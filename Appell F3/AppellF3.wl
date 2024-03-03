(* ::Package:: *)

BeginPackage["F3`"]


Print["AppellF3.wl v1.0\n","Authors : Souvik Bera & Tanay Pathak\n","Last modified : 03.03.2024"
];


F3::usage="The command gives the numerical value of the Appell F3.
 F3[a1, a2, b1, b2, c, x, y, precision, terms, verbose-> True]";
F3ROC::usage="The command gives the region of convergence (ROC) of the analytic continuation along with the given point
 F3ROC[{x,y}, analytic_continuation_number, {plot range}]";
F3findall::usage="The command finds all the analytic continuations that are valid for the given point
 F3findall[{x,y}]";
F3evaluate::usage="The command gives the value of F3 at a given point and Pochhammer parameters with a chosen analytic continuation
 F3evaluate[series_number, {a1, a2, b1, b2, c, x, y}, precision, terms]";
F3expose::usage="The command exposes the region of convergence (ROC) and the expression of the analytic continuation of F3[a1, a2, b1, b2, c, x, y, m, n]
 F3expose[series_number]";


Begin["`Private`"]


Off[General::infy,General::indet,General::munfl,General::stop ];
ParallelEvaluate[Off[General::infy,General::indet,General::munfl,General::stop ]];


F3expose[list_]:=Module[{i},
If[list>24,Print["Total number of ACs is 24"];Abort[];];
ClearAll[Global`x,Global`y,Global`a1,Global`a2,Global`b1,Global`b2,Global`c,Global`m,Global`m];
Return[{F3serieswsum[Global`a1,Global`a2,Global`b1,Global`b2,Global`c,Global`x,Global`y,Global`m,Global`n][[list,1]],
F3serieswsum[Global`a1,Global`a2,Global`b1,Global`b2,Global`c,Global`x,Global`y,Global`m,Global`n][[list,2]](*/.Gamma[x_. +1]-> x Gamma[x]*)/.Re[x_]-> x (*//Simplify//Expand*)}]];



F3findall[{x_?NumberQ, y_?NumberQ}]:=Module[{test,pos,a1,a2,b1,b2,c,m},

test=F3series[a1,a2,b1,b2,c,Rationalize[x],Rationalize[y],m][[All,1]];
pos=Position[test,True];
Return[Flatten[pos]];


]


F3evaluate[list_/;(IntegerQ[list]&&list>0), para_List, p_,terms_]:=Module[
{a1,a2,b1,b2,c,x,y,result,p0,seriesac,m,eps,peps,m1},

$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];

p0= 10 p;
peps= 5p;

If[Length[para]=!=7,Abort[];];
{a1,a2,b1,b2,c,x,y}=SetPrecision[para,p0];


(* If x=0 or y=0 and other conditions*)

If[para[[-2]]===0.0||para[[-2]]===0,result=Hypergeometric2F1[para[[2]],para[[4]],para[[5]],para[[-1]]];Goto[end];];
If[para[[-1]]===0.0||para[[-1]]===0,result=Hypergeometric2F1[para[[1]],para[[3]],para[[5]],para[[-2]]];Goto[end];];


(*conditions for log cases*)

If[IntegerQ[para[[1]]],a1=para[[1]]-eps/2];
If[IntegerQ[para[[2]]],a2=para[[2]]-eps/3];
If[IntegerQ[para[[3]]],b1=para[[3]]-eps];
If[IntegerQ[para[[4]]],b2=para[[4]]+eps/5];
If[IntegerQ[para[[5]]],c=para[[5]]+eps/7];


If[IntegerQ[para[[1]]-para[[3]]],a1=para[[1]]-eps/2];
If[IntegerQ[para[[2]]-para[[4]]],a2=para[[2]]-eps/3];
If[IntegerQ[-para[[1]]+para[[5]]],a1=para[[1]]-eps/2];
If[IntegerQ[-para[[3]]+para[[5]]],b1=para[[3]]-eps];
If[IntegerQ[-para[[2]]+para[[5]]],a2=para[[2]]-eps/3];
If[IntegerQ[-para[[4]]+para[[5]]],b2=para[[4]]+eps/5];

If[IntegerQ[para[[1]] + para[[3]] - para[[5]]], a1=para[[1]]-eps/2];
If[IntegerQ[para[[2]] + para[[4]] - para[[5]]], b2=para[[4]]+eps/5]; 
If[IntegerQ[-para[[1]] - para[[2]] + para[[5]]], a1=para[[1]]-eps/2]; 
If[IntegerQ[-para[[2]] - para[[3]] + para[[5]]], c=para[[5]]+eps/7]; 
If[IntegerQ[-para[[1]] - para[[4]] + para[[5]]], a1=para[[1]]-eps/2]; 
If[IntegerQ[-para[[3]] - para[[4]] + para[[5]]], b2=para[[4]]+eps/5];

If[IntegerQ[para[[1]]+para[[2]]+para[[3]]-para[[5]]],c=para[[5]]+eps/7];
If[IntegerQ[para[[1]]+para[[3]]+para[[4]]-para[[5]]],c=para[[5]]+eps/7];
If[IntegerQ[para[[1]]+para[[2]]+para[[4]]-para[[5]]],c=para[[5]]+eps/7];
If[IntegerQ[para[[2]]+para[[3]]+para[[4]]-para[[5]]],c=para[[5]]+eps/7];


{a1,a2,b1,b2,c,x,y}=SetPrecision[{a1,a2,b1,b2,c,x,y},p0];
(*Print[{a1,a2,b1,b2,c,x,y}];*)
eps= SetPrecision[ 10^(-p) I,peps];

If[F3series[a1,a2,b1,b2,c,Rationalize[x],Rationalize[y],m][[list,1]],Nothing[];,
Print["The point does not lie in ", list];Abort[];];


seriesac[m1_]= F3series[a1,a2,b1,b2,c,x,y,m1][[list,2]];
(*Print[seriesac[m]];*)

DistributeDefinitions[terms,p0,seriesac,m1];
result = N[ParallelSum[seriesac[m1],{m1,0,terms}],p0];




Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]]


F3ROC[point___List,list_/;(IntegerQ[list]&&list>0),range_List]:=Module[{i,a,b,c,d,e,x,y,m,roc},
roc[x_,y_]=F3series[a,b,c,d,e,x,y,m][[list,1]];
Show[ListPlot[{point},PlotStyle->Directive[PointSize[Medium],Red],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}}],RegionPlot[roc[x,y],{x,range[[1]]-1,range[[2]]+1},{y,range[[1]]-1,range[[2]]+1},PlotPoints->100],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}},AspectRatio-> 1]
]


Options[F3]={verbose->False};
F3::poch="Pochhammer parameters do not obey the constraint";
F3::val="The point is not covered";
F3[args___] := f3core[F3,args];

f3core[F3,a0_,b0_,c0_,d0_,e0_,x0_,y0_,p_,terms_,OptionsPattern[F3]]:=Module[
{aa1,aa2,bb1,bb2,cc,xx,yy,a1,a2,b1,b2,c,x1,y1,x,y,m,n,m0,m1,m11,n0,n1,v,
i,test1,test2,pos1,pos2,result,p0,seriesac,seriesselect
,eps,peps,para,selectedseries,eachser,eachserlist,convrate1},

$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];
(*p0=p+10;*)
m1=100;
(*m1=terms;*)
p0= 10 p;
peps= 5 p;




(* If x=0 or y=0 and other conditions*)

If[x0===0.0||x0===0,result=Hypergeometric2F1[b0,d0,e0,y0];Goto[end];];
If[y0===0.0||y0===0,result=Hypergeometric2F1[a0,c0,e0,x0];Goto[end];];


a1=SetPrecision[a0,p0];
a2=SetPrecision[b0,p0]; b1=SetPrecision[c0,p0]; 
b2=SetPrecision[d0,p0]; c=SetPrecision[e0,p0];
x=SetPrecision[x0,p0]; y=SetPrecision[y0,p0];

(*conditions for log cases*)

If[IntegerQ[a0],a1=a0-eps/2];
If[IntegerQ[b0],a2=b0-eps/3];
If[IntegerQ[c0],b1=c0-eps];
If[IntegerQ[d0],b2=d0+eps/5];
If[IntegerQ[e0],c=e0+eps/7];

If[IntegerQ[a0-c0],a1=a0-eps/2];
If[IntegerQ[b0-d0],a2=b0-eps/3];
If[IntegerQ[-a0+e0],a1=a0-eps/2];
If[IntegerQ[-c0+e0],b1=c0-eps];
If[IntegerQ[-b0+e0],a2=b0-eps/3];
If[IntegerQ[-d0+e0],b2=d0+eps/5];


If[IntegerQ[a0+c0-e0],a1=a0-eps/2];
If[IntegerQ[b0+d0-e0],b2=d0+eps/5];
If[IntegerQ[-a0-b0+e0],a1=a0-eps/2];
If[IntegerQ[-b0-c0+e0],c=e0+eps/7];
If[IntegerQ[-a0-d0+e0],a1=a0-eps/2];
If[IntegerQ[-c0-d0+e0],b2=d0+eps/5];


If[IntegerQ[a0+b0+c0-e0],c=e0+eps/7];
If[IntegerQ[a0+c0+d0-e0],c=e0+eps/7];
If[IntegerQ[a0+b0+d0-e0],c=e0+eps/7];
If[IntegerQ[b0+c0+d0-e0],c=e0+eps/7];


para ={a1,a2,b1,b2,c,x,y};

eps= SetPrecision[ 10^(-p) I,peps];


result=0;

Off[General::infy,General::indet];
(* check the region *)
test1=Table[F3serieswsum[a1,a2,b1,b2,c,Rationalize[x0],Rationalize[y0],m,n][[i,1]],{i,1,24}];
(*Print[test1];*)

PrintTemporary["Finding a proper AC..."];

pos1=Flatten[Position[test1,True]];

If[pos1==={},Message[F3::val];Abort[];];

If[OptionValue[verbose],Print["valid series : ",pos1]];


(*Selecting the best series based on the convergence rate*)
test2=Table[eachser = F3series[aa1,aa2,bb1,bb2,cc,xx,yy,m][[pos1[[i]],2]];
eachserlist = If[Head[#]===Plus,List@@#,{#}]&@eachser;

convrate1 = Abs[N[((eachserlist/.m->m+ 1)/(eachserlist))/.{m-> m1}
/.MapThread[Rule,{{aa1,aa2,bb1,bb2,cc,xx,yy},SetPrecision[para,p0]}],p0]];
(*Print[convrate1];*)
{N[Max[convrate1],10],pos1[[i]]}
,{i,1,Length[pos1]}]; 
  







(*Print[test2];*)

seriesselect=SortBy[test2,First];
If[OptionValue[verbose]===True,Print["convergence rates :",{N[#[[1]],10],#[[2]]}&/@seriesselect],Nothing[]];

pos2 = seriesselect[[1,2]];




If[OptionValue[verbose],Print["selected series : ",pos2]];

PrintTemporary["Evaluating sum..."];



seriesac[m11_]=(F3series[a1,a2,b1,b2,c,x,y,m11][[pos2,2]]);



DistributeDefinitions[seriesac,m11,p0,terms];



result=ParallelSum[seriesac[m11],{m11,0,terms}];






Label[end];
Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]

]



(* ::Subchapter:: *)
(*Set of 24 ACs*)


F3series[a1_,a2_,b1_,b2_,c_,x_,y_,m_]= {{Abs[x]<1&&Abs[y]<1,(y^m HypergeometricPFQ[{a1,b1},{c+m},x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Pochhammer[c,m])},{Abs[1-x]<1&&Abs[y]<1&&Abs[y]<-1+1/Abs[1-x]&&Abs[1-x]<1&&Abs[(-1+x) y]<1&&Abs[(-1+x) y]<1-Abs[1-x],(y^m Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+b1-c-m},1-x] Pochhammer[a2,m] Pochhammer[b2,m] Pochhammer[-a1-b1+c,m])/(m! Gamma[-a1+c] Gamma[-b1+c] Pochhammer[-a1+c,m] Pochhammer[-b1+c,m])+((-1)^m (1-x)^(-a1-b1+c+m) y^m Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])},{Abs[1-y]<1&&Abs[x]<1&&Abs[x]<-1+1/Abs[1-y]&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1-Abs[1-y],(x^m Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,b2},{1+a2+b2-c-m},1-y] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[-a2-b2+c,m])/(m! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m) Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2+c+m},{1-a2-b2+c+m},1-y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[b2] Pochhammer[1-a2-b2+c,m])},{1/Abs[x]<1&&Abs[y]<1&&Abs[y]<1/(1+1/Abs[x])&&1/Abs[x]<1&&Abs[y]<1&&Abs[y]<1/(1+1/Abs[x]),((-x)^-a1 x^-m Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a2,b2},{-a1+c-m},y] Pochhammer[a1,m] Pochhammer[1+a1-c,m])/(m! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{a2,b2},{-b1+c-m},y] Pochhammer[b1,m] Pochhammer[1+b1-c,m])/(m! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m])},{1/Abs[y]<1&&Abs[x]<1&&Abs[x]<1/(1+1/Abs[y])&&1/Abs[y]<1&&Abs[x]<1&&Abs[x]<1/(1+1/Abs[y]),((-1)^(2 m) x^m (-y)^-a2 Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,1+a2-c-m},{1+a2-b2},1/y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m])+((-1)^(2 m) x^m (-y)^-b2 Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,1+b2-c-m},{1-a2+b2},1/y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{1/Abs[x]<1&&1/Abs[y]<1&&1/Abs[y]<1-1/Abs[x],((-x)^-a1 x^-m (-y)^-a2 Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,1+a1+a2-c+m},{1+a2-b2},1/y] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m])/(m! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m (-y)^-a2 Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,1+a2+b1-c+m},{1+a2-b2},1/y] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m])+((-x)^-a1 x^-m (-y)^-b2 Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,1+a1+b2-c+m},{1-a2+b2},1/y] Pochhammer[a1,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m (-y)^-b2 Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2-c+m},{1-a2+b2},1/y] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m])},{Abs[1-x]<1&&1/Abs[y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y-x y]<1,((-y)^-a2 y^-m Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m},1-x] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m])+((-y)^-b2 y^-m Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2-c+m},1-x] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m] Pochhammer[1+a1+b1+b2-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) y^m Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-y)^(a1+b1-c) y^-m Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a1,b1,a1+a2+b1-c-m,a1+b1+b2-c-m},{a1-m,b1-m,1+a1+b1-c-m},(-1+x) y] Pochhammer[1-a1,m] Pochhammer[1-b1,m] Pochhammer[-a1-b1+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,m] Pochhammer[1-a1-b1-b2+c,m])},{Abs[1-y]<1&&1/Abs[x]<1&&1+1/Abs[x]<1/Abs[x-x y]&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1,((-x)^-a1 x^-m Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,b2},{1+a1+a2+b2-c+m},1-y] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m])+((-x)^-b1 x^-m Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,b2},{1+a2+b1+b2-c+m},1-y] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m) Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2+c+m},{1-a2-b2+c+m},1-y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[b2] Pochhammer[1-a2-b2+c,m])+((-x)^(a2+b2-c) x^-m Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,b2,a1+a2+b2-c-m,a2+b1+b2-c-m},{a2-m,b2-m,1+a2+b2-c-m},x (-1+y)] Pochhammer[1-a2,m] Pochhammer[1-b2,m] Pochhammer[-a2-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m] Pochhammer[1-a2-b1-b2+c,m])},{Abs[1-x]>1&&Abs[y]<1&&1/Abs[1-x]+Abs[y]<1,((1-x)^-a1 y^m Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a1,-b1+c+m},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a1+c] Pochhammer[-a1+c,m])+((1-x)^-b1 y^m Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{b1,-a1+c+m},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[a1] Gamma[-b1+c] Pochhammer[-b1+c,m])},{Abs[1-y]>1&&Abs[x]<1&&Abs[x]+1/Abs[1-y]<1,(x^m (1-y)^-a2 Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,-b2+c+m},{1+a2-b2},1/(1-y)] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m])+(x^m (1-y)^-b2 Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a2+c+m},{1-a2+b2},1/(1-y)] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x]),((1-x)^-a1 (-y)^-a2 y^-m Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-m},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m])/(m! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a2-b2,m])+((1-x)^-b1 (-y)^-a2 y^-m Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m])+((1-x)^-a1 (-y)^-b2 y^-m Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{a1,-b1-b2+c-m},{1+a1-b1},1/(1-x)] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1-a2+b2,m])+((1-x)^-b1 (-y)^-b2 y^-m Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b1,-a1-b2+c-m},{1-a1+b1},1/(1-x)] Pochhammer[b2,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m])},{1/Abs[-1+y]<1&&1/Abs[x]<1&&1/Abs[x]<1/(1+1/Abs[-1+y]),((-x)^-a1 x^-m (1-y)^-a2 Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,-a1-b2+c-m},{1+a2-b2},1/(1-y)] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m])/(m! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m (1-y)^-a2 Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,-b1-b2+c-m},{1+a2-b2},1/(1-y)] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m])+((-x)^-a1 x^-m (1-y)^-b2 Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a1-a2+c-m},{1-a2+b2},1/(1-y)] Pochhammer[a1,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m (1-y)^-b2 Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a2-b1+c-m},{1-a2+b2},1/(1-y)] Pochhammer[b1,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&1/Abs[y-x y]<1&&1/Abs[y-x y]<1/(1+Abs[-1+x])&&1/Abs[y]<1,((1-x)^(-a1-b1+c) (1/((-1+x) y))^m Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-a2+c-m,-a2-b1+c-m},{1-a1-a2-b1+c-m},1-x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "a2"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{"-", "a2"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[a1+a2+b1-c,m])/(m! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,m])+((-y)^-a2 y^-m Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m},1-x] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m])+((1-x)^(-a1-b1+c) (1/((-1+x) y))^m Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-b2+c-m,-b1-b2+c-m},{1-a1-b1-b2+c-m},1-x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "b2"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{"-", "b2"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[b2,m] Pochhammer[a1+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m])+((-y)^-b2 y^-m Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2-c+m},1-x] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m] Pochhammer[1+a1+b1+b2-c,m])+((-y)^(a1+b1-c) y^-m (y-x y)^m Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a2,1+a1+a2-c,1+a2+b1-c,a1+a2+b1-c-m},{1+a2-b2,1+a1+a2-c-m,1+a2+b1-c-m},1/((-1+x) y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b1", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b1", "+", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[-a1-a2+c,m] Pochhammer[-a2-b1+c,m])/(m! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,m])+((-y)^(a1+b1-c) y^-m (y-x y)^m Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,1+a1+b2-c,1+b1+b2-c,a1+b1+b2-c-m},{1-a2+b2,1+a1+b2-c-m,1+b1+b2-c-m},1/((-1+x) y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b2", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b2", "+", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[-a1-b2+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a1-b1-b2+c,m])},{Abs[1-y]<1&&Abs[-1+y]<1&&1+Abs[-1+y]<Abs[x-x y]&&1/Abs[x-x y]<1&&1/Abs[x-x y]<1/(1+Abs[-1+y])&&1/Abs[x]<1,((1-y)^(-a2-b2+c) (1/(x (-1+y)))^m Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{-a1-a2+c-m,-a1-b2+c-m},{1-a1-a2-b2+c-m},1-y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "a1"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[a1,m] Pochhammer[a1+a2+b2-c,m])/(m! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m])+((-x)^-a1 x^-m Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,b2},{1+a1+a2+b2-c+m},1-y] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m])+((1-y)^(-a2-b2+c) (1/(x (-1+y)))^m Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{-a2-b1+c-m,-b1-b2+c-m},{1-a2-b1-b2+c-m},1-y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "b1"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[b1,m] Pochhammer[a2+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m])+((-x)^-b1 x^-m Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,b2},{1+a2+b1+b2-c+m},1-y] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m])+((-x)^(a2+b2-c) x^-m (x-x y)^m Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a1,1+a1+a2-c,1+a1+b2-c,a1+a2+b2-c-m},{1+a1-b1,1+a1+a2-c-m,1+a1+b2-c-m},1/(x (-1+y))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b2", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b2", "+", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[-a1-a2+c,m] Pochhammer[-a1-b2+c,m])/(m! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m])+((-x)^(a2+b2-c) x^-m (x-x y)^m Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b1,1+a2+b1-c,1+b1+b2-c,a2+b1+b2-c-m},{1-a1+b1,1+a2+b1-c-m,1+b1+b2-c-m},1/(x (-1+y))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a2", "+", "b1", "+", "b2", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "y", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a2"}], "-", "b1", "-", "b2", "+", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[-a2-b1+c,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a2-b1-b2+c,m])},{Abs[x/(-1+x)]<1&&Abs[y]<1,((1-x)^-b1 y^m HypergeometricPFQ[{b1,-a1+c+m},{c+m},x/(-1+x)] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Pochhammer[c,m])},{Abs[y/(-1+y)]<1&&Abs[x]<1,(x^m (1-y)^-b2 HypergeometricPFQ[{b2,-a2+c+m},{c+m},y/(-1+y)] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Pochhammer[c,m])},{Abs[x/(-1+x)]<1&&1/Abs[y]<1&&Abs[x/(-1+x)]<1&&1/Abs[y]<1,((1-x)^-b1 (x/(-1+x))^m (-y)^-a2 Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2,1+a1+a2-c,1+a2-c-m},{1+a2-b2,1+a1+a2-c-m},1/y] Pochhammer[b1,m] Pochhammer[-a1-a2+c,m])/(m! Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m])+((1-x)^-b1 (x/(-1+x))^m (-y)^-b2 Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,1+a1+b2-c,1+b2-c-m},{1-a2+b2,1+a1+b2-c-m},1/y] Pochhammer[b1,m] Pochhammer[-a1-b2+c,m])/(m! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{Abs[y/(-1+y)]<1&&1/Abs[x]<1&&Abs[y/(-1+y)]<1&&1/Abs[x]<1,((-x)^-a1 x^-m (1-y)^-b2 Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{b2,-a1-a2+c-m},{-a1+c-m},y/(-1+y)] Pochhammer[a1,m] Pochhammer[1+a1-c,m])/(m! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m])+((-x)^-b1 x^-m (1-y)^-b2 Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{b2,-a2-b1+c-m},{-b1+c-m},y/(-1+y)] Pochhammer[b1,m] Pochhammer[1+b1-c,m])/(m! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1,((1/x)^a1 (-y)^-a2 y^-m Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m},{1+a1+a2+b1-c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m])+((1/x)^a1 (-y)^-b2 y^-m Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,1+a1+b2-c+m},{1+a1+b1+b2-c+m},(-1+x)/x] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m] Pochhammer[1+a1+b1+b2-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) (1/x)^(-a1+c) x^-m y^m Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1-b1+c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((1/x)^a1 (-y)^(a1+b1-c) y^-m Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a1,a1+a2+b1-c-m,a1+b1+b2-c-m},{a1-m,1+a1+b1-c-m},((-1+x) y)/x] Pochhammer[1-a1,m] Pochhammer[1-b1,m] Pochhammer[-a1-b1+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,m] Pochhammer[1-a1-b1-b2+c,m])},{Abs[(-1+y)/y]<1&&Abs[x]>1&&Abs[(x (-1+y))/y]<1,((-x)^-a1 x^-m (1/y)^a2 Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,1+a1+a2-c+m},{1+a1+a2+b2-c+m},(-1+y)/y] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m])+((-x)^-b1 x^-m (1/y)^a2 Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,1+a2+b1-c+m},{1+a2+b1+b2-c+m},(-1+y)/y] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m) (1/y)^(-a2+c) y^-m Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{1-a2,-a2+c+m},{1-a2-b2+c+m},(-1+y)/y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[b2] Pochhammer[1-a2-b2+c,m])+((-x)^(a2+b2-c) x^-m (1/y)^a2 Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,a1+a2+b2-c-m,a2+b1+b2-c-m},{a2-m,1+a2+b2-c-m},(x (-1+y))/y] Pochhammer[1-a2,m] Pochhammer[1-b2,m] Pochhammer[-a2-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m] Pochhammer[1-a2-b1-b2+c,m])},{Abs[x]>1&&Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1,(((-1+x)/x)^m (-x)^-a1 (x/(-1+x))^(a1+b1-c) y^m Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{1-b1,-b1+c+m},{1+a1-b1},1/x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[b1] Gamma[-a1+c] Pochhammer[-a1+c,m])+(((-1+x)/x)^m (-x)^-b1 (x/(-1+x))^(a1+b1-c) y^m Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1+b1},1/x] Pochhammer[a2,m] Pochhammer[b2,m])/(m! Gamma[a1] Gamma[-b1+c] Pochhammer[-b1+c,m])},{Abs[y]>1&&Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1,(x^m ((-1+y)/y)^m (-y)^-a2 (y/(-1+y))^(a2+b2-c) Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1-b2,-b2+c+m},{1+a2-b2},1/y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m])+(x^m ((-1+y)/y)^m (-y)^-b2 (y/(-1+y))^(a2+b2-c) Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{1-a2,-a2+c+m},{1-a2+b2},1/y] Pochhammer[a1,m] Pochhammer[b1,m])/(m! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[x/(y-x y)]<1,((-1+1/x)^(-a1+c) (1-x)^-b1 (x/((-1+x) y))^m Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-a2+c-m},{1-a1-a2-b1+c-m},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "y"}], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{
RowBox[{"-", 
RowBox[{"Re", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", "0"}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "a2"], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[a1+a2+b1-c,m])/(m! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,m])+((1/x)^a1 (-y)^-a2 y^-m Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m},{1+a1+a2+b1-c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m])/(m! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m])+((-1+1/x)^(-a1+c) (1-x)^-b1 (x/((-1+x) y))^m Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-b2+c-m},{1-a1-b1-b2+c-m},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "y"}], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{
RowBox[{"-", 
RowBox[{"Re", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", "0"}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "b2"], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[b2,m] Pochhammer[a1+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m])+((1/x)^a1 (-y)^-b2 y^-m Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,1+a1+b2-c+m},{1+a1+b1+b2-c+m},(-1+x)/x] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m] Pochhammer[1+a1+b1+b2-c,m])+((-1)^m (1/x)^a1 (-y)^(a1+b1-c) y^-m ((-1+1/x) y)^m Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a2,1+a2+b1-c,a1+a2+b1-c-m},{1+a2-b2,1+a2+b1-c-m},x/((-1+x) y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "y"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b1", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"-", 
RowBox[{"Re", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", "0"}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b1", "-", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[1-b1,m] Pochhammer[-a2-b1+c,m])/(m! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,m])+((-1)^m (1/x)^a1 (-y)^(a1+b1-c) y^-m ((-1+1/x) y)^m Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,1+b1+b2-c,a1+b1+b2-c-m},{1-a2+b2,1+b1+b2-c-m},x/((-1+x) y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "y"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b2", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"-", 
RowBox[{"Re", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", "0"}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"y", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b2", "-", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[1-b1,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a1-b1-b2+c,m])},{Abs[(-1+y)/y]<1&&Abs[x]>1&&Abs[y/(x-x y)]<1,((-1+1/y)^(-a2+c) (1-y)^-b2 (y/(x (-1+y)))^m Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{1-a2,-a1-a2+c-m},{1-a1-a2-b2+c-m},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "y"]}], ")"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], " ", "-", " ", 
RowBox[{"Re", "[", "y", "]"}], " ", "+", " ", 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "^", "2"}]}], ">", "0", " "}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "a1"], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[a1,m] Pochhammer[a1+a2+b2-c,m])/(m! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m])+((-x)^-a1 x^-m (1/y)^a2 Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,1+a1+a2-c+m},{1+a1+a2+b2-c+m},(-1+y)/y] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m])+((-1+1/y)^(-a2+c) (1-y)^-b2 (y/(x (-1+y)))^m Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{1-a2,-a2-b1+c-m},{1-a2-b1-b2+c-m},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "y"]}], ")"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], " ", "-", " ", 
RowBox[{"Re", "[", "y", "]"}], " ", "+", " ", 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "^", "2"}]}], ">", "0", " "}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], "b1"], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[b1,m] Pochhammer[a2+b1+b2-c,m])/(m! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m])+((-x)^-b1 x^-m (1/y)^a2 Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,1+a2+b1-c+m},{1+a2+b1+b2-c+m},(-1+y)/y] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m])+((-1)^m (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^a2 Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a1,1+a1+b2-c,a1+a2+b2-c-m},{1+a1-b1,1+a1+b2-c-m},y/(x (-1+y))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "y"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b2", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], " ", "-", " ", 
RowBox[{"Re", "[", "y", "]"}], " ", "+", " ", 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "^", "2"}]}], ">", "0", " "}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b2", "-", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[1-b2,m] Pochhammer[-a1-b2+c,m])/(m! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m])+((-1)^m (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^a2 Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b1,1+b1+b2-c,a2+b1+b2-c-m},{1-a1+b1,1+b1+b2-c-m},y/(x (-1+y))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "y"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a2"}], "-", "b1", "-", "b2", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], " ", "-", " ", 
RowBox[{"Re", "[", "y", "]"}], " ", "+", " ", 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "^", "2"}]}], ">", "0", " "}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "y"}]}]], ")"}], 
RowBox[{"a2", "+", "b1", "+", "b2", "-", "c"}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)) Pochhammer[1-b2,m] Pochhammer[-b1-b2+c,m])/(m! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a2-b1-b2+c,m])}};


F3serieswsum[a1_,a2_,b1_,b2_,c_,x_,y_,m_,n_]= {{Abs[x]<1&&Abs[y]<1,(x^m y^n Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Pochhammer[c,m+n])},{Abs[1-x]<1&&Abs[y]<1&&Abs[y]<-1+1/Abs[1-x]&&Abs[1-x]<1&&Abs[(-1+x) y]<1&&Abs[(-1+x) y]<1-Abs[1-x],((-1)^n (1-x)^m y^n Gamma[c] Gamma[-a1-b1+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a1+c] Gamma[-b1+c] Pochhammer[1+a1+b1-c,m-n] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])+((-1)^n (1-x)^(-a1-b1+c+m+n) y^n Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[-a1+c,m+n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n] Pochhammer[1-a1-b1+c,m+n])},{Abs[1-y]<1&&Abs[x]<1&&Abs[x]<-1+1/Abs[1-y]&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1-Abs[1-y],((-1)^m x^m (1-y)^n Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n])/(m! n! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[-a2+c,m+n] Pochhammer[-b2+c,m+n])/(m! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m] Pochhammer[1-a2-b2+c,m+n])},{1/Abs[x]<1&&Abs[y]<1&&Abs[y]<1/(1+1/Abs[x])&&1/Abs[x]<1&&Abs[y]<1&&Abs[y]<1/(1+1/Abs[x]),((-1)^n (-x)^-a1 x^-m y^n Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1-c,m-n])/(m! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m])+((-1)^n (-x)^-b1 x^-m y^n Gamma[a1-b1] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n])/(m! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m])},{1/Abs[y]<1&&Abs[x]<1&&Abs[x]<1/(1+1/Abs[y])&&1/Abs[y]<1&&Abs[x]<1&&Abs[x]<1/(1+1/Abs[y]),((-1)^m x^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2-c,-m+n])/(m! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n])+((-1)^m x^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n])/(m! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n])},{1/Abs[x]<1&&1/Abs[y]<1&&1/Abs[y]<1-1/Abs[x],((-x)^-a1 x^-m (-y)^-a2 y^-n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m+n])/(m! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^-m (-y)^-a2 y^-n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m+n])/(m! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n])+((-x)^-a1 x^-m (-y)^-b2 y^-n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,m+n])/(m! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n])+((-x)^-b1 x^-m (-y)^-b2 y^-n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n])},{Abs[1-x]<1&&1/Abs[y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y-x y]<1,((1-x)^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2+b1-c,m+n])+((1-x)^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b1+b2-c,m+n])+((-1)^n (1-x)^(-a1-b1+c+m+n) y^n Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[-a1+c,m+n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n] Pochhammer[1-a1-b1+c,m+n])+((-1)^m (1-x)^m (-y)^(a1+b1-c+m) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,-m+n] Pochhammer[b1,m] Pochhammer[-a1-b1+c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n])},{Abs[1-y]<1&&1/Abs[x]<1&&1+1/Abs[x]<1/Abs[x-x y]&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1,((-x)^-a1 x^-m (1-y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-x)^-b1 x^-m (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[-a2+c,m+n] Pochhammer[-b2+c,m+n])/(m! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^n (-x)^(a2+b2-c+n) x^-m (1-y)^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[1-b2,m-n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n])},{Abs[1-x]>1&&Abs[y]<1&&1/Abs[1-x]+Abs[y]<1,((1-x)^(-b1-m) y^n Gamma[a1-b1] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1+c,m+n])/(m! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])+((1-x)^(-a1-m) y^n Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])},{Abs[1-y]>1&&Abs[x]<1&&Abs[x]+1/Abs[1-y]<1,(x^m (1-y)^(-b2-n) Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n])/(m! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])+(x^m (1-y)^(-a2-n) Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[-b2+c,m+n])/(m! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])},{1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x]),((-1)^m (1-x)^(-b1-m) (-y)^-a2 y^-n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2-c,-m+n])+((-1)^m (1-x)^(-a1-m) (-y)^-a2 y^-n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n] Pochhammer[1+a2+b1-c,-m+n])+((-1)^m (1-x)^(-b1-m) (-y)^-b2 y^-n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b2-c,-m+n])+((-1)^m (1-x)^(-a1-m) (-y)^-b2 y^-n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n] Pochhammer[1+b1+b2-c,-m+n])},{1/Abs[-1+y]<1&&1/Abs[x]<1&&1/Abs[x]<1/(1+1/Abs[-1+y]),((-1)^n (-x)^-a1 x^-m (1-y)^(-b2-n) Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+a2-c,m-n])+((-1)^n (-x)^-a1 x^-m (1-y)^(-a2-n) Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+b2-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-b2-n) Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n] Pochhammer[1+a2+b1-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a2-n) Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n] Pochhammer[1+b1+b2-c,m-n])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&1/Abs[y-x y]<1&&1/Abs[y-x y]<1/(1+Abs[-1+x])&&1/Abs[y]<1,((-1)^n (1/((-1+x) y))^m (-y)^(a1+b1-c) y^-n (y-x y)^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-a2-b1+c),(1/(y-x y))^(a1+a2+b1-c)] Pochhammer[a2,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])+((-1)^m (1-x)^(-a1-b1+c+m) (1/((-1+x) y))^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-a2,(1/(y-x y))^a2] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a2+b1-c,-m+n])+((1-x)^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/((-1+x) y))^m (-y)^(a1+b1-c) y^-n (y-x y)^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-b1-b2+c),(1/(y-x y))^(a1+b1+b2-c)] Pochhammer[b2,m] Pochhammer[1+a1+b2-c,m] Pochhammer[1+b1+b2-c,m] Pochhammer[a1+b1+b2-c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m] Pochhammer[1+a1+b2-c,m-n] Pochhammer[1+b1+b2-c,m-n])+((-1)^m (1-x)^(-a1-b1+c+m) (1/((-1+x) y))^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-b2,(1/(y-x y))^b2] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[1+b1+b2-c,n] Pochhammer[a1+b1+b2-c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b2-c,-m+n] Pochhammer[1+b1+b2-c,-m+n])+((1-x)^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b1+b2-c,m+n])},{Abs[1-y]<1&&Abs[-1+y]<1&&1+Abs[-1+y]<Abs[x-x y]&&1/Abs[x-x y]<1&&1/Abs[x-x y]<1/(1+Abs[-1+y])&&1/Abs[x]<1,((-1)^n (1-y)^(-a2-b2+c+n) (1/(x (-1+y)))^m Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^-a1,(1/(x-x y))^a1] Pochhammer[a1,m] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m] Pochhammer[a1+a2+b2-c,m-n])/(m! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m-n])+((-1)^m (-x)^(a2+b2-c) x^-m (1/(x (-1+y)))^n (x-x y)^m Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^(-a1-a2-b2+c),(1/(x-x y))^(a1+a2+b2-c)] Pochhammer[a1,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a1+b2-c,n] Pochhammer[a1+a2+b2-c,-m+n])/(m! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a1+b2-c,-m+n])+((-x)^-a1 x^-m (1-y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-1)^n (1-y)^(-a2-b2+c+n) (1/(x (-1+y)))^m Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^-b1,(1/(x-x y))^b1] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m] Pochhammer[a2+b1+b2-c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m-n])+((-1)^m (-x)^(a2+b2-c) x^-m (1/(x (-1+y)))^n (x-x y)^m Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^(-a2-b1-b2+c),(1/(x-x y))^(a2+b1+b2-c)] Pochhammer[b1,n] Pochhammer[1+a2+b1-c,n] Pochhammer[1+b1+b2-c,n] Pochhammer[a2+b1+b2-c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+n] Pochhammer[1+b1+b2-c,-m+n])+((-x)^-b1 x^-m (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])},{Abs[x/(-1+x)]<1&&Abs[y]<1,((1-x)^-b1 (x/(-1+x))^m y^n Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1+c,m+n])/(m! n! Pochhammer[c,m+n] Pochhammer[-a1+c,n])},{Abs[y/(-1+y)]<1&&Abs[x]<1,(x^m (1-y)^-b2 (y/(-1+y))^n Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n])/(m! n! Pochhammer[c,m+n] Pochhammer[-a2+c,m])},{Abs[x/(-1+x)]<1&&1/Abs[y]<1&&Abs[x/(-1+x)]<1&&1/Abs[y]<1,((1-x)^-b1 (x/(-1+x))^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2-c,-m+n] Pochhammer[1+a1+a2-c,n])/(m! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2-c,-m+n])+((1-x)^-b1 (x/(-1+x))^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n] Pochhammer[1+a1+b2-c,n])/(m! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b2-c,-m+n])},{Abs[y/(-1+y)]<1&&1/Abs[x]<1&&Abs[y/(-1+y)]<1&&1/Abs[x]<1,((-x)^-a1 x^-m (1-y)^-b2 (y/(-1+y))^n Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1-c,m-n] Pochhammer[1+a1+a2-c,m])/(m! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-n])+((-x)^-b1 x^-m (1-y)^-b2 (y/(-1+y))^n Gamma[a1-b1] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n] Pochhammer[1+a2+b1-c,m])/(m! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-n])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1,((1/x)^a1 ((-1+x)/x)^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2+b1-c,m+n])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,m+n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b1+b2-c,m+n])+((-1)^n (1-x)^(-a1-b1+c+n) (1/x)^(-a1+c) ((-1+x)/x)^m x^-n y^n Gamma[a1+b1-c] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[-a1+c,m+n])/(m! n! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n] Pochhammer[1-a1-b1+c,m+n])+((-1)^m (1/x)^a1 ((-1+x)/x)^m (-y)^(a1+b1-c+m) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,n] Pochhammer[-a1-b1+c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n])},{Abs[(-1+y)/y]<1&&Abs[x]>1&&Abs[(x (-1+y))/y]<1,((-x)^-a1 x^-m (1/y)^a2 ((-1+y)/y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-x)^-b1 x^-m (1/y)^a2 ((-1+y)/y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m+n] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^m x^m (1-y)^(-a2-b2+c+m) (1/y)^(-a2+c) ((-1+y)/y)^n y^-m Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n] Pochhammer[b1,m] Pochhammer[-a2+c,m+n])/(m! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^n (-x)^(a2+b2-c+n) x^-m (1/y)^a2 ((-1+y)/y)^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[1-b2,m] Pochhammer[-a2-b2+c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n])},{Abs[x]>1&&Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1,(((-1+x)/x)^n (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) y^n Gamma[a1-b1] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[-a1+c,m+n])/(m! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])+(((-1+x)/x)^n (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) y^n Gamma[-a1+b1] Gamma[c] Pochhammer[a2,n] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[-b1+c,m+n])/(m! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])},{Abs[y]>1&&Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1,(x^m ((-1+y)/y)^m (-y)^-b2 y^-n (y/(-1+y))^(a2+b2-c) Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n] Pochhammer[b1,m] Pochhammer[-a2+c,m+n])/(m! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])+(x^m ((-1+y)/y)^m (-y)^-a2 y^-n (y/(-1+y))^(a2+b2-c) Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[-b2+c,m+n])/(m! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[x/(y-x y)]<1,((-1)^n (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c) y^-n ((-1+1/x) y)^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-a2-b1+c),(1/((-1+1/x) y))^(a1+a2+b1-c)] Pochhammer[a2,m] Pochhammer[1-b1,n] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m] Pochhammer[1+a2+b1-c,m-n])+((1-x)^(-a1-b1+c) (1/x)^(-a1+c) ((-1+x)/x)^m (x/((-1+x) y))^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-a2,(1/((-1+1/x) y))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2-c,-m+n])+((1/x)^a1 ((-1+x)/x)^m (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c) y^-n ((-1+1/x) y)^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-b1-b2+c),(1/((-1+1/x) y))^(a1+b1+b2-c)] Pochhammer[1-b1,n] Pochhammer[b2,m] Pochhammer[1+b1+b2-c,m] Pochhammer[a1+b1+b2-c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m] Pochhammer[1+b1+b2-c,m-n])+((1-x)^(-a1-b1+c) (1/x)^(-a1+c) ((-1+x)/x)^m (x/((-1+x) y))^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-b2,(1/((-1+1/x) y))^b2] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,n] Pochhammer[a1+b1+b2-c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b2-c,-m+n])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,m+n] Pochhammer[1+b1+b2-c,n])/(m! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+b1+b2-c,m+n])},{Abs[(-1+y)/y]<1&&Abs[x]>1&&Abs[y/(x-x y)]<1,((1-y)^(-a2-b2+c) (1/y)^(-a2+c) ((-1+y)/y)^n (y/(x (-1+y)))^m Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-a1,(1/(x (-1+1/y)))^a1] Pochhammer[a1,m] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[a1+a2+b2-c,m-n])/(m! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-n])+((-1)^m (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^a2 (y/(x (-1+y)))^n Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a1-a2-b2+c),(1/(x (-1+1/y)))^(a1+a2+b2-c)] Pochhammer[a1,n] Pochhammer[1-b2,m] Pochhammer[1+a1+b2-c,n] Pochhammer[a1+a2+b2-c,-m+n])/(m! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+b2-c,-m+n])+((-x)^-a1 x^-m (1/y)^a2 ((-1+y)/y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a1+b2-c,m])/(m! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((1-y)^(-a2-b2+c) (1/y)^(-a2+c) ((-1+y)/y)^n (y/(x (-1+y)))^m Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-b1,(1/(x (-1+1/y)))^b1] Pochhammer[1-a2,n] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a2+b1+b2-c,m-n])/(m! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-n])+((-1)^m (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^a2 (y/(x (-1+y)))^n Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a2-b1-b2+c),(1/(x (-1+1/y)))^(a2+b1+b2-c)] Pochhammer[b1,n] Pochhammer[1-b2,m] Pochhammer[1+b1+b2-c,n] Pochhammer[a2+b1+b2-c,-m+n])/(m! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+b1+b2-c,-m+n])+((-x)^-b1 x^-m (1/y)^a2 ((-1+y)/y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1+a2+b1-c,m+n] Pochhammer[1+b1+b2-c,m])/(m! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])}};


End[]


EndPackage[]
