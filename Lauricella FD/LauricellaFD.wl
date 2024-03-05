(* ::Package:: *)

BeginPackage["LauricellaFD`"]


Print["LauricellaFD.wl v1.0\n","Authors : Souvik Bera & Tanay Pathak\n","Last modified : 03.03.2024"];


FD3::usage="The command gives the numerical value of the Lauricella FD in three variables.
FD3[a, b1, b2, b3, c, x, y, z, precision, terms, verbose-> True]";
FD3findall::usage="The command finds all the analytic continuations that are valid for the given point
 FD3findall[{x,y,z}]";
FD3evaluate::usage="The command gives the value of FD at a given point and Pochhammer parameters with a chosen analytic continuation
 FD3evaluate[series_number,{a, b1, b2, b3, c, x, y, z}, precision, terms]";
FD3expose::usage="The command exposes the domain of convergence and the expression of the analytic continuation of FD[a, b1, b2, b3, c, x, y, z, m, n, p]
 FD3expose[series_number]";


Begin["`Private`"]


Get["AppellF1.wl"];


Off[General::infy,General::indet,General::munfl,General::stop ];
ParallelEvaluate[Off[General::infy,General::indet,General::munfl,General::stop ]];


(*it exposes the three variable series*)
FD3expose[list_]:=Module[{i},
If[list>96,Print["Total number of ACs is 96"];Abort[];];
ClearAll[Global`a,Global`b1,Global`b2,Global`b3, Global`c, Global`x, Global`y, Global`z,Global`m,Global`n,Global`p];
Return[FD3sum[{Global`a,Global`b1,Global`b2,Global`b3, Global`c, Global`x, Global`y, Global`z},Global`m,Global`n,Global`p][[list]]];
];



FD3findall[{x0_?NumberQ, y0_?NumberQ, z0_?NumberQ}]:=Module[{roc,test,pos,a,b1, b2,b3, c, x, y, z,m,n},
roc[{x_,y_,z_}]=FD2sum[{a,b1, b2,b3, c, x, y,z},m,n][[All,1]];
test = roc[Rationalize/@{x0,y0,z0}];
(*test=Table[Subscript[Roc, i][Rationalize[x0],Rationalize[y0]],{i,1,44}];*)
pos=Position[test,True];
Return[Flatten[pos]];


];



FD3evaluate::singular="FD3 is singular";
FD3evaluate[list_/;(IntegerQ[list]&&list>0), para_List, p_,terms_]:=Module[
{a,b1, b2,b3, c, x, y, z,m,n,result,p0,roc,acs,peps,eps,
a0,b10,b20,b30,c0,x0,y0,z0,selectedseries},
$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];


p0= 10 p;
peps= 5p;


If[Or@@((IntegerQ[#]&&#<=0)&/@(para[[-4;;-4]])),Message[FD3evaluate::singular];Abort[];];


{a0,b10,b20,b30,c0,x0,y0,z0} = para;
{ a,b1, b2,b3, c, x, y,z} = para;
result=0;



(*(* When one argument is zero *)

If[x0===0.0||x0===0, result=F1[a0,b20,b30,c0,y0,z0,p,terms];Goto[end];];
If[y0===0.0||y0===0, result=F1[a0,b10,b30,c0,x0,z0,p,terms];Goto[end];];
If[z0===0.0||z0===0, result=F1[a0,b10,b20,c0,x0,y0,p,terms];Goto[end];];

If[x0===1||x0===1., result= Hypergeometric2F1[a0,b10,c0,1] F1[a0,b20,b30,c0-b10,y0,z0,p,terms];Goto[end];];
If[y0===1||y0===1., result= Hypergeometric2F1[a0,b20,c0,1] F1[a0,b10,b30,c0-b20,x0,z0,p,terms];Goto[end];];
If[z0===1||z0===1., result= Hypergeometric2F1[a0,b30,c0,1] F1[a0,b10,b20,c0-b30,x0,y0,p,terms];Goto[end];];

(* When two arguments are zero -- No need *)
If[And[x0===0.0||x0===0,y0===0.0||y0===0],result=Hypergeometric2F1[a0,b30,c0,z0];Goto[end];];
If[And[y0===0.0||y0===0,z0===0.0||z0===0],result=Hypergeometric2F1[a0,b10,c0,x0];Goto[end];];
If[And[z0===0.0||z0===0,x0===0.0||x0===0],result=Hypergeometric2F1[a0,b20,c0,y0];Goto[end];];

(* When all the arguments are zero -- No need*)
If[And[z0===0.0||z0===0,x0===0.0||x0===0,y0===0.0||y0===0],result=1;Goto[end];];

*)


If[IntegerQ[a],a=a-1/2 eps];
If[IntegerQ[b1],b1=b1+1/5 eps];
If[IntegerQ[b2],b2=b2-1/7 eps];
If[IntegerQ[b3],b3=b3-1/11 eps];
If[IntegerQ[c],c=c+1/13eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b2"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b3"}], "+", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b2"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b3"}], "]"}],
HoldForm]\),a=a-1/2 eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b2", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b2"}], "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b2", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b3", "-", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1", "-", "b2"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1", "-", "b3"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b2", "-", "b3"}], "]"}],
HoldForm]\),a=a-1/2 eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b2", "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "+", "b2", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b2", "+", "b3", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "+", "b3", "-", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[IntegerQ[a-b1-b2-b3],a=a-1/2 eps];

If[IntegerQ[a+b1+b2+b3-c],a=a-1/2 eps];


eps= SetPrecision[ 10^(-p) I,peps];


roc[{x_,y_,z_}]=FD2sum[{a,b1, b2,b3, c, x, y,z},m,n][[All,1]];




If[Length[para]=!=8,Abort[];];



If[roc[Rationalize/@(para[[-3;;-1]])][[list]],{},Print["The point does not lie in ", list];Abort[];];


selectedseries[m_,n_]=FD2sum[SetPrecision[{a,b1, b2,b3, c, x, y,z},p0],m,n][[list,2]]; 

DistributeDefinitions[terms,p,p0,selectedseries,m,n,terms];
result = N[ParallelSum[selectedseries[m,n],{m,0,terms},{n,0,m}],p0];


Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]]


Options[FD3]={verbose->False};
FD3::poch="Pochhammer parameters do not obey the constraint";
FD3::val="The point is not covered";
FD3::singular="LauricellaFS is singular";
FD3[args___] := FDcore[FD3,args];

FDcore[FD3,a0_,b10_, b20_,b30_, c0_,x0_,y0_,z0_,p_,terms_,OptionsPattern[FD3]]:=Module[
{selectedseries,eachser,eachserlist,convrate1,convrate2,parameters, roc,
a,b1, b2,b3, c, x, y,z,m,n,acs, m0,m1,n0,
n1,v,i,test1,test2,pos1,pos2,result,p0,integrand2,integrand3,seriesselect,pos12,mm,nn,
peps,eps,aa,bb1, bb2,bb3, cc, xx, yy,zz},

m1=100;n1=100;

$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];

p0= 10 p;
peps= 5p;



If[Or@@((IntegerQ[#]&&#<=0)&/@{c0}),Message[FD3::singular];Abort[];];

parameters = SetPrecision[{a0,b10, b20,b30, c0, x0, y0,z0},p0];
{a,b1,b2,b3,c,x,y,z}={a0,b10, b20,b30, c0, x0, y0,z0};
result=0;



(* When one argument is zero *)

If[x0===0.0||x0===0, result=F1[a0,b20,b30,c0,y0,z0,p,terms];Goto[end];];
If[y0===0.0||y0===0, result=F1[a0,b10,b30,c0,x0,z0,p,terms];Goto[end];];
If[z0===0.0||z0===0, result=F1[a0,b10,b20,c0,x0,y0,p,terms];Goto[end];];

(*when arguments are 1*)
If[x0===1||x0===1., If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result= Hypergeometric2F1[a0,b10,c0,1] F1[a0,b20,b30,c0-b10,y0,z0,p,terms];Goto[end];];

If[y0===1||y0===1., If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result= Hypergeometric2F1[a0,b20,c0,1] F1[a0,b10,b30,c0-b20,x0,z0,p,terms];Goto[end];];

If[z0===1||z0===1., If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result= Hypergeometric2F1[a0,b30,c0,1] F1[a0,b10,b20,c0-b30,x0,y0,p,terms];Goto[end];];

(* When two arguments are *)
If[And[x0===0.0||x0===0,y0===0.0||y0===0],result=Hypergeometric2F1[a0,b30,c0,z0];Goto[end];];
If[And[y0===0.0||y0===0,z0===0.0||z0===0],result=Hypergeometric2F1[a0,b10,c0,x0];Goto[end];];
If[And[z0===0.0||z0===0,x0===0.0||x0===0],result=Hypergeometric2F1[a0,b20,c0,y0];Goto[end];];

(* When all the arguments are zero*)
If[And[z0===0.0||z0===0,x0===0.0||x0===0,y0===0.0||y0===0],result=1;Goto[end];];


(*Reduction formula when x = y = z*)
If[x0===y0, If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = F1[a0,b10+b20,b30,c0,y0,z0,p,terms,If[OptionValue[verbose],verbose-> True,Nothing[]]];Goto[end];];

If[x0===z0, If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = F1[a0,b10+b30,b20,c0,x0,y0,p,terms,If[OptionValue[verbose],verbose-> True,Nothing[]]];Goto[end];];

If[y0===z0, If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = F1[a0,b10,b20+b30,c0,x0,y0,p,terms,If[OptionValue[verbose],verbose-> True,Nothing[]]];Goto[end];];




(*condition for log situation*)

If[IntegerQ[a],a=a-1/2 eps];
If[IntegerQ[b1],b1=b1+1/5 eps];
If[IntegerQ[b2],b2=b2-1/7 eps];
If[IntegerQ[b3],b3=b3-1/11 eps];
If[IntegerQ[c],c=c+1/13eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b2"}], "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b3"}], "+", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b2"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b3"}], "]"}],
HoldForm]\),a=a-1/2 eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b2", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b2"}], "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b2", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b3", "-", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1", "-", "b2"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b1", "-", "b3"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "-", "b2", "-", "b3"}], "]"}],
HoldForm]\),a=a-1/2 eps];

If[\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{
RowBox[{"-", "b1"}], "-", "b2", "-", "b3", "+", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "+", "b2", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b2", "+", "b3", "-", "c"}], "]"}],
HoldForm]\)||\!\(\*
TagBox[
RowBox[{"IntegerQ", "[", 
RowBox[{"a", "+", "b1", "+", "b3", "-", "c"}], "]"}],
HoldForm]\),c=c+1/13eps];
If[IntegerQ[a-b1-b2-b3],a=a-1/2 eps];

If[IntegerQ[a+b1+b2+b3-c],a=a-1/2 eps];

(*Print[{a,b1, b2,b3, c, x, y,z}];*)

eps= SetPrecision[ 10^(-p) I,peps];
parameters = SetPrecision[{a,b1, b2,b3, c, x, y,z},p0];

(* check the region *)


test1 = FD2sum[{a,b1, b2,b3, c, x, y,z},m,n][[All,1]];


PrintTemporary["Finding a proper AC..."];

pos1=Flatten[Position[test1,True]];

If[pos1==={},Message[FD3::val];Abort[];];

(*Print[pos1];*)



If[OptionValue[verbose],Print["valid series : ",pos1]];
(*Abort[];*)



test2=Table[eachser = FD2sum[{aa,bb1, bb2,bb3, cc, xx, yy,zz},m,n][[pos1[[i]],2]];
eachserlist = If[Head[#]===Plus,List@@#,{#}]&@eachser;
eachserlist=Flatten[If[Head[Distribute[#]]===Plus,List@@Distribute[#],{#}]&/@eachserlist];(*<<<---- modification only for FD*)
convrate1 = Abs[N[((eachserlist/.m-> n/.n->n+ 1)/(eachserlist/.m-> n))
/.Pochhammer[a_,m_+q_Integer]/;q>=1:>Product[a+m +i,{i,0,q-1}]Pochhammer[a,m]/.{n-> n1} (*<<<---- modification only for FD*)
/.MapThread[Rule,{{aa,bb1, bb2,bb3, cc, xx, yy,zz}, parameters}],p0]];
convrate2 = Abs[N[((eachserlist/.n-> 0/.m->m+ 1)/(eachserlist/.n-> 0))
/.Pochhammer[a_,m_+q_Integer]/;q>=1:>Product[a+m +i,{i,0,q-1}]Pochhammer[a,m]/.{m-> m1}(*<<<---- modification only for FD*)
/.MapThread[Rule,{{aa,bb1, bb2,bb3, cc, xx, yy,zz},parameters}],p0]];
{Max[Norm[{convrate1,convrate2}]],pos1[[i]]}
,{i,1,Length[pos1]}];



(*Print[test2];Abort[];*)

seriesselect=SortBy[test2,First];
If[OptionValue[verbose]===True,Print["convergence rates :",{N[#[[1]],10],#[[2]]}&/@seriesselect]];
pos2=seriesselect[[1,2]];





If[OptionValue[verbose]===True,
Print["selected series : ",pos2]];

PrintTemporary["Evaluating sum..."];



selectedseries[m_,n_]=FD2sum[SetPrecision[{a,b1, b2,b3, c, x, y,z},p0],m,n][[pos2,2]]; 
DistributeDefinitions[terms,p,selectedseries,m,n,p0];
result = N[ParallelSum[selectedseries[m,n],{m,0,terms},{n,0,m}],p0];



Label[end];
Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]
]





(* ::Section:: *)
(*Set of 96 ACs of Lauricella FD*)


FD2sum[{a_,b1_,b2_,b3_,c_,x_,y_,z_},m_,n_]={{Abs[x]<1&&Abs[y]<1&&Abs[z]<1,(x^(m-n) y^n HypergeometricPFQ[{b3,a+m},{c+m},z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Pochhammer[c,m])},{Abs[1-x]+Abs[y]<1&&Abs[1-x]+Abs[z]<1,((1-x)^(m-n) y^n Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,a+m},{-b1+c+n},z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m-n] Pochhammer[-b1+c,n])+((1-x)^(-a-b1+c+m-n) y^n Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-b1+c+m},{-b1+c+n},z] Pochhammer[b2,n] Pochhammer[-a+c,m-n] Pochhammer[-b1+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[-b1+c,n] Pochhammer[1-a-b1+c,m-n])},{Abs[y]<1&&Abs[z]<1&&Abs[x]>1,((-1)^n (-x)^(-a-n) x^(-m+n) y^n Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,a+m},{1+a-b1+m},z/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 x^(-m+n) y^n Gamma[a-b1] Gamma[c] HypergeometricPFQ[{b3,a-b1-m+2 n},{-b1+c-m+2 n},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m-2 n])},{Abs[(-1+x)/(-1+y)]<1&&Abs[1-x]+Abs[z]<1&&Abs[1-y]+Abs[z]<1,((1-x)^(m-n) (1-y)^n Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,a+m},{-b1-b2+c},z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((1-x)^(-a-b1+c+m-n) (1-y)^(-b2-m+2 n) Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z] Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[-a+c,m-n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-b2,-m+2 n] Pochhammer[1-a-b1+c,m-n])+((1-x)^(m-n) (1-y)^(-a-b1-b2+c-m+2 n) Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z] Pochhammer[b1,m-n] Pochhammer[-a-b1+c,-m+2 n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,-m+2 n])},{Abs[(-1+y)/(-1+x)]<1&&Abs[1-x]+Abs[z]<1&&Abs[1-y]+Abs[z]<1,((1-x)^(m-n) (1-y)^n Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,a+m},{-b1-b2+c},z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z] (((-1)^n (1-x)^(-a-b1+c) (1-y)^(-b2+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[a+b1+b2-c,m-2 n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1+a+b2-c,m-2 n])+((-1)^n (1-y)^(-a-b1-b2+c+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b2", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b2", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[a+b1+b2-c,m-2 n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b2-c,m-2 n])+((1-y)^(-a-b1-b2+c+n) ((-1+y)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,m])+((1-x)^(-a-b1+c) (1-y)^(-b2+n) ((-1+y)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a-b2+c,m]))},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+x)/(-1+y)]<1&&Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1,((1-x)^(m-n) (1-y)^n (1-z)^(-a-b1-b2-b3+c-m) Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{-b1-b2-b3+c,-a-b1-b2+c-m},{1-a-b1-b2-b3+c-m},1-z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m])+((1-x)^(m-n) (1-y)^n Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,a+m},{1+a+b1+b2+b3-c+m},1-z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1-z] (((1-x)^(-a-b1+c+m-n) (1-y)^(-b2-m+2 n) (1-z)^(-b3-n) Gamma[a+b1-c] Gamma[c] Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-b2,-m+2 n] Pochhammer[1-a-b1+c,m-n])+((1-x)^(m-n) (1-y)^(-a-b1-b2+c-m+2 n) (1-z)^(-b3-n) Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,-m+2 n]))},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1&&Abs[1-z]<1&&Abs[(-1+x)/(-1+z)]<1,((1-x)^(m-n) (1-y)^n (1-z)^(-a-b1-b2-b3+c-m) Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{-b1-b2-b3+c,-a-b1-b2+c-m},{1-a-b1-b2-b3+c-m},1-z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m])+((1-x)^(m-n) (1-y)^n Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,a+m},{1+a+b1+b2+b3-c+m},1-z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1-z] (((-1)^n (1-x)^(-a-b1+c) (1-y)^(-b2+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^(m-n) (1-z)^(-b3-n) Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1+a+b2-c,m-2 n])+((-1)^n (1-y)^(-a-b1-b2+c+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^(m-n) (1-z)^(-b3-n) Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b2", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b2", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b2-c,m-2 n])+((1-y)^(-a-b1-b2+c+n) ((-1+y)/(-1+x))^(m-n) (1-z)^(-b3-n) Gamma[1+a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,m])+((1-x)^(-a-b1+c) (1-y)^(-b2+n) ((-1+y)/(-1+x))^(m-n) (1-z)^(-b3-n) Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a-b2+c,m]))},{Abs[x]+Abs[1-y]<1&&Abs[1-y]+Abs[z]<1,(x^(m-n) (1-y)^n Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,a+m},{-b2+c+m-n},z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,n] Pochhammer[-b2+c,m-n])+(x^(m-n) (1-y)^(-a-b2+c+n) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-b2+c+m},{-b2+c+m-n},z] Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b2+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[-b2+c,m-n] Pochhammer[1-a-b2+c,n])},{Abs[x]>1+Abs[1-y]&&Abs[1-y]+Abs[z]<1,((-1)^(2 n) (-x)^(-a-n) x^(-m+n) (1-y)^n Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,a+m},{1+a-b1+m},z/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[1+a+b2-c,m])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^n Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,a-b1-m+2 n},{-b1-b2+c-m+n},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-2 n] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^(-a-b2+c+n) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c-m+2 n},{-b1-b2+c-m+n},z] Pochhammer[b1,m-n] Pochhammer[1+b1+b2-c,m-n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1+b1+b2-c,m-2 n] Pochhammer[1-a-b2+c,n])},{Abs[(-1+z)/(-1+y)]<1&&Abs[x]+Abs[1-z]<1&&Abs[x]+Abs[1-y]<1,(x^(m-n) (1-y)^(-b2-n) (1-z)^(-a-b3+c+n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-b2-b3+c+m-n,-n},{1-b2-n},1-y] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,n])+(x^(m-n) (1-y)^n Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,a+m},{1+a+b2+b3-c+n},1-z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2+b3-c,n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-a-b2-b3+c+n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b2+b3-c-n},{1+a+b3-c-n},(1-z)/(1-y)] Pochhammer[b1,m-n] Pochhammer[-a-b3+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[-b2-b3+c,m-n] Pochhammer[1-a-b2-b3+c,n])},{Abs[(-1+y)/(-1+z)]<1&&Abs[x]+Abs[1-z]<1&&Abs[x]+Abs[1-y]<1,HypergeometricPFQ[{b3,-a+c+n},{1-a-b2+c+n},(-1+y)/(-1+z)] ((x^(m-n) (1-y)^(-a-b2-b3+c+n) Gamma[a+b2-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-b2+n) (1-z)^(-a-b3+c) Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[1-a-b2+c,n] Pochhammer[-b2-b3+c,m-n]))+(x^(m-n) (1-y)^n Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,a+m},{1+a+b2+b3-c+n},1-z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2+b3-c,n] Pochhammer[-b2-b3+c,m-n])+HypergeometricPFQ[{b2,a+b2+b3-c-n},{1+a+b2-c-n},(-1+y)/(-1+z)] (((-1)^n x^(m-n) (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((1-z)/(-1+y))^n Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a-b2+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[-b2-b3+c,m-n] Pochhammer[1-a-b2-b3+c,n])+((-1)^n x^(m-n) (1-y)^(-a-b2-b3+c+n) ((1-z)/(-1+y))^n Gamma[1+a+b3-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b2", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "+", "b2", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a-b2+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[-b2-b3+c,m-n] Pochhammer[1-a-b2-b3+c,n]))},{Abs[1-y]<1&&Abs[x]>1+Abs[1-y]&&Abs[1-z]<1&&Abs[x]>1+Abs[1-z]&&Abs[x]>1+Abs[-1+y]&&Abs[-1+y]<1&&Abs[(-1+z)/(-1+y)]<1,((-x)^(-a-m) (1-y)^n (1-z)^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2+b3-c+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^n Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,a-b1-m+2 n},{1+a+b2+b3-c+n},1-z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m-2 n] Pochhammer[1+a+b2+b3-c,n])+((-x)^-b1 x^(-m+n) (1-y)^(-b2-n) (1-z)^(-a-b3+c+n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-n,-b1-b2-b3+c-m+n},{1-b2-n},1-y] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^(-a-b2-b3+c+n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b2+b3-c-n},{1+a+b3-c-n},(1-z)/(1-y)] Pochhammer[b1,m-n] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[-a-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,m-2 n] Pochhammer[1-a-b2-b3+c,n])},{Abs[1-y]<1&&Abs[x]>1+Abs[1-y]&&Abs[1-z]<1&&Abs[x]>1+Abs[1-z]&&Abs[x]>1+Abs[-1+y]&&Abs[x]>1+Abs[-1+z]&&Abs[(-1+y)/(-1+z)]<1&&Abs[-1+z]<1&&Abs[-1+y]<1,((-x)^(-a-m) (1-y)^n (1-z)^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2+b3-c+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 (1-y)^n (1-z)^(m-n) Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1-a+b1-m},1/x] Pochhammer[a-b1,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b2+b3-c,m])+HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+b1+b2+b3-c-n},1/x] (((-1)^-n (-x)^-b1 (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^(m-n) Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[a+b2+b3-c,m-2 n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[1+a+b2-c,m-2 n])+((-1)^-n (-x)^-b1 (1-y)^(-a-b2-b3+c+n) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^(m-n) Gamma[1+a+b3-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b2", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "+", "b2", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[a+b2+b3-c,m-2 n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,m-2 n])+((-x)^-b1 (1-y)^(-a-b2-b3+c+n) ((-1+y)/(-1+z))^(m-n) Gamma[a+b2-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b3,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,m])+((-x)^-b1 (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((-1+y)/(-1+z))^(m-n) Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b3,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[1-a-b2+c,m]))},{Abs[y]>1&&Abs[y/x]<1&&Abs[z]<1,((-1)^m (-x)^(-a-m) y^n z^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^(m-n) (-x)^-b1 (-y)^(-a+b1-m+n) y^-n z^(m-n) Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2-m},{1-a+b1-m},y/x] Pochhammer[a-b1,m] Pochhammer[b3,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,m])+((-x)^-b1 (-y)^-b2 y^-n z^(m-n) Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2-c-m+2 n},{1-a+b1+b2-m+2 n},1/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,-m+2 n])},{Abs[x]>1+Abs[1-z]&&Abs[y]+Abs[1-z]<1,((-1)^(2 (m-n)+n) (-x)^(-a-m) y^n (1-z)^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b3-c+m-n},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^(2 n) (-x)^-b1 y^n (1-z)^(m-n) Gamma[a-b1] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b1,1+b1+b3-c-n},{1-a+b1-m},1/x] Pochhammer[a-b1,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b3-c,m-n] Pochhammer[-b1-b3+c,n])+((-1)^(2 n) (-x)^-b1 y^n (1-z)^(-a-b3+c+m-n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{b1,1+b1+b3-c-n},{1+b1+b3-c-m},1/x] Pochhammer[b2,n] Pochhammer[-a+c,m-n] Pochhammer[-b1-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m-n] Pochhammer[-b1-b3+c,n])},{Abs[y/x]<1&&Abs[x]>1+Abs[1-z]&&Abs[-1+z]<1&&Abs[y]>1+Abs[-1+z],((-1)^(2 (m-n)+n) (-x)^(-a-m) y^n (1-z)^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b3-c+m-n},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 (-y)^(-a+b1-m+n) y^-n (1-z)^(m-n) Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2-m},{1-a+b1-m},y/x] Pochhammer[a-b1,m] Pochhammer[b3,m-n] Pochhammer[1+a+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,m] Pochhammer[1+a+b3-c,m-n])+((-1)^(m-n) (-x)^-b1 (-y)^-b2 y^-n (1-z)^(m-n) Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+n},{1-a+b1+b2-m+2 n},1/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,-m+2 n] Pochhammer[1+a+b3-c,m-n])+((-1)^(m-n) (-x)^-b1 (-y)^-b2 y^-n (1-z)^(-a-b3+c+m-n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},1/x] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b3+c,m-n])},{Abs[y/x]<1&&Abs[z]>1&&Abs[z/y]<1,((-1)^m (-x)^(-a-m) y^n z^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^n (-x)^-b1 (-y)^-b2 y^-n (-z)^(-a+b1+b2+n) z^(-m+n) Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2+b3-m+2 n},{1-a+b1+b2-m+2 n},z/x] Pochhammer[a-b1-b2,m-2 n] Pochhammer[b2,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,m-2 n])+((-1)^(m-n) (-x)^-b1 (-y)^(-a+b1-m+n) y^-n z^(m-n) Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2-m},{1-a+b1-m},y/x] Pochhammer[a-b1,m] Pochhammer[b3,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,m])+((-x)^-b1 (-y)^-b2 y^-n (-z)^-b3 z^(-m+n) Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[(-1+x)/(-1+z)]<1&&Abs[1-x]+Abs[y]<1&&Abs[y]+Abs[1-z]<1,((1-x)^(m-n) y^n (1-z)^(-a-b1-b3+c-m+n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{-b1-b3+c+n,-a-b1+c-m+n},{1-a-b1-b3+c-m+n},1-z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-n])+((1-x)^(-a-b1+c+m-n) y^n (1-z)^(-b3-m+n) Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{-b1-b3+c+n,-m+n},{1-b3-m+n},1-z] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m-n])+((1-x)^(m-n) y^n Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,a+m},{1+a+b1+b3-c+m-n},1-z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1+b3-c,m-n] Pochhammer[-b1-b3+c,n])},{Abs[(-1+z)/(-1+x)]<1&&Abs[1-x]+Abs[y]<1&&Abs[y]+Abs[1-z]<1,((1-x)^(m-n) (1-z)^n Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b2,a+m},{-b1-b3+c},y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1+b3-c,m])+HypergeometricPFQ[{b2,-b1-b3+c+n},{-b1-b3+c},y] (((-1)^n (1-x)^(-a-b1+c) (1-z)^(-b3+n) ((1-x)/(-1+z))^n ((-1+z)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b3,m-n] Pochhammer[a+b1+b3-c,m-2 n] Pochhammer[-b1-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1+a+b3-c,m-2 n])+((-1)^n (1-z)^(-a-b1-b3+c+n) ((1-x)/(-1+z))^n ((-1+z)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b3,m-n] Pochhammer[a+b1+b3-c,m-2 n] Pochhammer[-b1-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b3-c,m-2 n])+((1-z)^(-a-b1-b3+c+n) ((-1+z)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b3+c,m])+((1-x)^(-a-b1+c) (1-z)^(-b3+n) ((-1+z)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[-a+c,m] Pochhammer[-b1-b3+c,n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a-b3+c,m]))},{Abs[1-x]<1&&Abs[1-z]<1&&Abs[(-1+x)/(-1+z)]<1&&Abs[1-y]<1&&Abs[(-1+z)/(-1+y)]<1,((1-x)^(m-n) (1-y)^(-a-b1-b2-b3+c-m) (1-z)^n Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{-b1-b2-b3+c,-a-b1-b3+c-m},{1-a-b1-b2-b3+c-m},1-y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m])+((1-x)^(m-n) (1-z)^n Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b2,a+m},{1+a+b1+b2+b3-c+m},1-y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b2-n},1-y] (((1-x)^(-a-b1+c+m-n) (1-y)^(-b2-n) (1-z)^(-b3-m+2 n) Gamma[a+b1-c] Gamma[c] Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-b3,-m+2 n] Pochhammer[1-a-b1+c,m-n])+((1-x)^(m-n) (1-y)^(-b2-n) (1-z)^(-a-b1-b3+c-m+2 n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a-b1-b3+c,-m+2 n]))},{Abs[1-x]<1&&Abs[1-z]<1&&Abs[(-1+z)/(-1+x)]<1&&Abs[1-y]<1&&Abs[(-1+x)/(-1+y)]<1,((1-x)^(m-n) (1-y)^(-a-b1-b2-b3+c-m) (1-z)^n Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{-b1-b2-b3+c,-a-b1-b3+c-m},{1-a-b1-b2-b3+c-m},1-y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m])+((1-x)^(m-n) (1-z)^n Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b2,a+m},{1+a+b1+b2+b3-c+m},1-y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b2-n},1-y] (((-1)^n (1-x)^(-a-b1+c) (1-y)^(-b2-n) (1-z)^(-b3+n) ((1-x)/(-1+z))^n ((-1+z)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1+a+b3-c,m-2 n])+((-1)^n (1-y)^(-b2-n) (1-z)^(-a-b1-b3+c+n) ((1-x)/(-1+z))^n ((-1+z)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b3-c,m-2 n])+((1-y)^(-b2-n) (1-z)^(-a-b1-b3+c+n) ((-1+z)/(-1+x))^(m-n) Gamma[1+a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b3+c,m])+((1-x)^(-a-b1+c) (1-y)^(-b2-n) (1-z)^(-b3+n) ((-1+z)/(-1+x))^(m-n) Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a-b3+c,m]))},{Abs[x]+Abs[1-z]<1&&Abs[y]+Abs[1-z]<1,(x^(m-n) (1-z)^n Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b2,a+m},{-b3+c+m-n},y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[-a+c] Gamma[-b3+c] Pochhammer[1+a+b3-c,n] Pochhammer[-b3+c,m-n])+(x^(m-n) (1-z)^(-a-b3+c+n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{b2,-b3+c+m},{-b3+c+m-n},y] Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[-b3+c,m-n] Pochhammer[1-a-b3+c,n])},{Abs[z]>1&&Abs[z/x]<1&&Abs[y]<1,((-1)^n (-x)^(-a-n) x^(-m+n) z^n Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b2,a+m},{1+a-b1+m},y/x] Pochhammer[a,m] Pochhammer[b3,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^(m-n) (-x)^-b1 x^(-m+n) (-z)^(-a+b1+m-n) z^-n Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b2,a-b1-m+2 n},{1+a-b1-b3-m+2 n},y/z] Pochhammer[a-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b3,-m+2 n])+((-x)^-b1 x^(-m+n) (-z)^-b3 z^-n Gamma[a-b1-b3] Gamma[c] HypergeometricPFQ[{b2,a-b1-b3-m},{-b1-b3+c-m},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m])},{Abs[z/x]<1&&Abs[x]>1+Abs[1-y]&&Abs[-1+y]<1&&Abs[z]>1+Abs[-1+y],((-1)^(m+n) (-x)^(-a-m) (1-y)^n z^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2-c+n},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-x)^-b1 (1-y)^n (-z)^(-a+b1-n) z^(-m+n) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b3-m},{1-a+b1-m},z/x] Pochhammer[a-b1,m] Pochhammer[b2,n] Pochhammer[1+a+b2-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b3,m] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 (1-y)^n (-z)^-b3 z^(-m+n) Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+m-n},{1-a+b1+b3+m-2 n},1/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b3,m-2 n] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 (1-y)^(-a-b2+c+n) (-z)^-b3 z^(-m+n) Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+m-n},{1+b1+b2+b3-c+m-2 n},1/x] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1+b1+b2+b3-c,m-2 n] Pochhammer[1-a-b2+c,n])},{Abs[z/x]<1&&Abs[y]>1&&Abs[y/z]<1,((-1)^m (-x)^(-a-m) y^n z^(m-n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b1+m},1/x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^n (-x)^-b1 y^n (-z)^(-a+b1-n) z^(-m+n) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b3-m},{1-a+b1-m},z/x] Pochhammer[a-b1,m] Pochhammer[b2,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b3,m])+((-1)^(m-n) (-x)^-b1 (-y)^(-a+b1+b3+m-n) y^-n (-z)^-b3 z^(-m+n) Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2+b3+m-2 n},{1-a+b1+b3+m-2 n},y/x] Pochhammer[a-b1-b3,-m+2 n] Pochhammer[b3,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,-m+2 n])+((-x)^-b1 (-y)^-b2 y^-n (-z)^-b3 z^(-m+n) Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[x]<1&&Abs[z]<1&&Abs[y]>1,((-1)^(m-n) (-y)^(-a-m+n) y^-n z^(m-n) Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b1,a+m},{1+a-b2+m},x/y] Pochhammer[a,m] Pochhammer[b3,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-y)^-b2 y^-n z^(m-n) Gamma[a-b2] Gamma[c] HypergeometricPFQ[{b1,a-b2+m-2 n},{-b2+c+m-2 n},x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b2-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,-m+2 n])},{Abs[y]>1+Abs[1-x]&&Abs[1-x]+Abs[z]<1,((-1)^(2 (m-n)) (1-x)^(m-n) (-y)^(-a-m+n) y^-n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,a+m},{1+a-b2+m},z/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[1+a+b1-c,m])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 y^-n Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,a-b2+m-2 n},{-b1-b2+c-n},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,-m+2 n] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(-a-b1+c+m-n) (-y)^-b2 y^-n Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+m-2 n},{-b1-b2+c-n},z] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2-c,-m+2 n] Pochhammer[1-a-b1+c,m-n])},{Abs[1-x]<1&&Abs[y]>1+Abs[1-x]&&Abs[1-z]<1&&Abs[y]>1+Abs[1-z]&&Abs[y]>1+Abs[-1+x]&&Abs[-1+x]<1&&Abs[(-1+z)/(-1+x)]<1,((1-x)^(m-n) (-y)^(-a-m) (1-z)^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1+b3-c+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((1-x)^(-a-b1-b3+c-m+n) (-y)^-b2 y^-n (1-z)^(m-n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{-b1-b2-b3+c-n,-a-b3+c-m+n},{1-a-b1-b3+c-m+n},1-x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a+b1+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b3-c,m-n])+((-1)^(m-n) (-y)^-b2 y^-n (1-z)^(m-n) Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b1,a-b2+m-2 n},{1+a+b1+b3-c+m-n},1-x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,-m+2 n] Pochhammer[1+a+b1+b3-c,m-n])+((1-x)^(-b1-m+n) (-y)^-b2 y^-n (1-z)^(-a-b3+c+m-n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c-n,-m+n},{1-b1-m+n},1-x] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m-n])},{Abs[1-x]<1&&Abs[y]>1+Abs[1-x]&&Abs[1-z]<1&&Abs[y]>1+Abs[1-z]&&Abs[y]>1+Abs[-1+x]&&Abs[y]>1+Abs[-1+z]&&Abs[(-1+x)/(-1+z)]<1&&Abs[-1+z]<1&&Abs[-1+x]<1,((1-x)^(m-n) (-y)^(-a-m) (1-z)^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1+b3-c+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 y^-n Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,a-b2+m-2 n},{1+a+b1+b3-c+m-n},1-z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,-m+2 n] Pochhammer[1+a+b1+b3-c,m-n])+HypergeometricPFQ[{b3,-a+c+m-n},{1-a-b1+c+m-n},(-1+x)/(-1+z)] (((-1)^(m-n) (1-x)^(-a-b1-b3+c+m-n) (-y)^-b2 y^-n Gamma[a+b1-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1+c,m-n])+((-1)^(m-n) (1-x)^(-b1+m-n) (-y)^-b2 y^-n (1-z)^(-a-b3+c) Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1+c,m-n]))+HypergeometricPFQ[{b1,a+b1+b3-c-m+n},{1+a+b1-c-m+n},(-1+x)/(-1+z)] (((1-x)^(-b1+m-n) (-y)^-b2 y^-n (1-z)^(-a-b3+c) ((1-z)/(-1+x))^(m-n) Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b1+c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1-b3+c,m-n])+((1-x)^(-a-b1-b3+c+m-n) (-y)^-b2 y^-n ((1-z)/(-1+x))^(m-n) Gamma[1+a+b3-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b1+c,m-n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1-b3+c,m-n]))},{Abs[x]>1&&Abs[x/y]<1&&Abs[z]<1,((-1)^n (-x)^(-a+b2+n) x^(-m+n) (-y)^-b2 y^-n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b3,a-b2+m-2 n},{1+a-b1-b2+m-2 n},z/x] Pochhammer[a-b2,m-2 n] Pochhammer[b2,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-2 n])+((-1)^(m-n) x^(m-n) (-y)^(-a-m+n) y^-n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,a+m},{1+a-b2+m},z/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b3,a-b1-b2-m},{-b1-b2+c-m},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])},{Abs[y]>1+Abs[1-z]&&Abs[x]+Abs[1-z]<1,((-1)^(m+n) x^(m-n) (-y)^(-a-m) (1-z)^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b3-c+n},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^(2 (m-n)) x^(m-n) (-y)^-b2 (1-z)^n Gamma[a-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1-a+b2-m},1/y] Pochhammer[b1,m-n] Pochhammer[a-b2,m] Pochhammer[b3,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b3-c,n] Pochhammer[-b2-b3+c,m-n])+((-1)^(2 (m-n)) x^(m-n) (-y)^-b2 (1-z)^(-a-b3+c+n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/y] Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,n] Pochhammer[-b2-b3+c,m-n])},{Abs[x/y]<1&&Abs[y]>1+Abs[1-z]&&Abs[-1+z]<1&&Abs[x]>1+Abs[-1+z],((-1)^(m+n) x^(m-n) (-y)^(-a-m) (1-z)^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b3-c+n},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-x)^(-a+b2-n) x^(-m+n) (-y)^-b2 (1-z)^n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b2,-a+b1+b2-m},{1-a+b2-m},x/y] Pochhammer[a-b2,m] Pochhammer[b3,n] Pochhammer[1+a+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m] Pochhammer[1+a+b3-c,n])+((-1)^n (-x)^-b1 x^(-m+n) (-y)^-b2 (1-z)^n Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1-a+b1+b2+m-2 n},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,m-2 n] Pochhammer[1+a+b3-c,n])+((-1)^n (-x)^-b1 x^(-m+n) (-y)^-b2 (1-z)^(-a-b3+c+n) Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1+b1+b2+b3-c+m-2 n},1/y] Pochhammer[b1,m-n] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1+b1+b2+b3-c,m-2 n] Pochhammer[1-a-b3+c,n])},{Abs[x/y]<1&&Abs[z]>1&&Abs[z/x]<1,((-1)^m x^(m-n) (-y)^(-a-m) z^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^n (-x)^(-a+b2-n) x^(-m+n) (-y)^-b2 z^n Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b2,-a+b1+b2-m},{1-a+b2-m},x/y] Pochhammer[a-b2,m] Pochhammer[b3,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m])+((-1)^(m-n) (-x)^-b1 x^(-m+n) (-y)^-b2 (-z)^(-a+b1+b2+m-n) z^-n Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b1+b2+b3+m-2 n},{1-a+b1+b2+m-2 n},z/y] Pochhammer[b1,m-n] Pochhammer[a-b1-b2,-m+2 n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,-m+2 n])+((-x)^-b1 x^(-m+n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[1-y]<1&&Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[1-x]<1&&Abs[(-1+z)/(-1+x)]<1,((1-x)^(-a-b1-b2-b3+c-m) (1-y)^n (1-z)^(m-n) Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{-b1-b2-b3+c,-a-b2-b3+c-m},{1-a-b1-b2-b3+c-m},1-x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2+b3-c,m])+((1-y)^n (1-z)^(m-n) Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b1,a+m},{1+a+b1+b2+b3-c+m},1-x] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1-x] (((1-x)^(-b1-m+n) (1-y)^(-a-b2+c+n) (1-z)^(-b3+m-2 n) Gamma[a+b2-c] Gamma[c] Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[b1,m-n] Pochhammer[-a+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-b3,m-2 n] Pochhammer[1-a-b2+c,n])+((1-x)^(-b1-m+n) (1-y)^n (1-z)^(-a-b2-b3+c+m-2 n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a-b2-b3+c,m-2 n]))},{Abs[1-y]<1&&Abs[1-z]<1&&Abs[(-1+z)/(-1+y)]<1&&Abs[1-x]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^(m-n) (1-z)^n Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b2,a+m},{1+a+b1+b2+b3-c+m},1-y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+HypergeometricPFQ[{b2,-a+c+n},{1-a-b3+c+n},(-1+z)/(-1+y)] (((1-x)^(-b1+m-2 n) (1-z)^(-a-b2-b3+c+n) Gamma[1+a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[-a+c,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-b1,m-2 n] Pochhammer[1-a-b3+c,n])+((1-x)^(-b1+m-2 n) (1-y)^(-a-b2+c) (1-z)^(-b3+n) Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[-a+c,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Pochhammer[1-b1,m-2 n] Pochhammer[1-a-b3+c,n]))+HypergeometricPFQ[{b3,a+b2+b3-c-n},{1+a+b3-c-n},(-1+z)/(-1+y)] (((-1)^n (1-x)^(-b1+m-2 n) (1-y)^(-a-b2+c) (1-z)^(-b3+n) ((1-y)/(-1+z))^n Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[-a-b3+c,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[1-b1,m-2 n] Pochhammer[1-a-b2-b3+c,n])+((-1)^n (1-x)^(-b1+m-2 n) (1-z)^(-a-b2-b3+c+n) ((1-y)/(-1+z))^n Gamma[1+a+b2-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "+", "b2", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b2", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[-a-b3+c,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-2 n] Pochhammer[1-a-b2-b3+c,n]))+((1-x)^(-a-b1-b2-b3+c+m-2 n) (1-z)^n Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b2,a+b1+b2+b3-c-m+2 n},{1+a+b2+b3-c-m+2 n},(-1+y)/(-1+x)] Pochhammer[b3,n] Pochhammer[-a-b2-b3+c,m-2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2-b3+c,m-2 n])},{Abs[z]>1&&Abs[z/y]<1&&Abs[x]<1,((-1)^m x^(m-n) (-y)^(-a-m) z^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^(m-n) x^(m-n) (-y)^-b2 (-z)^(-a+b2-m+n) z^-n Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b2+b3-m},{1-a+b2-m},z/y] Pochhammer[b1,m-n] Pochhammer[a-b2,m] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,m])+(x^(m-n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+2 n},{1-a+b2+b3-m+2 n},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,-m+2 n])},{Abs[z/y]<1&&Abs[y]>1+Abs[1-x]&&Abs[-1+x]<1&&Abs[z]>1+Abs[-1+x],((-1)^(2 (m-n)+n) (1-x)^(m-n) (-y)^(-a-m) z^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1-c+m-n},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((1-x)^(m-n) (-y)^-b2 (-z)^(-a+b2-m+n) z^-n Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b2+b3-m},{1-a+b2-m},z/y] Pochhammer[b1,m-n] Pochhammer[a-b2,m] Pochhammer[1+a+b1-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,m] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+n},{1-a+b2+b3-m+2 n},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,-m+2 n] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(-a-b1+c+m-n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},1/y] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1+c,m-n])},{Abs[z/y]<1&&Abs[x]>1&&Abs[x/z]<1,((-1)^m x^(m-n) (-y)^(-a-m) z^n Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b2+m},1/y] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^n (-x)^(-a+b2+b3+n) x^(-m+n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b1+b2+b3-m+2 n},{1-a+b2+b3-m+2 n},x/y] Pochhammer[a-b2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,m-2 n])+((-1)^(m-n) x^(m-n) (-y)^-b2 (-z)^(-a+b2-m+n) z^-n Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b2+b3-m},{1-a+b2-m},z/y] Pochhammer[b1,m-n] Pochhammer[a-b2,m] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,m])+((-x)^-b1 x^(-m+n) (-y)^-b2 (-z)^-b3 z^-n Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[x]<1&&Abs[y]<1&&Abs[z]>1,((-1)^(m-n) x^(m-n) (-z)^(-a-m+n) z^-n Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,a+m},{1+a-b3+m},y/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+(x^(m-n) (-z)^-b3 z^-n Gamma[a-b3] Gamma[c] HypergeometricPFQ[{b2,a-b3+m-2 n},{-b3+c+m-2 n},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b3-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-b3+c] Pochhammer[1-a+b3,-m+2 n])},{Abs[z]>1+Abs[1-x]&&Abs[1-x]+Abs[y]<1,((-1)^(2 (m-n)) (1-x)^(m-n) (-z)^(-a-m+n) z^-n Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,a+m},{1+a-b3+m},y/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[1+a+b1-c,m])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(m-n) (-z)^-b3 z^-n Gamma[a-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b2,a-b3+m-2 n},{-b1-b3+c-n},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b3,-m+2 n] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(-a-b1+c+m-n) (-z)^-b3 z^-n Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b2,-b1-b3+c+m-2 n},{-b1-b3+c-n},y] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1+b1+b3-c,-m+2 n] Pochhammer[1-a-b1+c,m-n])},{Abs[1-x]<1&&Abs[z]>1+Abs[1-x]&&Abs[1-y]<1&&Abs[z]>1+Abs[1-y]&&Abs[z]>1+Abs[-1+x]&&Abs[-1+x]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^(m-n) (1-y)^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1+b2-c+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((1-x)^(m-n) (1-y)^n (-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1-a+b3-m},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a-b3,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m])+HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},1/z] (((1-x)^(-b1+m-2 n) (1-y)^(-a-b2+c+n) (-z)^-b3 Gamma[a+b2-c] Gamma[c] Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[-a+c,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-b1,m-2 n] Pochhammer[1-a-b2+c,n])+((1-x)^(-a-b1-b2+c+m-2 n) (1-y)^n (-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,m-2 n]))},{Abs[1-x]<1&&Abs[z]>1+Abs[1-x]&&Abs[1-y]<1&&Abs[z]>1+Abs[1-y]&&Abs[z]>1+Abs[-1+x]&&Abs[z]>1+Abs[-1+y]&&Abs[(-1+x)/(-1+y)]<1&&Abs[-1+y]<1&&Abs[-1+x]<1,((1-x)^(m-n) (1-y)^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1+b2-c+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((1-x)^(m-n) (1-y)^n (-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1-a+b3-m},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a-b3,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m])+HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},1/z] (((-1)^(-m+n) (1-x)^(-b1+m-n) (1-y)^(-a-b2+c) ((1-y)/(-1+x))^(m-n) ((-1+x)/(-1+y))^n (-z)^-b3 Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[a+b1+b2-c,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1+a+b1-c,-m+2 n])+((-1)^(-m+n) (1-x)^(-a-b1-b2+c+m-n) ((1-y)/(-1+x))^(m-n) ((-1+x)/(-1+y))^n (-z)^-b3 Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "-", "b1", "-", "b2", "+", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "+", "b1", "+", "b2", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[a+b1+b2-c,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,-m+2 n])+((1-x)^(-a-b1-b2+c+m-n) ((-1+x)/(-1+y))^n (-z)^-b3 Gamma[a+b1-c] Gamma[1+a+b2-c] Gamma[c] Gamma[-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b1+c,m])+((1-x)^(-b1+m-n) (1-y)^(-a-b2+c) ((-1+x)/(-1+y))^n (-z)^-b3 Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "c"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a-b1+c,m]))},{Abs[x]>1&&Abs[x/z]<1&&Abs[y]<1,((-1)^m x^(m-n) y^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^n (-x)^(-a+b3-n) x^(-m+n) y^n (-z)^-b3 Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b3-m},{1-a+b3-m},x/z] Pochhammer[b2,n] Pochhammer[a-b3,m] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b3,m])+((-x)^-b1 x^(-m+n) y^n (-z)^-b3 Gamma[a-b1-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b3-c+m-2 n},{1-a+b1+b3+m-2 n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m-2 n])},{Abs[z]>1+Abs[1-y]&&Abs[x]+Abs[1-y]<1,((-1)^(m+n) x^(m-n) (1-y)^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m,1+a+b2-c+n},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^(2 (m-n)) x^(m-n) (1-y)^n (-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1-a+b3-m},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a-b3,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2-c,n] Pochhammer[-b2-b3+c,m-n])+((-1)^(2 (m-n)) x^(m-n) (1-y)^(-a-b2+c+n) (-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/z] Pochhammer[b1,m-n] Pochhammer[-a+c,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,n] Pochhammer[-b2-b3+c,m-n])},{Abs[x/z]<1&&Abs[y]>1&&Abs[y/x]<1,((-1)^m x^(m-n) y^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^n (-x)^(-a+b3-n) x^(-m+n) y^n (-z)^-b3 Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b3-m},{1-a+b3-m},x/z] Pochhammer[b2,n] Pochhammer[a-b3,m] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b3,m])+((-1)^(m-n) (-x)^-b1 x^(-m+n) (-y)^(-a+b1+b3+m-n) y^-n (-z)^-b3 Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b2+b3+m-2 n},{1-a+b1+b3+m-2 n},y/z] Pochhammer[b1,m-n] Pochhammer[a-b1-b3,-m+2 n] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,-m+2 n])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[y]>1&&Abs[y/z]<1&&Abs[x]<1,((-1)^m x^(m-n) y^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^(m-n) x^(m-n) (-y)^(-a+b3-m+n) y^-n (-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b2+b3-m},{1-a+b3-m},y/z] Pochhammer[b1,m-n] Pochhammer[a-b3,m] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,m])+(x^(m-n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+2 n},{1-a+b2+b3-m+2 n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,-m+2 n])},{Abs[y/z]<1&&Abs[z]>1+Abs[1-x]&&Abs[-1+x]<1&&Abs[y]>1+Abs[-1+x],((-1)^(2 (m-n)+n) (1-x)^(m-n) y^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m,1+a+b1-c+m-n},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((1-x)^(m-n) (-y)^(-a+b3-m+n) y^-n (-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b2+b3-m},{1-a+b3-m},y/z] Pochhammer[b1,m-n] Pochhammer[a-b3,m] Pochhammer[1+a+b1-c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,m] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1-a+b2+b3-m+2 n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,-m+2 n] Pochhammer[1+a+b1-c,m-n])+((-1)^(m-n) (1-x)^(-a-b1+c+m-n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},1/z] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2+b3-c,-m+2 n] Pochhammer[1-a-b1+c,m-n])},{Abs[y/z]<1&&Abs[x]>1&&Abs[x/y]<1,((-1)^m x^(m-n) y^n (-z)^(-a-m) Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{1+a-c,a+m},{1+a-b3+m},1/z] Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^n (-x)^(-a+b2+b3+n) x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b2+b3-m+2 n},{1-a+b2+b3-m+2 n},x/z] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-2 n] Pochhammer[1+a-c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,m-2 n])+((-1)^(m-n) x^(m-n) (-y)^(-a+b3-m+n) y^-n (-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b2+b3-m},{1-a+b3-m},y/z] Pochhammer[b1,m-n] Pochhammer[a-b3,m] Pochhammer[1+a-c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,m])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1-a+b1+b2+b3+m},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 HypergeometricPFQ[{b3,-a+c+m},{c+m},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Pochhammer[c,m])},{Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1,((1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a-b1] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{-b1+c+n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m-n] Pochhammer[-b1+c,n])+((1/(1-x))^(m-n) (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,-b1+c+m},{-b1+c+n},z/(-1+z)] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[-b1+c,m])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[-b1+c,n])},{Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1&&Abs[x/(-1+x)]>1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-a-b1+c-m+2 n},{-b1+c-m+2 n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m-2 n])+((-1)^n (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{-b1-b2+c},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])+((1/(1-x))^(m-n) (1-x)^-a (1/(1-y))^(-m+2 n) (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a,m-n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1-b2,-m+2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^(-m+2 n) (1-y)^(-a+b1) (1-z)^-b3 Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] Pochhammer[a-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+2 n])},{Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b1-b2] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{-b1-b2+c},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m])+((1-x)^-a (1/(1-y))^n ((-1+x)/(-1+y))^(m-n) (1-z)^-b3 Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "a"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "a"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((-1+x)/(-1+y))^(m-n) (1-z)^-b3 Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^n (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^(m-n) (1-z)^-b3 Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b2"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b1+b2,m-2 n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])+((-1)^n (1-x)^-a (1/(1-y))^n ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^(m-n) (1-z)^-b3 Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+n},{-b1-b2+c},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b1+b2,m-2 n] Pochhammer[-b1-b2+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-b1-b2] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])},{Abs[1-x/(-1+x)]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1,((1/(1-x))^(m-n) (1-x)^-a (1/(1-y))^(-m+2 n) (1/(1-z))^-n Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1-b2,-m+2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^(-m+2 n) (1-y)^(-a+b1) (1/(1-z))^-n Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] Pochhammer[a-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^-m (1-z)^(-a+b1+b2) Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,a-b1-b2-m},{1+a-b1-b2-b3-m},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+b1+b2+b3,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1+b2,m])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b1+b2+b3+m},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[1-x/(-1+x)]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1,((1-x)^-a (1/(1-y))^n ((-1+x)/(-1+y))^(m-n) (1/(1-z))^-n Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "a"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "a"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((-1+x)/(-1+y))^(m-n) (1/(1-z))^-n Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^n (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^(m-n) (1/(1-z))^-n Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b2"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b1+b2,m-2 n] Pochhammer[b3,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])+((-1)^n (1-x)^-a (1/(1-y))^n ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^(m-n) (1/(1-z))^-n Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b3-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b1+b2,m-2 n] Pochhammer[b3,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-b1-b2] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^-m (1-z)^(-a+b1+b2) Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,a-b1-b2-m},{1+a-b1-b2-b3-m},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+b1+b2+b3,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1+b2,m])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b1+b2+b3+m},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b2] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{-b2+c+m-n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,n] Pochhammer[-b2+c,m-n])+((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-a (1-z)^-b3 Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,-b2+c+m},{-b2+c+m-n},z/(-1+z)] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[-b2+c,m])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[-b2+c,m-n])},{Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-a-b1+c-m+2 n},{-b1-b2+c-m+n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b1-c,m-2 n])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-a (1-z)^-b3 Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c-m+2 n},{-b1-b2+c-m+n},z/(-1+z)] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[1+b1+b2-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1+b1+b2-c,m-2 n])+((-1)^(2 n) (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[b2,n] Pochhammer[1-a+b2,m] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a+b2,n] Pochhammer[1-a-b1+c,m])},{Abs[(-1+y)/(-1+z)]<1&&Abs[x/(-1+x)]+1/Abs[1-z]<1&&Abs[x/(-1+x)]+1/Abs[1-y]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^-n (1/(1-z))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{-b2-b3+c+m-n,-n},{1-b2-n},1/(1-y)] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,n])+((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^-n (1-y)^(-a+b3) (1/(1-z))^n (1-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{a-b3-n,-b2-b3+c+m-n},{1+a-b2-b3-n},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+b2+b3,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b3,n])+((1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] HypergeometricPFQ[{b2,-a+c+m},{1-a+b2+b3+n},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,n] Pochhammer[-b2-b3+c,m-n])},{Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b2+b3+n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,n] Pochhammer[-b2-b3+c,m-n])+((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-z)^-a Gamma[-a+b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b3,a+n},{1+a-b2+n},(-1+z)/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "a"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "a"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[-b2-b3+c,m-n])+((1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 Gamma[-a+b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] HypergeometricPFQ[{b3,a+n},{1+a-b2+n},(-1+z)/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[-b2-b3+c,m-n])+((-1)^n (1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-z)^-a ((1-y)/(-1+z))^n Gamma[a-b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b2+b3-n},{1-a+b2-n},(-1+z)/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[a-b2,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[1+a-b2-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,n] Pochhammer[-b2-b3+c,m-n])+((-1)^n (1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((1-y)/(-1+z))^n Gamma[a-b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b2,-a+b2+b3-n},{1-a+b2-n},(-1+z)/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b2", "+", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "b2", "-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[a-b2,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,n] Pochhammer[-b2-b3+c,m-n])},{Abs[1-y/(-1+y)]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]>1+Abs[-1+y/(-1+y)]&&Abs[-1+y/(-1+y)]<1&&Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^-n (1/(1-z))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{-n,-b1-b2-b3+c-m+n},{1-b2-n},1/(1-y)] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,n])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-a-b1+c-m+2 n},{1-a+b2+b3+n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,n] Pochhammer[1+a+b1-c,m-2 n])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b2+b3-n},{1-a+b3-n},(1-y)/(1-z)] Pochhammer[b1,m-n] Pochhammer[a-b3,n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{1-a+b2+b3+m,-a+c+m},{1-a-b1+c+m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])},{Abs[1-y/(-1+y)]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]>1+Abs[-1+y/(-1+y)]&&Abs[x/(-1+x)]>1+Abs[-1+z/(-1+z)]&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[-1+z/(-1+z)]<1&&Abs[-1+y/(-1+y)]<1,((-1+1/x)^b1 (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+a+b1-c-m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a-b1+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,m])+((-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{1-a+b2+b3+m,-a+c+m},{1-a-b1+c+m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1+1/x)^b1 (1-x)^-b1 (1/(1-y))^n (1-z)^-a ((-1+z)/(-1+y))^(m-n) Gamma[-a+b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+b1+b2+b3-c-n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "a"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "a"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b3,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1+1/x)^b1 (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((-1+z)/(-1+y))^(m-n) Gamma[-a+b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+b1+b2+b3-c-n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b3,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m])+((-1)^-n (-1+1/x)^b1 (1-x)^-b1 (1/(1-y))^n (1-z)^-a ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^(m-n) Gamma[a-b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+b1+b2+b3-c-n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b2+b3,m-2 n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[1+a-b2-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])+((-1)^-n (-1+1/x)^b1 (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^(m-n) Gamma[a-b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c},{1+b1+b2+b3-c-n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b2", "+", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "b2", "-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[-a+b2+b3,m-2 n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,m-2 n])},{Abs[y/(-1+y)]>1&&Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[z/(-1+z)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,-a-b1-b2+c-m},{-b1-b2+c-m},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((-1)^n (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^(-a-b1+c) (1-y)^-b2 (-(y/(-1+y)))^(m-n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-a-b1+c-m+2 n},{1-a-b1-b2+c-m+2 n},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,-m+2 n])},{Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((-1)^(2 (m-n)+n) (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b3+m-n},{1-a-b1+c+m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1)^(2 n) (-1+1/x)^b1 (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b1,1+b1+b3-c-n},{1+a+b1-c-m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a-b1+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b3,m-n] Pochhammer[-b1-b3+c,n])+((-1)^(2 n) (-1+1/x)^b1 (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(m-n) (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b3-c-n},{1+b1+b3-c-m},(-1+x)/x] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[-b1-b3+c,m])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m-n] Pochhammer[-b1-b3+c,n])},{Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[-1+z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[-1+z/(-1+z)],((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+n},{1+a+b1+b2-c-m+2 n},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,m-n] Pochhammer[1+a+b1+b2-c,-m+2 n])+((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1/(1-z))^(m-n) (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},(-1+x)/x] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m-n] Pochhammer[1+b1+b2+b3-c,-m+2 n])+((-1)^(2 (m-n)+n) (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b3+m-n},{1-a-b1+c+m},(-1+x)/x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1+1/x)^b1 (1-x)^-b1 (-1+1/y)^(-a-b1+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b1,a+b1+b2-c-m},{1+a+b1-c-m},((-1+x) y)/(x (-1+y))] Pochhammer[b3,m-n] Pochhammer[1-a+b3,m] Pochhammer[-a-b1+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b3,m-n] Pochhammer[1-a-b1-b2+c,m])},{Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[z/(-1+z)]>1&&Abs[((-1+y) z)/(y (-1+z))]<1,((-1)^m (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^(-a-b1-b2+c) (1-z)^-b3 (-(z/(-1+z)))^m Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{1-a,-a-b1-b2+c-m},{1-a-b1-b2-b3+c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m])+((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((-1)^n (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^(-a-b1+c) (1-y)^-b2 (-(y/(-1+y)))^(m-n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{b3,-a-b1+c-m+2 n},{1-a-b1-b2+c-m+2 n},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,-m+2 n])},{Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((1/(1-x))^(m-n) (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(-m+n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{-b1-b3+c+n,-m+n},{1-b3-m+n},1/(1-z)] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n])+((1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(-m+n) (1-z)^(-a+b1) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{-b1-b3+c+n,a-b1-m+n},{1+a-b1-b3-m+n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+b1+b3,m-n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,m-n])+((1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b1+b3+m-n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m-n] Pochhammer[-b1-b3+c,n])},{Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((1-x)^-a (1-y)^-b2 (y/(-1+y))^n ((-1+x)/(-1+z))^(m-n) Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m-n,-b1-b3+c+n},{1+a-b3+m-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "a"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "a"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m-n])+((1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^(-a+b1) ((-1+x)/(-1+z))^(m-n) Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a+m-n,-b1-b3+c+n},{1+a-b3+m-n},1/(1-z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m-n])+((1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^(-a+b1) ((-1+x)/(-1+z))^(m-n) Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{-b1-b3+c+n,a-b3-m+n},{1+a-b1-b3-m+n},-(1/(-1+x))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+b1+b3,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b3,m-n])+((1-x)^-a (1-y)^-b2 (y/(-1+y))^n ((-1+x)/(-1+z))^(m-n) Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b3] Gamma[c] HypergeometricPFQ[{-b1-b3+c+n,a-b3-m+n},{1+a-b1-b3-m+n},-(1/(-1+x))] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+b1+b3,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-b1-b3] Gamma[-a+c] Pochhammer[1-a+b3,m-n])+((1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b1+b3+m-n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m-n] Pochhammer[-b1-b3+c,n])},{Abs[1-x]>1&&Abs[1-z]>1&&Abs[(-1+z)/(-1+x)]<1&&Abs[1-y]>1&&Abs[(-1+y)/(-1+z)]<1,((1/(1-x))^(m-n) (1-x)^-a (1/(1-y))^-n (1/(1-z))^(-m+2 n) Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b2-n},1/(1-y)] Pochhammer[0,-m+2 n] Pochhammer[a,m-n] Pochhammer[b2,n])/(n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1-b3,-m+2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^-n (1/(1-z))^(-m+2 n) (1-z)^(-a+b1) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-n},{1-b2-n},1/(1-y)] Pochhammer[a-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b3,-m+2 n])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^-m (1-y)^(-a+b1+b3) (1/(1-z))^n (1-z)^-b3 Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,a-b1-b3-m},{1+a-b1-b2-b3-m},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+b1+b2+b3,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1+b3,m])+((1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b2,-a+c+m},{1-a+b1+b2+b3+m},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[1-x/(-1+x)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1,((1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a+c+m},{1-a+b1+b2+b3+m},1/(1-x)] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])+((1-x)^-a (1/(1-y))^(-m+2 n) (1/(1-z))^(m-n) Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,a+m-n},{1+a-b3+m-n},(-1+x)/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "a"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "a"], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,-m+2 n] Pochhammer[1+a-b3,m-n])+((1-x)^-b1 (1/(1-y))^(-m+2 n) (1/(1-z))^(m-n) (1-z)^(-a+b1) Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b1,a+m-n},{1+a-b3+m-n},(-1+x)/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,-m+2 n] Pochhammer[1+a-b3,m-n])+((-1)^(m-n) (1-x)^-b1 (1/(1-y))^(-m+2 n) (1/(1-z))^(m-n) (1-z)^(-a+b1) ((1-z)/(-1+x))^(m-n) Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b3-m+n},{1-a+b3-m+n},(-1+x)/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a-b3,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,-m+2 n] Pochhammer[1+a-b1-b3,m-n])+((-1)^(m-n) (1-x)^-a (1/(1-y))^(-m+2 n) (1/(1-z))^(m-n) ((1-z)/(-1+x))^(m-n) Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b3] Gamma[c] HypergeometricPFQ[{b3,-a+b1+b3-m+n},{1-a+b3-m+n},(-1+x)/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[0,-m+2 n] Pochhammer[1,m-n] Pochhammer[a-b3,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[1+a-b1-b3] Gamma[-a+c] Pochhammer[1-b2,-m+2 n] Pochhammer[1+a-b1-b3,m-n])+((1-x)^-b1 (1/(1-y))^(-m+2 n) (1-y)^(-a+b1+b3) (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a+b1+b2+b3+m-2 n},{1-a+b1+b3+m-2 n},(1-y)/(1-x)] Pochhammer[a-b1-b3,-m+2 n] Pochhammer[b3,m-n] Pochhammer[-b1-b2-b3+c,n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2-b3,-m+2 n])},{Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,1/((m-n)! n! Gamma[b3] Gamma[-a+c]) (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{a,-b3+c+m},{1+a-b3},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n]+((1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a-b3] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a+b3},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b3+c] Pochhammer[-b3+c,m])},{Abs[z/(-1+z)]>1&&Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[y/(-1+y)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a-b1+c) (1-z)^-b3 (-(z/(-1+z)))^(m-2 n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{1-a,-a-b1+c-m+2 n},{1-a-b1-b3+c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n] Pochhammer[a+b1+b3-c,m-2 n] Pochhammer[-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-2 n])+((-1)^n (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,1+b1+b3-c+m-2 n},{1+a+b1+b3-c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n] Pochhammer[-b1+c,-m+2 n] Pochhammer[-a-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1-c,m-2 n] Pochhammer[-a-b1+c,-m+2 n] Pochhammer[-b1-b3+c,-m+2 n])},{Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[-1+y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[-1+y/(-1+y)],((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (-1+1/z)^(-a-b1+c) (1-z)^-b3 (-(z/(-1+z)))^(m-2 n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{1-a+b2+n,-a-b1+c-m+2 n},{1-a-b1-b3+c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-2 n])+((-1)^(2 n) (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[b2,n] Pochhammer[1-a+b2,m] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a+b2,n] Pochhammer[1-a-b1+c,m])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m-n},{1+a+b1+b3-c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[-a-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b1-c,m-2 n] Pochhammer[-a-b1+c,-m+2 n])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1/(1-y))^n (1-y)^-a (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m-n},{1+b1+b2+b3-c+m-2 n},(-1+z)/z] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[-b1-b2-b3+c,-m+2 n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1+b1+b2-c,m-2 n] Pochhammer[-b1-b2+c,-m+2 n])},{Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[y/(-1+y)]>1&&Abs[(y (-1+z))/((-1+y) z)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a-b1+c) (1-z)^-b3 (-(z/(-1+z)))^(m-2 n) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] HypergeometricPFQ[{1-a,-a-b1+c-m+2 n},{1-a-b1-b3+c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n] Pochhammer[a+b1+b3-c,m-2 n] Pochhammer[-b1+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-2 n])+((-1)^n (-1+1/x)^(-a+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 Gamma[a+b1-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b1+c+m},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m])+((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^(-a-b1-b3+c) (1-y)^-b2 (-(y/(-1+y)))^(m-n) (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,a+b1+b2+b3-c+m-2 n},{1+a+b1+b3-c+m-2 n},(y (-1+z))/((-1+y) z)] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2-b3+c,-m+2 n])},{Abs[x/(-1+x)]<1&&Abs[z/(-1+z)]<1&&Abs[y/(-1+y)]>1,((1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,-a-b2+c+m-2 n},{-b2+c+m-2 n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,-m+2 n])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])},{Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1,((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,-a-b2+c+m-2 n},{-b1-b2+c-n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-n] Pochhammer[1+a+b2-c,-m+2 n])+((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-a (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,-b1-b2+c+m-2 n},{-b1-b2+c-n},z/(-1+z)] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1+b1+b2-c,-m+2 n])+((-1)^(2 (m-n)) (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[b1,m-n] Pochhammer[1-a+b1,m] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a+b1,m-n] Pochhammer[1-a-b2+c,m])},{Abs[1-x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1,((1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+a+b2-c-m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a-b2+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b3,m])+((1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^-m (1/(1-z))^n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{1-a+b1+b3+m,-a+c+m},{1-a-b2+c+m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((1/(1-x))^(m-2 n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[a,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-2 n] Pochhammer[1+a-b3,n])+((1/(1-x))^(m-2 n) (1-x)^(-a+b3) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] Pochhammer[a-b3,m-2 n] Pochhammer[b3,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b3,m-2 n])},{Abs[1-x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)]&&Abs[y/(-1+y)]>1+Abs[-1+z/(-1+z)]&&Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1&&Abs[-1+z/(-1+z)]<1&&Abs[-1+x/(-1+x)]<1,((1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+a+b2-c-m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a-b2+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b3,m])+((1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^-m (1/(1-z))^n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{1-a+b1+b3+m,-a+c+m},{1-a-b2+c+m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((1/(1-x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (1-z)^-a ((-1+z)/(-1+x))^n Gamma[-a+b1] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "a"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "a"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b3,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1,m])+((1/(1-x))^(m-n) (1-x)^(-a+b3) (-1+1/y)^b2 (1-y)^-b2 (1-z)^-b3 ((-1+z)/(-1+x))^n Gamma[-a+b1] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b3,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^(-m+n) (1/(1-x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (1-z)^-a ((1-x)/(-1+z))^(m-n) ((-1+z)/(-1+x))^n Gamma[a-b1] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[-a+b1+b3,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1+a-b1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,-m+2 n])+((-1)^(-m+n) (1/(1-x))^(m-n) (1-x)^(-a+b3) (-1+1/y)^b2 (1-y)^-b2 (1-z)^-b3 ((1-x)/(-1+z))^(m-n) ((-1+z)/(-1+x))^n Gamma[a-b1] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b1+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+y)/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[-a+b1+b3,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,-m+2 n])},{Abs[x/(-1+x)]>1&&Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[z/(-1+z)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b3,-a-b1-b2+c-m},{-b1-b2+c-m},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((-1)^n (-1+1/x)^(-a-b2+c) (1-x)^-b1 (-(x/(-1+x)))^n (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,-a-b2+c+m-2 n},{1-a-b1-b2+c+m-2 n},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,m-2 n])},{Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1,((-1)^(m+n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^-m (1/(1-z))^n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b3+n},{1-a-b2+c+m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((-1)^(2 (m-n)) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1+a+b2-c-m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a-b2+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b3,n] Pochhammer[-b2-b3+c,m-n])+((-1)^(2 (m-n)) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1+b2+b3-c-m},(-1+y)/y] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,n] Pochhammer[-b2-b3+c,m-n])},{Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[-1+z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[-1+z/(-1+z)],((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1+a+b1+b2-c+m-2 n},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,n] Pochhammer[1+a+b1+b2-c,m-2 n])+((-1)^n (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-a Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1+b1+b2+b3-c+m-2 n},(-1+y)/y] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((-1)^(m+n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^-m (1/(1-z))^n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b3+n},{1-a-b2+c+m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((-1+1/x)^(-a-b2+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (1/(1-z))^n (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b2,a+b1+b2-c-m},{1+a+b2-c-m},(x (-1+y))/((-1+x) y)] Pochhammer[b3,n] Pochhammer[1-a+b3,m] Pochhammer[-a-b2+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b3,n] Pochhammer[1-a-b1-b2+c,m])},{Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[z/(-1+z)]>1&&Abs[((-1+x) z)/(x (-1+z))]<1,((-1)^m (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^(-a-b1-b2+c) (1-z)^-b3 (-(z/(-1+z)))^m Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] HypergeometricPFQ[{1-a,-a-b1-b2+c-m},{1-a-b1-b2-b3+c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m])+((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((-1)^n (-1+1/x)^(-a-b2+c) (1-x)^-b1 (-(x/(-1+x)))^n (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{b3,-a-b2+c+m-2 n},{1-a-b1-b2+c+m-2 n},((-1+x) z)/(x (-1+z))] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b2+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2+c,m-2 n])},{Abs[1-y/(-1+y)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[1-x/(-1+x)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1,((1/(1-x))^(-m+n) (1/(1-y))^n (1-y)^-a (1/(1-z))^(m-2 n) Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[a,n] Pochhammer[b1,m-n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1-b3,m-2 n])+((1/(1-x))^(-m+n) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-2 n) (1-z)^(-a+b2) Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] Pochhammer[b1,m-n] Pochhammer[a-b2,m-2 n] Pochhammer[b2,n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2-b3,m-2 n])+((1/(1-x))^-m (1-x)^(-a+b2+b3) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,a-b2-b3-m},{1+a-b1-b2-b3-m},1/(1-x)] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+b1+b2+b3,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b2+b3,m])+((1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a+c+m},{1-a+b1+b2+b3+m},1/(1-x)] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[1-y/(-1+y)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1&&Abs[1-x/(-1+x)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1,((1/(1-x))^(-m+n) (1-y)^-a (1/(1-z))^(m-n) ((-1+y)/(-1+z))^n Gamma[1+a-b2] Gamma[-a+b2] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "a"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "a"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((1/(1-x))^(-m+n) (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^(-a+b2) ((-1+y)/(-1+z))^n Gamma[a-b2] Gamma[1-a+b2] Gamma[-a+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m])+((-1)^(m-n) (1/(1-x))^(-m+n) (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^(-a+b2) ((1-z)/(-1+y))^(m-n) ((-1+y)/(-1+z))^n Gamma[a-b2] Gamma[1-a+b2] Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "b2", "-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b2", "+", "b3"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+b2+b3,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b3,-m+2 n])+((-1)^(m-n) (1/(1-x))^(-m+n) (1-y)^-a (1/(1-z))^(m-n) ((1-z)/(-1+y))^(m-n) ((-1+y)/(-1+z))^n Gamma[1+a-b2] Gamma[-a+b2] Gamma[a-b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,-m+n},{1-b1-m+n},1/(1-x)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "z"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a+b2+b3,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[1+a-b2-b3] Gamma[-a+c] Pochhammer[1-a+b3,-m+2 n])+((1/(1-x))^-m (1-x)^(-a+b2+b3) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c,a-b2-b3-m},{1+a-b1-b2-b3-m},1/(1-x)] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+b1+b2+b3,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b2+b3,m])+((1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(m-n) (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a+c+m},{1-a+b1+b2+b3+m},1/(1-x)] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2+b3,m])},{Abs[z/(-1+z)]>1&&Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x/(-1+x)]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^(-a-b2+c) (1-z)^-b3 (-(z/(-1+z)))^(-m+2 n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{1-a,-a-b2+c+m-2 n},{1-a-b2-b3+c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+2 n] Pochhammer[a+b2+b3-c,-m+2 n] Pochhammer[-b2+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-m+2 n])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,1+b2+b3-c-m+2 n},{1+a+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+2 n] Pochhammer[-b2+c,m-2 n] Pochhammer[-a-b2-b3+c,m-2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2-c,-m+2 n] Pochhammer[-a-b2+c,m-2 n] Pochhammer[-b2-b3+c,m-2 n])},{Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)],((-1)^n (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^(-a-b2+c) (1-z)^-b3 (-(z/(-1+z)))^(-m+2 n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{-a-b2+c+m-2 n,1-a+b1+m-n},{1-a-b2-b3+c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-m+2 n])+((-1)^(2 (m-n)) (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[b1,m-n] Pochhammer[1-a+b1,m] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a+b1,m-n] Pochhammer[1-a-b2+c,m])+((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1+a+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m-n] Pochhammer[1+a+b2-c,-m+2 n] Pochhammer[-a-b2+c,m-2 n])+((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-a (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-b1-b2-b3+c,m-2 n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1+b1+b2-c,-m+2 n] Pochhammer[-b1-b2+c,m-2 n])},{Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x/(-1+x)]>1&&Abs[(x (-1+z))/((-1+x) z)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^(-a-b2+c) (1-z)^-b3 (-(z/(-1+z)))^(-m+2 n) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] HypergeometricPFQ[{1-a,-a-b2+c+m-2 n},{1-a-b2-b3+c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+2 n] Pochhammer[a+b2+b3-c,-m+2 n] Pochhammer[-b2+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-m+2 n])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (1-z)^-b3 Gamma[a+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a+c+m},{1-a-b2+c+m},((-1+y) z)/(y (-1+z))] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m])+((-1)^n (-1+1/x)^(-a-b2-b3+c) (1-x)^-b1 (-(x/(-1+x)))^n (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,a+b1+b2+b3-c-m+2 n},{1+a+b2+b3-c-m+2 n},(x (-1+z))/((-1+x) z)] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2-b3+c,m-2 n])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]>1,((1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,1+b3-c-m},{1+a+b3-c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b3+c] Pochhammer[-b3+c,m])+((-1)^m (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])},{Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1,((-1)^(2 (m-n)+n) (1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b1+m-n},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^(2 n) (1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,1+b1+b3-c-n},{1+a+b3-c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b1,m-n] Pochhammer[-b1-b3+c,n])+((-1)^(2 n) (1/(1-x))^(m-n) (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,1+b1+b3-c-n},{1+b1+b3-c-m},(-1+z)/z] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[-b1-b3+c,m])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[-b1-b3+c,n])},{Abs[1-x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1,((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+a+b3-c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,m])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a+b1+b2+m,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((1/(1-x))^(m-2 n) (1/(1-y))^n (1-y)^-a (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] Pochhammer[0,m-2 n] Pochhammer[1,n] Pochhammer[a,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1-b1,m-2 n] Pochhammer[1+a-b2,n])+((1/(1-x))^(m-2 n) (1-x)^(-a+b2) (1/(1-y))^n (1-y)^-b2 (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] Pochhammer[a-b2,m-2 n] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-2 n])},{Abs[1-x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)]&&Abs[z/(-1+z)]>1+Abs[-1+y/(-1+y)]&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[-1+y/(-1+y)]<1&&Abs[-1+x/(-1+x)]<1,((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+a+b3-c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,m])+((1/(1-x))^(m-n) (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a+b1+b2+m,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((1/(1-x))^(m-n) (1-y)^-a ((-1+y)/(-1+x))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b1] Gamma[1+a-b2] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "a"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"-", "a"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1,m])+((1/(1-x))^(m-n) (1-x)^(-a+b2) (1-y)^-b2 ((-1+y)/(-1+x))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b1] Gamma[a-b2] Gamma[1-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m])+((-1)^(-m+n) (1/(1-x))^(m-n) (1-y)^-a ((1-x)/(-1+y))^(m-n) ((-1+y)/(-1+x))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1] Gamma[1+a-b2] Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], "b1"], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[-a+b1+b2,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1+a-b1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,-m+2 n])+((-1)^(-m+n) (1/(1-x))^(m-n) (1-x)^(-a+b2) (1-y)^-b2 ((1-x)/(-1+y))^(m-n) ((-1+y)/(-1+x))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1] Gamma[a-b2] Gamma[1-a+b2] Gamma[-a+b1+b2] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c},{1+b1+b2+b3-c-m+n},(-1+z)/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "y"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "x"}]], ")"}], 
RowBox[{
RowBox[{"-", "a"}], "+", "b1", "+", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "x", "]"}], ">=", 
RowBox[{"Re", "[", "y", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{"1", "-", "x"}], 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
RowBox[{"a", "-", "b1", "-", "b2"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[-a+b1+b2,-m+2 n] Pochhammer[-b1-b2-b3+c,m-n])/((m-n)! n! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,-m+2 n])},{Abs[x/(-1+x)]>1&&Abs[(x (-1+z))/((-1+x) z)]<1&&Abs[y/(-1+y)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,1+b1+b3-c+m-2 n},{1+a+b1+b3-c+m-2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1+b3-c,m-2 n])+((-1)^m (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^n (-1+1/x)^(-a-b3+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b1+b3-c-m},{1+a+b3-c-m},(x (-1+z))/((-1+x) z)] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b3+c,m])},{Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1,((-1)^(m+n) (1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-b2 (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b2+n},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^(2 (m-n)) (1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-b2 (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b2] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1+a+b3-c-m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b2,n] Pochhammer[-b2-b3+c,m-n])+((-1)^(2 (m-n)) (1-x)^-b1 (x/(-1+x))^(m-n) (1/(1-y))^n (1-y)^-a (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b2] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1+b2+b3-c-m},(-1+z)/z] Pochhammer[a,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[-b2-b3+c,m-n])},{Abs[(x (-1+z))/((-1+x) z)]<1&&Abs[y/(-1+y)]>1&&Abs[((-1+x) y)/(x (-1+y))]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((-1)^m (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^n (-1+1/x)^(-a-b3+c) (1-x)^-b1 (-(x/(-1+x)))^-n (x/(-1+x))^(-m+n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b1+b3-c-m},{1+a+b3-c-m},(x (-1+z))/((-1+x) z)] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b3+c,m])+((-1)^(m-n) (-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^(-a-b1-b3+c) (1-y)^-b2 (-(y/(-1+y)))^(m-n) (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] HypergeometricPFQ[{b3,a+b1+b2+b3-c+m-2 n},{1+a+b1+b3-c+m-2 n},(y (-1+z))/((-1+y) z)] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b1-b2-b3+c,-m+2 n])},{Abs[y/(-1+y)]>1&&Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x/(-1+x)]<1,((1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,1+b2+b3-c-m+2 n},{1+a+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2+b3-c,-m+2 n])+((-1)^m (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a-b3+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b2+b3-c-m},{1+a+b3-c-m},(y (-1+z))/((-1+y) z)] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b2-b3+c,m])},{Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)],((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1+a+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m-n] Pochhammer[1+a+b2+b3-c,-m+2 n])+((-1)^(m-n) (1/(1-x))^(m-n) (1-x)^-a (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[-a+b1] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+n},{1+b1+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[a,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/((m-n)! n! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m-n] Pochhammer[1+b1+b2+b3-c,-m+2 n])+((-1)^(2 (m-n)+n) (1/(1-x))^(m-n) (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{-a+c+m,1-a+b1+m-n},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((1/(1-x))^(m-n) (1-x)^-b1 (-1+1/y)^(-a-b3+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b2+b3-c-m},{1+a+b3-c-m},(y (-1+z))/((-1+y) z)] Pochhammer[b1,m-n] Pochhammer[1-a+b1,m] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m-n] Pochhammer[1-a-b2-b3+c,m])},{Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x/(-1+x)]>1&&Abs[(x (-1+y))/((-1+x) y)]<1,((-1+1/x)^b1 (1-x)^-b1 (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[c] Gamma[-a-b1-b2-b3+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1+a+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2+b3-c,m])+((-1)^m (1-x)^-b1 (x/(-1+x))^(m-n) (1-y)^-b2 (y/(-1+y))^n (-1+1/z)^(-a+c) (1-z)^-b3 (-(z/(-1+z)))^-m Gamma[a+b3-c] Gamma[c] HypergeometricPFQ[{1-a,-a+c+m},{1-a-b3+c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a+c,m])/((m-n)! n! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m])+((-1)^(m-n) (1-x)^-b1 (x/(-1+x))^(m-n) (-1+1/y)^(-a-b3+c) (1-y)^-b2 (-(y/(-1+y)))^(-m+n) (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] HypergeometricPFQ[{b3,a+b2+b3-c-m},{1+a+b3-c-m},(y (-1+z))/((-1+y) z)] Pochhammer[1-a,n] Pochhammer[b1,m-n] Pochhammer[-a-b3+c,m])/((m-n)! n! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a-b2-b3+c,m])+((-1)^n (-1+1/x)^(-a-b2-b3+c) (1-x)^-b1 (-(x/(-1+x)))^n (x/(-1+x))^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (y/(-1+y))^-n (-1+1/z)^b3 (1-z)^-b3 Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] HypergeometricPFQ[{b3,a+b1+b2+b3-c-m+2 n},{1+a+b2+b3-c-m+2 n},(x (-1+z))/((-1+x) z)] Pochhammer[1-a,m-n] Pochhammer[b2,n] Pochhammer[-a-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a-b1-b2-b3+c,m-2 n])}};


FD3sum[{a_,b1_,b2_,b3_,c_,x_,y_,z_},m_,n_,p_]={{Abs[x]<1&&Abs[y]<1&&Abs[z]<1,(x^m y^n z^p Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Pochhammer[c,m+n+p])},{Abs[1-x]+Abs[y]<1&&Abs[1-x]+Abs[z]<1,((1-x)^m y^n z^p Gamma[c] Gamma[-a-b1+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+a+b1-c,m] Pochhammer[-b1+c,n+p])+((1-x)^(-a-b1+c+m) y^n z^p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[-b1+c,n+p] Pochhammer[1-a-b1+c,m])},{Abs[y]<1&&Abs[z]<1&&Abs[x]>1,((-x)^(-a-n-p) x^-m y^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a-c,m])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p])+((-x)^-b1 x^-m y^n z^p Gamma[a-b1] Gamma[c] Pochhammer[a-b1,n+p] Pochhammer[b1,m] Pochhammer[1-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n-p])/(m! n! p! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m-n-p] Pochhammer[1+b1-c,-n-p] Pochhammer[-b1+c,n+p])},{Abs[(-1+x)/(-1+y)]<1&&Abs[1-x]+Abs[z]<1&&Abs[1-y]+Abs[z]<1,((1-x)^m (1-y)^n z^p Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-b1-b2+c,p])+((1-x)^(-a-b1+c+m) (1-y)^(-b2-m+n) z^p Gamma[a+b1-c] Gamma[c] Pochhammer[0,-m+n] Pochhammer[1,m] Pochhammer[b3,p] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-b2,-m+n] Pochhammer[1-a-b1+c,m] Pochhammer[-b1-b2+c,p])+((1-x)^m (1-y)^(-a-b1-b2+c-m+n) z^p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a-b1+c,-m+n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[-b1-b2+c,p] Pochhammer[1-a-b1-b2+c,-m+n])},{Abs[(-1+y)/(-1+x)]<1&&Abs[1-x]+Abs[z]<1&&Abs[1-y]+Abs[z]<1,((1-x)^m (1-y)^n z^p Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-b1-b2+c,p])+((-1)^n (1-x)^(-a-b1+c) (1-y)^(-b2+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^m z^p Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b2+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b2,((-1+y)/(1-x))^b2] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[a+b1+b2-c,m-n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1+a+b2-c,m-n] Pochhammer[-b1-b2+c,p])+((-1)^n (1-y)^(-a-b1-b2+c+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^m z^p Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^(-a-b1-b2+c),((-1+y)/(1-x))^(a+b1+b2-c)] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[a+b1+b2-c,m-n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b2-c,m-n] Pochhammer[-b1-b2+c,p])+((1-y)^(-a-b1-b2+c+n) ((-1+y)/(-1+x))^m z^p Gamma[1+a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b1,((-1+y)/(1-x))^b1] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,m+n] Pochhammer[-b1-b2+c,p])+((1-x)^(-a-b1+c) (1-y)^(-b2+n) ((-1+y)/(-1+x))^m z^p Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b1+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^(a-c),((-1+y)/(1-x))^(-a+c)] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a-b2+c,m+n] Pochhammer[-b1-b2+c,p])},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+x)/(-1+y)]<1&&Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1,((1-x)^(-a-b1+c+m) (1-y)^(-b2-m+n) (1-z)^(-b3-n+p) Gamma[a+b1-c] Gamma[c] Pochhammer[0,-m+n] Pochhammer[0,-n+p] Pochhammer[1,m] Pochhammer[1,n] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-b2,-m+n] Pochhammer[1-b3,-n+p] Pochhammer[1-a-b1+c,m])+((1-x)^m (1-y)^(-a-b1-b2+c-m+n) (1-z)^(-b3-n+p) Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b1,m] Pochhammer[-a-b1+c,-m+n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-b3,-n+p] Pochhammer[1-a-b1-b2+c,-m+n])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b2+c,-m-n])+((1-x)^m (1-y)^n (1-z)^(-a-b1-b2-b3+c-m-n+p) Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2+c,-m-n+p] Pochhammer[-b1-b2-b3+c,p] Pochhammer[1-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[1-a-b1-b2-b3+c,-m-n+p])},{Abs[1-x]<1&&Abs[1-y]<1&&Abs[(-1+y)/(-1+x)]<1&&Abs[1-z]<1&&Abs[(-1+x)/(-1+z)]<1,((-1)^n (1-x)^(-a-b1+c) (1-y)^(-b2+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^m (1-z)^(-b3-n+p) Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b2+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b2,((-1+y)/(1-x))^b2] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b2,m] Pochhammer[a+b1+b2-c,m-n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1-b3,-n+p] Pochhammer[1+a+b2-c,m-n])+((-1)^n (1-y)^(-a-b1-b2+c+n) ((1-x)/(-1+y))^n ((-1+y)/(-1+x))^m (1-z)^(-b3-n+p) Gamma[1+a+b1-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^(-a-b1-b2+c),((-1+y)/(1-x))^(a+b1+b2-c)] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b2,m] Pochhammer[a+b1+b2-c,m-n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-b3,-n+p] Pochhammer[1+a+b2-c,m-n])+((1-y)^(-a-b1-b2+c+n) ((-1+y)/(-1+x))^m (1-z)^(-b3-n+p) Gamma[1+a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b1,((-1+y)/(1-x))^b1] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b1,m] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-b3,-n+p] Pochhammer[1-a-b2+c,m+n])+((1-x)^(-a-b1+c) (1-y)^(-b2+n) ((-1+y)/(-1+x))^m (1-z)^(-b3-n+p) Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b1+c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^(a-c),((-1+y)/(1-x))^(-a+c)] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b1,m] Pochhammer[-a+c,m+n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-b3,-n+p] Pochhammer[1-a-b2+c,m+n])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b2+c,-m-n])+((1-x)^m (1-y)^n (1-z)^(-a-b1-b2-b3+c-m-n+p) Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2+c,-m-n+p] Pochhammer[-b1-b2-b3+c,p] Pochhammer[1-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[1-a-b1-b2-b3+c,-m-n+p])},{Abs[x]+Abs[1-y]<1&&Abs[1-y]+Abs[z]<1,(x^m (1-y)^n z^p Gamma[c] Gamma[-a-b2+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+a+b2-c,n] Pochhammer[-b2+c,m+p])+(x^m (1-y)^(-a-b2+c+n) z^p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a+c,n] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[-b2+c,m+p] Pochhammer[1-a-b2+c,n])},{Abs[x]>1+Abs[1-y]&&Abs[1-y]+Abs[z]<1,((-1)^n (-x)^(-a-n-p) x^-m (1-y)^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,m+n])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b2-c,n])+((-1)^p (-x)^-b1 x^-m (1-y)^n z^p Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] Pochhammer[a-b1,n+p] Pochhammer[b1,m] Pochhammer[1-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m-n-p] Pochhammer[1+a+b2-c,n])+((-1)^p (-x)^-b1 x^-m (1-y)^(-a-b2+c+n) z^p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[1+b1+b2-c,-n-p] Pochhammer[-a+c,n] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1+b1+b2-c,m-n-p] Pochhammer[1-a-b2+c,n])},{Abs[(-1+z)/(-1+y)]<1&&Abs[x]+Abs[1-z]<1&&Abs[x]+Abs[1-y]<1,(x^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^(-b2+n-p) (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b1,m] Pochhammer[-a+c,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-b2,n-p] Pochhammer[1-a-b3+c,p] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^(-a-b2-b3+c+n-p) (1-z)^p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a-b3+c,n-p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[-b2-b3+c,m] Pochhammer[1-a-b2-b3+c,n-p])},{Abs[(-1+y)/(-1+z)]<1&&Abs[x]+Abs[1-z]<1&&Abs[x]+Abs[1-y]<1,(x^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-b2-b3+c,m])+((-1)^n x^m (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^p Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b2,((-1+y)/(1-z))^b2] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[a+b2+b3-c,-n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[1+a+b2-c,-n+p] Pochhammer[-b2-b3+c,m])+((-1)^n x^m (1-y)^(-a-b2-b3+c+n) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^p Gamma[1+a+b3-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^(-a-b2-b3+c),((-1+y)/(1-z))^(a+b2+b3-c)] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[a+b2+b3-c,-n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-n+p] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^(-a-b2-b3+c+n) ((-1+y)/(-1+z))^p Gamma[a+b2-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b3,((-1+y)/(1-z))^b3] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a+c,n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b2+c,n+p] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((-1+y)/(-1+z))^p Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^(a-c),((-1+y)/(1-z))^(-a+c)] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a+c,n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[1-a-b2+c,n+p] Pochhammer[-b2-b3+c,m])},{Abs[1-y]<1&&Abs[x]>1+Abs[1-y]&&Abs[1-z]<1&&Abs[x]>1+Abs[1-z]&&Abs[x]>1+Abs[-1+y]&&Abs[-1+y]<1&&Abs[(-1+z)/(-1+y)]<1,((-x)^-b1 x^-m (1-y)^n (1-z)^p Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[a-b1,n+p] Pochhammer[b1,m] Pochhammer[1-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m-n-p] Pochhammer[1+a+b2+b3-c,n+p])+((-1)^n (-x)^-b1 x^-m (1-y)^(-b2+n-p) (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b1,m] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a+c,p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-b2,n-p] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1-a-b3+c,p])+((-x)^(-a-n-p) x^-m (1-y)^n (1-z)^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2+b3-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b2+b3-c,n+p]^2 Pochhammer[-a-b2-b3+c,-n-p])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a-b2-b3+c+n-p) (1-z)^p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1-a-b2-b3+c,n-p])},{Abs[1-y]<1&&Abs[x]>1+Abs[1-y]&&Abs[1-z]<1&&Abs[x]>1+Abs[1-z]&&Abs[x]>1+Abs[-1+y]&&Abs[x]>1+Abs[-1+z]&&Abs[(-1+y)/(-1+z)]<1&&Abs[-1+z]<1&&Abs[-1+y]<1,((-x)^-b1 x^-m (1-y)^n (1-z)^p Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[a-b1,n+p] Pochhammer[b1,m] Pochhammer[1-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m-n-p] Pochhammer[1+a+b2+b3-c,n+p])+((-x)^-b1 x^-m (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^p Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[1-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b2,((-1+y)/(1-z))^b2] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[a+b2+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[1+a+b2-c,-n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((-x)^-b1 x^-m (1-y)^(-a-b2-b3+c+n) ((1-z)/(-1+y))^n ((-1+y)/(-1+z))^p Gamma[1+a+b3-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^(-a-b2-b3+c),((-1+y)/(1-z))^(a+b2+b3-c)] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[a+b2+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a-b2-b3+c+n) ((-1+y)/(-1+z))^p Gamma[a+b2-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b3,((-1+y)/(1-z))^b3] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1-a-b2+c,n+p])+((-1)^n (-x)^-b1 x^-m (1-y)^(-b2+n) (1-z)^(-a-b3+c) ((-1+y)/(-1+z))^p Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^(a-c),((-1+y)/(1-z))^(-a+c)] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1-a-b2+c,n+p])+((-x)^(-a-n-p) x^-m (1-y)^n (1-z)^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2+b3-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b2+b3-c,n+p]^2 Pochhammer[-a-b2-b3+c,-n-p])},{Abs[y]>1&&Abs[y/x]<1&&Abs[z]<1,((-x)^(-a-n-p) x^-m y^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a-c,m])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p])+((-x)^-b1 x^-m (-y)^(-a+b1+m-p) y^-n z^p Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b1-b2,-m+p] Pochhammer[-a+b1+b2,m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n] Pochhammer[1+b1-c,m-p] Pochhammer[-b1+c,-m+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1,-m+p] Pochhammer[1-a+b1,m-p] Pochhammer[1+a-b1-b2,-m+n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n z^p Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m+p] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,m-p] Pochhammer[b3,p] Pochhammer[1+b1-c,m-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[-b1+c,-m+p])/(m! n! p! Gamma[a] Gamma[-b1-b2+c] Pochhammer[a-b1,-m+p] Pochhammer[1-a+b1,m-p] Pochhammer[1-a+b1+b2,m+n-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[-b1-b2+c,-m+p])},{Abs[x]>1+Abs[1-z]&&Abs[y]+Abs[1-z]<1,((-1)^p (-x)^(-a-n-p) x^-m y^n (1-z)^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,m+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b3-c,p])+((-1)^n (-x)^-b1 x^-m y^n (1-z)^p Gamma[a-b1] Gamma[c] Gamma[-a-b3+c] Pochhammer[a-b1,n+p] Pochhammer[b1,m] Pochhammer[1-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b1,m-n-p] Pochhammer[1+a+b3-c,p])+((-1)^n (-x)^-b1 x^-m y^n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[1+b1+b3-c,-n-p] Pochhammer[-a+c,p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1+b1+b3-c,m-n-p] Pochhammer[1-a-b3+c,p])},{Abs[y/x]<1&&Abs[x]>1+Abs[1-z]&&Abs[-1+z]<1&&Abs[y]>1+Abs[-1+z],((-1)^p (-x)^(-a-n-p) x^-m y^n (1-z)^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,m+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b3-c,p])+((-1)^m (-x)^-b1 x^-m (-y)^(-a+b1+m-p) y^-n (1-z)^p Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b1-b2,-m+p] Pochhammer[-a+b1+b2,m-p] Pochhammer[b3,p] Pochhammer[1+a+b3-c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1,-m+p] Pochhammer[1-a+b1,m-p] Pochhammer[1+a-b1-b2,-m+n+p] Pochhammer[1+a+b3-c,p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^p Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m+p] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,m-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[a-b1,-m+p] Pochhammer[1-a+b1,m-p] Pochhammer[1-a+b1+b2,m+n-p] Pochhammer[1+a+b3-c,p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,m-p] Pochhammer[-a+c,p] Pochhammer[-b1-b2-b3+c,-m+p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1+b1+b3-c,m-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[1-a-b3+c,p] Pochhammer[-b1-b3+c,-m+p])},{Abs[y/x]<1&&Abs[z]>1&&Abs[z/y]<1,((-x)^(-a-n-p) x^-m y^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a-c,m])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p])+((-x)^-b1 x^-m (-y)^(-a+b1+m-p) y^-n z^p Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b1-b2,-m+p] Pochhammer[-a+b1+b2,m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n] Pochhammer[1+b1-c,m-p] Pochhammer[-b1+c,-m+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1,-m+p] Pochhammer[1-a+b1,m-p] Pochhammer[1+a-b1-b2,-m+n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^(-a+b1+b2+m+n) z^-p Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b1-b2-b3,-m-n] Pochhammer[-a+b1+b2+b3,m+n] Pochhammer[1+a-c,p] Pochhammer[1+b1+b2-c,m+n] Pochhammer[-b1-b2+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a-b1-b2-b3,-m-n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-n] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+n] Pochhammer[1+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b1-b2+c,-m-n])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[-b1-b2-b3+c,-m-n])},{Abs[(-1+x)/(-1+z)]<1&&Abs[1-x]+Abs[y]<1&&Abs[y]+Abs[1-z]<1,((1-x)^m y^n (1-z)^p Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-b1-b3+c,n])+((1-x)^(-a-b1+c+m) y^n (1-z)^(-b3-m+p) Gamma[a+b1-c] Gamma[c] Pochhammer[0,-m+p] Pochhammer[1,m] Pochhammer[b2,n] Pochhammer[-a+c,m] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-b3,-m+p] Pochhammer[1-a-b1+c,m] Pochhammer[-b1-b3+c,n])+((1-x)^m y^n (1-z)^(-a-b1-b3+c-m+p) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a-b1+c,-m+p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[-b1-b3+c,n] Pochhammer[1-a-b1-b3+c,-m+p])},{Abs[(-1+z)/(-1+x)]<1&&Abs[1-x]+Abs[y]<1&&Abs[y]+Abs[1-z]<1,((1-x)^m y^n (1-z)^p Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-b1-b3+c,n])+((-1)^p (1-x)^(-a-b1+c) y^n (1-z)^(-b3+p) ((1-x)/(-1+z))^p ((-1+z)/(-1+x))^m Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b3+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b3,((-1+z)/(1-x))^b3] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[a+b1+b3-c,m-p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1+a+b3-c,m-p] Pochhammer[-b1-b3+c,n])+((-1)^p y^n (1-z)^(-a-b1-b3+c+p) ((1-x)/(-1+z))^p ((-1+z)/(-1+x))^m Gamma[1+a+b1-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^(-a-b1-b3+c),((-1+z)/(1-x))^(a+b1+b3-c)] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[a+b1+b3-c,m-p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b3-c,m-p] Pochhammer[-b1-b3+c,n])+(y^n (1-z)^(-a-b1-b3+c+p) ((-1+z)/(-1+x))^m Gamma[1+a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b1,((-1+z)/(1-x))^b1] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-a-b3+c,m+p] Pochhammer[-b1-b3+c,n])+((1-x)^(-a-b1+c) y^n (1-z)^(-b3+p) ((-1+z)/(-1+x))^m Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b1+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^(a-c),((-1+z)/(1-x))^(-a+c)] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,m+p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a-b3+c,m+p] Pochhammer[-b1-b3+c,n])},{Abs[1-x]<1&&Abs[1-z]<1&&Abs[(-1+x)/(-1+z)]<1&&Abs[1-y]<1&&Abs[(-1+z)/(-1+y)]<1,((1-x)^(-a-b1+c+m) (1-y)^(-b2+n-p) (1-z)^(-b3-m+p) Gamma[a+b1-c] Gamma[c] Pochhammer[0,n-p] Pochhammer[0,-m+p] Pochhammer[1,m] Pochhammer[1,p] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-b2,n-p] Pochhammer[1-b3,-m+p] Pochhammer[1-a-b1+c,m])+((1-x)^m (1-y)^(-b2+n-p) (1-z)^(-a-b1-b3+c-m+p) Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b1,m] Pochhammer[-a-b1+c,-m+p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1-a-b1-b3+c,-m+p])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,m+p] Pochhammer[-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b3+c,-m-p])+((1-x)^m (1-y)^(-a-b1-b2-b3+c-m+n-p) (1-z)^p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b1+b2+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m+n-p] Pochhammer[-b1-b2-b3+c,n] Pochhammer[1-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[1-a-b1-b2-b3+c,-m+n-p])},{Abs[1-x]<1&&Abs[1-z]<1&&Abs[(-1+z)/(-1+x)]<1&&Abs[1-y]<1&&Abs[(-1+x)/(-1+y)]<1,((-1)^p (1-x)^(-a-b1+c) (1-y)^(-b2+n-p) (1-z)^(-b3+p) ((1-x)/(-1+z))^p ((-1+z)/(-1+x))^m Gamma[a+b1-c] Gamma[c] Gamma[1-a-b1+c] Gamma[-a-b3+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b3,((-1+z)/(1-x))^b3] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b3,m] Pochhammer[a+b1+b3-c,m-p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1-b2,n-p] Pochhammer[1+a+b3-c,m-p])+((-1)^p (1-y)^(-b2+n-p) (1-z)^(-a-b1-b3+c+p) ((1-x)/(-1+z))^p ((-1+z)/(-1+x))^m Gamma[1+a+b1-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^(-a-b1-b3+c),((-1+z)/(1-x))^(a+b1+b3-c)] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b3,m] Pochhammer[a+b1+b3-c,m-p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a+b3-c,m-p])+((1-y)^(-b2+n-p) (1-z)^(-a-b1-b3+c+p) ((-1+z)/(-1+x))^m Gamma[1+a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b1,((-1+z)/(1-x))^b1] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b1,m] Pochhammer[-a+c,m+p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1-a-b3+c,m+p])+((1-x)^(-a-b1+c) (1-y)^(-b2+n-p) (1-z)^(-b3+p) ((-1+z)/(-1+x))^m Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b1+c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^(a-c),((-1+z)/(1-x))^(-a+c)] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b1,m] Pochhammer[-a+c,m+p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-b2,n-p] Pochhammer[1-a-b3+c,m+p])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,m+p] Pochhammer[-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b3+c,-m-p])+((1-x)^m (1-y)^(-a-b1-b2-b3+c-m+n-p) (1-z)^p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b1+b2+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m+n-p] Pochhammer[-b1-b2-b3+c,n] Pochhammer[1-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[1-a-b1-b2-b3+c,-m+n-p])},{Abs[x]+Abs[1-z]<1&&Abs[y]+Abs[1-z]<1,(x^m y^n (1-z)^p Gamma[c] Gamma[-a-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a+c] Gamma[-b3+c] Pochhammer[1+a+b3-c,p] Pochhammer[-b3+c,m+n])+(x^m y^n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a+c,p] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[-b3+c,m+n] Pochhammer[1-a-b3+c,p])},{Abs[z]>1&&Abs[z/x]<1&&Abs[y]<1,((-x)^(-a-n-p) x^-m y^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a-c,m])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p])+((-x)^-b1 x^-m y^n (-z)^(-a+b1+m-n) z^-p Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b1-b3,-m+n] Pochhammer[-a+b1+b3,m-n] Pochhammer[1+a-c,p] Pochhammer[1+b1-c,m-n] Pochhammer[-b1+c,-m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1,-m+n] Pochhammer[1-a+b1,m-n] Pochhammer[1+a-b1-b3,-m+n+p])+((-x)^-b1 x^-m y^n (-z)^-b3 z^-p Gamma[a-b1-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b3,-m+n] Pochhammer[b3,p] Pochhammer[1-a+b1+b3,m-n] Pochhammer[1+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[-b1+c,-m+n])/(m! n! p! Gamma[a] Gamma[-b1-b3+c] Pochhammer[a-b1,-m+n] Pochhammer[1-a+b1,m-n] Pochhammer[1-a+b1+b3,m-n+p] Pochhammer[1+b1+b3-c,m-n] Pochhammer[-b1-b3+c,-m+n])},{Abs[z/x]<1&&Abs[x]>1+Abs[1-y]&&Abs[-1+y]<1&&Abs[z]>1+Abs[-1+y],((-1)^n (-x)^(-a-n-p) x^-m (1-y)^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,m+n])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p] Pochhammer[1+a+b2-c,n])+((-1)^m (-x)^-b1 x^-m (1-y)^n (-z)^(-a+b1+m-n) z^-p Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b1-b3,-m+n] Pochhammer[-a+b1+b3,m-n] Pochhammer[1+a+b2-c,n+p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1,-m+n] Pochhammer[1-a+b1,m-n] Pochhammer[1+a-b1-b3,-m+n+p] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 x^-m (1-y)^n (-z)^-b3 z^-p Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b3,-m+n] Pochhammer[b3,p] Pochhammer[1-a+b1+b3,m-n] Pochhammer[1+b1+b2+b3-c,m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[a-b1,-m+n] Pochhammer[1-a+b1,m-n] Pochhammer[1-a+b1+b3,m-n+p] Pochhammer[1+a+b2-c,n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a-b2+c+n) (-z)^-b3 z^-p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-a+c,n] Pochhammer[-b1-b2-b3+c,-m+n])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1+b1+b2-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n+p] Pochhammer[1-a-b2+c,n] Pochhammer[-b1-b2+c,-m+n])},{Abs[z/x]<1&&Abs[y]>1&&Abs[y/z]<1,((-x)^(-a-n-p) x^-m y^n z^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[1+a-b1,n+p] Pochhammer[-a+b1,-n-p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a-c,m])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n+p])+((-x)^-b1 x^-m y^n (-z)^(-a+b1+m-n) z^-p Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] Pochhammer[a-b1,-m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b1-b3,-m+n] Pochhammer[-a+b1+b3,m-n] Pochhammer[1+a-c,p] Pochhammer[1+b1-c,m-n] Pochhammer[-b1+c,-m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1,-m+n] Pochhammer[1-a+b1,m-n] Pochhammer[1+a-b1-b3,-m+n+p])+((-x)^-b1 x^-m (-y)^(-a+b1+b3+m+p) y^-n (-z)^-b3 z^-p Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b3,-m+n-p] Pochhammer[1+a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,m+p] Pochhammer[1+a-c,n] Pochhammer[1+b1+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1+a-b1-b2-b3,-m+n-p] Pochhammer[1-a+b1+b3,m+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b1-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-b1-b2-b3+c,-m-p])},{Abs[x]<1&&Abs[z]<1&&Abs[y]>1,(x^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p])+(x^m (-y)^-b2 y^-n z^p Gamma[a-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b2,m+p] Pochhammer[b2,n] Pochhammer[1-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n-p])/(m! n! p! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,-m+n-p] Pochhammer[1+b2-c,-m-p] Pochhammer[-b2+c,m+p])},{Abs[y]>1+Abs[1-x]&&Abs[1-x]+Abs[z]<1,((-1)^m (1-x)^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b1-c,m+n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^p (1-x)^m (-y)^-b2 y^-n z^p Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[a-b2,m+p] Pochhammer[b2,n] Pochhammer[1-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,-m+n-p] Pochhammer[1+a+b1-c,m])+((-1)^p (1-x)^(-a-b1+c+m) (-y)^-b2 y^-n z^p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,-m-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[-a+c,m] Pochhammer[-b1-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2-c,-m+n-p] Pochhammer[1-a-b1+c,m])},{Abs[1-x]<1&&Abs[y]>1+Abs[1-x]&&Abs[1-z]<1&&Abs[y]>1+Abs[1-z]&&Abs[y]>1+Abs[-1+x]&&Abs[-1+x]<1&&Abs[(-1+z)/(-1+x)]<1,((1-x)^m (-y)^-b2 y^-n (1-z)^p Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[a-b2,m+p] Pochhammer[b2,n] Pochhammer[1-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,-m+n-p] Pochhammer[1+a+b1+b3-c,m+p])+((-1)^m (1-x)^(-b1+m-p) (-y)^-b2 y^-n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-b1,m-p] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1-a-b3+c,p])+((1-x)^m (-y)^(-a-m-p) y^-n (1-z)^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b1+b3-c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b1+b3-c,m+p]^2 Pochhammer[-a-b1-b3+c,-m-p])+((-1)^m (1-x)^(-a-b1-b3+c+m-p) (-y)^-b2 y^-n (1-z)^p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1-a-b1-b3+c,m-p])},{Abs[1-x]<1&&Abs[y]>1+Abs[1-x]&&Abs[1-z]<1&&Abs[y]>1+Abs[1-z]&&Abs[y]>1+Abs[-1+x]&&Abs[y]>1+Abs[-1+z]&&Abs[(-1+x)/(-1+z)]<1&&Abs[-1+z]<1&&Abs[-1+x]<1,((1-x)^m (-y)^-b2 y^-n (1-z)^p Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[a-b2,m+p] Pochhammer[b2,n] Pochhammer[1-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,-m+n-p] Pochhammer[1+a+b1+b3-c,m+p])+((1-x)^(-b1+m) (-y)^-b2 y^-n (1-z)^(-a-b3+c) ((1-z)/(-1+x))^m ((-1+x)/(-1+z))^p Gamma[a+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b3+c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b1,((-1+x)/(1-z))^b1] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[a+b1+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Gamma[1-a-b1-b3+c] Pochhammer[1+a+b1-c,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((1-x)^(-a-b1-b3+c+m) (-y)^-b2 y^-n ((1-z)/(-1+x))^m ((-1+x)/(-1+z))^p Gamma[1+a+b3-c] Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b3+c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^(-a-b1-b3+c),((-1+x)/(1-z))^(a+b1+b3-c)] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[a+b1+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((-1)^m (1-x)^(-a-b1-b3+c+m) (-y)^-b2 y^-n ((-1+x)/(-1+z))^p Gamma[a+b1-c] Gamma[1+a+b3-c] Gamma[c] Gamma[-a-b3+c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b3,((-1+x)/(1-z))^b3] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1-a-b1+c,m+p])+((-1)^m (1-x)^(-b1+m) (-y)^-b2 y^-n (1-z)^(-a-b3+c) ((-1+x)/(-1+z))^p Gamma[a+b1-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b3+c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^(a-c),((-1+x)/(1-z))^(-a+c)] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a+c,m+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1-a-b1+c,m+p])+((1-x)^m (-y)^(-a-m-p) y^-n (1-z)^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b1+b3-c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b1+b3-c,m+p]^2 Pochhammer[-a-b1-b3+c,-m-p])},{Abs[x]>1&&Abs[x/y]<1&&Abs[z]<1,(x^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p])+((-x)^(-a+b2+n-p) x^-m (-y)^-b2 y^-n z^p Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n+p] Pochhammer[1+a-b1-b2,-n+p] Pochhammer[b2,n] Pochhammer[-a+b1+b2,n-p] Pochhammer[b3,p] Pochhammer[1+a-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2,-n+p] Pochhammer[1+a-b1-b2,m-n+p] Pochhammer[1-a+b2,n-p])+((-x)^-b1 x^-m (-y)^-b2 y^-n z^p Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-n+p] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,n-p] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2+c] Pochhammer[a-b2,-n+p] Pochhammer[1-a+b2,n-p] Pochhammer[1-a+b1+b2,m+n-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[-b1-b2+c,-n+p])},{Abs[y]>1+Abs[1-z]&&Abs[x]+Abs[1-z]<1,((-1)^p x^m (-y)^(-a-m-p) y^-n (1-z)^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b3-c,n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b3-c,p])+((-1)^m x^m (-y)^-b2 y^-n (1-z)^p Gamma[a-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[a-b2,m+p] Pochhammer[b2,n] Pochhammer[1-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b2,-m+n-p] Pochhammer[1+a+b3-c,p])+((-1)^m x^m (-y)^-b2 y^-n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[1+b2+b3-c,-m-p] Pochhammer[-a+c,p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1+b2+b3-c,-m+n-p] Pochhammer[1-a-b3+c,p])},{Abs[x/y]<1&&Abs[y]>1+Abs[1-z]&&Abs[-1+z]<1&&Abs[x]>1+Abs[-1+z],((-1)^n (-x)^(-a+b2+n-p) x^-m (-y)^-b2 y^-n (1-z)^p Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n+p] Pochhammer[1+a-b1-b2,-n+p] Pochhammer[b2,n] Pochhammer[-a+b1+b2,n-p] Pochhammer[b3,p] Pochhammer[1+a+b3-c,m+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2,-n+p] Pochhammer[1+a-b1-b2,m-n+p] Pochhammer[1-a+b2,n-p] Pochhammer[1+a+b3-c,p])+((-1)^p x^m (-y)^(-a-m-p) y^-n (1-z)^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b3-c,n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b3-c,p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^p Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-n+p] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,n-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[a-b2,-n+p] Pochhammer[1-a+b2,n-p] Pochhammer[1-a+b1+b2,m+n-p] Pochhammer[1+a+b3-c,p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^(-a-b3+c+p) Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,n-p] Pochhammer[-a+c,p] Pochhammer[-b1-b2-b3+c,-n+p])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[1-a-b3+c,p] Pochhammer[-b2-b3+c,-n+p])},{Abs[x/y]<1&&Abs[z]>1&&Abs[z/x]<1,(x^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p])+((-x)^(-a+b2+n-p) x^-m (-y)^-b2 y^-n z^p Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n+p] Pochhammer[1+a-b1-b2,-n+p] Pochhammer[b2,n] Pochhammer[-a+b1+b2,n-p] Pochhammer[b3,p] Pochhammer[1+a-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2,-n+p] Pochhammer[1+a-b1-b2,m-n+p] Pochhammer[1-a+b2,n-p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^(-a+b1+b2+m+n) z^-p Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b1-b2-b3,-m-n] Pochhammer[-a+b1+b2+b3,m+n] Pochhammer[1+a-c,p] Pochhammer[1+b1+b2-c,m+n] Pochhammer[-b1-b2+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a-b1-b2-b3,-m-n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-n] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+n] Pochhammer[1+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b1-b2+c,-m-n])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[-b1-b2-b3+c,-m-n])},{Abs[1-y]<1&&Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[1-x]<1&&Abs[(-1+z)/(-1+x)]<1,((1-x)^(-b1+m-p) (1-y)^(-a-b2+c+n) (1-z)^(-b3-n+p) Gamma[a+b2-c] Gamma[c] Pochhammer[0,m-p] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[1,p] Pochhammer[-a+c,n] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-b1,m-p] Pochhammer[1-b3,-n+p] Pochhammer[1-a-b2+c,n])+((1-x)^(-b1+m-p) (1-y)^n (1-z)^(-a-b2-b3+c-n+p) Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b2,n] Pochhammer[-a-b2+c,-n+p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1-a-b2-b3+c,-n+p])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,n+p] Pochhammer[-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b2-b3+c,-n-p])+((1-x)^(-a-b1-b2-b3+c+m-n-p) (1-y)^n (1-z)^p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,m-n-p] Pochhammer[-b1-b2-b3+c,m] Pochhammer[1-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[1-a-b1-b2-b3+c,m-n-p])},{Abs[1-y]<1&&Abs[1-z]<1&&Abs[(-1+z)/(-1+y)]<1&&Abs[1-x]<1&&Abs[(-1+y)/(-1+x)]<1,((-1)^p (1-x)^(-b1+m-p) (1-y)^(-a-b2+c) (1-z)^(-b3+p) ((1-y)/(-1+z))^p ((-1+z)/(-1+y))^n Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] Gamma[-a-b3+c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b3,((-1+z)/(1-y))^b3] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b3,n] Pochhammer[a+b2+b3-c,n-p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Gamma[1-a-b2-b3+c] Pochhammer[1-b1,m-p] Pochhammer[1+a+b3-c,n-p])+((-1)^p (1-x)^(-b1+m-p) (1-z)^(-a-b2-b3+c+p) ((1-y)/(-1+z))^p ((-1+z)/(-1+y))^n Gamma[1+a+b2-c] Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Gamma[-a-b3+c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^(-a-b2-b3+c),((-1+z)/(1-y))^(a+b2+b3-c)] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b3,n] Pochhammer[a+b2+b3-c,n-p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a+b3-c,n-p])+((1-x)^(-b1+m-p) (1-z)^(-a-b2-b3+c+p) ((-1+z)/(-1+y))^n Gamma[1+a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[-a-b2+c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b2,((-1+z)/(1-y))^b2] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b2,n] Pochhammer[-a+c,n+p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b3] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1-a-b3+c,n+p])+((1-x)^(-b1+m-p) (1-y)^(-a-b2+c) (1-z)^(-b3+p) ((-1+z)/(-1+y))^n Gamma[a+b2-c] Gamma[a+b3-c] Gamma[c] Gamma[1-a-b2+c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^(a-c),((-1+z)/(1-y))^(-a+c)] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b2,n] Pochhammer[-a+c,n+p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Pochhammer[1-b1,m-p] Pochhammer[1-a-b3+c,n+p])+((1-x)^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2+b3-c,n+p] Pochhammer[-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b2-b3+c,-n-p])+((1-x)^(-a-b1-b2-b3+c+m-n-p) (1-y)^n (1-z)^p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,m-n-p] Pochhammer[-b1-b2-b3+c,m] Pochhammer[1-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[1-a-b1-b2-b3+c,m-n-p])},{Abs[z]>1&&Abs[z/y]<1&&Abs[x]<1,(x^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p])+(x^m (-y)^-b2 y^-n (-z)^(-a+b2-m+n) z^-p Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b2,m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b2-b3,m-n] Pochhammer[-a+b2+b3,-m+n] Pochhammer[1+a-c,p] Pochhammer[1+b2-c,-m+n] Pochhammer[-b2+c,m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b2,m-n] Pochhammer[1-a+b2,-m+n] Pochhammer[1+a-b2-b3,m-n+p])+(x^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,-m+n] Pochhammer[1+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[-b2+c,m-n])/(m! n! p! Gamma[a] Gamma[-b2-b3+c] Pochhammer[a-b2,m-n] Pochhammer[1-a+b2,-m+n] Pochhammer[1-a+b2+b3,-m+n+p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-b2-b3+c,m-n])},{Abs[z/y]<1&&Abs[y]>1+Abs[1-x]&&Abs[-1+x]<1&&Abs[z]>1+Abs[-1+x],((-1)^m (1-x)^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a+b1-c,m+n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^n (1-x)^m (-y)^-b2 y^-n (-z)^(-a+b2-m+n) z^-p Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b2,m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b2-b3,m-n] Pochhammer[-a+b2+b3,-m+n] Pochhammer[1+a+b1-c,m+p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b2,m-n] Pochhammer[1-a+b2,-m+n] Pochhammer[1+a-b2-b3,m-n+p] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,-m+n] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[a-b2,m-n] Pochhammer[1-a+b2,-m+n] Pochhammer[1-a+b2+b3,-m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^(-a-b1+c+m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,m-n])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1+b1+b2-c,-m+n] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[1-a-b1+c,m] Pochhammer[-b1-b2+c,m-n])},{Abs[z/y]<1&&Abs[x]>1&&Abs[x/z]<1,(x^m (-y)^(-a-m-p) y^-n z^p Gamma[-a+b2] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[1+a-b2,m+p] Pochhammer[-a+b2,-m-p] Pochhammer[b3,p] Pochhammer[1+a-c,n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n+p])+(x^m (-y)^-b2 y^-n (-z)^(-a+b2-m+n) z^-p Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b2,m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b2-b3,m-n] Pochhammer[-a+b2+b3,-m+n] Pochhammer[1+a-c,p] Pochhammer[1+b2-c,-m+n] Pochhammer[-b2+c,m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b2,m-n] Pochhammer[1-a+b2,-m+n] Pochhammer[1+a-b2-b3,m-n+p])+((-x)^(-a+b2+b3+n+p) x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n-p] Pochhammer[1+a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,n+p] Pochhammer[1+a-c,m] Pochhammer[1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1+a-b1-b2-b3,m-n-p] Pochhammer[1-a+b2+b3,n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,n+p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,-n-p])},{Abs[x]<1&&Abs[y]<1&&Abs[z]>1,(x^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a-c,p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p])+(x^m y^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b3,m+n] Pochhammer[b3,p] Pochhammer[1-a+b3,-m-n] Pochhammer[1+b3-c,-m-n+p])/(m! n! p! Gamma[a] Gamma[-b3+c] Pochhammer[1-a+b3,-m-n+p] Pochhammer[1+b3-c,-m-n] Pochhammer[-b3+c,m+n])},{Abs[z]>1+Abs[1-x]&&Abs[1-x]+Abs[y]<1,((-1)^m (1-x)^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a+b1-c,m+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^n (1-x)^m y^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b3,m+n] Pochhammer[b3,p] Pochhammer[1-a+b3,-m-n] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b3,-m-n+p] Pochhammer[1+a+b1-c,m])+((-1)^n (1-x)^(-a-b1+c+m) y^n (-z)^-b3 z^-p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,-m-n] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[-a+c,m] Pochhammer[-b1-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1+b1+b3-c,-m-n+p] Pochhammer[1-a-b1+c,m])},{Abs[1-x]<1&&Abs[z]>1+Abs[1-x]&&Abs[1-y]<1&&Abs[z]>1+Abs[1-y]&&Abs[z]>1+Abs[-1+x]&&Abs[-1+x]<1&&Abs[(-1+y)/(-1+x)]<1,((1-x)^m (1-y)^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b3,m+n] Pochhammer[b3,p] Pochhammer[1-a+b3,-m-n] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,-m-n+p] Pochhammer[1+a+b1+b2-c,m+n])+((-1)^m (1-x)^(-b1+m-n) (1-y)^(-a-b2+c+n) (-z)^-b3 z^-p Gamma[a+b2-c] Gamma[c] Pochhammer[0,m-n] Pochhammer[1,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a+c,n])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-b1,m-n] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1-a-b2+c,n])+((1-x)^m (1-y)^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a+b1+b2-c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p] Pochhammer[1+a+b1+b2-c,m+n]^2 Pochhammer[-a-b1-b2+c,-m-n])+((-1)^m (1-x)^(-a-b1-b2+c+m-n) (1-y)^n (-z)^-b3 z^-p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a-b2+c,m-n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1-a-b1-b2+c,m-n])},{Abs[1-x]<1&&Abs[z]>1+Abs[1-x]&&Abs[1-y]<1&&Abs[z]>1+Abs[1-y]&&Abs[z]>1+Abs[-1+x]&&Abs[z]>1+Abs[-1+y]&&Abs[(-1+x)/(-1+y)]<1&&Abs[-1+y]<1&&Abs[-1+x]<1,((1-x)^m (1-y)^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b3,m+n] Pochhammer[b3,p] Pochhammer[1-a+b3,-m-n] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,-m-n+p] Pochhammer[1+a+b1+b2-c,m+n])+((1-x)^(-b1+m) (1-y)^(-a-b2+c) ((1-y)/(-1+x))^m ((-1+x)/(-1+y))^n (-z)^-b3 z^-p Gamma[a+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[1-a-b2+c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b1,((-1+x)/(1-y))^b1] Pochhammer[b1,n] Pochhammer[b3,p] Pochhammer[a+b1+b2-c,-m+n] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Gamma[1-a-b1-b2+c] Pochhammer[1+a+b1-c,-m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((1-x)^(-a-b1-b2+c+m) ((1-y)/(-1+x))^m ((-1+x)/(-1+y))^n (-z)^-b3 z^-p Gamma[1+a+b2-c] Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Gamma[-a-b2+c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^(-a-b1-b2+c),((-1+x)/(1-y))^(a+b1+b2-c)] Pochhammer[b1,n] Pochhammer[b3,p] Pochhammer[a+b1+b2-c,-m+n] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,-m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((-1)^m (1-x)^(-a-b1-b2+c+m) ((-1+x)/(-1+y))^n (-z)^-b3 z^-p Gamma[a+b1-c] Gamma[1+a+b2-c] Gamma[c] Gamma[-a-b2+c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b2,((-1+x)/(1-y))^b2] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a+c,m+n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-c] Gamma[-a+c] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1-a-b1+c,m+n])+((-1)^m (1-x)^(-b1+m) (1-y)^(-a-b2+c) ((-1+x)/(-1+y))^n (-z)^-b3 z^-p Gamma[a+b1-c] Gamma[a+b2-c] Gamma[c] Gamma[1-a-b2+c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^(a-c),((-1+x)/(1-y))^(-a+c)] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a+c,m+n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1-a-b1+c,m+n])+((1-x)^m (1-y)^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a+b1+b2-c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p] Pochhammer[1+a+b1+b2-c,m+n]^2 Pochhammer[-a-b1-b2+c,-m-n])},{Abs[x]>1&&Abs[x/z]<1&&Abs[y]<1,(x^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a-c,p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p])+((-x)^(-a+b3-n+p) x^-m y^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b3,m+n-p] Pochhammer[1+a-b1-b3,n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b3,-n+p] Pochhammer[1+a-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b3,n-p] Pochhammer[1+a-b1-b3,m+n-p] Pochhammer[1-a+b3,-n+p])+((-x)^-b1 x^-m y^n (-z)^-b3 z^-p Gamma[a-b1-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b3,n-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b3,-n+p] Pochhammer[1+b3-c,-n+p] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[-b1-b3+c] Pochhammer[a-b3,n-p] Pochhammer[1-a+b3,-n+p] Pochhammer[1-a+b1+b3,m-n+p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[-b1-b3+c,n-p])},{Abs[z]>1+Abs[1-y]&&Abs[x]+Abs[1-y]<1,((-1)^n x^m (1-y)^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a+b2-c,n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p] Pochhammer[1+a+b2-c,n])+((-1)^m x^m (1-y)^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b3,m+n] Pochhammer[b3,p] Pochhammer[1-a+b3,-m-n] Pochhammer[1+b2+b3-c,-m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b3,-m-n+p] Pochhammer[1+a+b2-c,n])+((-1)^m x^m (1-y)^(-a-b2+c+n) (-z)^-b3 z^-p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m-n] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a+c,n] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1+b2+b3-c,-m-n+p] Pochhammer[1-a-b2+c,n])},{Abs[x/z]<1&&Abs[y]>1&&Abs[y/x]<1,(x^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a-c,p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p])+((-x)^(-a+b3-n+p) x^-m y^n (-z)^-b3 z^-p Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b3,m+n-p] Pochhammer[1+a-b1-b3,n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b3,-n+p] Pochhammer[1+a-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b3,n-p] Pochhammer[1+a-b1-b3,m+n-p] Pochhammer[1-a+b3,-n+p])+((-x)^-b1 x^-m (-y)^(-a+b1+b3+m+p) y^-n (-z)^-b3 z^-p Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b3,-m+n-p] Pochhammer[1+a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,m+p] Pochhammer[1+a-c,n] Pochhammer[1+b1+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1+a-b1-b2-b3,-m+n-p] Pochhammer[1-a+b1+b3,m+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b1-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-b1-b2-b3+c,-m-p])},{Abs[y]>1&&Abs[y/z]<1&&Abs[x]<1,(x^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a-c,p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p])+(x^m (-y)^(-a+b3-m+p) y^-n (-z)^-b3 z^-p Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b3,m+n-p] Pochhammer[1+a-b2-b3,m-p] Pochhammer[b3,p] Pochhammer[-a+b2+b3,-m+p] Pochhammer[1+a-c,n] Pochhammer[1+b3-c,-m+p] Pochhammer[-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b3,m-p] Pochhammer[1+a-b2-b3,m+n-p] Pochhammer[1-a+b3,-m+p])+(x^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-p] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,-m+p] Pochhammer[1+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[-b2-b3+c] Pochhammer[a-b3,m-p] Pochhammer[1-a+b3,-m+p] Pochhammer[1-a+b2+b3,-m+n+p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-b2-b3+c,m-p])},{Abs[y/z]<1&&Abs[z]>1+Abs[1-x]&&Abs[-1+x]<1&&Abs[y]>1+Abs[-1+x],((-1)^p (1-x)^m (-y)^(-a+b3-m+p) y^-n (-z)^-b3 z^-p Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b3,m+n-p] Pochhammer[1+a-b2-b3,m-p] Pochhammer[b3,p] Pochhammer[-a+b2+b3,-m+p] Pochhammer[1+a+b1-c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b3,m-p] Pochhammer[1+a-b2-b3,m+n-p] Pochhammer[1-a+b3,-m+p] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a+b1-c,m+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-p] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,-m+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[a-b3,m-p] Pochhammer[1-a+b3,-m+p] Pochhammer[1-a+b2+b3,-m+n+p] Pochhammer[1+a+b1-c,m])+((-1)^m (1-x)^(-a-b1+c+m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a+c,m] Pochhammer[-b1-b2-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1+b1+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[1-a-b1+c,m] Pochhammer[-b1-b3+c,m-p])},{Abs[y/z]<1&&Abs[x]>1&&Abs[x/y]<1,(x^m y^n (-z)^(-a-m-n) z^-p Gamma[-a+b3] Gamma[c] Pochhammer[a,m+n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a-b3,m+n] Pochhammer[-a+b3,-m-n] Pochhammer[1+a-c,p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+n+p])+(x^m (-y)^(-a+b3-m+p) y^-n (-z)^-b3 z^-p Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b3,m+n-p] Pochhammer[1+a-b2-b3,m-p] Pochhammer[b3,p] Pochhammer[-a+b2+b3,-m+p] Pochhammer[1+a-c,n] Pochhammer[1+b3-c,-m+p] Pochhammer[-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b3,m-p] Pochhammer[1+a-b2-b3,m+n-p] Pochhammer[1-a+b3,-m+p])+((-x)^(-a+b2+b3+n+p) x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n-p] Pochhammer[1+a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,n+p] Pochhammer[1+a-c,m] Pochhammer[1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1+a-b1-b2-b3,m-n-p] Pochhammer[1-a+b2+b3,n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,n+p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1-a+b1+b2+b3,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,-n-p])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Pochhammer[c,m+n+p])},{Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1,((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1+c] Pochhammer[1-a+b1,m] Pochhammer[-b1+c,n+p])+((1/(1-x))^m (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[-b1+c,n+p])},{Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1&&Abs[x/(-1+x)]>1,((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n-p] Pochhammer[1+a+b1-c,-n-p] Pochhammer[-a-b1+c,n+p])/(m! n! p! Gamma[-a+c] Gamma[-b1+c] Pochhammer[1+b1-c,-n-p] Pochhammer[1+a+b1-c,m-n-p] Pochhammer[-b1+c,n+p])+((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n+p])},{Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n] Pochhammer[-b1-b2+c,p])+((1/(1-x))^m (1-x)^-a (1/(1-y))^(-m+n) (1-z)^-b3 (z/(-1+z))^p Gamma[-a+b1] Gamma[c] Pochhammer[0,-m+n] Pochhammer[1,m] Pochhammer[a,m] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1-b2,-m+n] Pochhammer[-b1-b2+c,p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^(-m+n) (1-y)^(-a+b1) (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n] Pochhammer[-b1-b2+c,p])},{Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2+c] Pochhammer[1-a+b1+b2,m+n] Pochhammer[-b1-b2+c,p])+((1-x)^-a (1/(1-y))^n ((-1+x)/(-1+y))^m (1-z)^-b3 (z/(-1+z))^p Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-a,((-1+x)/(1-y))^a] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[-b1-b2+c,p])+((1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((-1+x)/(-1+y))^m (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b1,((-1+x)/(1-y))^b1] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[-b1-b2+c,p])+((-1)^n (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^m (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^(a-b1-b2),((-1+x)/(1-y))^(-a+b1+b2)] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m-n] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b2,m-n] Pochhammer[-b1-b2+c,p])+((-1)^n (1-x)^-a (1/(1-y))^n ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^m (1-z)^-b3 (z/(-1+z))^p Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b2,((-1+x)/(1-y))^b2] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m-n] Pochhammer[b3,p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-b1-b2] Gamma[-a+c] Pochhammer[1-a+b2,m-n] Pochhammer[-b1-b2+c,p])},{Abs[1-x/(-1+x)]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-n] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+n] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1/(1-x))^m (1-x)^-a (1/(1-y))^(-m+n) (1/(1-z))^(-n+p) Gamma[-a+b1] Gamma[c] Pochhammer[0,-m+n] Pochhammer[0,-n+p] Pochhammer[1,m] Pochhammer[1,n] Pochhammer[a,m] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1-b2,-m+n] Pochhammer[1-b3,-n+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^(-m+n) (1-y)^(-a+b1) (1/(1-z))^(-n+p) Gamma[a-b1] Gamma[-a+b1+b2] Gamma[c] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[a-b1,-m+n] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1-b2,-m+n] Pochhammer[1-b3,-n+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(-m-n+p) (1-z)^(-a+b1+b2) Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b1-b2-b3,-m-n] Pochhammer[-a+b1+b2+b3,m+n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a-b1-b2-b3,-m-n+p])},{Abs[1-x/(-1+x)]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-n] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+n] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1-x)^-a (1/(1-y))^n ((-1+x)/(-1+y))^m (1/(1-z))^(-n+p) Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-a,((-1+x)/(1-y))^a] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[1-b3,-n+p])+((1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((-1+x)/(-1+y))^m (1/(1-z))^(-n+p) Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b1,((-1+x)/(1-y))^b1] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[a,m+n] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,m+n] Pochhammer[1-b3,-n+p])+((-1)^n (1-x)^-b1 (1/(1-y))^n (1-y)^(-a+b1) ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^m (1/(1-z))^(-n+p) Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^(a-b1-b2),((-1+x)/(1-y))^(-a+b1+b2)] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m-n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b2,m-n] Pochhammer[1-b3,-n+p])+((-1)^n (1-x)^-a (1/(1-y))^n ((1-y)/(-1+x))^n ((-1+x)/(-1+y))^m (1/(1-z))^(-n+p) Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b2] Gamma[c] If[Re[x]-Re[y]>0,((1-y)/(-1+x))^-b2,((-1+x)/(1-y))^b2] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[b2,m] Pochhammer[-a+b1+b2,m-n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-b1-b2] Gamma[-a+c] Pochhammer[1-a+b2,m-n] Pochhammer[1-b3,-n+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(-m-n+p) (1-z)^(-a+b1+b2) Gamma[a-b1-b2] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b2,-m-n+p] Pochhammer[b2,n] Pochhammer[1+a-b1-b2-b3,-m-n] Pochhammer[-a+b1+b2+b3,m+n] Pochhammer[-b1-b2-b3+c,p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a-b1-b2-b3,-m-n+p])},{Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b2+c] Pochhammer[1-a+b2,n] Pochhammer[-b2+c,m+p])+((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-a (1-z)^-b3 (z/(-1+z))^p Gamma[-a+b2] Gamma[c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[-b2+c,m+p])},{Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-y/(-1+y)]+Abs[z/(-1+z)]<1,((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a-b2] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1-c,-n-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b1-c,m-n-p])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[1-a+b2,m+n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a+b2,n] Pochhammer[1-a-b1+c,m+n+p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-a (1-z)^-b3 (z/(-1+z))^p Gamma[-a+b2] Gamma[c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[1+b1+b2-c,-n-p] Pochhammer[-b1-b2+c,n+p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1+b1+b2-c,m-n-p])},{Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1&&Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,n+p] Pochhammer[-b2-b3+c,m])+((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^(n-p) (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a-b3,p] Pochhammer[-b2-b3+c,m])+((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^(n-p) (1-y)^(-a+b3) (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b3,n-p] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,n-p] Pochhammer[-b2-b3+c,m])},{Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b2-b3+c] Pochhammer[1-a+b2+b3,n+p] Pochhammer[-b2-b3+c,m])+((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-z)^-a ((-1+z)/(-1+y))^p Gamma[-a+b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-a,((-1+z)/(1-y))^a] Pochhammer[a,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2,n+p] Pochhammer[-b2-b3+c,m])+((1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((-1+z)/(-1+y))^p Gamma[-a+b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b3,((-1+z)/(1-y))^b3] Pochhammer[a,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n+p] Pochhammer[-b2-b3+c,m])+((-1)^n (1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-z)^-a ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^p Gamma[a-b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b2,((-1+z)/(1-y))^b2] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[-a+b2+b3,-n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[1+a-b2-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,-n+p] Pochhammer[-b2-b3+c,m])+((-1)^n (1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^p Gamma[a-b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b2+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^(a-b2-b3),((-1+z)/(1-y))^(-a+b2+b3)] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[-a+b2+b3,-n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,-n+p] Pochhammer[-b2-b3+c,m])},{Abs[1-y/(-1+y)]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]>1+Abs[-1+y/(-1+y)]&&Abs[-1+y/(-1+y)]<1&&Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1,((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^(n-p) (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a-b3,p] Pochhammer[1+b1+b2+b3-c,m-n])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^(n-p) (1-y)^(-a+b3) (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b3,n-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1-c,-n-p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1+a+b1-c,m-n-p])+((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,m+n+p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p]^2 Pochhammer[1-a-b1+c,m+n+p])},{Abs[1-y/(-1+y)]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[1-z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]>1+Abs[-1+y/(-1+y)]&&Abs[x/(-1+x)]>1+Abs[-1+z/(-1+z)]&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[-1+z/(-1+z)]<1&&Abs[-1+y/(-1+y)]<1,((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-z)^-a ((-1+z)/(-1+y))^p Gamma[-a+b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-a,((-1+z)/(1-y))^a] Pochhammer[a,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((-1+z)/(-1+y))^p Gamma[-a+b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b3,((-1+z)/(1-y))^b3] Pochhammer[a,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-z)^-a ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^p Gamma[a-b2] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^-b2,((-1+z)/(1-y))^b2] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[-a+b2+b3,-n+p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[1+a-b2-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,-n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^(-a+b3) (1-z)^-b3 ((1-y)/(-1+z))^n ((-1+z)/(-1+y))^p Gamma[a-b2] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b2+b3] Gamma[c] If[-Re[y]+Re[z]>0,((1-y)/(-1+z))^(a-b2-b3),((-1+z)/(1-y))^(-a+b2+b3)] Pochhammer[b1,m] Pochhammer[b2,p] Pochhammer[-a+b2+b3,-n+p] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,-n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1-c,-n-p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1+a+b1-c,m-n-p])+((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a+b2+b3,m+n+p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p]^2 Pochhammer[1-a-b1+c,m+n+p])},{Abs[y/(-1+y)]>1&&Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[z/(-1+z)]<1,((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1-c,m-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[1+a+b1+b2-c,m-p] Pochhammer[-b1+c,-m+p] Pochhammer[-a-b1-b2+c,-m+p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b1-c,m-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[1+a+b1+b2-c,m+n-p] Pochhammer[-a-b1+c,-m+p] Pochhammer[-b1-b2+c,-m+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^(-a-b1+c) (-(y/(-1+y)))^(m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1-c,m-p] Pochhammer[a+b1+b2-c,m-p] Pochhammer[-b1+c,-m+p] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b2+c,-m+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,m-p] Pochhammer[-a-b1+c,-m+p] Pochhammer[1-a-b1-b2+c,-m+n+p])},{Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1-c,-n-p] Pochhammer[1+b1+b3-c,m-n] Pochhammer[-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b1-c,m-n-p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a+b3,m+p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a+b3,p] Pochhammer[1-a-b1+c,m+n+p])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[1+b1+b3-c,-n-p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,p] Pochhammer[1+b1+b3-c,m-n-p])},{Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[x/(-1+x)]>1+Abs[1-z/(-1+z)]&&Abs[-1+z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[-1+z/(-1+z)],((-1)^p (1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a+b3,m+p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a+b3,p] Pochhammer[1-a-b1+c,m+n+p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2-c,m-p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2+c,-m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b1-c,m-p] Pochhammer[1+a+b1+b2-c,m+n-p] Pochhammer[-a-b1+c,-m+p])+((-1)^m (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^(-a-b1+c) (-(y/(-1+y)))^(m-p) (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1-a+b3,n+p] Pochhammer[a+b1+b2-c,m-p] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b2+c,-m+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b1-c,m-p] Pochhammer[-a-b1+c,-m+p] Pochhammer[1-a-b1-b2+c,-m+n+p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,m-p] Pochhammer[-b1-b2-b3+c,-m+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,p] Pochhammer[1+b1+b3-c,m-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[-b1-b3+c,-m+p])},{Abs[((-1+x) y)/(x (-1+y))]<1&&Abs[z/(-1+z)]>1&&Abs[((-1+y) z)/(y (-1+z))]<1,((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^(-a-b1+c) (-(y/(-1+y)))^(m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1-c,m-p] Pochhammer[a+b1+b2-c,m-p] Pochhammer[-b1+c,-m+p] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b2+c,-m+p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1-c,m-p] Pochhammer[-a-b1+c,-m+p] Pochhammer[1-a-b1-b2+c,-m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,m+n] Pochhammer[-b1-b2+c,-m-n] Pochhammer[-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[-b1-b2-b3+c,-m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^(-a-b1-b2+c) (-(z/(-1+z)))^(m+n) (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m+n] Pochhammer[a+b1+b2+b3-c,m+n] Pochhammer[-b1-b2+c,-m-n] Pochhammer[-a-b1-b2+c,-m-n+p] Pochhammer[1-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[1-a-b1-b2-b3+c,-m-n+p])},{Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m+p] Pochhammer[-b1-b3+c,n])+((1/(1-x))^m (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(-m+p) Gamma[-a+b1] Gamma[c] Pochhammer[0,-m+p] Pochhammer[1,m] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1-b3,-m+p] Pochhammer[-b1-b3+c,n])+((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^(-m+p) (1-z)^(-a+b1) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] Pochhammer[a-b1,-m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1-b3,-m+p] Pochhammer[-b1-b3+c,n])},{Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b3+c] Pochhammer[1-a+b1+b3,m+p] Pochhammer[-b1-b3+c,n])+((1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p ((-1+x)/(-1+z))^m Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-a,((-1+x)/(1-z))^a] Pochhammer[a,m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+p] Pochhammer[-b1-b3+c,n])+((1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^(-a+b1) ((-1+x)/(-1+z))^m Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b1,((-1+x)/(1-z))^b1] Pochhammer[a,m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,m+p] Pochhammer[-b1-b3+c,n])+((-1)^p (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^(-a+b1) ((1-z)/(-1+x))^p ((-1+x)/(-1+z))^m Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^(a-b1-b3),((-1+x)/(1-z))^(-a+b1+b3)] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[-a+b1+b3,m-p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b3,m-p] Pochhammer[-b1-b3+c,n])+((-1)^p (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p ((1-z)/(-1+x))^p ((-1+x)/(-1+z))^m Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b3,((-1+x)/(1-z))^b3] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[-a+b1+b3,m-p] Pochhammer[-b1-b3+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-b1-b3] Gamma[-a+c] Pochhammer[1-a+b3,m-p] Pochhammer[-b1-b3+c,n])},{Abs[1-x/(-1+x)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1/(1-x))^m (1-x)^-a (1/(1-y))^(n-p) (1/(1-z))^(-m+p) Gamma[-a+b1] Gamma[c] Pochhammer[0,n-p] Pochhammer[0,-m+p] Pochhammer[1,m] Pochhammer[1,p] Pochhammer[a,m] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1-b2,n-p] Pochhammer[1-b3,-m+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^(n-p) (1/(1-z))^(-m+p) (1-z)^(-a+b1) Gamma[a-b1] Gamma[-a+b1+b3] Gamma[c] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[a-b1,-m+p] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a-b1-b3,-m+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^(-m+n-p) (1-y)^(-a+b1+b3) (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b3,-m+n-p] Pochhammer[1+a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,m+p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1+a-b1-b2-b3,-m+n-p] Pochhammer[1-a+b1+b3,m+p])},{Abs[1-x/(-1+x)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1&&Abs[1-y/(-1+y)]<1&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,m+p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1-x)^-a (1/(1-y))^(n-p) (1/(1-z))^p ((-1+x)/(-1+z))^m Gamma[1+a-b1] Gamma[-a+b1] Gamma[-a+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-a,((-1+x)/(1-z))^a] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[a,m+p] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a-b3,m+p])+((1-x)^-b1 (1/(1-y))^(n-p) (1/(1-z))^p (1-z)^(-a+b1) ((-1+x)/(-1+z))^m Gamma[a-b1] Gamma[1-a+b1] Gamma[-a+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b1,((-1+x)/(1-z))^b1] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[a,m+p] Pochhammer[b1,m] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1+a-b3,m+p])+((-1)^p (1-x)^-b1 (1/(1-y))^(n-p) (1/(1-z))^p (1-z)^(-a+b1) ((1-z)/(-1+x))^p ((-1+x)/(-1+z))^m Gamma[a-b1] Gamma[1-a+b1] Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^(a-b1-b3),((-1+x)/(1-z))^(-a+b1+b3)] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b3,m] Pochhammer[-a+b1+b3,m-p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1-a+b3,m-p])+((-1)^p (1-x)^-a (1/(1-y))^(n-p) (1/(1-z))^p ((1-z)/(-1+x))^p ((-1+x)/(-1+z))^m Gamma[1+a-b1] Gamma[-a+b1] Gamma[a-b3] Gamma[c] If[Re[x]-Re[z]>0,((1-z)/(-1+x))^-b3,((-1+x)/(1-z))^b3] Pochhammer[0,n-p] Pochhammer[1,p] Pochhammer[b3,m] Pochhammer[-a+b1+b3,m-p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[1+a-b1-b3] Gamma[-a+c] Pochhammer[1-b2,n-p] Pochhammer[1-a+b3,m-p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^(-m+n-p) (1-y)^(-a+b1+b3) (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a-b1-b3,-m+n-p] Pochhammer[1+a-b1-b2-b3,-m-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,m+p] Pochhammer[-b1-b2-b3+c,n])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[a-b1-b3,-m-p] Pochhammer[1+a-b1-b2-b3,-m+n-p] Pochhammer[1-a+b1+b3,m+p])},{Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]+Abs[1-z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b3+c] Pochhammer[1-a+b3,p] Pochhammer[-b3+c,m+n])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,p] Pochhammer[-b3+c,m+n])},{Abs[z/(-1+z)]>1&&Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[y/(-1+y)]<1,((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[1+a+b1+b3-c,m-n] Pochhammer[-b1+c,-m+n] Pochhammer[-a-b1-b3+c,-m+n])/(m! n! p! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[1+a+b1+b3-c,m-n+p] Pochhammer[-a-b1+c,-m+n] Pochhammer[-b1-b3+c,-m+n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a-b1+c) (-(z/(-1+z)))^(m-n) (z/(-1+z))^-p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n] Pochhammer[a+b1+b3-c,m-n] Pochhammer[-b1+c,-m+n] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b3+c,-m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-n] Pochhammer[-a-b1+c,-m+n] Pochhammer[1-a-b1-b3+c,-m+n+p])},{Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[x/(-1+x)]>1+Abs[1-y/(-1+y)]&&Abs[-1+y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[-1+y/(-1+y)],((-1)^n (1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[b2,n] Pochhammer[1-a+b2,m+n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a+b2,n] Pochhammer[1-a-b1+c,m+n+p])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b2] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b1-c,m-n] Pochhammer[1+a+b1+b3-c,m-n+p] Pochhammer[-a-b1+c,-m+n])+((-1)^m (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^(-a-b1+c) (-(z/(-1+z)))^(m-n) (z/(-1+z))^-p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-a+b2,n+p] Pochhammer[a+b1+b3-c,m-n] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b3+c,-m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b1-c,m-n] Pochhammer[-a-b1+c,-m+n] Pochhammer[1-a-b1-b3+c,-m+n+p])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1/(1-y))^n (1-y)^-a (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b2] Gamma[c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-b1-b2-b3+c,-m+n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1+b1+b2-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n+p] Pochhammer[-b1-b2+c,-m+n])},{Abs[((-1+x) z)/(x (-1+z))]<1&&Abs[y/(-1+y)]>1&&Abs[(y (-1+z))/((-1+y) z)]<1,((1-x)^-b1 (-((-1+x)/x))^(-a+c) (-(x/(-1+x)))^(-n-p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1-c] Gamma[c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[a+b1-c,-n-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b1+c,n+p])/(m! n! p! Gamma[a] Gamma[b1] Pochhammer[1-a-b1+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a-b1+c) (-(z/(-1+z)))^(m-n) (z/(-1+z))^-p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b1+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n] Pochhammer[a+b1+b3-c,m-n] Pochhammer[-b1+c,-m+n] Pochhammer[-a-b1+c,-m+n+p] Pochhammer[1-a-b1-b3+c,-m+n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1-c,m-n] Pochhammer[-a-b1+c,-m+n] Pochhammer[1-a-b1-b3+c,-m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p] Pochhammer[-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[-b1-b2-b3+c,-m-p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^(-a-b1-b3+c) (-(y/(-1+y)))^(m+p) (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[a+b1+b2+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p] Pochhammer[-a-b1-b3+c,-m+n-p] Pochhammer[1-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[1-a-b1-b2-b3+c,-m+n-p])},{Abs[x/(-1+x)]<1&&Abs[z/(-1+z)]<1&&Abs[y/(-1+y)]>1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n-p] Pochhammer[1+a+b2-c,-m-p] Pochhammer[-a-b2+c,m+p])/(m! n! p! Gamma[-a+c] Gamma[-b2+c] Pochhammer[1+b2-c,-m-p] Pochhammer[1+a+b2-c,-m+n-p] Pochhammer[-b2+c,m+p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n+p])},{Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-x/(-1+x)]+Abs[z/(-1+z)]<1,((-1)^p (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a-b1] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,-m-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b2-c,-m+n-p])+((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+n] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a+b1,m] Pochhammer[1-a-b2+c,m+n+p])+((-1)^p (1/(1-x))^m (1-x)^-a (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[-a+b1] Gamma[c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,-m-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[-b1-b2+c,m+p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1+b1+b2-c,-m+n-p])},{Abs[1-x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1,((-1)^m (1/(1-x))^(m-p) (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[a,p] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a-b3,p] Pochhammer[1+b1+b2+b3-c,-m+n])+((-1)^m (1/(1-x))^(m-p) (1-x)^(-a+b3) (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[-a+b1+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b3,m-p] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b3,m-p] Pochhammer[1+b1+b2+b3-c,-m+n])+((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,-m-p] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1+a+b2-c,-m+n-p])+((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1-a+b1+b3,m+n+p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p]^2 Pochhammer[1-a-b2+c,m+n+p])},{Abs[1-x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[1-z/(-1+z)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)]&&Abs[y/(-1+y)]>1+Abs[-1+z/(-1+z)]&&Abs[(-1+x/(-1+x))/(-1+z/(-1+z))]<1&&Abs[-1+z/(-1+z)]<1&&Abs[-1+x/(-1+x)]<1,((-1)^m (1/(1-x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-a ((-1+z)/(-1+x))^p Gamma[-a+b1] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-a,((-1+z)/(1-x))^a] Pochhammer[a,m+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b1,m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((-1)^m (1/(1-x))^m (1-x)^(-a+b3) (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 ((-1+z)/(-1+x))^p Gamma[-a+b1] Gamma[a-b3] Gamma[1-a+b3] Gamma[c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b3,((-1+z)/(1-x))^b3] Pochhammer[a,m+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((1/(1-x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-a ((1-x)/(-1+z))^m ((-1+z)/(-1+x))^p Gamma[a-b1] Gamma[1+a-b3] Gamma[-a+b3] Gamma[c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^-b1,((-1+z)/(1-x))^b1] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[-a+b1+b3,-m+p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[1+a-b1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((1/(1-x))^m (1-x)^(-a+b3) (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 ((1-x)/(-1+z))^m ((-1+z)/(-1+x))^p Gamma[a-b1] Gamma[a-b3] Gamma[1-a+b3] Gamma[-a+b1+b3] Gamma[c] If[-Re[x]+Re[z]>0,((1-x)/(-1+z))^(a-b1-b3),((-1+z)/(1-x))^(-a+b1+b3)] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[-a+b1+b3,-m+p] Pochhammer[1+b1+b2+b3-c,n])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n])+((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b3] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,-m-p] Pochhammer[1+b1+b2+b3-c,n] Pochhammer[-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b3,m+p] Pochhammer[1+a+b2-c,-m+n-p])+((1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1-a+b1+b3,m+n+p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[a-b1-b3,-m-p] Pochhammer[1-a+b1+b3,m+p]^2 Pochhammer[1-a-b2+c,m+n+p])},{Abs[x/(-1+x)]>1&&Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[z/(-1+z)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[1+a+b1+b2-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a-b1-b2+c,-n+p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2+c] Pochhammer[1+a+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[1+a+b1+b2-c,m+n-p] Pochhammer[-a-b2+c,-n+p] Pochhammer[-b1-b2+c,-n+p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b2+c) (-(x/(-1+x)))^(n-p) (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[a+b1+b2-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b1-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2-c,n-p] Pochhammer[-a-b2+c,-n+p] Pochhammer[1-a-b1-b2+c,m-n+p])},{Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[x/(-1+x)]+Abs[1-z/(-1+z)]<1,((-1)^m (1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2-c,-m-p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b2-c,-m+n-p])+((-1)^p (1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1-a+b3,n+p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a+b3,p] Pochhammer[1-a-b2+c,m+n+p])+((-1)^m (1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[1+b2+b3-c,-m-p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,p] Pochhammer[1+b2+b3-c,-m+n-p])},{Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[y/(-1+y)]>1+Abs[1-z/(-1+z)]&&Abs[-1+z/(-1+z)]<1&&Abs[x/(-1+x)]>1+Abs[-1+z/(-1+z)],((-1)^p (1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1-a+b3,n+p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a+b3,p] Pochhammer[1-a-b2+c,m+n+p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a-b3] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b1+b2-c,n-p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[-a-b1-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b2-c,n-p] Pochhammer[1+a+b1+b2-c,m+n-p] Pochhammer[-a-b2+c,-n+p])+((-1)^n (1-x)^-b1 (-((-1+x)/x))^(-a-b2+c) (-(x/(-1+x)))^(n-p) (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-b3 Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a+b3,m+p] Pochhammer[a+b1+b2-c,n-p] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b1-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1-a+b3,p] Pochhammer[1+a+b2-c,n-p] Pochhammer[-a-b2+c,-n+p] Pochhammer[1-a-b1-b2+c,m-n+p])+((-1)^p (1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1/(1-z))^p (1-z)^-a Gamma[-a+b3] Gamma[c] Pochhammer[a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,n-p] Pochhammer[-b1-b2-b3+c,-n+p])/(m! n! p! Gamma[b3] Gamma[-a+c] Pochhammer[1+a-b3,p] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[-b2-b3+c,-n+p])},{Abs[(x (-1+y))/((-1+x) y)]<1&&Abs[z/(-1+z)]>1&&Abs[((-1+x) z)/(x (-1+z))]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b2+c) (-(x/(-1+x)))^(n-p) (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b1+b2-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[a+b1+b2-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b1-b2+c,-n+p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2-c,n-p] Pochhammer[-a-b2+c,-n+p] Pochhammer[1-a-b1-b2+c,m-n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,m+n] Pochhammer[-b1-b2+c,-m-n] Pochhammer[-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[-b1-b2-b3+c,-m-n])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^(-a-b1-b2+c) (-(z/(-1+z)))^(m+n) (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b2+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m+n] Pochhammer[a+b1+b2+b3-c,m+n] Pochhammer[-b1-b2+c,-m-n] Pochhammer[-a-b1-b2+c,-m-n+p] Pochhammer[1-a-b1-b2-b3+c,-m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b1+b2-c,m+n] Pochhammer[-a-b1-b2+c,-m-n] Pochhammer[1-a-b1-b2-b3+c,-m-n+p])},{Abs[1-y/(-1+y)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+y/(-1+y))/(-1+z/(-1+z))]<1&&Abs[1-x/(-1+x)]<1&&Abs[(-1+z/(-1+z))/(-1+x/(-1+x))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,n+p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1/(1-x))^(m-p) (1/(1-y))^n (1-y)^-a (1/(1-z))^(-n+p) Gamma[-a+b2] Gamma[c] Pochhammer[0,m-p] Pochhammer[0,-n+p] Pochhammer[1,n] Pochhammer[1,p] Pochhammer[a,n] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a-b2,n] Pochhammer[1-b3,-n+p])+((1/(1-x))^(m-p) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^(-n+p) (1-z)^(-a+b2) Gamma[a-b2] Gamma[-a+b2+b3] Gamma[c] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[a-b2,-n+p] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a-b2-b3,-n+p])+((1/(1-x))^(m-n-p) (1-x)^(-a+b2+b3) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n-p] Pochhammer[1+a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,n+p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1+a-b1-b2-b3,m-n-p] Pochhammer[1-a+b2+b3,n+p])},{Abs[1-y/(-1+y)]<1&&Abs[1-z/(-1+z)]<1&&Abs[(-1+z/(-1+z))/(-1+y/(-1+y))]<1&&Abs[1-x/(-1+x)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1,((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b1-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[1-a+b1+b2+b3,n+p] Pochhammer[-a+c,m+n+p])/(m! n! p! Gamma[a] Gamma[-b1-b2-b3+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1-a+b2+b3,n+p] Pochhammer[1-a+b1+b2+b3,m+n+p])+((1/(1-x))^(m-p) (1-y)^-a (1/(1-z))^p ((-1+y)/(-1+z))^n Gamma[1+a-b2] Gamma[-a+b2] Gamma[-a+b3] Gamma[c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-a,((-1+y)/(1-z))^a] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[a,n+p] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a-b3,n+p])+((1/(1-x))^(m-p) (1-y)^-b2 (1/(1-z))^p (1-z)^(-a+b2) ((-1+y)/(-1+z))^n Gamma[a-b2] Gamma[1-a+b2] Gamma[-a+b3] Gamma[c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b2,((-1+y)/(1-z))^b2] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[a,n+p] Pochhammer[b2,n] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1+a-b3,n+p])+((-1)^p (1/(1-x))^(m-p) (1-y)^-b2 (1/(1-z))^p (1-z)^(-a+b2) ((1-z)/(-1+y))^p ((-1+y)/(-1+z))^n Gamma[a-b2] Gamma[1-a+b2] Gamma[a-b3] Gamma[-a+b2+b3] Gamma[c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^(a-b2-b3),((-1+y)/(1-z))^(-a+b2+b3)] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b3,n] Pochhammer[-a+b2+b3,n-p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1-a+b3,n-p])+((-1)^p (1/(1-x))^(m-p) (1-y)^-a (1/(1-z))^p ((1-z)/(-1+y))^p ((-1+y)/(-1+z))^n Gamma[1+a-b2] Gamma[-a+b2] Gamma[a-b3] Gamma[c] If[Re[y]-Re[z]>0,((1-z)/(-1+y))^-b3,((-1+y)/(1-z))^b3] Pochhammer[0,m-p] Pochhammer[1,p] Pochhammer[b3,n] Pochhammer[-a+b2+b3,n-p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b2] Gamma[1+a-b2-b3] Gamma[-a+c] Pochhammer[1-b1,m-p] Pochhammer[1-a+b3,n-p])+((1/(1-x))^(m-n-p) (1-x)^(-a+b2+b3) (1/(1-y))^n (1-y)^-b2 (1/(1-z))^p (1-z)^-b3 Gamma[a-b2-b3] Gamma[-a+b1+b2+b3] Gamma[c] Pochhammer[b2,n] Pochhammer[a-b2-b3,m-n-p] Pochhammer[1+a-b1-b2-b3,-n-p] Pochhammer[b3,p] Pochhammer[-a+b1+b2+b3,n+p] Pochhammer[-b1-b2-b3+c,m])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[a-b2-b3,-n-p] Pochhammer[1+a-b1-b2-b3,m-n-p] Pochhammer[1-a+b2+b3,n+p])},{Abs[z/(-1+z)]>1&&Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x/(-1+x)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n+p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[1+a+b2+b3-c,-m+n] Pochhammer[-b2+c,m-n] Pochhammer[-a-b2-b3+c,m-n])/(m! n! p! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[1+a+b2+b3-c,-m+n+p] Pochhammer[-a-b2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^(-a-b2+c) (-(z/(-1+z)))^(-m+n) (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n] Pochhammer[a+b2+b3-c,-m+n] Pochhammer[-b2+c,m-n] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b2-b3+c,m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-m+n] Pochhammer[-a-b2+c,m-n] Pochhammer[1-a-b2-b3+c,m-n+p])},{Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[y/(-1+y)]>1+Abs[1-x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)],((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+n] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a+b1,m] Pochhammer[1-a-b2+c,m+n+p])+((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2+b3-c,-m+n] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,m-n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b2-c,-m+n] Pochhammer[1+a+b2+b3-c,-m+n+p] Pochhammer[-a-b2+c,m-n])+((-1)^n (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^(-a-b2+c) (-(z/(-1+z)))^(-m+n) (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+p] Pochhammer[b2,n] Pochhammer[a+b2+b3-c,-m+n] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b2-b3+c,m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b2-c,-m+n] Pochhammer[-a-b2+c,m-n] Pochhammer[1-a-b2-b3+c,m-n+p])+((-1)^m (1/(1-x))^m (1-x)^-a (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b1] Gamma[c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,m-n])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1+b1+b2-c,-m+n] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[-b1-b2+c,m-n])},{Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x/(-1+x)]>1&&Abs[(x (-1+z))/((-1+x) z)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a+c) (-(y/(-1+y)))^(-m-p) (y/(-1+y))^-n (1-z)^-b3 (z/(-1+z))^p Gamma[a+b2-c] Gamma[c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[a+b2-c,-m-p] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b2+c,m+p])/(m! n! p! Gamma[a] Gamma[b2] Pochhammer[1-a-b2+c,m+n+p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^(-a-b2+c) (-(z/(-1+z)))^(-m+n) (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b2+c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n] Pochhammer[a+b2+b3-c,-m+n] Pochhammer[-b2+c,m-n] Pochhammer[-a-b2+c,m-n+p] Pochhammer[1-a-b2-b3+c,m-n])/(m! n! p! Gamma[a] Gamma[b3] Gamma[-a+c] Pochhammer[1+a+b2-c,-m+n] Pochhammer[-a-b2+c,m-n] Pochhammer[1-a-b2-b3+c,m-n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p] Pochhammer[-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[-b1-b2-b3+c,-n-p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b2-b3+c) (-(x/(-1+x)))^(n+p) (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[a+b1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p] Pochhammer[-a-b2-b3+c,m-n-p] Pochhammer[1-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[1-a-b1-b2-b3+c,m-n-p])},{Abs[x/(-1+x)]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]>1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-m-n+p] Pochhammer[1+a+b3-c,-m-n] Pochhammer[-a-b3+c,m+n])/(m! n! p! Gamma[-a+c] Gamma[-b3+c] Pochhammer[1+b3-c,-m-n] Pochhammer[1+a+b3-c,-m-n+p] Pochhammer[-b3+c,m+n])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m+n+p])},{Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-x/(-1+x)]+Abs[y/(-1+y)]<1,((-1)^n (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,-m-n] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b3-c,-m-n+p])+((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+p] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a+b1,m] Pochhammer[1-a-b3+c,m+n+p])+((-1)^n (1/(1-x))^m (1-x)^-a (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b1] Gamma[c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,-m-n] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[-b1-b3+c,m+n])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1+b1+b3-c,-m-n+p])},{Abs[1-x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[(-1+y/(-1+y))/(-1+x/(-1+x))]<1,((-1)^m (1/(1-x))^(m-n) (1/(1-y))^n (1-y)^-a (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b2] Gamma[c] Pochhammer[0,m-n] Pochhammer[1,n] Pochhammer[a,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1-b1,m-n] Pochhammer[1+a-b2,n] Pochhammer[1+b1+b2+b3-c,-m+p])+((-1)^m (1/(1-x))^(m-n) (1-x)^(-a+b2) (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b2] Gamma[-a+b1+b2] Gamma[c] Pochhammer[a-b2,m-n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1-b2,m-n] Pochhammer[1+b1+b2+b3-c,-m+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,-m-n] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a+b3-c,-m-n+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,m+n+p] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n]^2 Pochhammer[1-a-b3+c,m+n+p])},{Abs[1-x/(-1+x)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[1-y/(-1+y)]<1&&Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[z/(-1+z)]>1+Abs[-1+x/(-1+x)]&&Abs[z/(-1+z)]>1+Abs[-1+y/(-1+y)]&&Abs[(-1+x/(-1+x))/(-1+y/(-1+y))]<1&&Abs[-1+y/(-1+y)]<1&&Abs[-1+x/(-1+x)]<1,((-1)^m (1/(1-x))^m (1-y)^-a ((-1+y)/(-1+x))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b1] Gamma[1+a-b2] Gamma[-a+b2] Gamma[c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-a,((-1+y)/(1-x))^a] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((-1)^m (1/(1-x))^m (1-x)^(-a+b2) (1-y)^-b2 ((-1+y)/(-1+x))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b1] Gamma[a-b2] Gamma[1-a+b2] Gamma[c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b2,((-1+y)/(1-x))^b2] Pochhammer[a,m+n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[1-a] Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((1/(1-x))^m (1-y)^-a ((1-x)/(-1+y))^m ((-1+y)/(-1+x))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1] Gamma[1+a-b2] Gamma[-a+b2] Gamma[c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^-b1,((-1+y)/(1-x))^b1] Pochhammer[b1,n] Pochhammer[-a+b1+b2,-m+n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[1+a-b1-b2] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,-m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((1/(1-x))^m (1-x)^(-a+b2) (1-y)^-b2 ((1-x)/(-1+y))^m ((-1+y)/(-1+x))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1] Gamma[a-b2] Gamma[1-a+b2] Gamma[-a+b1+b2] Gamma[c] If[-Re[x]+Re[y]>0,((1-x)/(-1+y))^(a-b1-b2),((-1+y)/(1-x))^(-a+b1+b2)] Pochhammer[b1,n] Pochhammer[-a+b1+b2,-m+n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,p])/(m! n! p! Gamma[a] Gamma[1-b1] Gamma[b1] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,-m+n] Pochhammer[1+b1+b2+b3-c,-m+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,-m-n] Pochhammer[1+b1+b2+b3-c,p] Pochhammer[-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1+b2,m+n] Pochhammer[1+a+b3-c,-m-n+p])+((1/(1-x))^m (1-x)^-b1 (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-a+b1+b2,m+n+p] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[a-b1-b2,-m-n] Pochhammer[1-a+b1+b2,m+n]^2 Pochhammer[1-a-b3+c,m+n+p])},{Abs[x/(-1+x)]>1&&Abs[(x (-1+z))/((-1+x) z)]<1&&Abs[y/(-1+y)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[1+a+b1+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a-b1-b3+c,n-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b3+c] Pochhammer[1+a+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[1+a+b1+b3-c,m-n+p] Pochhammer[-a-b3+c,n-p] Pochhammer[-b1-b3+c,n-p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b3+c) (-(x/(-1+x)))^(-n+p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[a+b1+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a-b3+c,m+n-p] Pochhammer[1-a-b1-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b3-c,-n+p] Pochhammer[-a-b3+c,n-p] Pochhammer[1-a-b1-b3+c,m+n-p])},{Abs[z/(-1+z)]>1+Abs[1-y/(-1+y)]&&Abs[x/(-1+x)]+Abs[1-y/(-1+y)]<1,((-1)^m (1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b2] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b3-c,-m-n] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1-a+b2,n] Pochhammer[1+a+b3-c,-m-n+p])+((-1)^n (1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-b2 (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-a+b2,n+p] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a+b2,n] Pochhammer[1-a-b3+c,m+n+p])+((-1)^m (1-x)^-b1 (x/(-1+x))^m (1/(1-y))^n (1-y)^-a (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b2] Gamma[c] Pochhammer[a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m-n] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b2] Gamma[-a+c] Pochhammer[1+a-b2,n] Pochhammer[1+b2+b3-c,-m-n+p])},{Abs[(x (-1+z))/((-1+x) z)]<1&&Abs[y/(-1+y)]>1&&Abs[((-1+x) y)/(x (-1+y))]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m+n+p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b3+c) (-(x/(-1+x)))^(-n+p) (x/(-1+x))^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[a+b1+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a-b3+c,m+n-p] Pochhammer[1-a-b1-b3+c,n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b3-c,-n+p] Pochhammer[-a-b3+c,n-p] Pochhammer[1-a-b1-b3+c,m+n-p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p] Pochhammer[-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[-b1-b2-b3+c,-m-p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^(-a-b1-b3+c) (-(y/(-1+y)))^(m+p) (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b1-b3+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1+b3-c,m+p] Pochhammer[a+b1+b2+b3-c,m+p] Pochhammer[-b1-b3+c,-m-p] Pochhammer[-a-b1-b3+c,-m+n-p] Pochhammer[1-a-b1-b2-b3+c,-m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b1+b3-c,m+p] Pochhammer[-a-b1-b3+c,-m-p] Pochhammer[1-a-b1-b2-b3+c,-m+n-p])},{Abs[y/(-1+y)]>1&&Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x/(-1+x)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m+n+p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[1+a+b2+b3-c,-m+p] Pochhammer[-b3+c,m-p] Pochhammer[-a-b2-b3+c,m-p])/(m! n! p! Gamma[-a+c] Gamma[-b2-b3+c] Pochhammer[1+a+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[1+a+b2+b3-c,-m+n+p] Pochhammer[-a-b3+c,m-p] Pochhammer[-b2-b3+c,m-p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a-b3+c) (-(y/(-1+y)))^(-m+p) (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b3-c,-m+p] Pochhammer[a+b2+b3-c,-m+p] Pochhammer[-b3+c,m-p] Pochhammer[-a-b3+c,m+n-p] Pochhammer[1-a-b2-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b3-c,-m+p] Pochhammer[-a-b3+c,m-p] Pochhammer[1-a-b2-b3+c,m+n-p])},{Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[z/(-1+z)]>1+Abs[1-x/(-1+x)]&&Abs[-1+x/(-1+x)]<1&&Abs[y/(-1+y)]>1+Abs[-1+x/(-1+x)],((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+p] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a+b1,m] Pochhammer[1-a-b3+c,m+n+p])+((-1)^m (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a-b1] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a+b2+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b3-c,-m+p] Pochhammer[1+a+b2+b3-c,-m+n+p] Pochhammer[-a-b3+c,m-p])+((-1)^p (1/(1-x))^m (1-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^(-a-b3+c) (-(y/(-1+y)))^(-m+p) (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[b1,m] Pochhammer[1-a+b1,m+n] Pochhammer[b3,p] Pochhammer[a+b2+b3-c,-m+p] Pochhammer[-a-b3+c,m+n-p] Pochhammer[1-a-b2-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1-a+b1,m] Pochhammer[1+a+b3-c,-m+p] Pochhammer[-a-b3+c,m-p] Pochhammer[1-a-b2-b3+c,m+n-p])+((-1)^m (1/(1-x))^m (1-x)^-a (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[-a+b1] Gamma[c] Pochhammer[a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,m-p])/(m! n! p! Gamma[b1] Gamma[-a+c] Pochhammer[1+a-b1,m] Pochhammer[1+b1+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[-b1-b3+c,m-p])},{Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x/(-1+x)]>1&&Abs[(x (-1+y))/((-1+x) y)]<1,((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a+c) (-(z/(-1+z)))^(-m-n) (z/(-1+z))^-p Gamma[a+b3-c] Gamma[c] Pochhammer[1-a,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[a+b3-c,-m-n] Pochhammer[-a+c,m+n+p] Pochhammer[1-a-b3+c,m+n])/(m! n! p! Gamma[a] Gamma[b3] Pochhammer[1-a-b3+c,m+n+p])+((1-x)^-b1 (x/(-1+x))^m (1-y)^-b2 (-((-1+y)/y))^(-a-b3+c) (-(y/(-1+y)))^(-m+p) (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b2+b3-c] Gamma[c] Gamma[-a-b3+c] Pochhammer[1-a,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b3-c,-m+p] Pochhammer[a+b2+b3-c,-m+p] Pochhammer[-b3+c,m-p] Pochhammer[-a-b3+c,m+n-p] Pochhammer[1-a-b2-b3+c,m-p])/(m! n! p! Gamma[a] Gamma[b2] Gamma[-a+c] Pochhammer[1+a+b3-c,-m+p] Pochhammer[-a-b3+c,m-p] Pochhammer[1-a-b2-b3+c,m+n-p])+((1-x)^-b1 (-((-1+x)/x))^b1 (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[c] Gamma[-a-b1-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,m+n+p] Pochhammer[1+a+b1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p] Pochhammer[-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[-a+c] Gamma[-b1-b2-b3+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[1+a+b1+b2+b3-c,m+n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[-b1-b2-b3+c,-n-p])+((1-x)^-b1 (-((-1+x)/x))^(-a-b2-b3+c) (-(x/(-1+x)))^(n+p) (x/(-1+x))^-m (1-y)^-b2 (-((-1+y)/y))^b2 (y/(-1+y))^-n (1-z)^-b3 (-((-1+z)/z))^b3 (z/(-1+z))^-p Gamma[a+b1+b2+b3-c] Gamma[c] Gamma[-a-b2-b3+c] Pochhammer[1-a,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,n+p] Pochhammer[a+b1+b2+b3-c,n+p] Pochhammer[-b2-b3+c,-n-p] Pochhammer[-a-b2-b3+c,m-n-p] Pochhammer[1-a-b1-b2-b3+c,-n-p])/(m! n! p! Gamma[a] Gamma[b1] Gamma[-a+c] Pochhammer[1+a+b2+b3-c,n+p] Pochhammer[-a-b2-b3+c,-n-p] Pochhammer[1-a-b1-b2-b3+c,m-n-p])}};


(* ::Section:: *)
(*End package*)


End[]


EndPackage[]
