(* ::Package:: *)

BeginPackage["LauricellaSaranFS`"]


Print["LauricellaSaranFS.wl v1.0\n","Authors : Souvik Bera & Tanay Pathak\n","Last modified : 03.03.2024"];


Off[General::infy,General::indet,General::munfl,General::stop ];
ParallelEvaluate[Off[General::infy,General::indet,General::munfl,General::stop ]];


FS3::usage="The command gives the numerical value of the Lauricella FS in three variables.
 FS3[a1, a2, b1, b2, b3, c, x, y, z, precision, terms, verbose-> True]";
FS3findall::usage="The command finds all the analytic continuations that are valid for the given point
 FS3findall[{x,y,z}]";
FS3evaluate::usage="The command gives the value of the series at a given point and Pochhammer parameters with a chosen analytic continuation
 FS3evaluate[series_number,{a1, a2, b1, b2, b3, c, x, y, z}, precision, terms]";
FS3expose::usage="The command exposes the domain of convergence and the expression of the analytic continuation of FS[a1, a2, b1, b2, b3, c, x, y, z, m, n, p]
 FS3expose[series_number]";


Begin["`Private`"]


Get["AppellF1.wl"];Get["AppellF3.wl"];


(*it exposes the three variable series*)
FS3expose[list_]:=Module[{i},
If[list>102,Print["Total number of ACs is 102"];Abort[];];
ClearAll[Global`a1,Global`a2,Global`b1,Global`b2,Global`b3, Global`c, Global`x, Global`y, Global`z,Global`m,Global`n,Global`p];
Return[LS3sum[{Global`a1,Global`a2,Global`b1,Global`b2,Global`b3, Global`c, Global`x, Global`y, Global`z},Global`m,Global`n,Global`p][[list]]];
];



FS3findall[{x0_?NumberQ, y0_?NumberQ, z0_?NumberQ}]:=Module[{roc,test,pos,a1,a2,b1, b2,b3, c, x, y, z,m,n},
roc[{x_,y_,z_}]=LS2sum[{a1,a2,b1, b2,b3, c, x, y,z},m,n][[All,1]];
test = roc[Rationalize/@{x0,y0,z0}];

pos=Position[test,True];
Return[Flatten[pos]];


];



FS3evaluate::singular="FS3 is singular";
FS3evaluate[list_/;(IntegerQ[list]&&list>0), para_List, p_,terms_]:=Module[
{a1,a2,b1, b2,b3, c, x, y, z,m,n,
a10,a20,b10, b20,b30, c0, x0, y0,z0,
result,p0,roc,acs,selectedseries,eps,peps},

$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];


p0= 10 p;
peps= 5p;


If[Or@@((IntegerQ[#]&&#<=0)&/@(para[[-4;;-4]])),Message[FS3evaluate::singular];Abort[];];

(*parameters = SetPrecision[,p0];*)
{a10,a20,b10, b20,b30, c0, x0, y0,z0} = para;
{ a1,a2,b1, b2,b3, c, x, y,z} = para;
result=0;



(* When one of the arguments is zero*)

If[x0===0.0||x0===0, result=F1[a20,b20,b30,c0,y0,z0,p,terms];Goto[end];];
If[y0===0.0||y0===0, result=F3[a10,a20,b10,b30,c0,x0,z0,p,terms];Goto[end];];
If[z0===0.0||z0===0, result=F3[a10,a20,b10,b20,c0,x0,y0,p,terms];Goto[end];];


(* When two of the arguments are zero*)
If[And[x0===0.0||x0===0,y0===0.0||y0===0],result=Hypergeometric2F1[a20,b30,c0,z0];Goto[end];];
If[And[y0===0.0||y0===0,z0===0.0||z0===0],result=Hypergeometric2F1[a10,b10,c0,x0];Goto[end];];
If[And[z0===0.0||z0===0,x0===0.0||x0===0],result=Hypergeometric2F1[a20,b20,c0,y0];Goto[end];];

(* When all the arguments are zero*)
If[And[z0===0.0||z0===0,x0===0.0||x0===0,y0===0.0||y0===0],result=1;Goto[end];];




(*condition for log situation*)

If[IntegerQ[a1],a1=a1-1/2 eps];
If[IntegerQ[a2],a2=a2+1/3 eps];
If[IntegerQ[b1],b1=b1+1/5 eps];
If[IntegerQ[b2],b2=b2-1/7 eps];
If[IntegerQ[b3],b3=b3-1/11 eps];
If[IntegerQ[c],c=c+1/13eps];

If[IntegerQ[-a2+c],c=c+1/13eps];
If[IntegerQ[-b2+c],c=c+1/13eps];
If[IntegerQ[-b3+c],c=c+1/13eps];
If[IntegerQ[b2+b3],b2=b2-1/7 eps];
If[IntegerQ[-a1+c],c=c+1/13eps];
If[IntegerQ[-b1+c],c=c+1/13eps];
If[IntegerQ[a2-b2],b2=b2-1/7 eps];
If[IntegerQ[a2-b3],a2=a2+1/3 eps];
If[IntegerQ[a1-b1],b1=b1+1/5 eps];

If[IntegerQ[-b2-b3+c],c=c+1/13eps];
If[IntegerQ[-a1-a2+c],c=c+1/13eps];
If[IntegerQ[-a1-b2+c],c=c+1/13eps];
If[IntegerQ[-a2-b1+c],c=c+1/13eps];
If[IntegerQ[-b1-b2+c],c=c+1/13eps];
If[IntegerQ[-a1-b3+c],c=c+1/13eps];
If[IntegerQ[-b1-b3+c],c=c+1/13eps];
If[IntegerQ[a2+b2-c],c=c+1/13eps];
If[IntegerQ[a2+b3-c],c=c+1/13eps];
If[IntegerQ[a2-b2-b3],a2=a2+1/3 eps];
If[IntegerQ[a1+b1-c],c=c+1/13eps];

If[IntegerQ[-a1-b2-b3+c]||IntegerQ[-b1-b2-b3+c]||
IntegerQ[a2+b2+b3-c]||IntegerQ[-a2-b2-b3+c]||
IntegerQ[a1+a2+b2-c]||IntegerQ[a2+b1+b2-c]||
IntegerQ[-a1-a2-b2+c]||IntegerQ[-a2-b1-b2+c]||
IntegerQ[a1+a2+b3-c]||IntegerQ[a2+b1+b3-c]||
IntegerQ[-a1-a2-b3+c]||IntegerQ[-a2-b1-b3+c]||
IntegerQ[a1+a2+b1-c]||IntegerQ[a1+b1+b3-c]||
IntegerQ[-a1-a2-b1+c]||IntegerQ[-a1-b1-b3+c]||
IntegerQ[a1+b1+b2-c]||IntegerQ[-a1-b1-b2+c],c=c+1/13eps];

If[IntegerQ[a1+a2+b2+b3-c]||IntegerQ[a2+b1+b2+b3-c]||IntegerQ[a1+b1+b2+b3-c],c=c+1/13eps];


eps= SetPrecision[ 10^(-p) I,peps];





roc[{x_,y_,z_}]=LS2sum[SetPrecision[{a1,a2, b1, b2,b3, c, x, y,z},p0],m,n][[All,1]];



If[Length[para]=!=9,Abort[];];



If[roc[Rationalize/@(para[[-3;;-1]])][[list]],{},Print["The point does not lie in ", list];Abort[];];


selectedseries[m_,n_]=LS2sum[SetPrecision[{a1,a2, b1, b2,b3, c, x, y,z},p0],m,n][[list,2]]; 

DistributeDefinitions[selectedseries,m,n,terms,p,p0,peps];
result = N[ParallelSum[selectedseries[m,n],{m,0,terms},{n,0,m}],p0];


Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]]


Options[FS3]={verbose->False};
FS3::poch="Pochhammer parameters do not obey the constraint";
FS3::val="The point is not covered";
FS3::singular="FS3 is singular";
FS3[args___] := FScore[FS3,args];

FScore[FS3,a10_,a20_,b10_, b20_,b30_, c0_,x0_,y0_,z0_,p_,terms_,OptionsPattern[FS3]]:=Module[
{selectedseries,eachser,eachserlist,convrate1,convrate2, parameters,
 roc, a1,a2,b1, b2,b3, c, x, y,z,m,n, m0,m1,n0,
n1,v,i,test1,test2,pos1,pos2,result,p0,
seriesselect,pos12,mm,nn,peps,eps,
aa1,aa2,bb1, bb2,bb3, cc, xx, yy,zz},

m1=100;n1=100;
$MaxExtraPrecision=1000;
ParallelEvaluate[$MaxExtraPrecision=1000];

p0= 10 p;
peps= 5p;



If[Or@@((IntegerQ[#]&&#<=0)&/@{c0}),Message[FS3::singular];Abort[];];

parameters = SetPrecision[{a10,a20,b10, b20,b30, c0, x0, y0,z0},p0];
{ a1,a2,b1, b2,b3, c, x, y,z} = {a10,a20,b10, b20,b30, c0, x0, y0,z0};
result=0;



(* When one of the arguments is zero*)

If[x0===0.0||x0===0, result=F1[a20,b20,b30,c0,y0,z0,p,terms];Goto[end];];
If[y0===0.0||y0===0, result=F3[a10,a20,b10,b30,c0,x0,z0,p,terms];Goto[end];];
If[z0===0.0||z0===0, result=F3[a10,a20,b10,b20,c0,x0,y0,p,terms];Goto[end];];


(* When two of the arguments are zero*)
If[And[x0===0.0||x0===0,y0===0.0||y0===0],result=Hypergeometric2F1[a20,b30,c0,z0];Goto[end];];
If[And[y0===0.0||y0===0,z0===0.0||z0===0],result=Hypergeometric2F1[a10,b10,c0,x0];Goto[end];];
If[And[z0===0.0||z0===0,x0===0.0||x0===0],result=Hypergeometric2F1[a20,b20,c0,y0];Goto[end];];

(* When all the arguments are zero*)
If[And[z0===0.0||z0===0,x0===0.0||x0===0,y0===0.0||y0===0],result=1;Goto[end];];



(*reduction formula*)
If[y0===z0, If[OptionValue[verbose],PrintTemporary["Evaluating with reduction formula..."];,Nothing[]];
result = F3[a10,a20,b10,b20+b30,c0,x0,y0,p,terms,If[OptionValue[verbose],verbose-> True,Nothing[]]];Goto[end];];


(*condition for log situation*)

If[IntegerQ[a1],a1=a1-1/2 eps];
If[IntegerQ[a2],a2=a2+1/3 eps];
If[IntegerQ[b1],b1=b1+1/5 eps];
If[IntegerQ[b2],b2=b2-1/7 eps];
If[IntegerQ[b3],b3=b3-1/11 eps];
If[IntegerQ[c],c=c+1/13eps];

If[IntegerQ[-a2+c],c=c+1/13eps];
If[IntegerQ[-b2+c],c=c+1/13eps];
If[IntegerQ[-b3+c],c=c+1/13eps];
If[IntegerQ[b2+b3],b2=b2-1/7 eps];
If[IntegerQ[-a1+c],c=c+1/13eps];
If[IntegerQ[-b1+c],c=c+1/13eps];
If[IntegerQ[a2-b2],b2=b2-1/7 eps];
If[IntegerQ[a2-b3],a2=a2+1/3 eps];
If[IntegerQ[a1-b1],b1=b1+1/5 eps];

If[IntegerQ[-b2-b3+c],c=c+1/13eps];
If[IntegerQ[-a1-a2+c],c=c+1/13eps];
If[IntegerQ[-a1-b2+c],c=c+1/13eps];
If[IntegerQ[-a2-b1+c],c=c+1/13eps];
If[IntegerQ[-b1-b2+c],c=c+1/13eps];
If[IntegerQ[-a1-b3+c],c=c+1/13eps];
If[IntegerQ[-b1-b3+c],c=c+1/13eps];
If[IntegerQ[a2+b2-c],c=c+1/13eps];
If[IntegerQ[a2+b3-c],c=c+1/13eps];
If[IntegerQ[a2-b2-b3],a2=a2+1/3 eps];
If[IntegerQ[a1+b1-c],c=c+1/13eps];

If[IntegerQ[-a1-b2-b3+c]||IntegerQ[-b1-b2-b3+c]||
IntegerQ[a2+b2+b3-c]||IntegerQ[-a2-b2-b3+c]||
IntegerQ[a1+a2+b2-c]||IntegerQ[a2+b1+b2-c]||
IntegerQ[-a1-a2-b2+c]||IntegerQ[-a2-b1-b2+c]||
IntegerQ[a1+a2+b3-c]||IntegerQ[a2+b1+b3-c]||
IntegerQ[-a1-a2-b3+c]||IntegerQ[-a2-b1-b3+c]||
IntegerQ[a1+a2+b1-c]||IntegerQ[a1+b1+b3-c]||
IntegerQ[-a1-a2-b1+c]||IntegerQ[-a1-b1-b3+c]||
IntegerQ[a1+b1+b2-c]||IntegerQ[-a1-b1-b2+c],c=c+1/13eps];

If[IntegerQ[a1+a2+b2+b3-c]||IntegerQ[a2+b1+b2+b3-c]||IntegerQ[a1+b1+b2+b3-c],c=c+1/13eps];

eps= SetPrecision[ 10^(-p) I,peps];

parameters = SetPrecision[{a1,a2,b1, b2,b3, c, x, y,z},p0];
(*Print[parameters];*)



test1 = LS2sum[{a1,a2,b1, b2,b3, c, x, y,z},m,n][[All,1]];

PrintTemporary["Finding a proper AC..."];

pos1=Flatten[Position[test1,True]];

If[pos1==={},Message[FS3::val];Abort[];];



If[OptionValue[verbose],Print["valid series : ",pos1]];
(*Abort[];*)



test2=Table[eachser = LS2sum[{aa1,aa2,bb1, bb2,bb3, cc, xx, yy,zz},m,n][[pos1[[i]],2]];
eachserlist = If[Head[#]===Plus,List@@#,{#}]&@eachser;
(*Print[If[i==2,eachserlist]];*)
convrate1 = Abs[N[((eachserlist/.m-> n/.n->n+ 1)/(eachserlist/.m-> n))/.{n-> n1}
/.MapThread[Rule,{{aa1,aa2,bb1, bb2,bb3, cc, xx, yy,zz}, parameters}],p0]];
convrate2 = Abs[N[((eachserlist/.n-> 0/.m->m+ 1)/(eachserlist/.n-> 0))/.{m-> m1}
/.MapThread[Rule,{{aa1,aa2,bb1, bb2,bb3, cc, xx, yy,zz},parameters}],p0]];
(*Print[{convrate1,convrate2}];*)
{Max[Norm[{convrate1,convrate2}]],pos1[[i]]}
,{i,1,Length[pos1]}];



(*Print[test2];Abort[];*)

seriesselect=SortBy[test2,First];
If[OptionValue[verbose],Print["convergence rates :",{N[#[[1]],10],#[[2]]}&/@seriesselect]];
pos2=seriesselect[[1,2]];




If[OptionValue[verbose],
Print["selected series : ",pos2]];

PrintTemporary["Evaluating sum..."];



selectedseries[m_,n_]=LS2sum[SetPrecision[{a1,a2, b1, b2,b3, c, x, y,z},p0],m,n][[pos2,2]]; 
DistributeDefinitions[selectedseries,terms,p,p0,m,n];
result = N[ParallelSum[selectedseries[m,n],{m,0,terms},{n,0,m}],p0];




Label[end];
Return[Chop[Total[{1,I}*(SetPrecision[#,p]&/@ReIm[result])]]]
]





(* ::Section:: *)
(*Set of 102 ACs of LS*)


LS3sum[{a1_,a2_,b1_,b2_,b3_,c_,x_,y_,z_},m_,n_,p_]={{Abs[x]<1&&Abs[y]<1&&Abs[z]<1,(x^m y^n z^p Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Pochhammer[c,m+n+p])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[z]<1&&Abs[1-y]+Abs[z]<1,((-1)^m x^m (1-y)^n z^p Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) z^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p] Pochhammer[1-a2-b2+c,m+n])},{Abs[x]<1&&Abs[y]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[y]+Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1,((-1)^m x^m y^n (1-z)^p Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a2+c] Gamma[-b3+c] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])+((-1)^m x^m y^n (1-z)^(-a2-b3+c+m+p) Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+p] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n] Pochhammer[1-a2-b3+c,m+p])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[z]<1&&Abs[1-y]>1+Abs[z]&&Abs[z/(-1+y)]<1&&1/Abs[1-y]+Abs[z/(-1+y)]<1,(x^m (1-y)^(-b2-n) (-z)^p Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])+(x^m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])},{Abs[x]<1&&Abs[y]<1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&1/(1+Abs[y])>1/Abs[1-z]&&Abs[y/(-1+z)]<1&&1/Abs[1-z]+Abs[y/(-1+z)]<1,(x^m (-y)^n (1-z)^(-b3-p) Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])+(x^m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[z]>1&&1+1/Abs[z]<1/Abs[z/(-1+y)]&&Abs[z/(-1+y)]<1&&1/Abs[1-y]+Abs[z/(-1+y)]<1&&Abs[1-y]>1&&1/Abs[x]>1+1/Abs[z],((-1)^m x^m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m])+(x^m (1-y)^-b2 (-z)^(-a2+b2-p) (z/(-1+y))^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n-p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[-a2+c,m] Pochhammer[-a2+c,m+n-p])+(x^m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])},{Abs[x]<1&&Abs[y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[y/(-1+z)]<1&&1/Abs[1-z]+Abs[y/(-1+z)]<1&&1/Abs[x]>1+1/Abs[y]&&Abs[1-z]>1&&Abs[y]/(1+Abs[y])>Abs[y/(-1+z)],((-1)^m x^m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m])+(x^m (-y)^(-a2+b3-n) (1-z)^-b3 (y/(-1+z))^p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+b2+b3,-n+p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-a2+c,m-n+p])+(x^m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])},{Abs[x]<1&&Abs[-1+y]<1&&(1+Abs[x]) Abs[-1+y]<1&&Abs[x (-1+y)]<1&&Abs[-1+y]+Abs[x (-1+y)]<1&&Abs[(-1+y)/z]<1&&1+1/Abs[z]<1/Abs[x]&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+Abs[-1+y]),(x^m (1-y)^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b2-c,-m+n+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m])+(x^m (1-y)^n (-z)^(-b3-n) z^(n-p) Gamma[a2-b3] Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p])/(m! n! p! Gamma[a2] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m])+((-1)^(m+n) x^m (1-y)^(-a2-b2+c+m+n) (-z)^-b3 z^-p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1+b2+b3-c,-m-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[-1+z]<1&&(1+Abs[x]) Abs[-1+z]<1&&1+Abs[-1+z]<Abs[y]&&Abs[x (-1+z)]<1&&Abs[-1+z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1,(x^m (-y)^(-a2-p) y^-n (1-z)^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b3-c,-m+n+p])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m])+(x^m (-y)^(-b2-p) y^(-n+p) (1-z)^p Gamma[a2-b2] Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n])/(m! n! p! Gamma[a2] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m])+((-1)^(m+p) x^m (-y)^-b2 y^-n (1-z)^(-a2-b3+c+m+p) Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1+b2+b3-c,-m+n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&1/Abs[1-y]<1&&Abs[(1-y)/z]<1&&Abs[(1-y)/z]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/z]<1&&1+1/Abs[z]<1/Abs[x]&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1,(x^m ((-1+y)/z)^n (-z)^-a2 z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b2-c,-m+n+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m])+((-1)^m x^m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m])+((-1)^m x^m (1-y)^(-a2+b3-n) ((1-y)/z)^p (-z)^-b3 Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-b2-b3+c,m+n-p])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[-a2+c,m])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&1+1/Abs[1-z]<1/Abs[(1-z)/y]&&Abs[x]+1/Abs[1-z]<1&&1/Abs[1-z]<1&&Abs[(1-z)/y]<1&&Abs[(-1+z)/y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1,(x^m (-y)^-a2 y^-n ((-1+z)/y)^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b3-c,-m+n+p])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m])+((-1)^m x^m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m])+((-1)^m x^m (-y)^-b2 (1-z)^(-a2+b2-p) ((1-z)/y)^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-b2-b3+c,m-n+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[-a2+c,m])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[z]<1&&Abs[z/y]<1,((-1)^(m+p) x^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p])+((-1)^m x^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n-p])/(m! n! p! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n-p])},{Abs[x]<1&&Abs[y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1,((-1)^(m+n) x^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p])+((-1)^m x^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-m-n+p])/(m! n! p! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,-n+p])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1&&Abs[z/y]<1,((-1)^(m+p) x^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p])+((-1)^m x^m (-y)^-b2 y^-n (-z)^(-a2+b2) z^(n-p) Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-n+p])+((-1)^m x^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1,((-1)^m x^m (-y)^(-a2+b3) y^(-n+p) (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n-p])+((-1)^(m+n) x^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p])+((-1)^m x^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+n+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1,(x^m (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 (z/(-1+z))^p Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n] Pochhammer[-b3+c,m+n+p])/(m! n! p! Pochhammer[c,m+n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z/(-1+z)]<1,(x^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+n-p] Pochhammer[1+b2+b3-c,-m+n])/(m! n! p! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n-p] Pochhammer[-a2+c,m])+((-1)^m x^m (1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (-(y/(-1+y)))^-m (1-z)^-a2 (z/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1-a2+b3,n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1,(x^m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b3-c,-m-n+p] Pochhammer[1+b2+b3-c,-m+p])/(m! n! p! Gamma[-a2+c] Gamma[-b3+c] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m-n+p] Pochhammer[-a2+c,m])+((-1)^m x^m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (-(z/(-1+z)))^-m Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1-a2+b2,p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1-a2+b2,-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[((-1+y) z)/(y (-1+z))]<1,(x^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^a2 ((-1+z)/z)^p Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b2-c,-m+n+p] Pochhammer[1+b2+b3-c,-m+n])/(m! n! p! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[1+a2+b2+b3-c,-m+n+p] Pochhammer[-a2+c,m])+((-1)^(m+n) x^m (1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (-(y/(-1+y)))^-m (1-z)^-a2 (-((-1+z)/z))^(a2-b3) ((-1+z)/z)^p (-(z/(-1+z)))^n Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b3,p] Pochhammer[a2-b3,-n+p] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^(m+n) x^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^(-b2-b3+c) ((-1+z)/z)^p (-(z/(-1+z)))^(-m+n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-b2-b3+c,m-n+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2-b3+c,m-n+p])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(y (-1+z))/((-1+y) z)]<1,(x^m (1-y)^-a2 (-((-1+y)/y))^a2 ((-1+y)/y)^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b3-c,-m+n+p] Pochhammer[1+b2+b3-c,-m+p])/(m! n! p! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[1+a2+b2+b3-c,-m+n+p] Pochhammer[-a2+c,m])+((-1)^(m+p) x^m (1-y)^-a2 (-((-1+y)/y))^(a2-b2) ((-1+y)/y)^n (-(y/(-1+y)))^p (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (-(z/(-1+z)))^-m Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[a2-b2,n-p] Pochhammer[1-a2+b2,p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1-b2,n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])+((-1)^(m+p) x^m (1-y)^-a2 (-((-1+y)/y))^(-b2-b3+c) ((-1+y)/y)^n (-(y/(-1+y)))^(-m+p) (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-b2-b3+c,m+n-p])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2-b3+c,m+n-p])},{Abs[(y-z)/(-1+y)]<1&&Abs[(x (-1+z))/z]<1&&Abs[(x (-1+z))/z]+1/Abs[z]<1&&Abs[(y-z)/(z-y z)]<1&&Abs[(x (-1+z))/z]+Abs[(y-z)/(z-y z)]<1&&Abs[z]>1,((-1)^n x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m) ((y-z)/(-1+y))^n (-z)^(a2-c-m-n) z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) z^-p (z/(-1+z))^-m Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b2-b3,-n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[b2+b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])},{Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1&&Abs[(-y+z)/(-1+z)]<1&&Abs[(y-z)/(y-y z)]<1&&Abs[(x (-1+y))/y]+Abs[(y-z)/(y-y z)]<1&&Abs[y]>1,((-1)^p x^m (1-y)^(-a2-b2+c+m) (-y)^(a2-c-m-p) y^-n (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(-1+y))^-m (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2-b3,n-p] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b2+b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,((-1)^m x^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m+p) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+n+p] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2-b3+c+m+n-p) (1-z)^p Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m+n-p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m] Pochhammer[1-a2-b2-b3+c,m+n-p])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1,((-1)^m x^m (1-y)^n (1-z)^p Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n+p] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^n (1-z)^(-a2-b2-b3+c+m-n+p) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m] Pochhammer[1-a2-b2-b3+c,m-n+p])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,(x^m (1-y)^-n (1/(-1+y))^b2 (1-z)^p Gamma[a2-b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m+n-p])/(m! n! p! Gamma[a2] Gamma[1+a2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2-b3+c+m-n) (1/(-1+y))^(-a2-b3+c) (-1+y)^-m (1-z)^p Gamma[a2-b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m+n-p])/(m! n! p! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^p x^m (1-y)^-n (1/(-1+y))^a2 (-1+y)^-p (1-z)^p Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+p) x^m (1-y)^(-a2-b2-b3+c+m-n) (1/(-1+y))^(-b2-b3+c) (-1+y)^(-m-p) (1-z)^p Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m+p) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+n+p] Pochhammer[-b2-b3+c,m])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+y)/(-1+z)]<1,(x^m (1-y)^n (1-z)^-p (1/(-1+z))^b3 Gamma[a2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b2+c,m-n+p])/(m! n! p! Gamma[a2] Gamma[1+a2+b2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^n (1-z)^(-a2-b2-b3+c+m-p) (1/(-1+z))^(-a2-b2+c) (-1+z)^-m Gamma[a2-b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b2+c,m-n+p])/(m! n! p! Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n+p] Pochhammer[-b2-b3+c,m])+((-1)^n x^m (1-y)^n (1-z)^-p (1/(-1+z))^a2 (-1+z)^-n Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+n) x^m (1-y)^n (1-z)^(-a2-b2-b3+c+m-p) (1/(-1+z))^(-b2-b3+c) (-1+z)^(-m-n) Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+z)/(-1+y)]<1,((-1)^(m+n) x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m-p) (1/(-1+z))^(-a2+c) (-1+z)^(-m-n) ((-1+z)/(-1+y))^n Gamma[a2-b2-b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^n x^m (1/(-1+y))^b2 (1-z)^-p (1/(-1+z))^b3 (-1+z)^-n ((-1+z)/(-1+y))^n Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+n) x^m (1-y)^(-a2-b2-b3+c+m) (1/(-1+y))^(-a2-b3+c) (-1+y)^-m (1-z)^-p (1/(-1+z))^b3 (-1+z)^-n ((-1+z)/(-1+y))^n Gamma[a2-b2-b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+p) x^m (1-y)^(-a2-b2-b3+c+m-n) (1/(-1+y))^(-b2-b3+c) (-1+y)^(-m-p) (1-z)^p Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^p x^m (1-y)^(-b2-n) (1/(-1+y))^(a2+b2) (-1+y)^-p (1-z)^p Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^b2,(-(1/(-1+y)^2))^-b2] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m-p) (1/(-1+z))^(-b2-b3+c) (-1+z)^-m ((-1+z)/(-1+y))^n Gamma[-a2+b2+b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[1-a2+b2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+(x^m (1/(-1+y))^b2 (1-z)^-p (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2-b3+c+m) (1/(-1+y))^(-a2-b3+c) (-1+y)^-m (1-z)^-p (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+y)/(-1+z)]<1,((-1)^(m+p) x^m (1-y)^(-a2-b2+c+m-n) (1/(-1+y))^(-a2+c) (-1+y)^(-m-p) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a2-b2-b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^p x^m (1-y)^-n (1/(-1+y))^b2 (-1+y)^-p (1/(-1+z))^b3 ((-1+y)/(-1+z))^p Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+p) x^m (1-y)^-n (1/(-1+y))^b2 (-1+y)^-p (1-z)^(-a2-b2-b3+c+m) (1/(-1+z))^(-a2-b2+c) ((-1+y)/(-1+z))^p (-1+z)^-m Gamma[a2-b2-b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2+c+m-n) (1/(-1+y))^(-b2-b3+c) (-1+y)^-m (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[-a2+b2+b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[1-a2+b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+(x^m (1-y)^-n (1/(-1+y))^(a2-b3) (1/(-1+z))^b3 ((-1+y)/(-1+z))^p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^-n (1/(-1+y))^(a2-b3) (1-z)^(-a2-b2-b3+c+m) (1/(-1+z))^(-a2-b2+c) ((-1+y)/(-1+z))^p (-1+z)^-m Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^(m+n) x^m (1-y)^n (1-z)^(-a2-b2-b3+c+m-p) (1/(-1+z))^(-b2-b3+c) (-1+z)^(-m-n) Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])+((-1)^n x^m (1-y)^n (1-z)^(-b3-p) (1/(-1+z))^(a2+b3) (-1+z)^-n Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^b3,(-(1/(-1+z)^2))^-b3] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])},{1/Abs[x]<1&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x-x y]<Abs[x]/(1+Abs[x])&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[1-y]+Abs[z]<1,((-1)^p (-x)^-a1 x^-m (1-y)^n z^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-1)^p (-x)^-b1 x^-m (1-y)^n z^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) z^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p] Pochhammer[1-a2-b2+c,m+n])+((-1)^(n+p) (-x)^(a2+b2-c+n) x^-m (1-y)^n z^p Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-a2,m-n-p] Pochhammer[a2,n+p] Pochhammer[1-b2,m-n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b2+c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n])},{1/Abs[x]<1&&Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x-x z]<Abs[x]/(1+Abs[x])&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y]+Abs[1-z]<1,((-1)^n (-x)^-a1 x^-m y^n (1-z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b3-c,m+p])+((-1)^n (-x)^-b1 x^-m y^n (1-z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b3-c,m+p])+((-1)^m x^m y^n (1-z)^(-a2-b3+c+m+p) Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+p] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n] Pochhammer[1-a2-b3+c,m+p])+((-1)^(n+p) (-x)^(a2+b3-c+p) x^-m y^n (1-z)^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[1-a2,m-n-p] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1-b3,m-p] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-p] Pochhammer[1-a2-b1-b3+c,m-p])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&1+Abs[z]<Abs[-1+y]&&Abs[z/(-1+y)]<1&&1/Abs[-1+y]+Abs[z/(-1+y)]<1,((-1)^(n+p) (-x)^-a1 x^-m (1-y)^(-b2-n) (-z)^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+a2-c,m-n])+((-1)^n (-x)^-a1 x^-m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2-c,m-n-p])+((-1)^(n+p) (-x)^-b1 x^-m (1-y)^(-b2-n) (-z)^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b1-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2-c,m-n-p])},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&1+Abs[y]<Abs[-1+z]&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]+1/Abs[-1+z]<1,((-1)^(n+p) (-x)^-a1 x^-m (-y)^n (1-z)^(-b3-p) Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+a2-c,m-p])+((-1)^p (-x)^-a1 x^-m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b3-c,m-n-p])+((-1)^(n+p) (-x)^-b1 x^-m (-y)^n (1-z)^(-b3-p) Gamma[a1-b1] Gamma[a2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b1-c,m-p])+((-1)^p (-x)^-b1 x^-m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b3-c,m-n-p])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1+1/Abs[z]<1/Abs[z/(-1+y)]&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[z/(-1+y)]<1&&1/Abs[-1+y]+Abs[z/(-1+y)]<1,((-1)^n (-x)^-a1 x^-m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2-c,m-n-p])+((-1)^n (-x)^-b1 x^-m (1-y)^(-a2-n) (z/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^n (-x)^-a1 x^-m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n])+((-1)^n (-x)^-a1 x^-m (1-y)^-b2 (-z)^(-a2+b2-p) (z/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n-p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2-c,m-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+a2-c,-n+p] Pochhammer[-a1-a2+c,n-p])+((-1)^n (-x)^-b1 x^-m (1-y)^-b2 (-z)^(-a2+b2-p) (z/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n-p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1-c,m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+a2+b1-c,-n+p] Pochhammer[-a2-b1+c,n-p])},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1+1/Abs[y]<1/Abs[y/(-1+z)]&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]+1/Abs[-1+z]<1,((-1)^p (-x)^-a1 x^-m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b3-c,m-n-p])+((-1)^p (-x)^-b1 x^-m (1-z)^(-a2-p) (y/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^p (-x)^-a1 x^-m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-p])+((-1)^p (-x)^-a1 x^-m (-y)^(-a2+b3-n) (1-z)^-b3 (y/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b3,p] Pochhammer[-a2+b2+b3,-n+p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2-c,m+n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+a2-c,m-p] Pochhammer[1+a1+a2-c,n-p] Pochhammer[-a1-a2+c,-n+p])+((-1)^p (-x)^-b1 x^-m (-y)^(-a2+b3-n) (1-z)^-b3 (y/(-1+z))^p Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+b2+b3,-n+p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1-c,m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b1-c,m-p] Pochhammer[1+a2+b1-c,n-p] Pochhammer[-a2-b1+c,-n+p])},{1/Abs[x]<1&&Abs[-1+y]<1&&Abs[x (-1+y)]<1&&Abs[-1+y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x-x y]<Abs[x]/(1+Abs[x])&&Abs[(-1+y)/z]<1&&1/Abs[x]+1/Abs[z]<1&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+Abs[-1+y]),((-x)^-a1 x^-m (1-y)^n (-z)^(-a2-n) z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b2-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b2-c,m+n])+((-x)^-b1 x^-m (1-y)^n (-z)^(-a2-n) z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b2-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b1+b2-c,m+n])+((-x)^-a1 x^-m (1-y)^n (-z)^(-b3-n) z^(n-p) Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+a2+b2-c,m+n])+((-x)^-b1 x^-m (1-y)^n (-z)^(-b3-n) z^(n-p) Gamma[a1-b1] Gamma[a2-b3] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^(m+n) x^m (1-y)^(-a2-b2+c+m+n) (-z)^-b3 z^-p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1+b2+b3-c,-m-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^p (-x)^(a2+b2-c+n) x^-m (1-y)^n (-z)^(-b3-n) z^(n-p) Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-b2,m-n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a2+b3,m-n+p] Pochhammer[-a2-b2+c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[a2-b3,n-p] Pochhammer[1-a2+b3,-n+p]^2 Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n])},{1/Abs[x]<1&&Abs[-1+z]<1&&Abs[x (-1+z)]<1&&Abs[-1+z]+Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x-x z]<Abs[x]/(1+Abs[x])&&Abs[(-1+z)/y]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+Abs[-1+z]),((-x)^-a1 x^-m (-y)^(-a2-p) y^-n (1-z)^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b3-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b3-c,m+p])+((-x)^-b1 x^-m (-y)^(-a2-p) y^-n (1-z)^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b3-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b1+b3-c,m+p])+((-x)^-a1 x^-m (-y)^(-b2-p) y^(-n+p) (1-z)^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+a2+b3-c,m+p])+((-x)^-b1 x^-m (-y)^(-b2-p) y^(-n+p) (1-z)^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b1+b3-c,m+p])+((-1)^(m+p) x^m (-y)^-b2 y^-n (1-z)^(-a2-b3+c+m+p) Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1+b2+b3-c,-m+n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])+((-1)^n (-x)^(a2+b3-c+p) x^-m (-y)^(-b2-p) y^(-n+p) (1-z)^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[b2,n] Pochhammer[1-a2+b2,m+n-p] Pochhammer[1-b3,m-p] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[a2-b2,-n+p] Pochhammer[1-a2+b2,n-p]^2 Pochhammer[1-a1-a2-b3+c,m-p] Pochhammer[1-a2-b1-b3+c,m-p])},{1/Abs[x]<1&&1+1/Abs[1-y]<Abs[x]&&1/Abs[1-y]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[(1-y)/z]<1&&Abs[(1-y)/z]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/z]<1&&1/Abs[x]+1/Abs[z]<1&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1,((-x)^-a1 x^-m ((-1+y)/z)^n (-z)^-a2 z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b2-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b2-c,m+n])+((-x)^-b1 x^-m ((-1+y)/z)^n (-z)^-a2 z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b2-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^n (-x)^-a1 x^-m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n])+((-1)^n (-x)^-b1 x^-m (1-y)^(-b2-n) (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n])+((-x)^-a1 x^-m (1-y)^(-a2+b3-n) ((1-y)/z)^p (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+p] Pochhammer[1+a1+b2+b3-c,-n+p] Pochhammer[-a1-b2-b3+c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+b2+b3-c,m-n+p])+((-x)^-b1 x^-m (1-y)^(-a2+b3-n) ((1-y)/z)^p (-z)^-b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,-n+p] Pochhammer[-b1-b2-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n+p])},{1/Abs[x]<1&&1+1/Abs[1-z]<Abs[x]&&1/Abs[1-z]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[(1-z)/y]<1&&Abs[(1-z)/y]<1/(1+1/Abs[1-z])&&Abs[(-1+z)/y]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1&&1/Abs[y]<1,((-x)^-a1 x^-m (-y)^-a2 y^-n ((-1+z)/y)^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b3-c,m+n+p])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b3-c,m+p])+((-x)^-b1 x^-m (-y)^-a2 y^-n ((-1+z)/y)^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b3-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b1+b3-c,m+p])+((-1)^p (-x)^-a1 x^-m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-p])+((-1)^p (-x)^-b1 x^-m (-y)^-b2 y^-n (1-z)^(-b3-p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-p])+((-x)^-a1 x^-m (-y)^-b2 (1-z)^(-a2+b2-p) ((1-z)/y)^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m+n] Pochhammer[1+a1+b2+b3-c,n-p] Pochhammer[-a1-b2-b3+c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+b2+b3-c,m+n-p])+((-x)^-b1 x^-m (-y)^-b2 (1-z)^(-a2+b2-p) ((1-z)/y)^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,n-p] Pochhammer[-b1-b2-b3+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+b1+b2+b3-c,m+n-p])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[z/y]<1,((-1)^p (-x)^-a1 x^-m (-y)^(-a2-p) y^-n z^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p])+((-x)^-a1 x^-m (-y)^-b2 y^-n (-z)^(-a2+b2) z^(n-p) Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p])+((-1)^p (-x)^-b1 x^-m (-y)^(-a2-p) y^-n z^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m+n])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^(-a2+b2) z^(n-p) Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p])+((-x)^-a1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[y/z]<1,((-x)^-a1 x^-m (-y)^(-a2+b3) y^(-n+p) (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p])+((-1)^n (-x)^-a1 x^-m y^n (-z)^(-a2-n) z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p])+((-x)^-b1 x^-m (-y)^(-a2+b3) y^(-n+p) (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p])+((-1)^n (-x)^-b1 x^-m y^n (-z)^(-a2-n) z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m+p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p])+((-x)^-a1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2+b3-c,m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p])},{Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1&&Abs[x]>1,((-x)^-a1 x^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 (z/(-1+z))^p Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[1+a1-c,m-n-p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m-n-p])+((-x)^-b1 x^-m (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 (z/(-1+z))^p Gamma[a1-b1] Gamma[c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n-p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n-p])},{Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z/(-1+z)]<1&&Abs[x]>1,((-1)^m x^m (1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (-(y/(-1+y)))^-m (1-z)^-a2 (z/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1-a2+b3,n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^n (-x)^(a2+b2-c+n) x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-a2,m-p] Pochhammer[a2,p] Pochhammer[1-b2,m-n] Pochhammer[b2,n] Pochhammer[1-a2+b3,m] Pochhammer[1+b2-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a2-b2+c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a2+b3,m-p] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n] Pochhammer[-b2-b3+c,-n+p])+((-x)^-a1 x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[1+a1+b2+b3-c,m+n] Pochhammer[1+a1+b2+b3-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a1-b2-b3+c,-n+p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+a1+a2+b2-c,m+n] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+a1+b2+b3-c,m+n-p] Pochhammer[-a1-b2+c,-n+p] Pochhammer[-b2-b3+c,-n+p])+((-x)^-b1 x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-b1-b2-b3+c,-n+p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b2-c,n-p] Pochhammer[1+a2+b1+b2-c,m+n] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[-b1-b2+c,-n+p] Pochhammer[-b2-b3+c,-n+p])},{Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[y/(-1+y)]<1&&Abs[x]>1,((-1)^m x^m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (-(z/(-1+z)))^-m Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1-a2+b2,p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1-a2+b2,-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])+((-1)^p (-x)^(a2+b3-c+p) x^-m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[1-a2+b2,m] Pochhammer[1-b3,m-p] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a2-b3+c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a2+b2,m-n] Pochhammer[1+b2+b3-c,-n+p] Pochhammer[1-a1-a2-b3+c,m-p] Pochhammer[1-a2-b1-b3+c,m-p] Pochhammer[-b2-b3+c,n-p])+((-x)^-a1 x^-m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[1+a1+b2+b3-c,m+p] Pochhammer[1+a1+b2+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a1-b2-b3+c,n-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+a1+a2+b3-c,m+p] Pochhammer[1+b2+b3-c,-n+p] Pochhammer[1+a1+b2+b3-c,m-n+p] Pochhammer[-a1-b3+c,n-p] Pochhammer[-b2-b3+c,n-p])+((-x)^-b1 x^-m (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-b1-b2-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[1+a2+b1+b3-c,m+p] Pochhammer[1+b2+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m-n+p] Pochhammer[-b1-b3+c,n-p] Pochhammer[-b2-b3+c,n-p])},{Abs[(y-z)/(-1+y)]<1&&1+1/Abs[z]<1/Abs[z/(x-x z)]&&1/Abs[z]<1&&Abs[z/(x-x z)]<1&&Abs[(y-z)/(z-y z)]<1&&1+Abs[(y-z)/(z-y z)]<1/Abs[z/(x-x z)],((-1)^p (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(a2-c-n) z^-p (z/(x (-1+z)))^m Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-a1,(1/(x (-1+1/z)))^a1] Pochhammer[a1,m] Pochhammer[1-a2,p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((-1)^p (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) z^-p (z/(x (-1+z)))^m Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-a1,(1/(x (-1+1/z)))^a1] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1-b2-b3,-n+p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((-1)^p (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(a2-c-n) z^-p (z/(x (-1+z)))^m Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-b1,(1/(x (-1+1/z)))^b1] Pochhammer[1-a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((-1)^p (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) z^-p (z/(x (-1+z)))^m Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-b1,(1/(x (-1+1/z)))^b1] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b2-b3,-n+p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+b1+b2+b3-c,m-p])},{Abs[(-y+z)/(-1+z)]<1&&1+1/Abs[y]<1/Abs[y/(x-x y)]&&1/Abs[y]<1&&Abs[y/(x-x y)]<1&&Abs[(-y+z)/(y-y z)]<1&&1+Abs[(-y+z)/(y-y z)]<1/Abs[y/(x-x y)],((-1)^n (1-y)^(-a2-b2+c) (-y)^(a2-c-p) y^-n (y/(x (-1+y)))^m (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-a1,(1/(x (-1+1/y)))^a1] Pochhammer[a1,m] Pochhammer[1-a2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((-1)^n (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(x (-1+y)))^m (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-a1,(1/(x (-1+1/y)))^a1] Pochhammer[a1,m] Pochhammer[1-b2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+b2+b3-c,m-n])+((-1)^n (1-y)^(-a2-b2+c) (-y)^(a2-c-p) y^-n (y/(x (-1+y)))^m (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-b1,(1/(x (-1+1/y)))^b1] Pochhammer[1-a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((-1)^n (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(x (-1+y)))^m (1-z)^-b3 ((-y+z)/(-1+z))^p Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-b1,(1/(x (-1+1/y)))^b1] Pochhammer[b1,m] Pochhammer[1-b2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n])},{1/Abs[x]<1&&1+1/Abs[1-y]<Abs[x]&&1/Abs[1-y]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1/Abs[x]+1/Abs[x-x y]<1&&1/Abs[x-x y]<1&&Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[x (-1+z)]<Abs[x]/(1+Abs[x])&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,((1/x)^m (-x)^-a1 (1/(-1+y))^(a2+n) ((-1+z)/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1-y)^(-a2-b3+c) (1/(-1+y))^(-2 b2-b3+c+n) ((-1+z)/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^-b2,(-(1/(-1+y)^2))^b2] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-b2-b3+c+n) ((-1+z)/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1-y)^-b2 (1/(-1+y))^(a2+b2+n) ((-1+z)/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^b2,(-(1/(-1+y)^2))^-b2] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1/(1-y))^n (1-y)^b2 (1-z)^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b3+c] Gamma[-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^-b2,(-(1/(-1+y)^2))^b2] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b3-c,-n+p] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[-a1-a2-b3+c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1+a2+b3-c] Gamma[-a1-a2+c] Gamma[-a2-b3+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+a2+b3-c,m-n+p])+((1/x)^m (-x)^-a1 (1/(1-y))^n (1-y)^(-a2-b3+c) (1/(-1+y))^(-a2-b2-b3+c) (1-z)^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b3+c] Gamma[1-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^-b2,(-(1/(-1+y)^2))^b2] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b3-c,-n+p] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[-a1-a2-b3+c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+a2+b3-c,m-n+p])+((1/x)^m (-x)^-b1 (1/(1-y))^n (1/(-1+y))^b2 (1-z)^p Gamma[a1-b1] Gamma[a2-b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b3+c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a2-b1-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1+a2+b3-c] Gamma[-a2-b1+c] Gamma[-a2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b1+b3-c,m-n+p])+((1/x)^m (-x)^-b1 (1/(1-y))^n (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1-z)^p Gamma[a1-b1] Gamma[a2-b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a2-b1-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a2+b1+b3-c,m-n+p])+((1/x)^m (-x)^(a2+b3-c) (1/(-1+y))^b2 (1/(x (-1+y)))^n (x (-1+z))^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b2,n] Pochhammer[1-a2+b2,m+n-p] Pochhammer[1-b3,m+n-p] Pochhammer[b3,p] Pochhammer[a1+a2+b3-c,-n+p] Pochhammer[a2+b1+b3-c,-n+p] Pochhammer[-a2-b3+c,m+n-p] Pochhammer[1-a1-a2-b3+c,n-p] Pochhammer[1-a2-b1-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1+a2+b3-c] Pochhammer[a2-b2,-n+p] Pochhammer[1-a2+b2,n-p]^2 Pochhammer[1-b3,n-p] Pochhammer[b3,-n+p] Pochhammer[1-a1-a2-b3+c,m+n-p] Pochhammer[1-a2-b1-b3+c,m+n-p])+((1/x)^m (-x)^(a2+b3-c) (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1/(x (-1+y)))^n (x (-1+z))^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b2,n] Pochhammer[1-a2+b2,m+n-p] Pochhammer[1-b3,m+n-p] Pochhammer[b3,p] Pochhammer[a1+a2+b3-c,-n+p] Pochhammer[a2+b1+b3-c,-n+p] Pochhammer[-a2-b3+c,m+n-p] Pochhammer[1-a1-a2-b3+c,n-p] Pochhammer[1-a2-b1-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[b3] Pochhammer[a2-b2,-n+p] Pochhammer[1-a2+b2,n-p]^2 Pochhammer[1-b3,n-p] Pochhammer[b3,-n+p] Pochhammer[1-a1-a2-b3+c,m+n-p] Pochhammer[1-a2-b1-b3+c,m+n-p])+((1-y)^-b2 (1-z)^(-a2-b3+c+p) (x (-1+z))^m ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+n+p] Pochhammer[-b2-b3+c,m])},{1/Abs[x]<1&&1+1/Abs[1-z]<Abs[x]&&1/Abs[1-z]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1/Abs[x]+1/Abs[x-x z]<1&&1/Abs[x-x z]<1&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[x (-1+y)]<Abs[x]/(1+Abs[x])&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[(-1+y)/(-1+z)]<1,((1/x)^m (-x)^-a1 (1/(-1+z))^(a2+p) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-a1 (1-z)^(-a2-b2+c) (1/(-1+z))^(-b2-2 b3+c+p) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^-b3,(-(1/(-1+z)^2))^b3] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c+p) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-z)^-b3 (1/(-1+z))^(a2+b3+p) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^b3,(-(1/(-1+z)^2))^-b3] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b2+b3-c,m-p])+((1/x)^m (-x)^-a1 (1-y)^n (1/(1-z))^p (1-z)^b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b2+c] Gamma[-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^-b3,(-(1/(-1+z)^2))^b3] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b2-c,n-p] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[-a1-a2-b2+c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1+a2+b2-c] Gamma[-a1-a2+c] Gamma[-a2-b2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+a2+b2-c,m+n-p])+((1/x)^m (-x)^-a1 (1-y)^n (1/(1-z))^p (1-z)^(-a2-b2+c) (1/(-1+z))^(-a2-b2-b3+c) Gamma[-a1+b1] Gamma[a2-b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b2+c] Gamma[1-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^-b3,(-(1/(-1+z)^2))^b3] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b2-c,n-p] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[-a1-a2-b2+c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+a2+b2-c,m+n-p])+((1/x)^m (-x)^-b1 (1-y)^n (1/(1-z))^p (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b2+c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b2-c,n-p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a2-b1-b2+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1+a2+b2-c] Gamma[-a2-b1+c] Gamma[-a2-b2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b1+b2-c,m+n-p])+((1/x)^m (-x)^-b1 (1-y)^n (1/(1-z))^p (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[a1-b1] Gamma[a2-b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b2-c,n-p] Pochhammer[1+b1+b2+b3-c,m] Pochhammer[-a2-b1-b2+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a2+b1+b2-c,m+n-p])+((1/x)^m (-x)^(a2+b2-c) (x (-1+y))^n (1/(-1+z))^b3 (1/(x (-1+z)))^p Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[1-b2,m-n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a2+b3,m-n+p] Pochhammer[a1+a2+b2-c,n-p] Pochhammer[a2+b1+b2-c,n-p] Pochhammer[-a2-b2+c,m-n+p] Pochhammer[1-a1-a2-b2+c,-n+p] Pochhammer[1-a2-b1-b2+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1+a2+b2-c] Pochhammer[1-b2,-n+p] Pochhammer[b2,n-p] Pochhammer[a2-b3,n-p] Pochhammer[1-a2+b3,-n+p]^2 Pochhammer[1-a1-a2-b2+c,m-n+p] Pochhammer[1-a2-b1-b2+c,m-n+p])+((1/x)^m (-x)^(a2+b2-c) (x (-1+y))^n (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) (1/(x (-1+z)))^p Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[1-b2,m-n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1-a2+b3,m-n+p] Pochhammer[a1+a2+b2-c,n-p] Pochhammer[a2+b1+b2-c,n-p] Pochhammer[-a2-b2+c,m-n+p] Pochhammer[1-a1-a2-b2+c,-n+p] Pochhammer[1-a2-b1-b2+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[1-b2,-n+p] Pochhammer[b2,n-p] Pochhammer[a2-b3,n-p] Pochhammer[1-a2+b3,-n+p]^2 Pochhammer[1-a1-a2-b2+c,m-n+p] Pochhammer[1-a2-b1-b2+c,m-n+p])+((1-y)^(-a2-b2+c+n) (x (-1+y))^m (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n+p] Pochhammer[-b2-b3+c,m])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[(-1+z)/(-1+y)]<1,((1/x)^m (-x)^-a1 (1-y)^-b2 (1/(-1+y))^n (1-z)^(-a2-b3+c) (1/(-1+z))^(-a2+c+p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1/(-1+y))^(b2+n) (1/(-1+z))^(b3+p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c+n) (1/(-1+z))^(b3+p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-b2-b3+c+n) ((-1+z)/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1-y)^-b2 (1/(-1+y))^(a2+b2+n) ((-1+z)/(-1+y))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^b2,(-(1/(-1+y)^2))^-b2] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1-y)^-b2 (1-z)^(-a2-b3+c) (1/(-1+z))^(-b2-b3+c+p) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-a2+b2] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-a1 (1/(-1+y))^b2 (1/(-1+z))^(a2-b2+p) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1/(-1+z))^(a2-b2+p) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-y)^-b2 (1/(-1+y))^n (1-z)^(-a2-b3+c) (1/(-1+z))^(-a2+c+p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1/(-1+y))^(b2+n) (1/(-1+z))^(b3+p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c+n) (1/(-1+z))^(b3+p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-b2-b3+c+n) ((-1+z)/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1-y)^-b2 (1/(-1+y))^(a2+b2+n) ((-1+z)/(-1+y))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[y]>0,(-(-1+y)^2)^b2,(-(1/(-1+y)^2))^-b2] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1-y)^-b2 (1-z)^(-a2-b3+c) (1/(-1+z))^(-b2-b3+c+p) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-a2+b2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+b1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1/(-1+y))^b2 (1/(-1+z))^(a2-b2+p) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+b1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1/(-1+z))^(a2-b2+p) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+b1+b2+b3-c,m-p])},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[(-1+y)/(-1+z)]<1,((1/x)^m (-x)^-a1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-a2+c+n) (1-z)^-b3 (1/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1/(-1+y))^(b2+n) (1/(-1+z))^(b3+p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1/(-1+y))^(b2+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c+p) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+a2-c,m-n-p])+((1/x)^m (-x)^-a1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-b2-b3+c+n) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1/(-1+y))^(a2-b3+n) (1/(-1+z))^b3 ((-1+y)/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1/(-1+y))^(a2-b3+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) ((-1+y)/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+b2+b3-c,m-n])+((1/x)^m (-x)^-a1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c+p) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-a1 (1-z)^-b3 (1/(-1+z))^(a2+b3+p) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^b3,(-(1/(-1+z)^2))^-b3] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-a2+c+n) (1-z)^-b3 (1/(-1+z))^p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1/(-1+y))^(b2+n) (1/(-1+z))^(b3+p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1/(-1+y))^(b2+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c+p) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a2+b1-c,m-n-p])+((1/x)^m (-x)^-b1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-b2-b3+c+n) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1/(-1+y))^(a2-b3+n) (1/(-1+z))^b3 ((-1+y)/(-1+z))^p Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1/(-1+y))^(a2-b3+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) ((-1+y)/(-1+z))^p Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+b1+b2+b3-c,m-n])+((1/x)^m (-x)^-b1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c+p) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b2+b3-c,m-p])+((1/x)^m (-x)^-b1 (1-z)^-b3 (1/(-1+z))^(a2+b3+p) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] If[-2+2 Re[z]>0,(-(-1+z)^2)^b3,(-(1/(-1+z)^2))^-b3] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b2+b3-c,m-p])},{Abs[x]>1&&Abs[1-y]<1&&Abs[1-z]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x]/(1+Abs[x])>Abs[x-x y]&&Abs[x-x z]<1&&Abs[x]/(1+Abs[x])>Abs[x-x z],((-x)^-a1 x^-m (1-y)^n (1-z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2+b3-c,m+n+p])+((-x)^-b1 x^-m (1-y)^n (1-z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2+b3-c,m+n+p])+((-1)^m x^m (1-y)^-b2 (1-z)^(-a2-b3+c+m+p) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+n+p] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^(-a2-b2-b3+c+m+n-p) (1-z)^p Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m+n-p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m] Pochhammer[1-a2-b2-b3+c,m+n-p])+((-1)^(n+p) (-x)^(a2+b2+b3-c+n+p) x^-m (1-y)^n (1-z)^p Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[1-a2,m-n-p] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1-b2-b3,m-n-p] Pochhammer[b3,p] Pochhammer[-a2-b2-b3+c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-a1-a2-b2-b3+c,m-n-p] Pochhammer[1-a2-b1-b2-b3+c,m-n-p])},{Abs[x]>1&&Abs[1-z]<1&&Abs[1-y]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x]/(1+Abs[x])>Abs[x-x z]&&Abs[x-x y]<1&&Abs[x]/(1+Abs[x])>Abs[x-x y],((-x)^-a1 x^-m (1-y)^n (1-z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2+b3-c,m])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2+b3-c,m+n+p])+((-x)^-b1 x^-m (1-y)^n (1-z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2+b3-c,m])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2+b3-c,m+n+p])+((-1)^m x^m (1-y)^(-a2-b2+c+m+n) (1-z)^-b3 ((-1+y)/(-1+z))^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n+p] Pochhammer[-b2-b3+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n+p] Pochhammer[-b2-b3+c,m])+((-1)^m x^m (1-y)^n (1-z)^(-a2-b2-b3+c+m-n+p) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-n+p] Pochhammer[-b2-b3+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m] Pochhammer[1-a2-b2-b3+c,m-n+p])+((-1)^(n+p) (-x)^(a2+b2+b3-c+n+p) x^-m (1-y)^n (1-z)^p Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[1-a2,m-n-p] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1-b2-b3,m-n-p] Pochhammer[b3,p] Pochhammer[-a2-b2-b3+c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-a1-a2-b2-b3+c,m-n-p] Pochhammer[1-a2-b1-b2-b3+c,m-n-p])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[z/y]<1,((-1)^p (-x)^-a1 x^-m (-y)^(-a2-p) y^-n z^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p])+((-1)^p (-x)^-b1 x^-m (-y)^(-a2-p) y^-n z^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m+n])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p])+((-x)^-a1 x^-m (-y)^-b2 y^-n z^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[-b2+c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[-a1-b2+c,-n+p])+((-x)^-b1 x^-m (-y)^-b2 y^-n z^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,n-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[-b2+c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[-b1-b2+c,-n+p])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1,((-1)^n (-x)^-a1 x^-m y^n (-z)^(-a2-n) z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p])+((-1)^n (-x)^-b1 x^-m y^n (-z)^(-a2-n) z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m+p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p])+((-x)^-a1 x^-m y^n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[-b3+c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[-a1-b3+c,n-p])+((-x)^-b1 x^-m y^n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-n+p] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[-b1-b3+c,n-p])},{Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x]>1,((-x)^-a1 x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^a2 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b2-c,m+n+p] Pochhammer[1+a1+b2+b3-c,m+n])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n] Pochhammer[1+a1+a2+b2+b3-c,m+n+p])+((-x)^-b1 x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^a2 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b2-c,m+n+p] Pochhammer[1+b1+b2+b3-c,m+n])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n] Pochhammer[1+a2+b1+b2+b3-c,m+n+p])+((-1)^(m+n) x^m (1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (-(y/(-1+y)))^-m (1-z)^-a2 (-((-1+z)/z))^(a2-b3) ((-1+z)/z)^p (-(z/(-1+z)))^n Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b3,p] Pochhammer[a2-b3,-n+p] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[1-b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2+c,m+n])+((-1)^(m+n) x^m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^(-b2-b3+c) ((-1+z)/z)^p (-(z/(-1+z)))^(-m+n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[-b2-b3+c,m-n+p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2+b2-c,-m+n] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2-b3+c,m-n+p])+((-1)^(n+p) (-x)^(a2+b2+b3-c+n+p) x^-m (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (-((-1+z)/z))^a2 ((-1+z)/z)^p Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[1-a2,m-p] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[1-b3,m] Pochhammer[1-b2-b3,m-n-p] Pochhammer[-a2-b2-b3+c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-b3,m-p] Pochhammer[1-a1-a2-b2-b3+c,m-n-p] Pochhammer[1-a2-b1-b2-b3+c,m-n-p])},{Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x]>1,((-x)^-a1 x^-m (1-y)^-a2 (-((-1+y)/y))^a2 ((-1+y)/y)^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+a2+b3-c,m+n+p] Pochhammer[1+a1+b2+b3-c,m+p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b3-c,m+p] Pochhammer[1+a1+a2+b2+b3-c,m+n+p])+((-x)^-b1 x^-m (1-y)^-a2 (-((-1+y)/y))^a2 ((-1+y)/y)^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+a2+b1+b3-c,m+n+p] Pochhammer[1+b1+b2+b3-c,m+p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b3-c,m+p] Pochhammer[1+a2+b1+b2+b3-c,m+n+p])+((-1)^(m+p) x^m (1-y)^-a2 (-((-1+y)/y))^(a2-b2) ((-1+y)/y)^n (-(y/(-1+y)))^p (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (-(z/(-1+z)))^-m Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[a2-b2,n-p] Pochhammer[1-a2+b2,p] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[1-b2,n-p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b3+c,m+p])+((-1)^(m+p) x^m (1-y)^-a2 (-((-1+y)/y))^(-b2-b3+c) ((-1+y)/y)^n (-(y/(-1+y)))^(-m+p) (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[-b2-b3+c,m+n-p])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2+b3-c,-m+p] Pochhammer[-a2+c,m] Pochhammer[1-a2-b2-b3+c,m+n-p])+((-1)^(n+p) (-x)^(a2+b2+b3-c+n+p) x^-m (1-y)^-a2 (-((-1+y)/y))^a2 ((-1+y)/y)^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[1-b2,m] Pochhammer[1-b2-b3,m-n-p] Pochhammer[b3,p] Pochhammer[-a2-b2-b3+c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-b2,m-n] Pochhammer[1-a1-a2-b2-b3+c,m-n-p] Pochhammer[1-a2-b1-b2-b3+c,m-n-p])},{Abs[1-x]<1&&Abs[y]<1&&1+Abs[y]<1/Abs[1-x]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[z]<1&&1+Abs[z]<1/Abs[1-x]&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1,((-1)^(n+p) (1-x)^m y^n z^p Gamma[c] Gamma[-a1-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p])/(m! n! p! Gamma[-a1+c] Gamma[-b1+c] Pochhammer[1+a1+b1-c,m-n-p] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+m+n+p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x],((-1)^(n+p) (-x)^-a1 x^-m y^n z^p Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1-c,m-n-p])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m])+((-1)^(n+p) (-x)^-b1 x^-m y^n z^p Gamma[a1-b1] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m])},{Abs[1-x]<1&&Abs[y]<1&&1/Abs[1-x]>1+Abs[y]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z]>1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[z-x z]<1,((-1)^n (1-x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1-x)^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b1+b3-c,m-n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+m+n+p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+n) (1-x)^m y^n (-z)^(a1+b1-c+m-n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,-m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b3+c,-m+n+p])},{Abs[y]<1&&1/Abs[1-x]+Abs[y]<1&&Abs[z]<1&&1/Abs[1-x]+Abs[z]<1&&Abs[1-x]>1,((1-x)^(-b1-m) y^n z^p Gamma[a1-b1] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p])+((1-x)^(-a1-m) y^n z^p Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p])},{1/Abs[-1+x]<1&&Abs[y]<1&&1/Abs[-1+x]+Abs[y]<1&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[-1+x]),((-1)^(m+n) (1-x)^(-b1-m) y^n (-z)^(-a2-n) z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^(m+n) (1-x)^(-a1-m) y^n (-z)^(-a2-n) z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b1-c,-m+p])+((-1)^m (1-x)^(-b1-m) y^n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p])+((-1)^m (1-x)^(-a1-m) y^n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+b1+b3-c,-m-n+p])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[z-x z]&&Abs[y]<1&&1+Abs[y]<1/Abs[1-x]&&Abs[(-1+x) y]<1&&Abs[-1+x]+Abs[(-1+x) y]<1&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z-x z]<1&&1/Abs[z-x z]<1/(1+Abs[-1+x])&&Abs[z]>1,((-1)^(n+p) y^n (1/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p (z-x z)^p Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^(-a1-a2-b1+c),(1/(z-x z))^(a1+a2+b1-c)] Pochhammer[a2,m+n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-p])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m+n] Pochhammer[1+a1+a2-c,m-p] Pochhammer[1+a2+b1-c,m-p])+((-1)^(m+2 n) (1-x)^(-a1-b1+c+m+n) y^n (1/((-1+x) z))^p (z-x z)^-n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^-a2,(1/(z-x z))^a2] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p] Pochhammer[a1+a2+b1-c,-m+p])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p] Pochhammer[1+a2+b1-c,-m+p])+((-1)^n (1-x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(2 n+p) y^n (1/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p (z-x z)^(n+p) Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^(-a1-b1-b3+c),(1/(z-x z))^(a1+b1+b3-c)] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[1+a1+b3-c,m-n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a1+b1+b3-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-n] Pochhammer[1+a1+b3-c,m-n-p] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^(m+n) (1-x)^(-a1-b1+c+m+n) y^n (1/((-1+x) z))^p Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^-b3,(1/(z-x z))^b3] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[a1+b1+b3-c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p] Pochhammer[1+b1+b3-c,-m-n+p])+((1-x)^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b1+b3-c,m-n+p])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[1-z]<1&&Abs[y]+Abs[1-z]<1&&Abs[-1+z]<1&&1+Abs[-1+z]<Abs[x-x z]&&Abs[y]+Abs[-1+z]<1&&1/Abs[x-x z]<1&&1/Abs[x-x z]<1/(1+Abs[-1+z]),((-1)^p y^n (1-z)^(-a2-b3+c+p) (1/(x (-1+z)))^m Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[-1+Re[x]+Re[z]>0,(x-x z)^-a1,(1/(x-x z))^a1] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n] Pochhammer[a1+a2+b3-c,m-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-p] Pochhammer[1+a1+b3-c,m-n-p])+((-1)^m (-x)^(a2+b3-c) x^-m y^n (1/(x (-1+z)))^p (x-x z)^m Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] If[-1+Re[x]+Re[z]>0,(x-x z)^(-a1-a2-b3+c),(1/(x-x z))^(a1+a2+b3-c)] Pochhammer[a1,p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[a1+a2+b3-c,-m+p])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,p] Pochhammer[1+a1+a2-c,-m+p] Pochhammer[1+a1+b3-c,-m-n+p])+((-1)^n (-x)^-a1 x^-m y^n (1-z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b3-c,m+p])+((-1)^p y^n (1-z)^(-a2-b3+c+p) (1/(x (-1+z)))^m Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[-1+Re[x]+Re[z]>0,(x-x z)^-b1,(1/(x-x z))^b1] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a2+b1+b3-c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-p] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^m (-x)^(a2+b3-c) x^-m y^n (1/(x (-1+z)))^p (x-x z)^m Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] If[-1+Re[x]+Re[z]>0,(x-x z)^(-a2-b1-b3+c),(1/(x-x z))^(a2+b1+b3-c)] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[a2+b1+b3-c,-m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,p] Pochhammer[1+a2+b1-c,-m+p] Pochhammer[1+b1+b3-c,-m-n+p])+((-1)^n (-x)^-b1 x^-m y^n (1-z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b3-c,m+p])},{Abs[x/(-1+x)]<1&&Abs[y]<1&&Abs[z]<1,((1-x)^-b1 (x/(-1+x))^m y^n z^p Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Pochhammer[c,m+n+p] Pochhammer[-a1+c,n+p])},{Abs[x]<1&&Abs[y/(1-z)]<1&&Abs[z/(-1+z)]<1&&Abs[y/(1-z)]+Abs[z/(-1+z)]<1,(x^m y^n (1-z)^(-a2-n) (z/(-1+z))^p Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-b3+c,m+n+p])/(m! n! p! Pochhammer[c,m+n+p] Pochhammer[-b3+c,m+n])},{Abs[x/(-1+x)]<1&&Abs[y]<1&&Abs[y/z]<1&&Abs[z]>1,((-1)^n (1-x)^-b1 (x/(-1+x))^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p] Pochhammer[1+a1+a2-c,p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((1-x)^-b1 (x/(-1+x))^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-m-n+p] Pochhammer[1+a1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p])},{1/Abs[x]<1&&1/Abs[x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}]}], 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "<", "1"}], "&&", 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]<Abs[x]/(1+Abs[x])&&Abs[z/(-1+z)]<1&&Abs[y/(-1+z)]+Abs[z/(-1+z)]<1,((-1)^n (-x)^-a1 x^-m y^n (1-z)^(-a2-n) (z/(-1+z))^p Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1-c,m-n-p] Pochhammer[1+a1+b3-c,m-n])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b3-c,m-n-p])+((-1)^n (-x)^-b1 x^-m y^n (1-z)^(-a2-n) (z/(-1+z))^p Gamma[a1-b1] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-n-p] Pochhammer[1+b1+b3-c,m-n])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b3-c,m-n-p])},{Abs[(-1+x)/x]<1&&Abs[y]<1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z]>1,((-1)^n (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1/x)^a1 ((-1+x)/x)^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b1+b3-c,m-n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+n+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^(-n-p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+n) (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(a1+b1-c+m-n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b3+c,-m+n+p])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1&&Abs[(-1+z)/z]<1&&1+Abs[(-1+z)/z]<1/Abs[y]&&Abs[y/z]+Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1,((-1)^n (-x)^-a1 x^-m y^n (1/z)^b3 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n+p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b3-c,m+p])+((-1)^n (-x)^-b1 x^-m y^n (1/z)^b3 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n+p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b3-c,m+p])+((-1)^m x^m y^n (1-z)^(-a2-b3+c+m) (1/z)^(-b3+c) ((-1+z)/z)^p z^(-m-n) Gamma[a2+b3-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b3] Pochhammer[-b3+c,m+n] Pochhammer[1-a2-b3+c,m+p])+((-1)^(n+p) (-x)^(a2+b3-c+p) x^-m y^n (1/z)^b3 ((-1+z)/z)^p Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1-b3,m-p] Pochhammer[b3,p] Pochhammer[-a2-b3+c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-p] Pochhammer[1-a2-b1-b3+c,m-p])},{Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1&&Abs[((-1+x) z)/x]<1&&1/Abs[x]+Abs[((-1+x) z)/x]<1&&Abs[x]>1,(((-1+x)/x)^(n+p) (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) y^n z^p Gamma[a1-b1] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p])+(((-1+x)/x)^(n+p) (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) y^n z^p Gamma[-a1+b1] Gamma[c] Pochhammer[a2,n+p] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p])},{Abs[y]<1&&Abs[y/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(x (-1+z))/z]+1/Abs[z]<1&&Abs[z]>1,(x^m y^n ((-1+z)/z)^m (-z)^-b3 z^-p (z/(-1+z))^(a2+b3-c) Gamma[a2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a2+c,m+p])/(m! n! p! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])+((-1)^n x^m y^n ((-1+z)/z)^m (-z)^(-a2-n) z^-p (z/(-1+z))^(a2+b3-c) Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[-b3+c,m+n+p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m+n])},{Abs[(-1+x)/x]<1&&Abs[y]<1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[x/(z-x z)]<1&&Abs[z]>1,((-1)^(n+p) (1/x)^a1 y^n (x/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p ((-1+1/x) z)^p Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^(-a1-a2-b1+c),(1/((-1+1/x) z))^(a1+a2+b1-c)] Pochhammer[a2,m+n] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-p])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m+n] Pochhammer[1+a2+b1-c,m-p])+((-1)^(2 n) (1-x)^(-a1-b1+c+n) (1/x)^(-a1+c) ((-1+x)/x)^m x^-n y^n (x/((-1+x) z))^p ((-1+1/x) z)^-n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-a2,(1/((-1+1/x) z))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[a1+a2+b1-c,-m+p])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^n (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(2 n+p) (1/x)^a1 y^n (x/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p ((-1+1/x) z)^(n+p) Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^(-a1-b1-b3+c),(1/((-1+1/x) z))^(a1+b1+b3-c)] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a1+b1+b3-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-n] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^n (1-x)^(-a1-b1+c+n) (1/x)^(-a1+c) ((-1+x)/x)^m x^-n y^n (x/((-1+x) z))^p Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-b3,(1/((-1+1/x) z))^b3] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[a1+b1+b3-c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p])+((1/x)^a1 ((-1+x)/x)^m y^n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b1+b3-c,m-n+p])},{x!=0&&Abs[y]>0&&Abs[x]!=1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1&&Abs[(-1+z)/z]<1&&Abs[y] (1+Abs[(-1+z)/z])<1&&Abs[y/z]+Abs[(-1+z)/z]<1&&Abs[z/(x-x z)]<1&&Abs[z/(x-x z)]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}]}], 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], " ", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}]}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], "<", "1"}], "&&", 
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[x]<Abs[x]^2,(y^n (1-z)^(-a2-b3+c) (1/z)^(-b3+c) ((-1+z)/z)^p z^-n (z/(x (-1+z)))^m Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-a1,(1/(x (-1+1/z)))^a1] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[1+a1+b3-c,m-n] Pochhammer[a1+a2+b3-c,m-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b3-c,m-n-p])+((-1)^(m+n) (-x)^(a2+b3-c) x^-m y^n (x (-1+1/z))^m (1/z)^b3 (z/(x (-1+z)))^p Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^(-a1-a2-b3+c),(1/(x (-1+1/z)))^(a1+a2+b3-c)] Pochhammer[a1,p] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[a1+a2+b3-c,-m+p])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^n (-x)^-a1 x^-m y^n (1/z)^b3 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b3-c,m-n+p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b3-c,m+p])+(y^n (1-z)^(-a2-b3+c) (1/z)^(-b3+c) ((-1+z)/z)^p z^-n (z/(x (-1+z)))^m Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-b1,(1/(x (-1+1/z)))^b1] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1-b3,p] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a2+b1+b3-c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^(m+n) (-x)^(a2+b3-c) x^-m y^n (x (-1+1/z))^m (1/z)^b3 (z/(x (-1+z)))^p Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^(-a2-b1-b3+c),(1/(x (-1+1/z)))^(a2+b1+b3-c)] Pochhammer[1-a2,m-n] Pochhammer[a2,n] Pochhammer[b1,p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,p] Pochhammer[a2+b1+b3-c,-m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,p] Pochhammer[1+a2+b1-c,-m+p])+((-1)^n (-x)^-b1 x^-m y^n (1/z)^b3 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b3-c,m-n+p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b3-c,m+p])},{Abs[1-x]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y]>1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y-x y]<1&&Abs[z]<1&&1/Abs[1-x]>1+Abs[z]&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[z/y]<1,((-1)^p (1-x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((1-x)^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b1+b2-c,m+n-p])+((-1)^(n+p) (1-x)^(-a1-b1+c+m+n+p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+p) (1-x)^m (-y)^(a1+b1-c+m-p) y^-n z^p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,-m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n+p])},{1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x])&&Abs[z]<1&&1/Abs[-1+x]+Abs[z]<1&&Abs[z/y]<1,((-1)^(m+p) (1-x)^(-b1-m) (-y)^(-a2-p) y^-n z^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^(m+p) (1-x)^(-a1-m) (-y)^(-a2-p) y^-n z^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b1-c,-m+n])+((-1)^m (1-x)^(-b1-m) (-y)^-b2 y^-n z^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p])+((-1)^m (1-x)^(-a1-m) (-y)^-b2 y^-n z^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+b1+b2-c,-m+n-p])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&1/Abs[y]<1&&1/Abs[y-x y]<1&&1/Abs[y-x y]<1/(1+Abs[-1+x])&&Abs[z]<1&&1+Abs[z]<1/Abs[1-x]&&Abs[(-1+x) z]<1&&Abs[-1+x]+Abs[(-1+x) z]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^(n+p) (1/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n (y-x y)^n z^p Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-a2-b1+c),(1/(y-x y))^(a1+a2+b1-c)] Pochhammer[a2,m+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m+p] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])+((-1)^(m+2 p) (1-x)^(-a1-b1+c+m+p) (1/((-1+x) y))^n (y-x y)^-p z^p Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-a2,(1/(y-x y))^a2] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a2+b1-c,-m+n])+((-1)^p (1-x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^(n+2 p) (1/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n (y-x y)^(n+p) z^p Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-b1-b2+c),(1/(y-x y))^(a1+b1+b2-c)] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,m-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a1+b1+b2-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-p] Pochhammer[1+a1+b2-c,m-n-p] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^(m+p) (1-x)^(-a1-b1+c+m+p) (1/((-1+x) y))^n z^p Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-b2,(1/(y-x y))^b2] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[a1+b1+b2-c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p] Pochhammer[1+b1+b2-c,-m+n-p])+((1-x)^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b1+b2-c,m+n-p])},{1/Abs[x]<1&&Abs[1-y]<1&&Abs[-1+y]<1&&1+Abs[-1+y]<Abs[x-x y]&&1/Abs[x-x y]<1&&1/Abs[x-x y]<1/(1+Abs[-1+y])&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[1-y]+Abs[z]<1&&Abs[-1+y]+Abs[z]<1,((-1)^n (1-y)^(-a2-b2+c+n) (1/(x (-1+y)))^m z^p Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^-a1,(1/(x-x y))^a1] Pochhammer[a1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p] Pochhammer[a1+a2+b2-c,m-n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m-n-p])+((-1)^m (-x)^(a2+b2-c) x^-m (1/(x (-1+y)))^n (x-x y)^m z^p Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^(-a1-a2-b2+c),(1/(x-x y))^(a1+a2+b2-c)] Pochhammer[a1,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a1+b2-c,n-p] Pochhammer[a1+a2+b2-c,-m+n])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a1+b2-c,-m+n-p])+((-1)^p (-x)^-a1 x^-m (1-y)^n z^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-1)^n (1-y)^(-a2-b2+c+n) (1/(x (-1+y)))^m z^p Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^-b1,(1/(x-x y))^b1] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a2+b1+b2-c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^m (-x)^(a2+b2-c) x^-m (1/(x (-1+y)))^n (x-x y)^m z^p Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] If[-1+Re[x]+Re[y]>0,(x-x y)^(-a2-b1-b2+c),(1/(x-x y))^(a2+b1+b2-c)] Pochhammer[b1,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,n] Pochhammer[1+b1+b2-c,n-p] Pochhammer[a2+b1+b2-c,-m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+n] Pochhammer[1+b1+b2-c,-m+n-p])+((-1)^p (-x)^-b1 x^-m (1-y)^n z^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[z/(1-y)]<1&&Abs[y/(-1+y)]+Abs[z/(1-y)]<1,(x^m (1-y)^(-a2-p) (y/(-1+y))^n z^p Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Pochhammer[c,m+n+p] Pochhammer[-b2+c,m+p])},{Abs[x/(-1+x)]<1&&Abs[z]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^p (1-x)^-b1 (x/(-1+x))^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n] Pochhammer[1+a1+a2-c,n])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((1-x)^-b1 (x/(-1+x))^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n-p] Pochhammer[1+a1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p])},{1/Abs[x]<1&&1/Abs[x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "<", "1"}], "&&", 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[y/(-1+y)]<1&&Abs[z/(-1+y)]<1&&Abs[z/(-1+y)]<Abs[x]/(1+Abs[x])&&Abs[y/(-1+y)]+Abs[z/(-1+y)]<1,((-1)^p (-x)^-a1 x^-m (1-y)^(-a2-p) (y/(-1+y))^n z^p Gamma[-a1+b1] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1-c,m-n-p] Pochhammer[1+a1+b2-c,m-p])/(m! n! p! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b2-c,m-n-p])+((-1)^p (-x)^-b1 x^-m (1-y)^(-a2-p) (y/(-1+y))^n z^p Gamma[a1-b1] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n-p] Pochhammer[1+b1+b2-c,m-p])/(m! n! p! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b2-c,m-n-p])},{Abs[(-1+x)/x]<1&&Abs[((-1+x) y)/x]<1&&Abs[z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b1+b2-c,m+n-p])+((-1)^(n+p) (1-x)^(-a1-b1+c+n+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^(-n-p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+p) (1/x)^a1 ((-1+x)/x)^m (-y)^(a1+b1-c+m-p) y^-n z^p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n+p])},{1/Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&(1+Abs[(-1+y)/y]) Abs[z]<1&&Abs[z/y]<1&&Abs[(-1+y)/y]+Abs[z/y]<1,((-1)^p (-x)^-a1 x^-m (1/y)^b2 ((-1+y)/y)^n z^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m+n-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((-1)^p (-x)^-b1 x^-m (1/y)^b2 ((-1+y)/y)^n z^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m+n-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])+((-1)^m x^m (1-y)^(-a2-b2+c+m) (1/y)^(-b2+c) ((-1+y)/y)^n y^(-m-p) z^p Gamma[a2+b2-c] Gamma[c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[a2] Gamma[b2] Pochhammer[-b2+c,m+p] Pochhammer[1-a2-b2+c,m+n])+((-1)^(n+p) (-x)^(a2+b2-c+n) x^-m (1/y)^b2 ((-1+y)/y)^n z^p Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] Pochhammer[1-a2,m-p] Pochhammer[a2,p] Pochhammer[1-b2,m-n] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a2-b2+c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-n] Pochhammer[1-a2-b1-b2+c,m-n])},{Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1&&Abs[z]<1&&Abs[z/y]<1&&Abs[y]>1,(x^m ((-1+y)/y)^m (-y)^-b2 y^-n (y/(-1+y))^(a2+b2-c) z^p Gamma[a2-b2] Gamma[c] Pochhammer[a1,m] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a2+c,m+n])/(m! n! p! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])+((-1)^p x^m ((-1+y)/y)^m (-y)^(-a2-p) y^-n (y/(-1+y))^(a2+b2-c) z^p Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[-b2+c,m+n+p])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m+p])},{Abs[(-1+x)/x]<1&&Abs[x/(y-x y)]<1&&Abs[z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^(n+p) (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n ((-1+1/x) y)^n z^p Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-a2-b1+c),(1/((-1+1/x) y))^(a1+a2+b1-c)] Pochhammer[a2,m+p] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m+p] Pochhammer[1+a2+b1-c,m-n])+((-1)^(2 p) (1-x)^(-a1-b1+c+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^-p (x/((-1+x) y))^n ((-1+1/x) y)^-p z^p Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-a2,(1/((-1+1/x) y))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^(n+2 p) (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n ((-1+1/x) y)^(n+p) z^p Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-b1-b2+c),(1/((-1+1/x) y))^(a1+b1+b2-c)] Pochhammer[1-b1,n] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a1+b1+b2-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-p] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^p (1-x)^(-a1-b1+c+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^-p (x/((-1+x) y))^n z^p Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-b2,(1/((-1+1/x) y))^b2] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[a1+b1+b2-c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n z^p Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b1+b2-c,m+n-p])},{1/Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[y/(x-x y)]<1&&Abs[y/(x-x y)]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}]}], 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], " ", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}]}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], "<", "1"}], "&&", 
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&(1+Abs[(-1+y)/y]) Abs[z]<1&&Abs[z/y]<1&&Abs[(-1+y)/y]+Abs[z/y]<1,((1-y)^(-a2-b2+c) (1/y)^(-b2+c) ((-1+y)/y)^n y^-p (y/(x (-1+y)))^m z^p Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-a1,(1/(x (-1+1/y)))^a1] Pochhammer[a1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,m-p] Pochhammer[a1+a2+b2-c,m-n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b2-c,m-n-p])+((-1)^(m+p) (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^b2 (y/(x (-1+y)))^n z^p Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a1-a2-b2+c),(1/(x (-1+1/y)))^(a1+a2+b2-c)] Pochhammer[a1,n] Pochhammer[1-a2,m-p] Pochhammer[a2,p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b2-c,-m+n])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+n])+((-1)^p (-x)^-a1 x^-m (1/y)^b2 ((-1+y)/y)^n z^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a1+b2-c,m+n-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+a2+b2-c,m+n])+((1-y)^(-a2-b2+c) (1/y)^(-b2+c) ((-1+y)/y)^n y^-p (y/(x (-1+y)))^m z^p Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-b1,(1/(x (-1+1/y)))^b1] Pochhammer[b1,m] Pochhammer[1-b2,n] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a2+b1+b2-c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^(m+p) (-x)^(a2+b2-c) x^-m (x (-1+1/y))^m (1/y)^b2 (y/(x (-1+y)))^n z^p Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a2-b1-b2+c),(1/(x (-1+1/y)))^(a2+b1+b2-c)] Pochhammer[1-a2,m-p] Pochhammer[a2,p] Pochhammer[b1,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,n] Pochhammer[a2+b1+b2-c,-m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+n])+((-1)^p (-x)^-b1 x^-m (1/y)^b2 ((-1+y)/y)^n z^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b1+b2-c,m+n-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2+b1+b2-c,m+n])},{Abs[y]>1&&Abs[z]>1&&Abs[1-x]<1&&Abs[y/z]<1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1,((-1)^p (1-x)^m (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1-x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+m+n+p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+n) (1-x)^m y^n (-z)^(a1+b1-c+m-n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,-m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b3+c,-m+n+p])+((-1)^(m+p) (1-x)^m (-y)^(a1+b1+b3-c+m+p) y^-n (-z)^-b3 z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,-m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a1-b1-b3+c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2-b3+c,-m+n-p])},{Abs[z]>1&&Abs[y]>1&&Abs[1-x]<1&&Abs[z/y]<1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1,((-1)^p (1-x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1-x)^m (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+m+n+p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p] Pochhammer[-b1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[-b1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+p) (1-x)^m (-y)^(a1+b1-c+m-p) y^-n z^p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,-m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n+p])+((-1)^(m+n) (1-x)^m (-y)^-b2 y^-n (-z)^(a1+b1+b2-c+m+n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,-m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1-b1-b2+c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b2-b3+c,-m-n+p])},{1/Abs[1-x]<1&&1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[1-x])&&1/Abs[y]<1/(1+1/Abs[-1+x])&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[-1+x]),((-1)^(m+n) (1-x)^(-b1-m) y^n (-z)^(-a2-n) z^-p Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^(m+n) (1-x)^(-a1-m) y^n (-z)^(-a2-n) z^-p Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b1-c,-m+p])+((1-x)^(-b1-m) (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a1+b3-c,-m+p] Pochhammer[-a1-b3+c,m-p])+((1-x)^(-a1-m) (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a2+b1-c,-m+n] Pochhammer[1+b1+b3-c,-m+p] Pochhammer[-b1-b3+c,m-p])+((-1)^m (1-x)^(-b1-m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,-m+p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a1-b2-b3+c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b3-c,-m+p] Pochhammer[1+a1+b2+b3-c,-m+n+p] Pochhammer[-a1-b3+c,m-p])+((-1)^m (1-x)^(-a1-m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,m-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+b1+b3-c,-m+p] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[-b1-b3+c,m-p])},{1/Abs[1-x]<1&&1/Abs[-1+x]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[1-x])&&1/Abs[z]<1/(1+1/Abs[-1+x])&&Abs[z/y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x]),((-1)^(m+p) (1-x)^(-b1-m) (-y)^(-a2-p) y^-n z^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^(m+p) (1-x)^(-a1-m) (-y)^(-a2-p) y^-n z^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b1-c,-m+n])+((1-x)^(-b1-m) (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2-c,-m+p] Pochhammer[1+a1+b2-c,-m+n] Pochhammer[-a1-b2+c,m-n])+((1-x)^(-a1-m) (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a2+b1-c,-m+p] Pochhammer[1+b1+b2-c,-m+n] Pochhammer[-b1-b2+c,m-n])+((-1)^m (1-x)^(-b1-m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,-m+n] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-a1-b2-b3+c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b2-c,-m+n] Pochhammer[1+a1+b2+b3-c,-m+n+p] Pochhammer[-a1-b2+c,m-n])+((-1)^m (1-x)^(-a1-m) (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,-m+n] Pochhammer[1+b1+b2+b3-c,n+p] Pochhammer[-b1-b2-b3+c,m-n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+b1+b2-c,-m+n] Pochhammer[1+b1+b2+b3-c,-m+n+p] Pochhammer[-b1-b2+c,m-n])},{Abs[y]>1&&Abs[z]>1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[z-x z]&&Abs[z-x z]>1&&Abs[y/z]<1&&1/(1+Abs[-1+x])>1/Abs[z-x z]&&Abs[1-x]<1&&Abs[-1+x]+Abs[(-1+x) y]<1&&Abs[(-1+x) y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1,((-1)^(n+p) y^n (1/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p (z-x z)^p Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^(-a1-a2-b1+c),(1/(z-x z))^(a1+a2+b1-c)] Pochhammer[a2,m+n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-p])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m+n] Pochhammer[1+a1+a2-c,m-p] Pochhammer[1+a2+b1-c,m-p])+((-1)^(m+2 n) (1-x)^(-a1-b1+c+m+n) y^n (1/((-1+x) z))^p (z-x z)^-n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^-a2,(1/(z-x z))^a2] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p] Pochhammer[a1+a2+b1-c,-m+p])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p] Pochhammer[1+a2+b1-c,-m+p])+((-1)^p (1-x)^m (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1-x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(2 n+p) y^n (1/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p (z-x z)^(n+p) Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^(-a1-b1-b3+c),(1/(z-x z))^(a1+b1+b3-c)] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[1+a1+b3-c,m-n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a1+b1+b3-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-n] Pochhammer[1+a1+b3-c,m-n-p] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^(m+n) (1-x)^(-a1-b1+c+m+n) y^n (1/((-1+x) z))^p Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[z]>0,(z-x z)^-b3,(1/(z-x z))^b3] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[a1+b1+b3-c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p] Pochhammer[1+b1+b3-c,-m-n+p])+((1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(m+p) (1-x)^m (-y)^(a1+b1+b3-c+m+p) y^-n (-z)^-b3 z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,-m+n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[-a1-b1-b3+c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2-b3+c,-m+n-p])},{Abs[z]>1&&Abs[y]>1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&Abs[y-x y]>1&&Abs[z/y]<1&&1/(1+Abs[-1+x])>1/Abs[y-x y]&&Abs[1-x]<1&&Abs[-1+x]+Abs[(-1+x) z]<1&&Abs[(-1+x) z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1,((-1)^(n+p) (1/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n (y-x y)^n z^p Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-a2-b1+c),(1/(y-x y))^(a1+a2+b1-c)] Pochhammer[a2,m+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m+p] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])+((-1)^(m+2 p) (1-x)^(-a1-b1+c+m+p) (1/((-1+x) y))^n (y-x y)^-p z^p Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-a2,(1/(y-x y))^a2] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a2+b1-c,-m+n])+((-1)^p (1-x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1-x)^m (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(n+2 p) (1/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n (y-x y)^(n+p) z^p Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^(-a1-b1-b2+c),(1/(y-x y))^(a1+b1+b2-c)] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,m-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a1+b1+b2-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-p] Pochhammer[1+a1+b2-c,m-n-p] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^(m+p) (1-x)^(-a1-b1+c+m+p) (1/((-1+x) y))^n z^p Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-1+Re[x]+Re[y]>0,(y-x y)^-b2,(1/(y-x y))^b2] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p] Pochhammer[a1+b1+b2-c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p] Pochhammer[1+b1+b2-c,-m+n-p])+((1-x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(m+n) (1-x)^m (-y)^-b2 y^-n (-z)^(a1+b1+b2-c+m+n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,-m+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[-a1-b1-b2+c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b2-b3+c,-m-n+p])},{Abs[x/(-1+x)]<1&&Abs[y]>1&&Abs[y/z]<1&&Abs[z]>1,((-1)^n (1-x)^-b1 (x/(-1+x))^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p] Pochhammer[1+a1+a2-c,p])/(m! n! p! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^p (1-x)^-b1 (x/(-1+x))^m (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+b3-c,-m+p] Pochhammer[-b3+c,m-p])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2-c,-m+n] Pochhammer[1+a1+b3-c,-m+p] Pochhammer[-a1-b3+c,m-p])+((1-x)^-b1 (x/(-1+x))^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[1+a1+b2+b3-c,-m+p] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[-b3+c,m-p] Pochhammer[-a1-b2-b3+c,m-p])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b3-c,-m+p] Pochhammer[1+b2+b3-c,-m+p] Pochhammer[1+a1+b2+b3-c,-m+n+p] Pochhammer[-a1-b3+c,m-p] Pochhammer[-b2-b3+c,m-p])},{Abs[x/(-1+x)]<1&&Abs[z]>1&&Abs[z/y]<1&&Abs[y]>1,((-1)^p (1-x)^-b1 (x/(-1+x))^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2-c,-m+n] Pochhammer[1+a1+a2-c,n])/(m! n! p! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^n (1-x)^-b1 (x/(-1+x))^m (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a2-c,-m+p] Pochhammer[1+a1+a2-c,p] Pochhammer[1+b2-c,-m+n] Pochhammer[-b2+c,m-n])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2-c,-m+p] Pochhammer[1+a1+b2-c,-m+n] Pochhammer[-a1-b2+c,m-n])+((1-x)^-b1 (x/(-1+x))^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n+p] Pochhammer[1+a1+b2+b3-c,-m+n] Pochhammer[1+a1+b2+b3-c,n+p] Pochhammer[-b2+c,m-n] Pochhammer[-a1-b2-b3+c,m-n])/(m! n! p! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b2-c,-m+n] Pochhammer[1+b2+b3-c,-m+n] Pochhammer[1+a1+b2+b3-c,-m+n+p] Pochhammer[-a1-b2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1,((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+n+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^(-n-p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+n) (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(a1+b1-c+m-n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b3+c,-m+n+p])+((-1)^(m+p) (1/x)^a1 ((-1+x)/x)^m (-y)^(a1+b1+b3-c+m+p) y^-n (-z)^-b3 z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[-a1-b1-b3+c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2-b3+c,-m+n-p])},{Abs[(-1+x)/x]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1,((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(n+p) (1-x)^(-a1-b1+c+n+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^(-n-p) y^n z^p Gamma[a1+b1-c] Gamma[c] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[-a1+c,m+n+p])/(m! n! p! Gamma[a1] Gamma[b1] Pochhammer[-a1+c,n+p] Pochhammer[1-a1-b1+c,m+n+p])+((-1)^(m+p) (1/x)^a1 ((-1+x)/x)^m (-y)^(a1+b1-c+m-p) y^-n z^p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[-a1-b1+c,-m+n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2+c,-m+n+p])+((-1)^(m+n) (1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^(a1+b1+b2-c+m+n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[-a1-b1-b2+c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b2-b3+c,-m-n+p])},{1/Abs[x]<1&&Abs[x/(y-x y)]<1&&Abs[x/(y-x y)]<Abs[x]/(1+Abs[x])&&Abs[((-1+x) z)/x]<1&&1/Abs[x]+Abs[((-1+x) z)/x]<1&&Abs[z/y]<1,((-1)^(m+p) ((-1+x)/x)^p (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^n ((-1+1/x) y)^-p z^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-a2,(1/((-1+1/x) y))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^(m+p) ((-1+x)/x)^p (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^n ((-1+1/x) y)^-p z^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-a2,(1/((-1+1/x) y))^a2] Pochhammer[a2,n+p] Pochhammer[1-b1,m] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a2+b1-c,-m+n])+((-1)^m ((-1+x)/x)^p (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^n z^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-b2,(1/((-1+1/x) y))^b2] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p])+((-1)^m ((-1+x)/x)^p (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^n z^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-b2,(1/((-1+1/x) y))^b2] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+b1+b2-c,n-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+b1+b2-c,-m+n-p])},{1/Abs[x]<1&&Abs[x/(z-x z)]<1&&Abs[x/(z-x z)]<Abs[x]/(1+Abs[x])&&Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1&&Abs[y/z]<1,((-1)^(m+n) ((-1+x)/x)^n (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) y^n (x/((-1+x) z))^p ((-1+1/x) z)^-n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-a2,(1/((-1+1/x) z))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^(m+n) ((-1+x)/x)^n (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) y^n (x/((-1+x) z))^p ((-1+1/x) z)^-n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-a2,(1/((-1+1/x) z))^a2] Pochhammer[a2,n+p] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a2+b1-c,-m+p])+((-1)^m ((-1+x)/x)^n (-x)^-b1 x^-m (x/(-1+x))^(a1+b1-c) y^n (x/((-1+x) z))^p Gamma[a1-b1] Gamma[a2-b3] Gamma[c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-b3,(1/((-1+1/x) z))^b3] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p])+((-1)^m ((-1+x)/x)^n (-x)^-a1 x^-m (x/(-1+x))^(a1+b1-c) y^n (x/((-1+x) z))^p Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-b3,(1/((-1+1/x) z))^b3] Pochhammer[1-b1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+b1+b3-c,-n+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+b1+b3-c,-m-n+p])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[z]>1&&Abs[x/(z-x z)]<1,((-1)^(n+p) (1/x)^a1 y^n (x/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p ((-1+1/x) z)^p Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^(-a1-a2-b1+c),(1/((-1+1/x) z))^(a1+a2+b1-c)] Pochhammer[a2,m+n] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-p])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m+n] Pochhammer[1+a2+b1-c,m-p])+((-1)^(2 n) (1-x)^(-a1-b1+c+n) (1/x)^(-a1+c) ((-1+x)/x)^m x^-n y^n (x/((-1+x) z))^p ((-1+1/x) z)^-n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-a2,(1/((-1+1/x) z))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,p] Pochhammer[a1+a2+b1-c,-m+p])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2-c,-m+p])+((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2+b3+p) y^-n (-z)^-b3 z^-p Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2-b3,n-p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,n-p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/x)^a1 ((-1+x)/x)^m y^n (-z)^(-a2-n) z^-p Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(2 n+p) (1/x)^a1 y^n (x/((-1+x) z))^m (-z)^(a1+b1-c-n) z^-p ((-1+1/x) z)^(n+p) Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^(-a1-b1-b3+c),(1/((-1+1/x) z))^(a1+b1+b3-c)] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[b3,m] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a1+b1+b3-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-n] Pochhammer[1+b1+b3-c,m-n-p])+((-1)^n (1-x)^(-a1-b1+c+n) (1/x)^(-a1+c) ((-1+x)/x)^m x^-n y^n (x/((-1+x) z))^p Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[z]>0,((-1+1/x) z)^-b3,(1/((-1+1/x) z))^b3] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[a1+b1+b3-c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b3-c,-m-n+p])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(m+p) (1/x)^a1 ((-1+x)/x)^m (-y)^(a1+b1+b3-c+m+p) y^-n (-z)^-b3 z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] Pochhammer[1-a1,-m+n] Pochhammer[a1,m] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[-a1-b1-b3+c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+n] Pochhammer[1-a1-b1-b2-b3+c,-m+n-p])},{Abs[(-1+x)/x]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1&&Abs[x/(y-x y)]<1,((-1)^(n+p) (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n ((-1+1/x) y)^n z^p Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-a2-b1+c),(1/((-1+1/x) y))^(a1+a2+b1-c)] Pochhammer[a2,m+p] Pochhammer[1-b1,n] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[a1+a2+b1-c,m-n])/(m! n! p! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m+p] Pochhammer[1+a2+b1-c,m-n])+((-1)^(2 p) (1-x)^(-a1-b1+c+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^-p (x/((-1+x) y))^n ((-1+1/x) y)^-p z^p Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-a2,(1/((-1+1/x) y))^a2] Pochhammer[1-a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b1-c,-m+n])/(m! n! p! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2-c,-m+n])+((-1)^p (1/x)^a1 ((-1+x)/x)^m (-y)^(-a2-p) y^-n z^p Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a2+b1-c,n])/(m! n! p! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+a2+b1-c,m+n])+((-1)^n (1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^(-a2+b2+n) z^-p Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m] Pochhammer[a2-b2,-n+p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a2+b1-c,p])/(m! n! p! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-n+p] Pochhammer[1+a1+a2+b1-c,m+p])+((-1)^(n+2 p) (1/x)^a1 (x/((-1+x) y))^m (-y)^(a1+b1-c-p) y^-n ((-1+1/x) y)^(n+p) z^p Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^(-a1-b1-b2+c),(1/((-1+1/x) y))^(a1+b1+b2-c)] Pochhammer[1-b1,n] Pochhammer[b2,m] Pochhammer[b3,p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[a1+b1+b2-c,m-n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-p] Pochhammer[1+b1+b2-c,m-n-p])+((-1)^p (1-x)^(-a1-b1+c+p) (1/x)^(-a1+c) ((-1+x)/x)^m x^-p (x/((-1+x) y))^n z^p Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] If[-Re[x]+Re[x]^2+Re[y]>0,((-1+1/x) y)^-b2,(1/((-1+1/x) y))^b2] Pochhammer[1-a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2-c,n-p] Pochhammer[a1+b1+b2-c,-m+n-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,-m+n-p])+((1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^-b3 z^-p Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[b3,p] Pochhammer[1+a1+b2+b3-c,m+n+p] Pochhammer[1+b1+b2+b3-c,n+p])/(m! n! p! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,n+p] Pochhammer[1+a1+b1+b2+b3-c,m+n+p])+((-1)^(m+n) (1/x)^a1 ((-1+x)/x)^m (-y)^-b2 y^-n (-z)^(a1+b1+b2-c+m+n) z^-p Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] Pochhammer[1-a1,-m+p] Pochhammer[a1,m] Pochhammer[1-b1,p] Pochhammer[b2,n] Pochhammer[-a1-b1-b2+c,-m-n+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+p] Pochhammer[1-a1-b1-b2-b3+c,-m-n+p])},{Abs[x] Abs[y] (Abs[y]-Abs[x] Abs[y]+Abs[x z]+Abs[x-x z])<0&&Abs[x]<x Conjugate[x]&&1/Abs[x]+Abs[(1-z)/y]<1&&1/Abs[x]+Abs[z/y]<1&&Abs[z/(-1+z)]<1&&1+Abs[z/(-1+z)]<Abs[y/(1-z)],((-x)^-a1 x^-m (1-z)^-a2 ((1-z)/y)^n (y/(-1+z))^-p (z/(-1+z))^p Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] If[1+Re[y]-Re[z]>0,(y/(-1+z))^-a2,((-1+z)/y)^a2] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[1+a1-c,m-p] Pochhammer[1+a1+a2-c,m+n] Pochhammer[1+a1+a2+b3-c,m+n+p] Pochhammer[-a1+c,-m+p])/(m! n! p! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+a1+b3-c,m-p] Pochhammer[1+a1+a2+b3-c,m+n] Pochhammer[-a1-b3+c,-m+p])+((-x)^-b1 x^-m (1-z)^-a2 ((1-z)/y)^n (y/(-1+z))^-p (z/(-1+z))^p Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] If[1+Re[y]-Re[z]>0,(y/(-1+z))^-a2,((-1+z)/y)^a2] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[1+b1-c,m-p] Pochhammer[1+a2+b1-c,m+n] Pochhammer[1+a2+b1+b3-c,m+n+p] Pochhammer[-b1+c,-m+p])/(m! n! p! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b2,n+p] Pochhammer[1+b1+b3-c,m-p] Pochhammer[1+a2+b1+b3-c,m+n] Pochhammer[-b1-b3+c,-m+p])+((-1)^p (-x)^-a1 x^-m (1-z)^-a2 ((1-z)/y)^n (z/(-1+z))^p Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] If[1+Re[y]-Re[z]>0,(y/(-1+z))^-b2,((-1+z)/y)^b2] Pochhammer[a1,m] Pochhammer[b2,n] Pochhammer[1+a1-c,m-p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[1+a1+b2+b3-c,m+n] Pochhammer[1+a1+b2+b3-c,m-p] Pochhammer[-a1+c,-m+p] Pochhammer[-a1-b2-b3+c,-m+p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+a1+b2-c,m-p] Pochhammer[1+a1+b3-c,m-p] Pochhammer[1+a1+b2+b3-c,m+n-p] Pochhammer[-a1-b2+c,-m+p] Pochhammer[-a1-b3+c,-m+p])+((-1)^p (-x)^-b1 x^-m (1-z)^-a2 ((1-z)/y)^n (z/(-1+z))^p Gamma[a1-b1] Gamma[a2-b2] Gamma[c] If[1+Re[y]-Re[z]>0,(y/(-1+z))^-b2,((-1+z)/y)^b2] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+b1-c,m-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,m-p] Pochhammer[-b1+c,-m+p] Pochhammer[-b1-b2-b3+c,-m+p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,n-p] Pochhammer[1+b1+b2-c,m-p] Pochhammer[1+b1+b3-c,m-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[-b1-b2+c,-m+p] Pochhammer[-b1-b3+c,-m+p])},{Abs[x] Abs[z] (Abs[x y]+Abs[x-x y]+Abs[z]-Abs[x] Abs[z])<0&&Abs[x]<x Conjugate[x]&&1/Abs[x]+Abs[(1-y)/z]<1&&1/Abs[x]+Abs[y/z]<1&&Abs[y/(-1+y)]<1&&1+Abs[y/(-1+y)]<Abs[z/(1-y)],((-x)^-a1 x^-m (1-y)^-a2 (y/(-1+y))^n ((1-y)/z)^p (z/(-1+y))^-n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] If[1-Re[y]+Re[z]>0,(z/(-1+y))^-a2,((-1+y)/z)^a2] Pochhammer[a1,m] Pochhammer[a2,n+p] Pochhammer[1+a1-c,m-n] Pochhammer[1+a1+a2-c,m+p] Pochhammer[1+a1+a2+b2-c,m+n+p] Pochhammer[-a1+c,-m+n])/(m! n! p! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+a1+b2-c,m-n] Pochhammer[1+a1+a2+b2-c,m+p] Pochhammer[-a1-b2+c,-m+n])+((-x)^-b1 x^-m (1-y)^-a2 (y/(-1+y))^n ((1-y)/z)^p (z/(-1+y))^-n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] If[1-Re[y]+Re[z]>0,(z/(-1+y))^-a2,((-1+y)/z)^a2] Pochhammer[a2,n+p] Pochhammer[b1,m] Pochhammer[1+b1-c,m-n] Pochhammer[1+a2+b1-c,m+p] Pochhammer[1+a2+b1+b2-c,m+n+p] Pochhammer[-b1+c,-m+n])/(m! n! p! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m] Pochhammer[1+a2-b3,n+p] Pochhammer[1+b1+b2-c,m-n] Pochhammer[1+a2+b1+b2-c,m+p] Pochhammer[-b1-b2+c,-m+n])+((-1)^n (-x)^-a1 x^-m (1-y)^-a2 (y/(-1+y))^n ((1-y)/z)^p Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] If[1-Re[y]+Re[z]>0,(z/(-1+y))^-b3,((-1+y)/z)^b3] Pochhammer[a1,m] Pochhammer[b3,p] Pochhammer[1+a1-c,m-n] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[1+a1+b2+b3-c,m-n] Pochhammer[1+a1+b2+b3-c,m+p] Pochhammer[-a1+c,-m+n] Pochhammer[-a1-b2-b3+c,-m+n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+a1+b2-c,m-n] Pochhammer[1+a1+b3-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n+p] Pochhammer[-a1-b2+c,-m+n] Pochhammer[-a1-b3+c,-m+n])+((-1)^n (-x)^-b1 x^-m (1-y)^-a2 (y/(-1+y))^n ((1-y)/z)^p Gamma[a1-b1] Gamma[a2-b3] Gamma[c] If[1-Re[y]+Re[z]>0,(z/(-1+y))^-b3,((-1+y)/z)^b3] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[1+b1+b2+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[-b1+c,-m+n] Pochhammer[-b1-b2-b3+c,-m+n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,-n+p] Pochhammer[1+b1+b2-c,m-n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n+p] Pochhammer[-b1-b2+c,-m+n] Pochhammer[-b1-b3+c,-m+n])},{Abs[y/(-1+y)]<1&&Abs[(-1+z)/z]<1&&Abs[z/(x-x z)]<1&&Abs[x]>1,((1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (z/(x (-1+z)))^m Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-a1,(1/(x (-1+1/z)))^a1] Pochhammer[a1,m] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[1-a2+b2,p] Pochhammer[1+a1+a2-c,m] Pochhammer[a1+a2+b3-c,m-p])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,-n+p] Pochhammer[1+a1+a2-c,m-p])+((-x)^(a2+b3-c) (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p (z/(x (-1+z)))^m Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^(-a1-a2-b3+c),(1/(x (-1+1/z)))^(a1+a2+b3-c)] Pochhammer[a1,m] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[1-a2+b2,p] Pochhammer[1+a1+a2-c,m] Pochhammer[a1+a2+b3-c,m-p])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b2,-n+p] Pochhammer[1+a1+a2-c,m-p])+((1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^(-a2+c) ((-1+z)/z)^p (z/(x (-1+z)))^m Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^-b1,(1/(x (-1+1/z)))^b1] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1-a2+b2,p] Pochhammer[1+a2+b1-c,m] Pochhammer[a2+b1+b3-c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,-n+p] Pochhammer[1+a2+b1-c,m-p])+((-x)^(a2+b3-c) (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p (z/(x (-1+z)))^m Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] If[Re[x]-Re[z]+Re[z]^2>0,(x (-1+1/z))^(-a2-b1-b3+c),(1/(x (-1+1/z)))^(a2+b1+b3-c)] Pochhammer[1-a2,-n+p] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[1-a2+b2,p] Pochhammer[1+a2+b1-c,m] Pochhammer[a2+b1+b3-c,m-p])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b2,-n+p] Pochhammer[1+a2+b1-c,m-p])+((1/x)^m (-x)^-a1 (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] Pochhammer[a1,m] Pochhammer[a2,n] Pochhammer[b3,p] Pochhammer[1+a1+a2-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[1+a1+b3-c,m-n+p] Pochhammer[1+a1+b2+b3-c,m+p] Pochhammer[1+a1+b2+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-a1-b2-b3+c,n-p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b3-c,-n+p] Pochhammer[1+a1+a2+b3-c,m+p] Pochhammer[1+b2+b3-c,-n+p] Pochhammer[1+a1+b2+b3-c,m-n+p] Pochhammer[-a1-b3+c,n-p] Pochhammer[-b2-b3+c,n-p])+((1/x)^m (-x)^-b1 (1-y)^-a2 (y/(-1+y))^n (1-z)^-b3 (-((-1+z)/z))^b3 ((-1+z)/z)^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] Pochhammer[a2,n] Pochhammer[b1,m] Pochhammer[b3,p] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b3-c,-n+p] Pochhammer[1+b1+b3-c,m-n+p] Pochhammer[1+b1+b2+b3-c,m+p] Pochhammer[1+b1+b2+b3-c,-n+p] Pochhammer[-b3+c,n-p] Pochhammer[-b1-b2-b3+c,n-p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b3-c,-n+p] Pochhammer[1+a2+b1+b3-c,m+p] Pochhammer[1+b2+b3-c,-n+p] Pochhammer[1+b1+b2+b3-c,m-n+p] Pochhammer[-b1-b3+c,n-p] Pochhammer[-b2-b3+c,n-p])},{Abs[z/(-1+z)]<1&&Abs[(-1+y)/y]<1&&Abs[y/(x-x y)]<1&&Abs[x]>1,((1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (y/(x (-1+y)))^m (1-z)^-a2 (z/(-1+z))^p Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-a1,(1/(x (-1+1/y)))^a1] Pochhammer[a1,m] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[1-a2+b3,n] Pochhammer[1+a1+a2-c,m] Pochhammer[a1+a2+b2-c,m-n])/(m! n! p! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,n-p] Pochhammer[1+a1+a2-c,m-n])+((-x)^(a2+b2-c) (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (y/(x (-1+y)))^m (1-z)^-a2 (z/(-1+z))^p Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a1-a2-b2+c),(1/(x (-1+1/y)))^(a1+a2+b2-c)] Pochhammer[a1,m] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[1-a2+b3,n] Pochhammer[1+a1+a2-c,m] Pochhammer[a1+a2+b2-c,m-n])/(m! n! p! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,m] Pochhammer[1-a2+b3,n-p] Pochhammer[1+a1+a2-c,m-n])+((1-y)^-b2 (-((-1+y)/y))^(-a2+c) ((-1+y)/y)^n (y/(x (-1+y)))^m (1-z)^-a2 (z/(-1+z))^p Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^-b1,(1/(x (-1+1/y)))^b1] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[1-a2+b3,n] Pochhammer[1+a2+b1-c,m] Pochhammer[a2+b1+b2-c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,n-p] Pochhammer[1+a2+b1-c,m-n])+((-x)^(a2+b2-c) (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (y/(x (-1+y)))^m (1-z)^-a2 (z/(-1+z))^p Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] If[Re[x]-Re[y]+Re[y]^2>0,(x (-1+1/y))^(-a2-b1-b2+c),(1/(x (-1+1/y)))^(a2+b1+b2-c)] Pochhammer[1-a2,n-p] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[1-a2+b3,n] Pochhammer[1+a2+b1-c,m] Pochhammer[a2+b1+b2-c,m-n])/(m! n! p! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,m] Pochhammer[1-a2+b3,n-p] Pochhammer[1+a2+b1-c,m-n])+((1/x)^m (-x)^-a1 (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] Pochhammer[a1,m] Pochhammer[a2,p] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[1+a1+b2-c,m+n-p] Pochhammer[1+a1+b2+b3-c,m+n] Pochhammer[1+a1+b2+b3-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-a1-b2-b3+c,-n+p])/(m! n! p! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m] Pochhammer[1+a1+b2-c,n-p] Pochhammer[1+a1+a2+b2-c,m+n] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+a1+b2+b3-c,m+n-p] Pochhammer[-a1-b2+c,-n+p] Pochhammer[-b2-b3+c,-n+p])+((1/x)^m (-x)^-b1 (1-y)^-b2 (-((-1+y)/y))^b2 ((-1+y)/y)^n (1-z)^-a2 (z/(-1+z))^p Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] Pochhammer[a2,p] Pochhammer[b1,m] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m] Pochhammer[1+b2-c,n-p] Pochhammer[1+b1+b2-c,m+n-p] Pochhammer[1+b1+b2+b3-c,m+n] Pochhammer[1+b1+b2+b3-c,n-p] Pochhammer[-b2+c,-n+p] Pochhammer[-b1-b2-b3+c,-n+p])/(m! n! p! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m] Pochhammer[1+b1+b2-c,n-p] Pochhammer[1+a2+b1+b2-c,m+n] Pochhammer[1+b2+b3-c,n-p] Pochhammer[1+b1+b2+b3-c,m+n-p] Pochhammer[-b1-b2+c,-n+p] Pochhammer[-b2-b3+c,-n+p])}};


LS2sum[{a1_,a2_,b1_,b2_,b3_,c_,x_,y_,z_},m_,n_]={{Abs[x]<1&&Abs[y]<1&&Abs[z]<1,(x^(m-n) y^n HypergeometricPFQ[{b3,a2+n},{c+m},z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Pochhammer[c,m])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[z]<1&&Abs[1-y]+Abs[z]<1,((-1)^(m-n) x^(m-n) (1-y)^n Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b3,a2+n},{-b2+c+m-n},z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[1+a2+b2-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[-b2+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-b2+c+m},{-b2+c+m-n},z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[-b2+c,m-n] Pochhammer[1-a2-b2+c,m])},{Abs[x]<1&&Abs[y]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[y]+Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1,(x^(m-n) y^n Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a2+b3-c-m+n},1-z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2-b3+c,m-n])/((m-n)! n! Gamma[-a2+c] Gamma[-b3+c] Pochhammer[-a2+c,m-n] Pochhammer[-b3+c,m])+((-1)^(m-n) x^(m-n) y^n (1-z)^(-a2-b3+c+m-n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{-b3+c+m,-a2+c+m-n},{1-a2-b3+c+m-n},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[1-a2-b3+c,m-n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[z]<1&&Abs[1-y]>1+Abs[z]&&Abs[z/(-1+y)]<1&&1/Abs[1-y]+Abs[z/(-1+y)]<1,(x^(m-n) (1-y)^-a2 (z/(-1+y))^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{-b2+c+m,a2+n},{1+a2-b2+n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n])+((-1)^-n x^(m-n) (1-y)^-b2 (-z)^n Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a2+c+m-n},{1-a2+b2-n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n])/((m-n)! n! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{Abs[x]<1&&Abs[y]<1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&1/(1+Abs[y])>1/Abs[1-z]&&Abs[y/(-1+z)]<1&&1/Abs[1-z]+Abs[y/(-1+z)]<1,(x^(m-n) (1-z)^-a2 (y/(-1+z))^n Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{-b3+c+m,a2+n},{1+a2-b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+((-1)^-n x^(m-n) (-y)^n (1-z)^-b3 Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m-n},{1-a2+b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n])/((m-n)! n! Gamma[a2] Gamma[-b3+c] Pochhammer[-b3+c,m])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[z]>1&&1+1/Abs[z]<1/Abs[z/(-1+y)]&&Abs[z/(-1+y)]<1&&1/Abs[1-y]+Abs[z/(-1+y)]<1&&Abs[1-y]>1&&1/Abs[x]>1+1/Abs[z],(x^(m-n) (1-y)^-a2 (z/(-1+y))^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{-b2+c+m,a2+n},{1+a2-b2+n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n])+(x^(m-n) (1-y)^-b2 (-z)^(-a2+b2) (z/(-1+y))^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a2-c-m,a2-b2-n},{1+a2-b2-b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m-n])+((-1)^(2 (m-n)) x^(m-n) (1-y)^(-b2-n) (-z)^-b3 Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[y/(-1+z)]<1&&1/Abs[1-z]+Abs[y/(-1+z)]<1&&1/Abs[x]>1+1/Abs[y]&&Abs[1-z]>1&&Abs[y]/(1+Abs[y])>Abs[y/(-1+z)],((-1)^(m-n) x^(m-n) (-y)^-b2 y^-n (1-z)^-b3 Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m-n},{1-a2+b2+b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n])+(x^(m-n) (1-z)^-a2 (y/(-1+z))^n Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{-b3+c+m,a2+n},{1+a2-b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+(x^(m-n) (-y)^(-a2+b3) (1-z)^-b3 (y/(-1+z))^n Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a2-c-m,a2-b3-n},{1+a2-b2-b3-n},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m-n])},{Abs[x]<1&&Abs[-1+y]<1&&(1+Abs[x]) Abs[-1+y]<1&&Abs[x (-1+y)]<1&&Abs[-1+y]+Abs[x (-1+y)]<1&&Abs[(-1+y)/z]<1&&1+1/Abs[z]<1/Abs[x]&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+Abs[-1+y]),(x^(m-n) (1-y)^n (-z)^(-a2-n) Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,1+a2+b2-c-m+2 n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+((-1)^(m-2 n) x^(m-n) (1-y)^n (-z)^(-b3-n) z^n Gamma[a2-b3] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1-a2+b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n])/((m-n)! n! Gamma[a2] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) (-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[-1+z]<1&&(1+Abs[x]) Abs[-1+z]<1&&1+Abs[-1+z]<Abs[y]&&Abs[x (-1+z)]<1&&Abs[-1+z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1,(x^(m-n) (-y)^(-a2-n) (1-z)^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,1+a2+b3-c-m+2 n},{1+a2-b2+n},1/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n])+((-1)^(m-2 n) x^(m-n) (-y)^(-b2-n) y^n (1-z)^n Gamma[a2-b2] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1-a2+b2-n},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n])/((m-n)! n! Gamma[a2] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (-y)^-b2 (1-z)^(-a2-b3+c+m) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&1/Abs[1-y]<1&&Abs[(1-y)/z]<1&&Abs[(1-y)/z]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/z]<1&&1+1/Abs[z]<1/Abs[x]&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1,(x^(m-n) ((-1+y)/z)^n (-z)^-a2 Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,1+a2+b2-c-m+2 n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2+b3) ((1-y)/z)^n (-z)^-b3 Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-b2-b3+c+m-2 n,a2-b3-n},{1+a2-b2-b3-n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1-a2+b3,n] Pochhammer[-a2+c,m-n])+((-1)^(2 (m-n)) x^(m-n) (1-y)^(-b2-n) (-z)^-b3 Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&1+1/Abs[1-z]<1/Abs[(1-z)/y]&&Abs[x]+1/Abs[1-z]<1&&1/Abs[1-z]<1&&Abs[(1-z)/y]<1&&Abs[(-1+z)/y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1,(x^(m-n) (-y)^-a2 ((-1+z)/y)^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,1+a2+b3-c-m+2 n},{1+a2-b2+n},1/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n])+((-1)^(m-n) x^(m-n) (-y)^-b2 (1-z)^(-a2+b2) ((1-z)/y)^n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-b2-b3+c+m-2 n,a2-b2-n},{1+a2-b2-b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m-n])+((-1)^(2 (m-n)) x^(m-n) (-y)^-b2 (1-z)^(-b3-n) Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1-a2+b2+b3+n},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[z]<1&&Abs[z/y]<1,((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a1,b1},{-a2+c-m+n},x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,m])+((-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{a1,b1},{-b2+c-m+2 n},x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,m-2 n])},{Abs[x]<1&&Abs[y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1,((-1)^(m-n) y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a1,b1},{-a2+c-n},x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a2-c,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,m])+(y^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{a1,b1},{-b3+c+m-2 n},x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,-m+2 n])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1&&Abs[z/y]<1,((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a1,b1},{-a2+c-m+n},x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,m])+((-y)^-b2 y^(-m+n) (-z)^(-a2+b2) z^(m-2 n) Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{a1,b1},{-a2+c-n},x] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a2-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-m+2 n])+((-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{a1,b1},{-b2-b3+c-m},x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,m])},{Abs[x]<1&&1+1/Abs[y]<1/Abs[x]&&1/Abs[y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[x]&&1/Abs[z]<1,((-1)^(m-n) x^(m-n) (-y)^(-a2+b3) y^-n (-z)^-b3 Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a2+b2+b3-n},{1-a2+b3-n},y/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a2-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n])+((-1)^(m-n) x^(m-n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+2 n},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n])+((-1)^(2 (m-n)+n) x^(m-n) y^n (-z)^(-a2-n) Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,1+a2-c-m+n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1,(x^(m-n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 HypergeometricPFQ[{a2,-b3+c+m},{c+m},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Pochhammer[c,m] Pochhammer[-a2+c,m-n])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z/(-1+z)]<1,(x^(m-n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,-b2-b3+c+m-2 n},{-b2+c+m-2 n},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2-c,-m+2 n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2+c] Pochhammer[1+a2+b2-c,-m+2 n] Pochhammer[-a2+c,m-n])+((-1)^(m-n) x^(m-n) (-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (-(y/(-1+y)))^(-m+n) (1-z)^-a2 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{a2,a2-b3-n},{a2-n},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1,(x^(m-n) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{a2,-b2-b3+c+m-2 n},{-b3+c+m-2 n},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b3-c,-m+2 n])/((m-n)! n! Gamma[-a2+c] Gamma[-b3+c] Pochhammer[1+a2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^-a2 (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (-(z/(-1+z)))^(-m+n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{a2,a2-b2-n},{a2-n},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[((-1+y) z)/(y (-1+z))]<1,(x^(m-n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (-1+1/z)^a2 (1-z)^-a2 Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{a2,1+a2+b2-c-m+2 n},{1+a2+b2+b3-c-m+2 n},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n])+((-1)^m x^(m-n) (-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (-(y/(-1+y)))^(-m+n) (-1+1/z)^(a2-b3) (1-z)^-a2 (-(z/(-1+z)))^n Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{1-b3,a2-b3-n},{1-b3-n},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m])+((-1)^m x^(m-n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (-1+1/z)^(-b2-b3+c) (1-z)^-a2 (-(z/(-1+z)))^(-m+2 n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{1-b3,-b2-b3+c+m-2 n},{1-a2-b2-b3+c+m-2 n},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2+b2-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])},{Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(y (-1+z))/((-1+y) z)]<1,(x^(m-n) (-1+1/y)^a2 (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{a2,1+a2+b3-c-m+2 n},{1+a2+b2+b3-c-m+2 n},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,-m+2 n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n])+((-1)^m x^(m-n) (-1+1/y)^(a2-b2) (1-y)^-a2 (-(y/(-1+y)))^n (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (-(z/(-1+z)))^(-m+n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{1-b2,a2-b2-n},{1-b2-n},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^m x^(m-n) (-1+1/y)^(-b2-b3+c) (1-y)^-a2 (-(y/(-1+y)))^(-m+2 n) (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{1-b2,-b2-b3+c+m-2 n},{1-a2-b2-b3+c+m-2 n},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])},{Abs[(y-z)/(-1+y)]<1&&Abs[(x (-1+z))/z]<1&&Abs[(x (-1+z))/z]+1/Abs[z]<1&&Abs[(y-z)/(z-y z)]<1&&Abs[(x (-1+z))/z]+Abs[(y-z)/(z-y z)]<1&&Abs[z]>1,(x^(m-n) (1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) (z/(-1+z))^(-m+n) Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1-b2-b3-n,-b2-b3+c+m-n},{1+a2-b2-b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n])/((m-n)! n! Gamma[b2+b3] Gamma[-a2+c] Pochhammer[b2+b3,n] Pochhammer[-a2+c,m-n])+((-1)^n x^(m-n) (1-y)^-b2 (1-z)^(-a2-b3+c+m-n) ((y-z)/(-1+y))^n (-z)^(a2-c-m) Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{1-a2,-a2+c+m},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1&&Abs[(-y+z)/(-1+z)]<1&&Abs[(y-z)/(y-y z)]<1&&Abs[(x (-1+y))/y]+Abs[(y-z)/(y-y z)]<1&&Abs[y]>1,(x^(m-n) (1-y)^(-a2-b2+c+m-n) (-y)^(a2-c-m+n) y^-n (1-z)^-b3 Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m},{1-a2+b2+b3+n},(-y+z)/(y (-1+z))] Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(-1+y))^(-m+n) (1-z)^-b3 Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b3,-a2+b2+b3-n},{b2+b3-n},(-y+z)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[1-b2-b3,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[b2+b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,((-1)^(m-n) x^(m-n) (1-y)^-b2 (1-z)^(-a2-b3+c+m-n) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2-b3+c+m-n},{1-a2-b3+c+m},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^(m-n) x^(m-n) (1-y)^n Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a2+b2+b3-c-m+2 n},1-z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2-b3+c+m) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b3,a2+b2+b3-c-m},{1+a2+b3-c-m},(1-z)/(1-y)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2-b3+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n] Pochhammer[1-a2-b2-b3+c,m])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1,((-1)^(m-n) x^(m-n) (1-y)^n Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a2+b2+b3-c-m+2 n},1-z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2+b2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) (1-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m},{1-a2-b2+c+m},(-1+y)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^n (1-z)^(-a2-b2-b3+c+m-2 n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{-a2-b2+c+m-2 n,-b2-b3+c+m-n},{1-a2-b2-b3+c+m-2 n},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[1-z]<1&&(1+Abs[x]) Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,((-1)^(m-n) x^(m-n) (1-y)^-b2 (1-z)^(-a2-b3+c+m-n) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2-b3+c+m-n},{1-a2-b3+c+m},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+(x^(m-n) (1-y)^(-a2-b2-b3-n) (1/(-1+y))^(-a2-b3) (-1+y)^(-m+n) Gamma[a2-b2] Gamma[c] ((1-y)^(a2+b2+b3) (1/(-1+y))^(a2+b2+b3) (-1+y)^(m-n) Gamma[1-b2] Gamma[b2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c]+(-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,a2-b2-n},{1+a2+b3-c-m},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1+a2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-a2-b2-b3-n) (1/(-1+y))^(-b2-b3) (-1+y)^(-m+n) Gamma[-a2+b2] Gamma[c] ((1-y)^(a2+b2+b3) (1/(-1+y))^(a2+b2+b3) (-1+y)^(m-n) Gamma[1-a2] Gamma[a2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c]+(-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},(-1+z)/(-1+y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[1-y]<1&&(1+Abs[x]) Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+y)/(-1+z)]<1,((-1)^n x^(m-n) (1-y)^n (1-z)^(-a2-b2-b3) (1/(-1+z))^(-b2-b3) (-1+z)^-m Gamma[-a2+b3] Gamma[c] ((1-z)^(a2+b2+b3) (1/(-1+z))^(a2+b2+b3) (-1+z)^(m-n) Gamma[1-a2] Gamma[a2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c]+(-1)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{-b2-b3+c+m-n,a2+n},{1+a2-b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+((-1)^-n x^(m-n) (1-y)^n (1-z)^(-a2-b2-b3) (1/(-1+z))^(-a2-b2) (-1+z)^(-m+n) Gamma[a2-b3] Gamma[c] ((1-z)^(a2+b2+b3) (1/(-1+z))^(a2+b2+b3) (-1+z)^(m-n) Gamma[1-b3] Gamma[b3] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c]+(-1)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,-a2-b2+c+m-2 n},{1-a2+b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[1+a2+b2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) (1-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m},{1-a2-b2+c+m},(-1+y)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+z)/(-1+y)]<1,(x^(m-n) (1-y)^(-a2-b2-b3) (1/(-1+y))^(-a2-b3) (-1+y)^(-m+n) (1-z)^(-a2-b3) (1/(-1+z))^(-b2-b3) (-1+z)^(-m+n) ((-1+z)/(-1+y))^n Gamma[-a2+b2+b3] Gamma[c] ((1-y)^(a2+b3) (1/(-1+y))^(a2+b3) (-1+y)^(m-n) Gamma[1-b2] Gamma[b2] ((-1)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[a2+b3-c] Gamma[1+b2+b3-c] Gamma[1-a2-b3+c] Gamma[-b2-b3+c]+(1-y)^b2 (1/(-1+y))^b2 (1-z)^(a2+b3) (1/(-1+z))^(a2+b3) (-1+z)^(m-n) Gamma[a2-b2] Gamma[1-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c])+(-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c (1-z)^(a2+b3) (1/(-1+z))^(a2+b3) (-1+z)^(m-n) Gamma[a2-b2] Gamma[1-a2+b2] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{a2-b2-n,-b2-b3+c+m-n},{1+a2-b2-b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n])/((m-n)! n! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1-a2+b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2,n] Pochhammer[-a2+c,m-n])+((-1)^n x^(m-n) (1-y)^(-a2-b2-b3) (1/(-1+y))^(-a2-b3) (-1+y)^(-m+n) (1-z)^(-a2-b3) (1/(-1+z))^-a2 (-1+z)^-m ((-1+z)/(-1+y))^n Gamma[a2-b2-b3] Gamma[c] ((1-y)^(a2+b3) (1/(-1+y))^(a2+b3) (-1+y)^(m-n) Gamma[1-b2] Gamma[b2] ((-1)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[1+a2-c] Gamma[a2+b3-c] Gamma[-a2+c] Gamma[1-a2-b3+c]+(1-y)^b2 (1/(-1+y))^b2 (1-z)^(a2+b3) (1/(-1+z))^(a2+b3) (-1+z)^(m-n) Gamma[1-b3] Gamma[b3] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c])+(-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c (1-z)^(a2+b3) (1/(-1+z))^(a2+b3) (-1+z)^(m-n) Gamma[1-b3] Gamma[b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,-a2+c+m},{1-a2+b2+b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-a2-b2-b3-n) (1/(-1+y))^(-b2-b3) (-1+y)^(-m+n) Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},(-1+z)/(-1+y)] ((-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c]+(1-y)^(a2+b3) (1/(-1+y))^(a2+2 b2+b3) (-1+y)^(m-n) Gamma[1-a2] Gamma[a2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], "b2"], 
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
StripWrapperBoxes->True]\))) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{Abs[x]<1&&Abs[x]+1/Abs[1-y]<1&&Abs[1-y]>1&&Abs[x]+1/Abs[1-z]<1&&Abs[1-z]>1&&Abs[(-1+y)/(-1+z)]<1,((-1)^n x^(m-n) (1-y)^n (1-z)^(-a2-b2-b3) (1/(-1+z))^(-b2-b3) (-1+z)^-m Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{-b2-b3+c+m-n,a2+n},{1+a2-b3+n},1/(1-z)] ((-1)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c]+(1-z)^(a2+b2) (1/(-1+z))^(a2+b2+2 b3) (-1+z)^(m-n) Gamma[1-a2] Gamma[a2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], "b3"], 
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
StripWrapperBoxes->True]\))) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+(x^(m-n) (1-y)^(-a2-b2-n) (1/(-1+y))^-a2 (-1+y)^(-m+n) (1-z)^(-a2-b2-b3) (1/(-1+z))^(-a2-b2) (-1+z)^(-m+n) Gamma[a2-b2-b3] Gamma[c] ((1-z)^(a2+b2) (1/(-1+z))^(a2+b2) (-1+z)^(m-n) Gamma[1-b3] Gamma[b3] ((-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c Gamma[1+a2-c] Gamma[a2+b2-c] Gamma[-a2+c] Gamma[1-a2-b2+c]+(1-y)^(a2+b2) (1/(-1+y))^(a2+b2) (-1+y)^(m-n) (1-z)^b3 (1/(-1+z))^b3 Gamma[1-b2] Gamma[b2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c])+(-1)^(m-n) (1-y)^(a2+b2) (1/(-1+y))^(a2+b2) (-1+y)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[1-b2] Gamma[b2] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,-a2+c+m},{1-a2+b2+b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])+(x^(m-n) (1-y)^(-a2-b2-n) (1/(-1+y))^(-b2-b3) (-1+y)^(-m+n) (1-z)^(-a2-b2-b3) (1/(-1+z))^(-a2-b2) (-1+z)^(-m+n) Gamma[-a2+b2+b3] Gamma[c] ((1-z)^(a2+b2) (1/(-1+z))^(a2+b2) (-1+z)^(m-n) Gamma[1-b3] Gamma[b3] ((-1)^(m-n) (1-y)^(c+m-n) (1/(-1+y))^c Gamma[a2+b2-c] Gamma[1+b2+b3-c] Gamma[1-a2-b2+c] Gamma[-b2-b3+c]+(1-y)^(a2+b2) (1/(-1+y))^(a2+b2) (-1+y)^(m-n) (1-z)^b3 (1/(-1+z))^b3 Gamma[a2-b3] Gamma[1-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c])+(-1)^(m-n) (1-y)^(a2+b2) (1/(-1+y))^(a2+b2) (-1+y)^(m-n) (1-z)^(c+m-n) (1/(-1+z))^c Gamma[a2-b3] Gamma[1-a2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{b3,-a2+b2+b3-n},{1-a2+b3-n},(-1+y)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1-a2+b3] Gamma[1+b2+b3-c] Gamma[-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a2-b2-b3,n] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n])},{1/Abs[x]<1&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x-x y]<Abs[x]/(1+Abs[x])&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[1-y]+Abs[z]<1,((-x)^-a1 x^(-m+n) (1-y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{b3,a2+n},{-a1-b2+c-m+n},z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((-x)^-b1 x^(-m+n) (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{b3,a2+n},{-b1-b2+c-m+n},z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-b2+c+m},{-b2+c+m-n},z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[-b2+c,m-n] Pochhammer[1-a2-b2+c,m])+((-1)^n (-x)^(a2+b2-c+n) x^(-m+n) (1-y)^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b3,a2+n},{a2-m+2 n},z] Pochhammer[1-a2,m-2 n] Pochhammer[a2,n] Pochhammer[1-b2,m-2 n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-2 n] Pochhammer[1-a2-b1-b2+c,m-2 n])},{1/Abs[x]<1&&Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x-x z]<Abs[x]/(1+Abs[x])&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y]+Abs[1-z]<1,((-x)^-a1 x^(-m+n) (1-z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{b2,a2+n},{-a1-b3+c-m+n},y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-x)^-b1 x^(-m+n) (1-z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{b2,a2+n},{-b1-b3+c-m+n},y] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])+((-1)^(m-n) x^(m-n) (1-z)^(-a2-b3+c+m) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{b2,-b3+c+m},{-b3+c+m-n},y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[-b3+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^n (-x)^(a2+b3-c+n) x^(-m+n) (1-z)^n Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b2,a2+n},{a2-m+2 n},y] Pochhammer[1-a2,m-2 n] Pochhammer[a2,n] Pochhammer[1-b3,m-2 n] Pochhammer[b3,n] Pochhammer[-a2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-2 n] Pochhammer[1-a2-b1-b3+c,m-2 n])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&1+Abs[z]<Abs[-1+y]&&Abs[z/(-1+y)]<1&&1/Abs[-1+y]+Abs[z/(-1+y)]<1,((-x)^-a1 x^(-m+n) (1-y)^-a2 (z/(-1+y))^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b2+c-m+2 n},{1+a2-b2+n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (1-y)^-a2 (z/(-1+y))^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b2+c-m+2 n},{1+a2-b2+n},1/(1-y)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-a1 x^(-m+n) (1-y)^-b2 (-z)^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a1-a2+c-m+n},{1-a2+b2-n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n])+((-x)^-b1 x^(-m+n) (1-y)^-b2 (-z)^n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b2,-a2-b1+c-m+n},{1-a2+b2-n},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n])},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&1+Abs[y]<Abs[-1+z]&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]+1/Abs[-1+z]<1,((-x)^-a1 x^(-m+n) (1-z)^-a2 (y/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b3+c-m+2 n},{1+a2-b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-b1 x^(-m+n) (1-z)^-a2 (y/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b3+c-m+2 n},{1+a2-b3+n},1/(1-z)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-a1 x^(-m+n) (-y)^n (1-z)^-b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b3,-a1-a2+c-m+n},{1-a2+b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a1+b3-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n])+((-x)^-b1 x^(-m+n) (-y)^n (1-z)^-b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2-b1+c-m+n},{1-a2+b3-n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1+1/Abs[z]<1/Abs[z/(-1+y)]&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[z/(-1+y)]<1&&1/Abs[-1+y]+Abs[z/(-1+y)]<1,((-x)^-a1 x^(-m+n) (1-y)^-a2 (z/(-1+y))^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b2+c-m+2 n},{1+a2-b2+n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-a1 x^(-m+n) (1-y)^-b2 (-z)^(-a2+b2) (z/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2-c+m-2 n,a2-b2-n},{1+a2-b2-b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-b1 x^(-m+n) (1-y)^-a2 (z/(-1+y))^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b2+c-m+2 n},{1+a2-b2+n},1/(1-y)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (1-y)^-b2 (-z)^(-a2+b2) (z/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1-c+m-2 n,a2-b2-n},{1+a2-b2-b3-n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-a1 x^(-m+n) (1-y)^-b2 (-z)^-b3 z^-n Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b2,-a1-a2+c-m+n},{1-a2+b2+b3+n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n])+((-x)^-b1 x^(-m+n) (1-y)^-b2 (-z)^-b3 z^-n Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b2,-a2-b1+c-m+n},{1-a2+b2+b3+n},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n])},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1+1/Abs[y]<1/Abs[y/(-1+z)]&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]+1/Abs[-1+z]<1,((-x)^-a1 x^(-m+n) (1-z)^-a2 (y/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b3+c-m+2 n},{1+a2-b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-a1 x^(-m+n) (-y)^(-a2+b3) (1-z)^-b3 (y/(-1+z))^n Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2-c+m-2 n,a2-b3-n},{1+a2-b2-b3-n},1/y] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b3,n])+((-x)^-b1 x^(-m+n) (1-z)^-a2 (y/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b3+c-m+2 n},{1+a2-b3+n},1/(1-z)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-b1 x^(-m+n) (-y)^(-a2+b3) (1-z)^-b3 (y/(-1+z))^n Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1-c+m-2 n,a2-b3-n},{1+a2-b2-b3-n},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b3,n])+((-x)^-a1 x^(-m+n) (-y)^-b2 y^-n (1-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a1-a2+c-m+n},{1-a2+b2+b3+n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (1-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2-b1+c-m+n},{1-a2+b2+b3+n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n])},{1/Abs[x]<1&&Abs[-1+y]<1&&Abs[x (-1+y)]<1&&Abs[-1+y]+Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x-x y]<Abs[x]/(1+Abs[x])&&Abs[(-1+y)/z]<1&&1/Abs[x]+1/Abs[z]<1&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+Abs[-1+y]),((-x)^-a1 x^(-m+n) (1-y)^n (-z)^(-a2-n) Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2+b2-c+m,a2+n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-b1 x^(-m+n) (1-y)^n (-z)^(-a2-n) Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1+b2-c+m,a2+n},{1+a2-b3+n},1/z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-1)^-n (-x)^-a1 x^(-m+n) (1-y)^n (-z)^(-b3-n) z^n Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{b3,1+a1+b2+b3-c+m-n},{1-a2+b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((-1)^-n (-x)^-b1 x^(-m+n) (1-y)^n (-z)^(-b3-n) z^n Gamma[a1-b1] Gamma[a2-b3] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m-n},{1-a2+b3-n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])+((-1)^(-2 n) (-x)^(a2+b2-c+n) x^(-m+n) (1-y)^n (-z)^(-b3-n) z^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b3,1-a2+b3+m-2 n},{1-a2+b3-n},1/z] Pochhammer[1-b2,m-2 n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1-a2+b3,m-2 n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-2 n] Pochhammer[1-a2-b1-b2+c,m-2 n])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) (-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])},{1/Abs[x]<1&&Abs[-1+z]<1&&Abs[x (-1+z)]<1&&Abs[-1+z]+Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x-x z]<Abs[x]/(1+Abs[x])&&Abs[(-1+z)/y]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+Abs[-1+z]),((-x)^-a1 x^(-m+n) (-y)^(-a2-n) (1-z)^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a1+a2+b3-c+m,a2+n},{1+a2-b2+n},1/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (-y)^(-a2-n) (1-z)^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a2+b1+b3-c+m,a2+n},{1+a2-b2+n},1/y] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n])+((-1)^-n (-x)^-a1 x^(-m+n) (-y)^(-b2-n) y^n (1-z)^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{b2,1+a1+b2+b3-c+m-n},{1-a2+b2-n},1/y] Pochhammer[a1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-1)^-n (-x)^-b1 x^(-m+n) (-y)^(-b2-n) y^n (1-z)^n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1-a2+b2-n},1/y] Pochhammer[b1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])+((-1)^(-2 n) (-x)^(a2+b3-c+n) x^(-m+n) (-y)^(-b2-n) y^n (1-z)^n Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b2,1-a2+b2+m-2 n},{1-a2+b2-n},1/y] Pochhammer[a2-b2,n] Pochhammer[1-a2+b2,m-2 n] Pochhammer[1-b3,m-2 n] Pochhammer[b3,n] Pochhammer[-a2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-2 n] Pochhammer[1-a2-b1-b3+c,m-2 n])+((-1)^(m-n) x^(m-n) (-y)^-b2 (1-z)^(-a2-b3+c+m) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{b2,1+b2+b3-c-m+n},{1+b2+b3-c-m},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m] Pochhammer[-b2-b3+c,m-n])},{1/Abs[x]<1&&1+1/Abs[1-y]<Abs[x]&&1/Abs[1-y]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[(1-y)/z]<1&&Abs[(1-y)/z]<1/(1+1/Abs[1-y])&&Abs[(-1+y)/z]<1&&1/Abs[x]+1/Abs[z]<1&&Abs[(-1+y)/z]+1/Abs[z]<1&&1/Abs[z]<1,((-x)^-a1 x^(-m+n) ((-1+y)/z)^n (-z)^-a2 Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2+b2-c+m,a2+n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-1)^n (-x)^-a1 x^(-m+n) (1-y)^(-a2+b3) ((1-y)/z)^n (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-a1-b2-b3+c-m,a2-b3-n},{1+a2-b2-b3-n},1/(1-y)] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b3,n])+((-x)^-b1 x^(-m+n) ((-1+y)/z)^n (-z)^-a2 Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1+b2-c+m,a2+n},{1+a2-b3+n},1/z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^(-a2+b3) ((1-y)/z)^n (-z)^-b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c-m,a2-b3-n},{1+a2-b2-b3-n},1/(1-y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b3,n])+((-1)^n (-x)^-a1 x^(-m+n) (1-y)^(-b2-n) (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+a1+b2+b3-c+m-n},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^(-b2-n) (-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m-n},{1-a2+b2+b3+n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])},{1/Abs[x]<1&&1+1/Abs[1-z]<Abs[x]&&1/Abs[1-z]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[(1-z)/y]<1&&Abs[(1-z)/y]<1/(1+1/Abs[1-z])&&Abs[(-1+z)/y]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]+Abs[(-1+z)/y]<1&&1/Abs[y]<1,((-x)^-a1 x^(-m+n) (-y)^-a2 ((-1+z)/y)^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a1+a2+b3-c+m,a2+n},{1+a2-b2+n},1/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-1)^n (-x)^-a1 x^(-m+n) (-y)^-b2 (1-z)^(-a2+b2) ((1-z)/y)^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-a1-b2-b3+c-m,a2-b2-n},{1+a2-b2-b3-n},1/(1-z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-b1 x^(-m+n) (-y)^-a2 ((-1+z)/y)^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a2+b1+b3-c+m,a2+n},{1+a2-b2+n},1/y] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n])+((-1)^n (-x)^-b1 x^(-m+n) (-y)^-b2 (1-z)^(-a2+b2) ((1-z)/y)^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c-m,a2-b2-n},{1+a2-b2-b3-n},1/(1-z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])+((-1)^n (-x)^-a1 x^(-m+n) (-y)^-b2 (1-z)^(-b3-n) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+a1+b2+b3-c+m-n},{1-a2+b2+b3+n},1/y] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((-1)^n (-x)^-b1 x^(-m+n) (-y)^-b2 (1-z)^(-b3-n) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b2,1+b1+b2+b3-c+m-n},{1-a2+b2+b3+n},1/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[z/y]<1,((-x)^-a1 x^(-m+n) (-y)^-b2 y^-n (-z)^(-a2+b2) z^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{a2-b2-n,1+a1+a2-c+m-n},{1+a2-b2-b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},z/y] (((-x)^-a1 x^(-m+n) (-y)^-a2 y^-n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (-y)^-a2 y^-n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n]))+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (-z)^(-a2+b2) z^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{a2-b2-n,1+a2+b1-c+m-n},{1+a2-b2-b3-n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-a1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+a1+b2+b3-c+m},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1-a2+b2+b3+n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[y/z]<1,((-1)^n (-x)^-a1 x^(-m+n) y^n (-z)^(-a2-n) Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2-c+m-n,a2+n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+HypergeometricPFQ[{b3,-a2+b2+b3-n},{1-a2+b3-n},y/z] (((-x)^-a1 x^(-m+n) (-y)^(-a2+b3) y^-n (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[a1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2-b3,n])+((-x)^-b1 x^(-m+n) (-y)^(-a2+b3) y^-n (-z)^-b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2-b3,n]))+((-1)^n (-x)^-b1 x^(-m+n) y^n (-z)^(-a2-n) Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1-c+m-n,a2+n},{1+a2-b3+n},1/z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-a1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+a1+b2+b3-c+m},{1-a2+b2+b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n (-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b2+b3-c+m},{1-a2+b2+b3+n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n])},{Abs[y/(-1+y)]<1&&Abs[z/(-1+z)]<1&&Abs[x]>1,((-x)^-a1 x^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a2,-a1-b3+c-m+2 n},{-a1+c-m+2 n},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1-c,m-2 n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n])+((-x)^-b1 x^(-m+n) (1-y)^-b2 (y/(-1+y))^n (1-z)^-a2 Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{a2,-b1-b3+c-m+2 n},{-b1+c-m+2 n},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n])},{Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z/(-1+z)]<1&&Abs[x]>1,((-x)^-a1 x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,-a1-b2-b3+c-m},{-a1-b2+c-m},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((-x)^-b1 x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,-b1-b2-b3+c-m},{-b1-b2+c-m},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])+((-1)^(m-n) x^(m-n) (-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (-(y/(-1+y)))^(-m+n) (1-z)^-a2 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{a2,a2-b3-n},{a2-n},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m])+((-1)^n (-x)^(a2+b2-c+n) x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,a2-b3-m+n},{a2-m+n},z/(-1+z)] Pochhammer[1-a2,m-n] Pochhammer[1-b2,m-2 n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-2 n] Pochhammer[1-a2-b1-b2+c,m-2 n])},{Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[y/(-1+y)]<1&&Abs[x]>1,((-x)^-a1 x^(-m+n) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{a2,-a1-b2-b3+c-m},{-a1-b3+c-m},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-x)^-b1 x^(-m+n) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{a2,-b1-b2-b3+c-m},{-b1-b3+c-m},y/(-1+y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])+((-1)^(m-n) x^(m-n) (1-y)^-a2 (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (-(z/(-1+z)))^(-m+n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{a2,a2-b2-n},{a2-n},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^n (-x)^(a2+b3-c+n) x^(-m+n) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{a2,a2-b2-m+n},{a2-m+n},y/(-1+y)] Pochhammer[1-a2,m-n] Pochhammer[1-b3,m-2 n] Pochhammer[b3,n] Pochhammer[-a2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-2 n] Pochhammer[1-a2-b1-b3+c,m-2 n])},{Abs[(y-z)/(-1+y)]<1&&1+1/Abs[z]<1/Abs[z/(x-x z)]&&1/Abs[z]<1&&Abs[z/(x-x z)]<1&&Abs[(y-z)/(z-y z)]<1&&1+Abs[(y-z)/(z-y z)]<1/Abs[z/(x-x z)],((1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) (z/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1-b2-b3-n,-a1-b2-b3+c-m+n},{1+a2-b2-b3-n},1/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[b2+b3,n])+((1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(b2+b3-c) (z/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{1-b2-b3-n,-b1-b2-b3+c-m+n},{1+a2-b2-b3-n},1/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[b2+b3,n])+((1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(a2-c-n) (z/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{1-a2,-a1-a2+c-m+2 n},{1-a2+b2+b3+n},1/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((1-y)^-b2 (1-z)^(-a2-b3+c) ((y-z)/(-1+y))^n (-z)^(a2-c-n) (z/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{1-a2,-a2-b1+c-m+2 n},{1-a2+b2+b3+n},1/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])},{Abs[(-y+z)/(-1+z)]<1&&1+1/Abs[y]<1/Abs[y/(x-x y)]&&1/Abs[y]<1&&Abs[y/(x-x y)]<1&&Abs[(-y+z)/(y-y z)]<1&&1+Abs[(-y+z)/(y-y z)]<1/Abs[y/(x-x y)],((-1)^n (1-y)^-b2 y^-n (y/(-1+y))^(a2-c) (y/(x (-1+y)))^(m-n) (1-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a1-a2+c-m+2 n},{1-a2+b2+b3+n},(-y+z)/(y (-1+z))] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((-1)^n (1-y)^-b2 y^-n (y/(-1+y))^(a2-c) (y/(x (-1+y)))^(m-n) (1-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b3,-a2-b1+c-m+2 n},{1-a2+b2+b3+n},(-y+z)/(y (-1+z))] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])+HypergeometricPFQ[{b3,-a2+b2+b3-n},{b2+b3-n},(-y+z)/(-1+z)] (((-1)^n (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(x (-1+y)))^(m-n) (1-z)^-b3 Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-b2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((-1)^n (1-y)^(-a2-b2+c) (-y)^(b2+b3-c) y^-n (y/(x (-1+y)))^(m-n) (1-z)^-b3 Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[1-b2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n]))},{1/Abs[x]<1&&1+1/Abs[1-y]<Abs[x]&&1/Abs[1-y]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1/Abs[x]+1/Abs[x-x y]<1&&1/Abs[x-x y]<1&&Abs[1-z]<1&&Abs[x (-1+z)]<1&&Abs[x (-1+z)]<Abs[x]/(1+Abs[x])&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1,HypergeometricPFQ[{b3,a2-b2-n},{1+a1+a2+b3-c+m-2 n},1-z] (((-1)^n (1/x)^(m-n) (-x)^-a1 (1/(1-y))^n (1-y)^b2 Gamma[-a1+b1] Gamma[a2-b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b3+c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1+a2+b3-c] Gamma[-a1-a2+c] Gamma[-a2-b3+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+a2+b3-c,m-2 n])+((-1)^n (1/x)^(m-n) (-x)^-a1 (1/(1-y))^n (1-y)^(-a2-b3+c) (1/(-1+y))^(-a2-b2-b3+c) Gamma[-a1+b1] Gamma[a2-b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b3+c] Gamma[1-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n] Pochhammer[1+a1+a2+b3-c,m-2 n]))+HypergeometricPFQ[{b3,a2-b2-n},{1+a2+b1+b3-c+m-2 n},1-z] (((-1)^n (1/x)^(m-n) (-x)^-b1 (1/(1-y))^n (1/(-1+y))^b2 Gamma[a1-b1] Gamma[a2-b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b3+c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1+a2+b3-c] Gamma[-a2-b1+c] Gamma[-a2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n] Pochhammer[1+a2+b1+b3-c,m-2 n])+((-1)^n (1/x)^(m-n) (-x)^-b1 (1/(1-y))^n (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) Gamma[a1-b1] Gamma[a2-b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n] Pochhammer[1+a2+b1+b3-c,m-2 n]))+((1/x)^(m-n) (-x)^(-a1-b1) (1-y)^(-a2-b2-b3) (1/(-1+y))^(-2 b2-b3+n) Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},(-1+z)/(-1+y)] Pochhammer[a2,n] ((-x)^b1 (1-y)^(a2+b2+b3) (1/(-1+y))^(a2+2 b2+b3) Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[-a1+b1] Gamma[1+a2+b2+b3-c] Gamma[-a2-b1+c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m-2 n]+(-x)^b1 (1-y)^(b2+c) (1/(-1+y))^c Gamma[a1] Gamma[-a1+b1] Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], "b2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n] Pochhammer[1+b1+b2+b3-c,m-2 n]+(-x)^a1 (1-y)^c (1/(-1+y))^(b2+c) Gamma[a1-b1] Gamma[b1] Gamma[1+b2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+a1+b2+b3-c,m-2 n] Pochhammer[1+b1+b2+b3-c,m-n]+(-x)^a1 (1-y)^(a2+b3) (1/(-1+y))^(a2+3 b2+b3) Gamma[1-a2] Gamma[a2] Gamma[a1-b1] Gamma[b1] Gamma[1+a2+b2+b3-c] Gamma[-a1-a2+c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[1+a1-b1,m-n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+a1+b2+b3-c,m-2 n] Pochhammer[1+b1+b2+b3-c,m-n]))/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+b2+b3-c,m-2 n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((-x)^(a2+b3-c) (1-y)^(-a2-b2-b3) (1/(-1+y))^(-a2-b3) (1/(x (-1+y)))^(m-n) (x (-1+z))^n Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] ((1-y)^(a2+b2+b3) (1/(-1+y))^(a2+b2+b3) Gamma[1-b2] Gamma[b2] Gamma[1+a2+b2+b3-c] Gamma[-a2-b2-b3+c]+(1-y)^c (1/(-1+y))^c Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c]) HypergeometricPFQ[{1-a2+b2+m-2 n,1-b3+m-2 n,-a2-b3+c+m-2 n},{1-a1-a2-b3+c+m-2 n,1-a2-b1-b3+c+m-2 n},1/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b3-c,-m+2 n] Pochhammer[a2+b1+b3-c,-m+2 n] Pochhammer[-a2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+a2+b3-c] Pochhammer[a2-b2,-m+2 n] Pochhammer[1-a2+b2,m-2 n] Pochhammer[b3,-m+2 n])+((1-y)^-b2 (1-z)^(-a2-b3+c) (x (-1+z))^(m-n) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2-b3+c+m-n},{1-a2-b3+c+m},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])},{1/Abs[x]<1&&1+1/Abs[1-z]<Abs[x]&&1/Abs[1-z]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1/Abs[x]+1/Abs[x-x z]<1&&1/Abs[x-x z]<1&&Abs[1-y]<1&&Abs[x (-1+y)]<1&&Abs[x (-1+y)]<Abs[x]/(1+Abs[x])&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[(-1+y)/(-1+z)]<1,HypergeometricPFQ[{a2+n,-a1-b2-b3+c-m+n},{1+a2-b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-a1 (1/(-1+z))^a2 ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((1/x)^(m-n) (-x)^-a1 (1-z)^(-a2-b2+c) (1/(-1+z))^(-b2-2 b3+c) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n]))+HypergeometricPFQ[{a2+n,-b1-b2-b3+c-m+n},{1+a2-b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-b1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((1/x)^(m-n) (-x)^-b1 (1-z)^-b3 (1/(-1+z))^(a2+b3) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n]))+HypergeometricPFQ[{b3,-a1-a2-b2+c-m},{1-a2+b3-n},1/(1-z)] (((1/x)^(m-n) (-x)^-a1 (1-y)^n (1-z)^b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b2+c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1+a2+b2-c] Gamma[-a1-a2+c] Gamma[-a2-b2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((1/x)^(m-n) (-x)^-a1 (1-y)^n (1-z)^(-a2-b2+c) (1/(-1+z))^(-a2-b2-b3+c) Gamma[-a1+b1] Gamma[a2-b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a1-a2-b2+c] Gamma[1-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m]))+HypergeometricPFQ[{b3,-a2-b1-b2+c-m},{1-a2+b3-n},1/(1-z)] (((1/x)^(m-n) (-x)^-b1 (1-y)^n (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b2+c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1+a2+b2-c] Gamma[-a2-b1+c] Gamma[-a2-b2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])+((1/x)^(m-n) (-x)^-b1 (1-y)^n (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[a1-b1] Gamma[a2-b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b1-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m]))+HypergeometricPFQ[{1-b2-m+2 n,1-a2+b3-m+2 n,-a2-b2+c-m+2 n},{1-a1-a2-b2+c-m+2 n,1-a2-b1-b2+c-m+2 n},1/x] (((-x)^(a2+b2-c) (x (-1+y))^(m-n) (1/(-1+z))^b3 (1/(x (-1+z)))^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2-c,m-2 n] Pochhammer[a2+b1+b2-c,m-2 n] Pochhammer[-a2-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1+a2+b2-c] Pochhammer[b2,m-2 n] Pochhammer[a2-b3,m-2 n] Pochhammer[1-a2+b3,-m+2 n])+((-x)^(a2+b2-c) (x (-1+y))^(m-n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) (1/(x (-1+z)))^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2-c,m-2 n] Pochhammer[a2+b1+b2-c,m-2 n] Pochhammer[-a2-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-b3] Gamma[b3] Pochhammer[b2,m-2 n] Pochhammer[a2-b3,m-2 n] Pochhammer[1-a2+b3,-m+2 n]))+((1-y)^(-a2-b2+c+n) (x (-1+y))^(m-n) (1-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m},{1-a2-b2+c+m},(-1+y)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])},{1/Abs[x]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&Abs[(-1+z)/(-1+y)]<1,HypergeometricPFQ[{a2-b2-n,-a1-b2-b3+c-m+n},{1+a2-b2-b3-n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-a1 (1-y)^-b2 (1-z)^(-a2-b3+c) (1/(-1+z))^(-b2-b3+c) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-a2+b2] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^b2 (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((1/x)^(m-n) (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n]))+HypergeometricPFQ[{a2-b2-n,-b1-b2-b3+c-m+n},{1+a2-b2-b3-n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-b1 (1-y)^-b2 (1-z)^(-a2-b3+c) (1/(-1+z))^(-b2-b3+c) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-a2+b2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^b2 (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])+((1/x)^(m-n) (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c) (1/(-1+z))^(a2-b2) ((-1+z)/(-1+y))^n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n]))+HypergeometricPFQ[{b3,-a1-a2+c-m+2 n},{1-a2+b2+b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-a1 (1-y)^-b2 (1/(-1+y))^n (1-z)^(-a2-b3+c) (1/(-1+z))^(-a2+c) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^(b2+n) (1/(-1+z))^b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c+n) (1/(-1+z))^b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n]))+HypergeometricPFQ[{b3,-a2-b1+c-m+2 n},{1-a2+b2+b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-b1 (1-y)^-b2 (1/(-1+y))^n (1-z)^(-a2-b3+c) (1/(-1+z))^(-a2+c) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^(b2+n) (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-a2-b3+c+n) (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b3-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n]))+HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},(-1+z)/(-1+y)] (((1/x)^(m-n) (-x)^-a1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-b2-b3+c+n) Gamma[-a1+b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1-y)^-b2 (1/(-1+y))^(a2+b2+n) Gamma[-a1+b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1-y)^(-a2-b2-b3+c) (1/(-1+y))^(-b2-b3+c+n) Gamma[a1-b1] Gamma[-a2+b2] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1-y)^-b2 (1/(-1+y))^(a2+b2+n) Gamma[a1-b1] Gamma[-a2+b2] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"]}], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n] Pochhammer[1+b1+b2+b3-c,m-2 n]))},{1/Abs[x]<1&&1+1/Abs[-1+z]<Abs[x]&&1/Abs[-1+z]<1&&1+1/Abs[-1+y]<Abs[x]&&1/Abs[-1+y]<1&&Abs[(-1+y)/(-1+z)]<1,HypergeometricPFQ[{a2+n,-a1-b2-b3+c-m+n},{1+a2-b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-a1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((1/x)^(m-n) (-x)^-a1 (1-z)^-b3 (1/(-1+z))^(a2+b3) ((-1+y)/(-1+z))^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n]))+HypergeometricPFQ[{a2+n,-b1-b2-b3+c-m+n},{1+a2-b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-b1 (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-b2-b3+c) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[a2+b2+b3-c] Gamma[c] Gamma[1-a2-b2-b3+c] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((1/x)^(m-n) (-x)^-b1 (1-z)^-b3 (1/(-1+z))^(a2+b3) ((-1+y)/(-1+z))^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
FractionBox["1", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]]}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{"Re", "[", "z", "]"}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], ")"}], "2"]}], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n]))+HypergeometricPFQ[{b3,-a1-a2+c-m+2 n},{1-a2+b2+b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-a1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-a2+c+n) (1-z)^-b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^(b2+n) (1/(-1+z))^b3 Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^(b2+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a1+a2-c,m-2 n]))+HypergeometricPFQ[{b3,-a2-b1+c-m+2 n},{1-a2+b2+b3+n},-(1/(-1+z))] (((1/x)^(m-n) (-x)^-b1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-a2+c+n) (1-z)^-b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b2] Gamma[b2] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^(b2+n) (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^(b2+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b3] Gamma[b3] Gamma[1+a2-c] Gamma[-a2+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2+b3,n] Pochhammer[1+a2+b1-c,m-2 n]))+HypergeometricPFQ[{b3,-a2+b2+b3-n},{1-a2+b3-n},(-1+y)/(-1+z)] (((1/x)^(m-n) (-x)^-a1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-b2-b3+c+n) (1-z)^-b3 Gamma[-a1+b1] Gamma[-a2+b2+b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[a1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a2+b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^(a2-b3+n) (1/(-1+z))^b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-a1 (1/(-1+y))^(a2-b3+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[a1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a1-a2+c] Gamma[-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+a1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1-y)^(-a2-b2+c) (1/(-1+y))^(-b2-b3+c+n) (1-z)^-b3 Gamma[a1-b1] Gamma[-a2+b2+b3] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2+b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^(a2-b3+n) (1/(-1+z))^b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n])+((1/x)^(m-n) (-x)^-b1 (1/(-1+y))^(a2-b3+n) (1-z)^(-a2-b2-b3+c) (1/(-1+z))^(-a2-b2+c) Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[1+a2+b2-c] Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] Gamma[1-a2-b2-b3+c] Pochhammer[b1,m-n] Pochhammer[a2-b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-b3] Gamma[b3] Gamma[1+b2+b3-c] Gamma[-a2-b1+c] Gamma[-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2-b3,n] Pochhammer[1+b1+b2+b3-c,m-2 n]))},{Abs[x]>1&&Abs[1-y]<1&&Abs[1-z]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[(-1+z)/(-1+y)]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1&&Abs[x-x y]<1&&Abs[x]/(1+Abs[x])>Abs[x-x y]&&Abs[x-x z]<1&&Abs[x]/(1+Abs[x])>Abs[x-x z],((-x)^-a1 x^(-m+n) (1-y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a1+a2+b2+b3-c+m},1-z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2+b3-c,m])+((-x)^-b1 x^(-m+n) (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a2+b1+b2+b3-c+m},1-z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2+b3-c,m])+((-1)^(m-n) x^(m-n) (1-y)^-b2 (1-z)^(-a2-b3+c+m-n) ((-1+z)/(-1+y))^n Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{-a2+c+m,-b2-b3+c+m-n},{1-a2-b3+c+m},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2-b3+c+m) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b3,a2+b2+b3-c-m},{1+a2+b3-c-m},(1-z)/(1-y)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2-b3+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[-a2+c,m-n] Pochhammer[-b2-b3+c,m-n] Pochhammer[1-a2-b2-b3+c,m])+((-1)^n (-x)^(a2+b2+b3-c+n) x^(-m+n) (1-y)^n Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{b3,a2+n,a1+a2+b2+b3-c-m+2 n,a2+b1+b2+b3-c-m+2 n},{a2-m+2 n,b2+b3-m+2 n,1+a2+b2+b3-c-m+2 n},-x (1-z)] Pochhammer[1-a2,m-2 n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1-b2-b3,m-2 n] Pochhammer[-a2-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-a1-a2-b2-b3+c,m-2 n] Pochhammer[1-a2-b1-b2-b3+c,m-2 n])},{Abs[x]>1&&Abs[1-z]<1&&Abs[1-y]<1&&Abs[1-y]+Abs[x (-1+y)]<1&&Abs[x (-1+y)]<1&&Abs[(-1+y)/(-1+z)]<1&&Abs[1-z]+Abs[x (-1+z)]<1&&Abs[x (-1+z)]<1&&Abs[x-x z]<1&&Abs[x]/(1+Abs[x])>Abs[x-x z]&&Abs[x-x y]<1&&Abs[x]/(1+Abs[x])>Abs[x-x y],((-x)^-a1 x^(-m+n) (1-y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a1+a2+b2+b3-c+m},1-z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2+b3-c,m])+((-1)^(2 m) (-x)^(a2+b2+b3-c+m) (1-y)^(m-n) (1-z)^n Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{1-a2-m,1-b2-b3-m,-a2-b2-b3+c-m},{1-a1-a2-b2-b3+c-m,1-a2-b1-b2-b3+c-m},1/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2+b3-c,m] Pochhammer[a2+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[b2+b3,m] Pochhammer[1+a2+b2+b3-c,m])+((-x)^-b1 x^(-m+n) (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] HypergeometricPFQ[{b3,a2+n},{1+a2+b1+b2+b3-c+m},1-z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2+b3-c,m])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m) (1-z)^-b3 Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-a2+c+m},{1-a2-b2+c+m},(-1+y)/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[-a2+c,m] Pochhammer[-b2-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m] Pochhammer[-b2-b3+c,m-n])+((-1)^(m-n) x^(m-n) (1-y)^n (1-z)^(-a2-b2-b3+c+m-2 n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{-a2-b2+c+m-2 n,-b2-b3+c+m-n},{1-a2-b2-b3+c+m-2 n},1-z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[y]<1&&1/Abs[y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[z/y]<1,HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},z/y] (((-x)^-a1 x^(-m+n) (-y)^-a2 y^-n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (-y)^-a2 y^-n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n]))+((-x)^-a1 x^(-m+n) (-y)^-b2 y^-n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b3,a2-b2-n},{-a1-b2+c-m},z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-b1 x^(-m+n) (-y)^-b2 y^-n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b3,a2-b2-n},{-b1-b2+c-m},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])},{1/Abs[x]<1&&1/Abs[x]+1/Abs[z]<1&&1/Abs[z]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1,((-1)^n (-x)^-a1 x^(-m+n) y^n (-z)^(-a2-n) Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2-c+m-n,a2+n},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-1)^n (-x)^-b1 x^(-m+n) y^n (-z)^(-a2-n) Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1-c+m-n,a2+n},{1+a2-b3+n},1/z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-1)^-n (-x)^-a1 x^(-m+n) y^n (-z)^-b3 Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b3,1+a1+b3-c+m-2 n},{1-a2+b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+a1+b3-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n])+((-1)^-n (-x)^-b1 x^(-m+n) y^n (-z)^-b3 Gamma[a1-b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b3,1+b1+b3-c+m-2 n},{1-a2+b3-n},1/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n] Pochhammer[1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n])},{Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[((-1+y) z)/(y (-1+z))]<1&&Abs[x]>1,((-x)^-a1 x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (-1+1/z)^a2 (1-z)^-a2 Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] HypergeometricPFQ[{a2,1+a1+a2+b2-c+m},{1+a1+a2+b2+b3-c+m},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2+b3-c,m])+((-x)^-b1 x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (-1+1/z)^a2 (1-z)^-a2 Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] HypergeometricPFQ[{a2,1+a2+b1+b2-c+m},{1+a2+b1+b2+b3-c+m},(-1+z)/z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2+b3-c,m])+((-1)^m x^(m-n) (-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (-(y/(-1+y)))^(-m+n) (-1+1/z)^(a2-b3) (1-z)^-a2 (-(z/(-1+z)))^n Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{1-b3,a2-b3-n},{1-b3-n},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2+c,m])+((-1)^m x^(m-n) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (-1+1/z)^(-b2-b3+c) (1-z)^-a2 (-(z/(-1+z)))^(-m+2 n) Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{1-b3,-b2-b3+c+m-2 n},{1-a2-b2-b3+c+m-2 n},(-1+z)/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2+b2-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])+((-1)^n (-x)^(a2+b2+b3-c+n) x^(-m+n) (-1+1/y)^b2 (1-y)^-b2 (-1+1/z)^a2 (1-z)^-a2 ((-1+z)/z)^n Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{b2,a1+a2+b2+b3-c-m+2 n,a2+b1+b2+b3-c-m+2 n},{b2+b3-m+2 n,1+a2+b2+b3-c-m+2 n},(x (-1+y))/y] Pochhammer[1-a2,m-2 n] Pochhammer[a2,n] Pochhammer[1-b3,m-n] Pochhammer[1-b2-b3,m-2 n] Pochhammer[-a2-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-b3,m-2 n] Pochhammer[1-a1-a2-b2-b3+c,m-2 n] Pochhammer[1-a2-b1-b2-b3+c,m-2 n])},{Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[(y (-1+z))/((-1+y) z)]<1&&Abs[x]>1,((-x)^-a1 x^(-m+n) (-1+1/y)^a2 (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2-b3+c] HypergeometricPFQ[{a2,1+a1+a2+b3-c+m},{1+a1+a2+b2+b3-c+m},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2+b3-c,m])+((-x)^-b1 x^(-m+n) (-1+1/y)^a2 (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2-b3+c] HypergeometricPFQ[{a2,1+a2+b1+b3-c+m},{1+a2+b1+b2+b3-c+m},(-1+y)/y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2+b3-c,m])+((-1)^m x^(m-n) (-1+1/y)^(a2-b2) (1-y)^-a2 (-(y/(-1+y)))^n (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (-(z/(-1+z)))^(-m+n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{1-b2,a2-b2-n},{1-b2-n},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[-a2+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^m x^(m-n) (-1+1/y)^(-b2-b3+c) (1-y)^-a2 (-(y/(-1+y)))^(-m+2 n) (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a2+b2+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{1-b2,-b2-b3+c+m-2 n},{1-a2-b2-b3+c+m-2 n},(-1+y)/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,-m+2 n] Pochhammer[-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2+b3-c,-m+2 n] Pochhammer[-a2+c,m-n] Pochhammer[1-a2-b2-b3+c,m-2 n])+((-1)^n (-x)^(a2+b2+b3-c+n) x^(-m+n) (-1+1/y)^a2 (1-y)^-a2 ((-1+y)/y)^n (-1+1/z)^b3 (1-z)^-b3 Gamma[a1+a2+b2+b3-c] Gamma[a2+b1+b2+b3-c] Gamma[c] Gamma[-a2-b2-b3+c] HypergeometricPFQ[{b3,a1+a2+b2+b3-c-m+2 n,a2+b1+b2+b3-c-m+2 n},{b2+b3-m+2 n,1+a2+b2+b3-c-m+2 n},(x (-1+z))/z] Pochhammer[1-a2,m-2 n] Pochhammer[a2,n] Pochhammer[1-b2,m-n] Pochhammer[1-b2-b3,m-2 n] Pochhammer[-a2-b2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2+b3] Pochhammer[1-b2,m-2 n] Pochhammer[1-a1-a2-b2-b3+c,m-2 n] Pochhammer[1-a2-b1-b2-b3+c,m-2 n])},{Abs[1-x]<1&&Abs[y]<1&&1+Abs[y]<1/Abs[1-x]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[z]<1&&1+Abs[z]<1/Abs[1-x]&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1,(y^(m-n) z^n Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+b1-c-m},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[-a1-b1+c,m])/((m-n)! n! Gamma[-a1+c] Gamma[-b1+c] Pochhammer[-a1+c,m] Pochhammer[-b1+c,m])+((-1)^m (1-x)^(-a1-b1+c+m) y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x],((-1)^n (-x)^-a1 x^(-m+n) y^n Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{b3,a2+n},{-a1+c-m+2 n},z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1-c,m-2 n])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m-n])+((-1)^n (-x)^-b1 x^(-m+n) y^n Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{b3,a2+n},{-b1+c-m+2 n},z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m-n])},{Abs[1-x]<1&&Abs[y]<1&&1/Abs[1-x]>1+Abs[y]&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y/z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z]>1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[z-x z]<1,((-1)^(m-n) y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+n},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,n])+(y^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b3-c-m+2 n},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n] Pochhammer[1+a1+b1+b3-c,-m+2 n])+((-1)^m (1-x)^(-a1-b1+c+m) y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1-x)^(m-n) (-z)^(a1+b1-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,-a1-b1+c-m+2 n},{1-a1-b1-b3+c-m+2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b3+c,-m+2 n])},{Abs[y]<1&&1/Abs[1-x]+Abs[y]<1&&Abs[z]<1&&1/Abs[1-x]+Abs[z]<1&&Abs[1-x]>1,((1-x)^-a1 y^(m-n) z^n Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a1,-b1+c+m},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[-a1+c,m])+((1-x)^-b1 y^(m-n) z^n Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{b1,-a1+c+m},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[-b1+c,m])},{1/Abs[-1+x]<1&&Abs[y]<1&&1/Abs[-1+x]+Abs[y]<1&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[-1+x]),((-1)^(m-n) (1-x)^-a1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-n},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a2-b3,m])+((-1)^(m-n) (1-x)^-b1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-n},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m])+((1-x)^-a1 y^(m-n) (-z)^-b3 z^-n Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{a1,-b1-b3+c+m-2 n},{1+a1-b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1-a2+b3,-m+2 n])+((1-x)^-b1 y^(m-n) (-z)^-b3 z^-n Gamma[a1-b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b1,-a1-b3+c+m-2 n},{1-a1+b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[z-x z]&&Abs[y]<1&&1+Abs[y]<1/Abs[1-x]&&Abs[(-1+x) y]<1&&Abs[-1+x]+Abs[(-1+x) y]<1&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z-x z]<1&&1/Abs[z-x z]<1/(1+Abs[-1+x])&&Abs[z]>1,((-1)^n (1/((-1+x) z))^(m-n) (-z)^(a1+b1-c) z^-n (z-x z)^n Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,a2+m-n},{1+a2-b3+m-n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b1", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m-n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a1+a2+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m-n] Pochhammer[1+a1+a2-c,m-2 n] Pochhammer[1+a2+b1-c,m-2 n])+((-1)^(m-n) (1-x)^(-a1-b1+c+m-n) (1/((-1+x) z))^n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{b2,a2+n},{1+a2-b3+n},((1-x) y)/(z-x z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a1+a2+b1-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2-c,-m+2 n] Pochhammer[1+a2+b1-c,-m+2 n])+((1-x)^(m-n) (-z)^-a2 z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{b2,a2+n},{1+a2-b3+n},y/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2+b1-c,m])+((-1)^(2 n) y^n (1/((-1+x) z))^(m-n) (-z)^(a1+b1-c-n) (z-x z)^n Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{-a1-b3+c-m+2 n,-b1-b3+c-m+2 n},{1-a1-b1-b3+c-m+2 n},(z-x z)/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-2 n])+((-1)^(m-n) (1-x)^(-a1-b1+c+m-n) y^(m-n) (1/((-1+x) z))^n Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-b3+c+m-2 n,-b1-b3+c+m-2 n},{1-a1-b1-b3+c+m-2 n},1-x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n])+(y^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b3-c-m+2 n},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n] Pochhammer[1+a1+b1+b3-c,-m+2 n])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[1-z]<1&&Abs[y]+Abs[1-z]<1&&Abs[-1+z]<1&&1+Abs[-1+z]<Abs[x-x z]&&Abs[y]+Abs[-1+z]<1&&1/Abs[x-x z]<1&&1/Abs[x-x z]<1/(1+Abs[-1+z]),((-1)^n (1-z)^(-a2-b3+c+n) (1/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] HypergeometricPFQ[{b2,-a1-b3+c-m+2 n},{-a1-b3+c-m+n},y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a1"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m-n] Pochhammer[a1+a2+b3-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n] Pochhammer[1+a1+b3-c,m-2 n])+((-1)^(m-n) (-x)^(a2+b3-c) x^(-m+n) (1/(x (-1+z)))^n (x-x z)^(m-n) Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b2,-a1-b3+c+m-2 n},{-a1-b3+c-n},y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a1+b3-c,n] Pochhammer[a1+a2+b3-c,-m+2 n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+2 n] Pochhammer[1+a1+b3-c,-m+2 n])+((-x)^-a1 x^(-m+n) (1-z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{b2,a2+n},{-a1-b3+c-m+n},y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m-n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-1)^n (1-z)^(-a2-b3+c+n) (1/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] HypergeometricPFQ[{b2,-b1-b3+c-m+2 n},{-b1-b3+c-m+n},y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b1"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a2+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n] Pochhammer[1+b1+b3-c,m-2 n])+((-1)^(m-n) (-x)^(a2+b3-c) x^(-m+n) (1/(x (-1+z)))^n (x-x z)^(m-n) Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{b2,-b1-b3+c+m-2 n},{-b1-b3+c-n},y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a2", "+", "b1", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a2"}], "-", "b1", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[1+a2+b1-c,n] Pochhammer[1+b1+b3-c,n] Pochhammer[a2+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+2 n] Pochhammer[1+b1+b3-c,-m+2 n])+((-x)^-b1 x^(-m+n) (1-z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{b2,a2+n},{-b1-b3+c-m+n},y] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])},{Abs[x/(-1+x)]<1&&Abs[y]<1&&Abs[z]<1,((1-x)^-b1 y^(m-n) z^n HypergeometricPFQ[{b1,-a1+c+m},{c+m},x/(-1+x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Pochhammer[c,m])},{Abs[x]<1&&Abs[y/(1-z)]<1&&Abs[z/(-1+z)]<1&&Abs[y/(1-z)]+Abs[z/(-1+z)]<1,(x^(m-n) y^n (1-z)^(-a2-n) HypergeometricPFQ[{-b3+c+m,a2+n},{c+m},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Pochhammer[c,m])},{Abs[x/(-1+x)]<1&&Abs[y]<1&&Abs[y/z]<1&&Abs[z]>1,((1-x)^-b1 (x/(-1+x))^(m-n) (-z)^-a2 z^-n Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{b2,a2+n},{1+a2-b3+n},y/z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2-c,-m+2 n] Pochhammer[1+a1+a2-c,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2-c,-m+2 n])+((1-x)^-b1 (x/(-1+x))^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{b2,-a1-b3+c+m-2 n,a2-b3-n},{-b3+c+m-2 n,-a1-b3+c-n},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b3-c,-m+2 n] Pochhammer[1+a1+b3-c,n])/((m-n)! n! Gamma[a2] Gamma[-b3+c] Pochhammer[1-a2+b3,n] Pochhammer[1+a1+b3-c,-m+2 n])},{1/Abs[x]<1&&1/Abs[x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}]}], 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "<", "1"}], "&&", 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[y/(-1+z)]<1&&Abs[y/(-1+z)]<Abs[x]/(1+Abs[x])&&Abs[z/(-1+z)]<1&&Abs[y/(-1+z)]+Abs[z/(-1+z)]<1,((-1)^n (-x)^-a1 x^(-m+n) y^n (1-z)^(-a2-n) Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b3+c-m+2 n},{-a1+c-m+2 n},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b2,n] Pochhammer[1+a1-c,m-2 n])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m-n])+((-1)^n (-x)^-b1 x^(-m+n) y^n (1-z)^(-a2-n) Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b3+c-m+2 n},{-b1+c-m+2 n},z/(-1+z)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m-n])},{Abs[(-1+x)/x]<1&&Abs[y]<1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z]>1,((-1)^(m-n) (1/x)^a1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+n},{1+a1+a2+b1-c+n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,n])+((1/x)^a1 y^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{a1,1+a1+b3-c-m+2 n},{1+a1+b1+b3-c-m+2 n},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n] Pochhammer[1+a1+b1+b3-c,-m+2 n])+((-1)^m (1-x)^(-a1-b1+c+m) (1/x)^(-a1+c) x^-m y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1-b1+c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-z)^(a1+b1-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,-a1-b1+c-m+2 n},{1-a1-b1-b3+c-m+2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b3+c,-m+2 n])},{1/Abs[x]<1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1&&Abs[(-1+z)/z]<1&&1+Abs[(-1+z)/z]<1/Abs[y]&&Abs[y/z]+Abs[(-1+z)/z]<1&&Abs[(x (-1+z))/z]<1,((-x)^-a1 x^(-m+n) (1/z)^b3 ((-1+z)/z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{a2,b2},{-a1-b3+c-m},y] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-x)^-b1 x^(-m+n) (1/z)^b3 ((-1+z)/z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{a2,b2},{-b1-b3+c-m},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])+((-1)^(m-n) x^(m-n) (1-z)^(-a2-b3+c+m-n) (1/z)^(-b3+c) ((-1+z)/z)^n z^(-m+n) Gamma[a2+b3-c] Gamma[c] HypergeometricPFQ[{b2,-b3+c+m},{-b3+c+m-n},y/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[1-b3,n] Pochhammer[-b3+c,m])/((m-n)! n! Gamma[a2] Gamma[b3] Pochhammer[-b3+c,m-n] Pochhammer[1-a2-b3+c,m])+((-1)^n (-x)^(a2+b3-c+n) x^(-m+n) (1/z)^b3 ((-1+z)/z)^n Gamma[a1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] HypergeometricPFQ[{a2,b2},{a2-m+n},y] Pochhammer[1-a2,m-n] Pochhammer[1-b3,m-2 n] Pochhammer[b3,n] Pochhammer[-a2-b3+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b3+c,m-2 n] Pochhammer[1-a2-b1-b3+c,m-2 n])},{Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1&&Abs[((-1+x) z)/x]<1&&1/Abs[x]+Abs[((-1+x) z)/x]<1&&Abs[x]>1,(((-1+x)/x)^n (-x)^-b1 x^(-m+n) (x/(-1+x))^(a1+b1-c) z^n Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{b2,-a1+c+m,a2+n},{-a1+c+n,-b1+c+n},((-1+x) y)/x] Pochhammer[1-a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[-a1+c,m])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])+(((-1+x)/x)^n (-x)^-a1 x^(-m+n) (x/(-1+x))^(a1+b1-c) z^n Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{b2,-b1+c+m,a2+n},{-a1+c+n,-b1+c+n},((-1+x) y)/x] Pochhammer[a2,n] Pochhammer[1-b1,m-n] Pochhammer[b3,n] Pochhammer[-b1+c,m])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m-n] Pochhammer[-a1+c,n] Pochhammer[-b1+c,n])},{Abs[y]<1&&Abs[y/z]<1&&Abs[(x (-1+z))/z]<1&&Abs[(x (-1+z))/z]+1/Abs[z]<1&&Abs[z]>1,((-1)^n x^(m-n) y^n ((-1+z)/z)^(m-n) (-z)^(-a2-n) (z/(-1+z))^(a2+b3-c) Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1-b3,-b3+c+m},{1+a2-b3+n},1/z] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,n] Pochhammer[-a2+c,m-n])+(x^(m-n) y^n ((-1+z)/z)^(m-n) (-z)^-b3 (z/(-1+z))^(a2+b3-c) Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{1-a2-n,-a2+c+m-n},{1-a2+b3-n},1/z] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[a2-b3,n])/((m-n)! n! Gamma[a2] Gamma[-b3+c] Pochhammer[-b3+c,m])},{Abs[(-1+x)/x]<1&&Abs[y]<1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[x/(z-x z)]<1&&Abs[z]>1,((-1)^n (1/x)^a1 (x/((-1+x) z))^(m-n) (-z)^(a1+b1-c) z^-n ((-1+1/x) z)^n Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,a2+m-n},{1+a2-b3+m-n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b1", "+", "c"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m-n] Pochhammer[1-b1,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a1+a2+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m-n] Pochhammer[1+a2+b1-c,m-2 n])+HypergeometricPFQ[{b2,a2+n},{1+a2-b3+n},y/z] (((-1+1/x)^(-a1+c) (1-x)^-b1 ((-1+x)/x)^(m-n) (x/((-1+x) z))^n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[1-a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b1-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2-c,-m+2 n])+((1/x)^a1 ((-1+x)/x)^(m-n) (-z)^-a2 z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2+b1-c,m]))+((-1)^(2 n) (1/x)^a1 y^n (x/((-1+x) z))^(m-n) (-z)^(a1+b1-c-n) ((-1+1/x) z)^n Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-b1-b3+c-m+2 n},{1-a1-b1-b3+c-m+2 n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-2 n])+((-1)^(m-n) (-1+1/x)^(-a1+c) (1-x)^(-b1+m-n) x^(-m+n) y^(m-n) (x/((-1+x) z))^n Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-b3+c+m-2 n},{1-a1-b1-b3+c+m-2 n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n])+((1/x)^a1 y^(m-n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{a1,1+a1+b3-c-m+2 n},{1+a1+b1+b3-c-m+2 n},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b3+c] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n] Pochhammer[1+a1+b1+b3-c,-m+2 n])},{x!=0&&Abs[y]>0&&Abs[x]!=1&&Abs[y]<1&&(1+Abs[x]) Abs[y]<Abs[x]&&Abs[y/z]<1&&Abs[(-1+z)/z]<1&&Abs[y] (1+Abs[(-1+z)/z])<1&&Abs[y/z]+Abs[(-1+z)/z]<1&&Abs[z/(x-x z)]<1&&Abs[z/(x-x z)]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}]}], 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], " ", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}]}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["y", "z"], "]"}], "<", "1"}], "&&", 
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "z"], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[x]<Abs[x]^2,((-1+1/z)^(-b3+c) (1-z)^-a2 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] HypergeometricPFQ[{b2,-a1-b3+c-m+2 n},{-a1-b3+c-m+n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-b3,n] Pochhammer[1+a1+b3-c,m-n] Pochhammer[a1+a2+b3-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+b3-c,m-2 n])+((-x)^-a1 x^(-m+n) (1/z)^b3 ((-1+z)/z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{a2,b2},{-a1-b3+c-m},y] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+((-1+1/z)^(-b3+c) (1-z)^-a2 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] HypergeometricPFQ[{b2,-b1-b3+c-m+2 n},{-b1-b3+c-m+n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[1-b3,n] Pochhammer[1+b1+b3-c,m-n] Pochhammer[a2+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+b1+b3-c,m-2 n])+HypergeometricPFQ[{a2,b2},{a2-m+n},y] (((-1)^(m-n) (-x)^(a2+b3-c) x^(-m+n) (x (-1+1/z))^(m-n) (1/z)^b3 (z/(x (-1+z)))^n Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,n] Pochhammer[1-a2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b3-c,-m+2 n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+2 n])+((-1)^(m-n) (-x)^(a2+b3-c) x^(-m+n) (x (-1+1/z))^(m-n) (1/z)^b3 (z/(x (-1+z)))^n Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a2"}], "-", "b1", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a2", "+", "b1", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,m-n] Pochhammer[b1,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a2+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+2 n]))+((-x)^-b1 x^(-m+n) (1/z)^b3 ((-1+z)/z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{a2,b2},{-b1-b3+c-m},y] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])},{Abs[1-x]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y]>1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[y-x y]<1&&Abs[z]<1&&1/Abs[1-x]>1+Abs[z]&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[z/y]<1,((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2-c+m-2 n},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n] Pochhammer[1+a1+b1+b2-c,m-2 n])+((-1)^m (1-x)^(-a1-b1+c+m) y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1-x)^(m-n) (-y)^(a1+b1-c+m-n) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b3,-a1-b1+c-m+2 n},{1-a1-b1-b2+c-m+2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2+c,-m+2 n])},{1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x])&&Abs[z]<1&&1/Abs[-1+x]+Abs[z]<1&&Abs[z/y]<1,((-1)^n (1-x)^-a1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-m+n},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a2-b2,m])+((-1)^n (1-x)^-b1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m])+((1-x)^-a1 (-y)^-b2 y^(-m+n) z^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{a1,-b1-b2+c-m+2 n},{1+a1-b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1-a2+b2,m-2 n])+((1-x)^-b1 (-y)^-b2 y^(-m+n) z^n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b1,-a1-b2+c-m+2 n},{1-a1+b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])},{Abs[1-x]<1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&1/Abs[y]<1&&1/Abs[y-x y]<1&&1/Abs[y-x y]<1/(1+Abs[-1+x])&&Abs[z]<1&&1+Abs[z]<1/Abs[1-x]&&Abs[(-1+x) z]<1&&Abs[-1+x]+Abs[(-1+x) z]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^(2 n) (1-x)^(-a1-b1+c+n) (1/((-1+x) y))^(m-n) (y-x y)^-n z^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-a2+c-m+n,-a2-b1+c-m+n},{1-a1-a2-b1+c-m+n},1-x] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,m])+((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^n (1-x)^(-a1-b1+c+n) (1/((-1+x) y))^(m-n) z^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-b2+c-m+2 n,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},1-x] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])+((-1)^(2 n) (1/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) (y-x y)^n z^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{-a1-b2+c-m+2 n,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},(y-x y)/y] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-2 n])+((-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2-c+m-2 n},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n] Pochhammer[1+a1+b1+b2-c,m-2 n])+((-1)^n (-y)^(a1+b1-c-n) y^(-m+n) (y-x y)^(m-n) z^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1+a1+a2-c,1+a2+b1-c,a2+n,a1+a2+b1-c-m+n},{1+a2-b2+n,1+a1+a2-c-m+n,1+a2+b1-c-m+n},1/((-1+x) y)] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[-a1-a2+c,m-n] Pochhammer[-a2-b1+c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,n] Pochhammer[1-a1-a2-b1+c,m-n])},{1/Abs[x]<1&&Abs[1-y]<1&&Abs[-1+y]<1&&1+Abs[-1+y]<Abs[x-x y]&&1/Abs[x-x y]<1&&1/Abs[x-x y]<1/(1+Abs[-1+y])&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&Abs[1-y]+Abs[z]<1&&Abs[-1+y]+Abs[z]<1,((1-y)^(-a2-b2+c) (1/(x (-1+y)))^(m-n) z^n Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{-a1-a2+c-m+n,-a1-b2+c-m+2 n},{1-a1-a2-b2+c-m+n},1-y] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m-n])+((-x)^(a2+b2-c) (1/(x (-1+y)))^(m-n) z^n Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{-a1-a2+c-m+n,-a1-b2+c-m+2 n},{1-a1-a2-b2+c-m+n},(x-x y)/x] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2-c,m-n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,m-n])+((-1)^n (-x)^-a1 x^(-m+n) z^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{b2,a2+n},{1+a1+a2+b2-c+m-n},1-y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m-2 n])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m-n])+((1-y)^(-a2-b2+c) (1/(x (-1+y)))^(m-n) z^n Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{-a2-b1+c-m+n,-b1-b2+c-m+2 n},{1-a2-b1-b2+c-m+n},1-y] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[a2+b1+b2-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m-n])+((-1)^(m-n) (-x)^(a2+b2-c) x^(-m+n) (1/(x (-1+y)))^n (x-x y)^(m-n) Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{b3,-b1-b2+c+m-2 n},{-b1-b2+c-n},z] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b1,n] Pochhammer[1+a2+b1-c,n] Pochhammer[1+b1+b2-c,n] Pochhammer[a2+b1+b2-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+2 n] Pochhammer[1+b1+b2-c,-m+2 n])+((-x)^-b1 x^(-m+n) (1-y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{b3,a2+n},{-b1-b2+c-m+n},z] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m-n])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])},{Abs[x]<1&&Abs[y/(-1+y)]<1&&Abs[z/(1-y)]<1&&Abs[y/(-1+y)]+Abs[z/(1-y)]<1,(x^(m-n) (1-y)^(-a2-n) z^n HypergeometricPFQ[{-b2+c+m,a2+n},{c+m},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Pochhammer[c,m])},{Abs[x/(-1+x)]<1&&Abs[z]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^n (1-x)^-b1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{-a2+c-m+n},x/(-1+x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,m])+((1-x)^-b1 (-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{b1,-a1-b2+c-m+2 n},{-b2+c-m+2 n},x/(-1+x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-b2+c] Pochhammer[1-a2+b2,m-2 n])},{1/Abs[x]<1&&1/Abs[x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "<", "1"}], "&&", 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[y/(-1+y)]<1&&Abs[z/(-1+y)]<1&&Abs[z/(-1+y)]<Abs[x]/(1+Abs[x])&&Abs[y/(-1+y)]+Abs[z/(-1+y)]<1,((-1)^n (-x)^-a1 x^(-m+n) (1-y)^(-a2-n) z^n Gamma[-a1+b1] Gamma[c] HypergeometricPFQ[{a2+n,-a1-b2+c-m+2 n},{-a1+c-m+2 n},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b3,n] Pochhammer[1+a1-c,m-2 n])/((m-n)! n! Gamma[b1] Gamma[-a1+c] Pochhammer[1+a1-b1,m-n])+((-1)^n (-x)^-b1 x^(-m+n) (1-y)^(-a2-n) z^n Gamma[a1-b1] Gamma[c] HypergeometricPFQ[{a2+n,-b1-b2+c-m+2 n},{-b1+c-m+2 n},y/(-1+y)] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[-b1+c] Pochhammer[1-a1+b1,m-n])},{Abs[(-1+x)/x]<1&&Abs[((-1+x) y)/x]<1&&Abs[z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^n (1/x)^a1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m-n},{1+a1+a2+b1-c+m-n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((1/x)^a1 (-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,1+a1+b2-c+m-2 n},{1+a1+b1+b2-c+m-2 n},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n] Pochhammer[1+a1+b1+b2-c,m-2 n])+((-1)^m (1-x)^(-a1-b1+c+m) (1/x)^(-a1+c) x^-m y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1-b1+c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^(a1+b1-c+m-n) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b3,-a1-b1+c-m+2 n},{1-a1-b1-b2+c-m+2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2+c,-m+2 n])},{1/Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[(x (-1+y))/y]<1&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&(1+Abs[(-1+y)/y]) Abs[z]<1&&Abs[z/y]<1&&Abs[(-1+y)/y]+Abs[z/y]<1,((-x)^-a1 x^(-m+n) (1/y)^b2 ((-1+y)/y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,b3},{-a1-b2+c-m},z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((-x)^-b1 x^(-m+n) (1/y)^b2 ((-1+y)/y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,b3},{-b1-b2+c-m},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])+((-1)^(m-n) x^(m-n) (1-y)^(-a2-b2+c+m-n) (1/y)^(-b2+c) ((-1+y)/y)^n y^(-m+n) Gamma[a2+b2-c] Gamma[c] HypergeometricPFQ[{b3,-b2+c+m},{-b2+c+m-n},z/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[1-b2,n] Pochhammer[-b2+c,m])/((m-n)! n! Gamma[a2] Gamma[b2] Pochhammer[-b2+c,m-n] Pochhammer[1-a2-b2+c,m])+((-1)^n (-x)^(a2+b2-c+n) x^(-m+n) (1/y)^b2 ((-1+y)/y)^n Gamma[a1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] HypergeometricPFQ[{a2,b3},{a2-m+n},z] Pochhammer[1-a2,m-n] Pochhammer[1-b2,m-2 n] Pochhammer[b2,n] Pochhammer[-a2-b2+c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b2+c,m-2 n] Pochhammer[1-a2-b1-b2+c,m-2 n])},{Abs[(x (-1+y))/y]<1&&Abs[(x (-1+y))/y]+1/Abs[y]<1&&Abs[z]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^n x^(m-n) ((-1+y)/y)^(m-n) (-y)^(-a2-n) (y/(-1+y))^(a2+b2-c) z^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1-b2,-b2+c+m},{1+a2-b2+n},1/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,n] Pochhammer[-a2+c,m-n])+(x^(m-n) ((-1+y)/y)^(m-n) (-y)^-b2 (y/(-1+y))^(a2+b2-c) z^n Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{1-a2-n,-a2+c+m-n},{1-a2+b2-n},1/y] Pochhammer[a1,m-n] Pochhammer[b1,m-n] Pochhammer[a2-b2,n] Pochhammer[b3,n])/((m-n)! n! Gamma[a2] Gamma[-b2+c] Pochhammer[-b2+c,m])},{Abs[(-1+x)/x]<1&&Abs[x/(y-x y)]<1&&Abs[z]<1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1,((-1)^n (1/x)^a1 (x/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) z^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-a2-b1+c-m+n},{1-a1-a2-b1+c-m+n},1-1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m])+((-1+1/x)^(-a1+c) (1-x)^-b1 ((-1+x)/x)^(m-n) (x/((-1+x) y))^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},((1-x) z)/((-1+1/x) x y)] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[1-a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b1-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2-c,-m+2 n])+((1/x)^a1 ((-1+x)/x)^(m-n) (-y)^-a2 y^-n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{b3,a2+n},{1+a2-b2+n},z/y] Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,n] Pochhammer[1+a1+a2+b1-c,m])+((-1)^n (-1+1/x)^(-a1+c) (1-x)^(-b1+n) x^-n (x/((-1+x) y))^(m-n) z^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},(-1+x)/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])+((-1)^(2 n) (1/x)^a1 (x/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) ((-1+1/x) y)^n z^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},1-1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-2 n])+((1/x)^a1 (-y)^-b2 y^(-m+n) z^n Gamma[a2-b2] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{a1,1+a1+b2-c+m-2 n},{1+a1+b1+b2-c+m-2 n},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[-a1-b2+c] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n] Pochhammer[1+a1+b1+b2-c,m-2 n])},{1/Abs[x]<1&&Abs[(-1+y)/y]<1&&Abs[y/(x-x y)]<1&&Abs[y/(x-x y)]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], "+", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}]}], 
RowBox[{
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], " ", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}]}]]}], 
RowBox[{
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "y"], "]"}], "<", "1"}], "&&", 
RowBox[{"0", "<", 
RowBox[{"Abs", "[", 
FractionBox["z", "y"], "]"}], "<", "1"}]}]},
{"\[Infinity]", 
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
StripWrapperBoxes->True]\))&&Abs[z]<1&&(1+Abs[x]) Abs[z]<Abs[x]&&(1+Abs[(-1+y)/y]) Abs[z]<1&&Abs[z/y]<1&&Abs[(-1+y)/y]+Abs[z/y]<1,((-1+1/y)^(-b2+c) (1-y)^-a2 y^-n (y/(x (-1+y)))^(m-n) z^n Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{1-b2,-a1-b2+c-m+2 n},{1-a1-a2-b2+c-m+n},(-1+y)/y] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[a1+a2+b2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m-n])+((-x)^-a1 x^(-m+n) (1/y)^b2 ((-1+y)/y)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,b3},{-a1-b2+c-m},z] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+((-1+1/y)^(-b2+c) (1-y)^-a2 ((-1+y)/y)^n (y/(x (-1+y)))^(m-n) Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] HypergeometricPFQ[{b3,-b1-b2+c-m+2 n},{-b1-b2+c-m+n},z/y] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[1-b2,n] Pochhammer[1+b1+b2-c,m-n] Pochhammer[a2+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+b1+b2-c,m-2 n])+HypergeometricPFQ[{a2,b3},{a2-m+n},z] (((-1)^(m-n) (-x)^(a2+b2-c) x^(-m+n) (x (-1+1/y))^(m-n) (1/y)^b2 (y/(x (-1+y)))^n Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,n] Pochhammer[1-a2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[a1+a2+b2-c,-m+2 n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,n] Pochhammer[1+a1+a2-c,-m+2 n])+((-1)^(m-n) (-x)^(a2+b2-c) x^(-m+n) (x (-1+1/y))^(m-n) (1/y)^b2 (y/(x (-1+y)))^n Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,m-n] Pochhammer[b1,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a2+b1+b2-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,n] Pochhammer[1+a2+b1-c,-m+2 n]))+((-x)^-b1 x^(-m+n) (1/y)^b2 ((-1+y)/y)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,b3},{-b1-b2+c-m},z] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])},{Abs[y]>1&&Abs[z]>1&&Abs[1-x]<1&&Abs[y/z]<1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1,((-1)^n y^n (-z)^(-a2-n) z^(-m+n) Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (-y)^(-a2+b3+m-n) y^-n (-z)^-b3 z^(-m+n) Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+n},1-x] Pochhammer[a2-b3,-m+2 n] Pochhammer[b3,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+((-y)^-b2 y^-n (-z)^-b3 z^(-m+n) Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2+b3-c+m},1-x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) y^n z^(m-n) Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,n] Pochhammer[b3,m-n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1-x)^(m-n) (-z)^(a1+b1-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,-a1-b1+c-m+2 n},{1-a1-b1-b3+c-m+2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b3+c,-m+2 n])+((-1)^(m-n) (1-x)^(m-n) (-y)^(a1+b1+b3-c+m-n) y^-n (-z)^-b3 Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{b3,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b3-c+m-2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[z]>1&&Abs[y]>1&&Abs[1-x]<1&&Abs[z/y]<1&&Abs[(-1+x) z]<1&&Abs[1-x]+Abs[(-1+x) z]<1&&Abs[(-1+x) y]<1&&Abs[1-x]+Abs[(-1+x) y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1,((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+n},1-x] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+((-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2+b3-c+m},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{-a1+c+m,-b1+c+m},{1-a1-b1+c+m},1-x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1-x)^(m-n) (-y)^(a1+b1-c+m-n) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b3,-a1-b1+c-m+2 n},{1-a1-b1-b2+c-m+2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2+c,-m+2 n])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 (-z)^(a1+b1+b2-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{b2,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b2-c+m-2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{1/Abs[1-x]<1&&1/Abs[-1+x]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[1-x])&&1/Abs[y]<1/(1+1/Abs[-1+x])&&Abs[y/z]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[-1+x]),((-1)^-n (1-x)^-a1 (-y)^(-a2+b3+n) y^(-m+n) (-z)^-b3 z^-n Gamma[-a1+b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-m+n},{1+a1-b1},1/(1-x)] Pochhammer[a2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a2-b2-b3,m-2 n])+((-1)^(m-n) (1-x)^-a1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-n},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a2-b3,m])+((-1)^-n (1-x)^-b1 (-y)^(-a2+b3+n) y^(-m+n) (-z)^-b3 z^-n Gamma[a1-b1] Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{1-a1+b1},1/(1-x)] Pochhammer[a2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,m-2 n])+((-1)^(m-n) (1-x)^-b1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-n},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m])+((1-x)^-a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{a1,-b1-b2-b3+c-m},{1+a1-b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1-a2+b2+b3,m])+((1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a1-b2-b3+c-m},{1-a1+b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m])},{1/Abs[1-x]<1&&1/Abs[-1+x]<1&&1/Abs[z]<1&&1/Abs[z]<1/(1+1/Abs[1-x])&&1/Abs[z]<1/(1+1/Abs[-1+x])&&Abs[z/y]<1&&1/Abs[y]<1&&1/Abs[y]<1/(1+1/Abs[-1+x]),((-1)^n (1-x)^-a1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-m+n},{1+a1-b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a2-b2,m])+((-1)^(-m+n) (1-x)^-a1 (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[-a1+b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{a1,-a2-b1+c-n},{1+a1-b1},1/(1-x)] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a2-b2-b3,-m+2 n])+((-1)^n (1-x)^-b1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{1-a1+b1},1/(1-x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m])+((-1)^(-m+n) (1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a1-b1] Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-n},{1-a1+b1},1/(1-x)] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n])+((1-x)^-a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[-a1+b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{a1,-b1-b2-b3+c-m},{1+a1-b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2-b3+c] Pochhammer[1-a2+b2+b3,m])+((1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a1-b1] Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a1-b2-b3+c-m},{1-a1+b1},1/(1-x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m])},{Abs[y]>1&&Abs[z]>1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[z-x z]&&Abs[z-x z]>1&&Abs[y/z]<1&&1/(1+Abs[-1+x])>1/Abs[z-x z]&&Abs[1-x]<1&&Abs[-1+x]+Abs[(-1+x) y]<1&&Abs[(-1+x) y]<1&&1+1/Abs[y]<1/Abs[y-x y]&&Abs[y-x y]<1,((-1)^n (1/((-1+x) z))^(m-n) (-z)^(a1+b1-c) z^-n (z-x z)^n Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,a2+m-n},{1+a2-b3+m-n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b1", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m-n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a1+a2+b1-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m-n] Pochhammer[1+a1+a2-c,m-2 n] Pochhammer[1+a2+b1-c,m-2 n])+((-1)^(m-n) (1-x)^(-a1-b1+c+m-n) (1/((-1+x) z))^n Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{b2,a2+n},{1+a2-b3+n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n] Pochhammer[a1+a2+b1-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,n] Pochhammer[1+a1+a2-c,-m+2 n] Pochhammer[1+a2+b1-c,-m+2 n])+((-1)^n y^n (-z)^(-a2-n) z^(-m+n) Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (-y)^(-a2+b3+m-n) y^-n (-z)^-b3 z^(-m+n) Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+n},1-x] Pochhammer[a2-b3,-m+2 n] Pochhammer[b3,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+HypergeometricPFQ[{-a1-b3+c-m+2 n,-b1-b3+c-m+2 n},{1-a1-b1-b3+c-m+2 n},1-x] (((-1)^n (1-x)^(-a1-b1+c+n) y^n (1/((-1+x) z))^(m-n) Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,m-2 n])+((-1)^(2 n) y^n (1/((-1+x) z))^(m-n) (-z)^(a1+b1-c-n) (z-x z)^n Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["1", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b3", "-", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
RowBox[{"Re", "[", "z", "]"}]}], "<=", "1"}]},
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b3", "+", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-2 n]))+((-y)^-b2 y^-n (-z)^-b3 z^(-m+n) Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2+b3-c+m},1-x] Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^(m-n) (1-x)^(m-n) (-y)^(a1+b1+b3-c+m-n) y^-n (-z)^-b3 Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{b3,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b3-c+m-2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[z]>1&&Abs[y]>1&&Abs[-1+x]<1&&1+Abs[-1+x]<Abs[y-x y]&&Abs[y-x y]>1&&Abs[z/y]<1&&1/(1+Abs[-1+x])>1/Abs[y-x y]&&Abs[1-x]<1&&Abs[-1+x]+Abs[(-1+x) z]<1&&Abs[(-1+x) z]<1&&1+1/Abs[z]<1/Abs[z-x z]&&Abs[z-x z]<1,((-1)^(2 n) (1-x)^(-a1-b1+c+n) (1/((-1+x) y))^(m-n) (y-x y)^-n z^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-a2+c-m+n,-a2-b1+c-m+n},{1-a1-a2-b1+c-m+n},1-x] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,m])+((-1)^n (1/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) z^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{-a1-a2+c-m+n,-a2-b1+c-m+n},{1-a1-a2-b1+c-m+n},(y-x y)/y] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m])+((-1)^n (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+m-n},1-x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,b1},{1+a1+a2+b1-c+n},1-x] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+((-1)^n (1-x)^(-a1-b1+c+n) (1/((-1+x) y))^(m-n) z^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{-a1-b2+c-m+2 n,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},1-x] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])+((-1)^(2 n) (1/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) (y-x y)^n z^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{-a1-b2+c-m+2 n,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},(y-x y)/y] (\!\(\*
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-2 n])+((-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,b1},{1+a1+b1+b2+b3-c+m},1-x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^(m-n) (1-x)^(m-n) (-y)^-b2 (-z)^(a1+b1+b2-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{b2,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b2-c+m-2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,-m+2 n] Pochhammer[b1,m-n] Pochhammer[-a1-b1-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[x/(-1+x)]<1&&Abs[y]>1&&Abs[y/z]<1&&Abs[z]>1,((-1)^n (1-x)^-b1 (-y)^(-a2+b3+n) y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{-a2+c-m+n},x/(-1+x)] Pochhammer[a2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a2-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,m-2 n])+((-1)^(m-n) (1-x)^-b1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-n},{-a2+c-n},x/(-1+x)] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a2-c,n])/((m-n)! n! Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b3,m])+((1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a1-b2-b3+c-m},{-b2-b3+c-m},x/(-1+x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,m])},{Abs[x/(-1+x)]<1&&Abs[z]>1&&Abs[z/y]<1&&Abs[y]>1,((-1)^n (1-x)^-b1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-m+n},{-a2+c-m+n},x/(-1+x)] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a2+c] Pochhammer[1+a2-b2,m])+((-1)^(m-n) (1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] HypergeometricPFQ[{b1,-a1-a2+c-n},{-a2+c-n},x/(-1+x)] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a2-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a2+c] Pochhammer[1+a2-b2-b3,-m+2 n])+((1-x)^-b1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] HypergeometricPFQ[{b1,-a1-b2-b3+c-m},{-b2-b3+c-m},x/(-1+x)] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-b2-b3+c] Pochhammer[1-a2+b2+b3,m])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1,((-1)^n (1/x)^a1 (-y)^(-a2+b3+n) y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m-n},{1+a1+a2+b1-c+m-n},(-1+x)/x] Pochhammer[a2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,m-2 n] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (1/x)^a1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+n},{1+a1+a2+b1-c+n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,n])+((1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,1+a1+b2+b3-c+m},{1+a1+b1+b2+b3-c+m},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) (1/x)^(-a1+c) x^-m y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1-b1+c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-z)^(a1+b1-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b2,-a1-b1+c-m+2 n},{1-a1-b1-b3+c-m+2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b3+c,-m+2 n])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^(a1+b1+b3-c+m-n) y^-n (-z)^-b3 Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{b3,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b3-c+m-2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[(-1+x)/x]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1,((-1)^n (1/x)^a1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m-n},{1+a1+a2+b1-c+m-n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+n},{1+a1+a2+b1-c+n},(-1+x)/x] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+((1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,1+a1+b2+b3-c+m},{1+a1+b1+b2+b3-c+m},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^m (1-x)^(-a1-b1+c+m) (1/x)^(-a1+c) x^-m y^(m-n) z^n Gamma[a1+b1-c] Gamma[c] HypergeometricPFQ[{1-a1,-a1+c+m},{1-a1-b1+c+m},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[b3,n])/((m-n)! n! Gamma[a1] Gamma[b1] Pochhammer[1-a1-b1+c,m])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^(a1+b1-c+m-n) y^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{b3,-a1-b1+c-m+2 n},{1-a1-b1-b2+c-m+2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2+c,-m+2 n])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^-b2 (-z)^(a1+b1+b2-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{b2,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b2-c+m-2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{1/Abs[x]<1&&Abs[x/(y-x y)]<1&&Abs[x/(y-x y)]<Abs[x]/(1+Abs[x])&&Abs[((-1+x) z)/x]<1&&1/Abs[x]+Abs[((-1+x) z)/x]<1&&Abs[z/y]<1,((-1)^n ((-1+x)/x)^n (-x)^-a1 (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^(m-n) ((-1+1/x) y)^-n z^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1-b1,-a2-b1+c-m+n},{1+a1-b1},1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a2-b2,m])+((-1)^n ((-1+x)/x)^n (-x)^-b1 (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^(m-n) ((-1+1/x) y)^-n z^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1-a1,-a1-a2+c-m+n},{1-a1+b1},1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m])+(((-1+x)/x)^n (-x)^-a1 (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^(m-n) z^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{1-b1,-b1-b2+c-m+2 n},{1+a1-b1},1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1-a2+b2,m-2 n])+(((-1+x)/x)^n (-x)^-b1 (x/(-1+x))^(a1+b1-c) (x/((-1+x) y))^(m-n) z^n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{1-a1,-a1-b2+c-m+2 n},{1-a1+b1},1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])},{1/Abs[x]<1&&Abs[x/(z-x z)]<1&&Abs[x/(z-x z)]<Abs[x]/(1+Abs[x])&&Abs[((-1+x) y)/x]<1&&1/Abs[x]+Abs[((-1+x) y)/x]<1&&Abs[y/z]<1,((-1)^(m-n) ((-1+x)/x)^(m-n) (-x)^-a1 (x/(-1+x))^(a1+b1-c) y^(m-n) (x/((-1+x) z))^n ((-1+1/x) z)^(-m+n) Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1-b1,-a2-b1+c-n},{1+a1-b1},1/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a2-b3,m])+((-1)^(m-n) ((-1+x)/x)^(m-n) (-x)^-b1 (x/(-1+x))^(a1+b1-c) y^(m-n) (x/((-1+x) z))^n ((-1+1/x) z)^(-m+n) Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1-a1,-a1-a2+c-n},{1-a1+b1},1/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m])+(((-1+x)/x)^(m-n) (-x)^-a1 (x/(-1+x))^(a1+b1-c) y^(m-n) (x/((-1+x) z))^n Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{1-b1,-b1-b3+c+m-2 n},{1+a1-b1},1/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,-m+2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1-a2+b3,-m+2 n])+(((-1+x)/x)^(m-n) (-x)^-b1 (x/(-1+x))^(a1+b1-c) y^(m-n) (x/((-1+x) z))^n Gamma[a1-b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{1-a1,-a1-b3+c+m-2 n},{1-a1+b1},1/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n])},{Abs[(-1+x)/x]<1&&Abs[y]>1&&Abs[((-1+x) y)/x]<1&&Abs[y/z]<1&&Abs[z]>1&&Abs[x/(z-x z)]<1,((-1)^(2 (m-n)) (-1+1/x)^(-a1+c) (1-x)^(-b1+m-n) x^(-m+n) y^(m-n) (x/((-1+x) z))^n ((-1+1/x) z)^(-m+n) Gamma[-a2+b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-a2+c-n},{1-a1-a2-b1+c-n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[a1+a2+b1-c,n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b3,m])+((-1)^n (1/x)^a1 (-y)^(-a2+b3+n) y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b3] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m-n},{1+a1+a2+b1-c+m-n},(-1+x)/x] Pochhammer[a2-b3,m-2 n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[a2] Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,m-2 n] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (1/x)^a1 y^(m-n) (-z)^(-a2-m+n) z^-n Gamma[-a2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+n},{1+a1+a2+b1-c+n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b3,m] Pochhammer[1+a1+a2+b1-c,n])+((-1)^(2 n) (1/x)^a1 y^n (x/((-1+x) z))^(m-n) (-z)^(a1+b1-c-n) ((-1+1/x) z)^n Gamma[a2-b3] Gamma[1+a1+b1-c] Gamma[a1+b1+b3-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-b1-b3+c-m+2 n},{1-a1-b1-b3+c-m+2 n},1-1/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "b1", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "b1", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,n] Pochhammer[b3,m-n] Pochhammer[a1+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b3] Gamma[b3] Pochhammer[1-a2+b3,m-2 n])+((-1)^(m-n) (-1+1/x)^(-a1+c) (1-x)^(-b1+m-n) x^(-m+n) y^(m-n) (x/((-1+x) z))^n Gamma[a2-b3] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-b3+c+m-2 n},{1-a1-b1-b3+c+m-2 n},(-1+x)/x] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{"-", "b3"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b3"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b3-c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b3+c] Pochhammer[1-a2+b3,-m+2 n])+((1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,1+a1+b2+b3-c+m},{1+a1+b1+b2+b3-c+m},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^m (1/x)^a1 y^(m-n) (-z)^(a1+b1-c-m+n) z^-n ((-1+1/x) z)^n Gamma[-a2+b3] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1+a2+b1-c,a1+a2+b1-c-n,a2+m-n},{1+a2+b1-c-n,1+a2-b3+m-n},x/((-1+x) z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "x"]}], ")"}], " ", "z"}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b1", "+", "c"}]], 
RowBox[{
RowBox[{
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "z", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["x", 
RowBox[{"z", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m-n] Pochhammer[1-b1,n] Pochhammer[b2,m-n] Pochhammer[-a2-b1+c,n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a2-b3,m-n] Pochhammer[1-a1-a2-b1+c,n])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^(a1+b1+b3-c+m-n) y^-n (-z)^-b3 Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b3+c] HypergeometricPFQ[{b3,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b3-c+m-2 n},y/z] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1-b3+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[(-1+x)/x]<1&&Abs[z]>1&&Abs[((-1+x) z)/x]<1&&Abs[z/y]<1&&Abs[y]>1&&Abs[x/(y-x y)]<1,((-1)^(2 n) (-1+1/x)^(-a1+c) (1-x)^(-b1+n) x^-n (x/((-1+x) y))^(m-n) ((-1+1/x) y)^-n z^n Gamma[-a2+b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-a2+c-m+n},{1-a1-a2-b1+c-m+n},(-1+x)/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b1+c] Pochhammer[1+a2-b2,m])+((-1)^n (1/x)^a1 (x/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) z^n Gamma[-a2+b2] Gamma[1+a1+b1-c] Gamma[a1+a2+b1-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-a2-b1+c-m+n},{1-a1-a2-b1+c-m+n},1-1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[a1+a2+b1-c,m-n])/((m-n)! n! Gamma[a1] Gamma[1-a2] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a2-b2,m])+((-1)^n (1/x)^a1 (-y)^(-a2-n) y^(-m+n) z^n Gamma[-a2+b2] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+m-n},{1+a1+a2+b1-c+m-n},(-1+x)/x] Pochhammer[a2,m] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a2+b1-c,m-n])/((m-n)! n! Gamma[b2] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2,m] Pochhammer[1+a1+a2+b1-c,m-n])+((-1)^(m-n) (1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^(-a2+b2+m-n) z^-n Gamma[a2-b2] Gamma[-a2+b2+b3] Gamma[c] Gamma[-a1-a2-b1+c] HypergeometricPFQ[{a1,1+a1+a2-c+n},{1+a1+a2+b1-c+n},(-1+x)/x] Pochhammer[a2-b2,-m+2 n] Pochhammer[b2,m-n] Pochhammer[1+a1+a2-c,n] Pochhammer[1+a2+b1-c,n])/((m-n)! n! Gamma[a2] Gamma[b3] Gamma[-a1-a2+c] Gamma[-a2-b1+c] Pochhammer[1+a2-b2-b3,-m+2 n] Pochhammer[1+a1+a2+b1-c,n])+((-1)^n (-1+1/x)^(-a1+c) (1-x)^(-b1+n) x^-n (x/((-1+x) y))^(m-n) z^n Gamma[a2-b2] Gamma[a1+b1-c] Gamma[c] Gamma[1-a1-b1+c] HypergeometricPFQ[{1-a1,-a1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},(-1+x)/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-a1-b1-b2+c] Pochhammer[1-a2+b2,m-2 n])+((-1)^(2 n) (1/x)^a1 (x/((-1+x) y))^(m-n) (-y)^(a1+b1-c-n) ((-1+1/x) y)^n z^n Gamma[a2-b2] Gamma[1+a1+b1-c] Gamma[a1+b1+b2-c] Gamma[c] Gamma[-a1-b1+c] HypergeometricPFQ[{1-b1,-b1-b2+c-m+2 n},{1-a1-b1-b2+c-m+2 n},1-1/x] (\!\(\*
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
SuperscriptBox[
RowBox[{"Re", "[", "x", "]"}], "2"], "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "x", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[a1+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[1-b2] Gamma[b2] Pochhammer[1-a2+b2,m-2 n])+((1/x)^a1 (-y)^-b2 y^(-m+n) (-z)^-b3 z^-n Gamma[a2-b2-b3] Gamma[c] Gamma[-a1-b1-b2-b3+c] HypergeometricPFQ[{a1,1+a1+b2+b3-c+m},{1+a1+b1+b2+b3-c+m},(-1+x)/x] Pochhammer[b2,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b2+b3-c,m] Pochhammer[1+b1+b2+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[-a1-b2-b3+c] Gamma[-b1-b2-b3+c] Pochhammer[1-a2+b2+b3,m] Pochhammer[1+a1+b1+b2+b3-c,m])+((-1)^(m-n) (1/x)^a1 ((-1+x)/x)^(m-n) (-y)^-b2 (-z)^(a1+b1+b2-c+m-n) z^-n Gamma[a1+a2+b1-c] Gamma[a1+b1+b2+b3-c] Gamma[c] Gamma[-a1-b1-b2+c] HypergeometricPFQ[{b2,a1+b1+b2+b3-c+m-2 n},{1+a1+b1+b2-c+m-2 n},z/y] Pochhammer[1-a1,-m+2 n] Pochhammer[a1,m-n] Pochhammer[1-b1,n] Pochhammer[-a1-b1-b2+c,-m+2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1-a1-a2-b1+c,-m+2 n] Pochhammer[1-a1-b1-b2-b3+c,-m+2 n])},{Abs[x] Abs[y] (Abs[y]-Abs[x] Abs[y]+Abs[x z]+Abs[x-x z])<0&&Abs[x]<x Conjugate[x]&&1/Abs[x]+Abs[(1-z)/y]<1&&1/Abs[x]+Abs[z/y]<1&&Abs[z/(-1+z)]<1&&1+Abs[z/(-1+z)]<Abs[y/(1-z)],((-x)^-a1 x^(-m+n) (1-z)^-a2 ((1-z)/y)^n Gamma[-a1+b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a1+a2+b3-c+m,a2+n},{1+a2-b2+n},z/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{"1", "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "y"], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m])/((m-n)! n! Gamma[b1] Gamma[b2] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-b1 x^(-m+n) (1-z)^-a2 ((1-z)/y)^n Gamma[a1-b1] Gamma[-a2+b2] Gamma[c] HypergeometricPFQ[{1+a2+b1+b3-c+m,a2+n},{1+a2-b2+n},z/y] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "a2"}]], 
RowBox[{
RowBox[{"1", "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "y"], ")"}], "a2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m])/((m-n)! n! Gamma[a1] Gamma[b2] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b2,n])+((-x)^-a1 x^(-m+n) (1-z)^-a2 ((1-z)/y)^n Gamma[-a1+b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{-a1-b2-b3+c-m,a2-b2-n},{-a1-b2+c-m},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"1", "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "y"], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b2,n])+((-x)^-b1 x^(-m+n) (1-z)^-a2 ((1-z)/y)^n Gamma[a1-b1] Gamma[a2-b2] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c-m,a2-b2-n},{-b1-b2+c-m},z/(-1+z)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["y", 
RowBox[{
RowBox[{"-", "1"}], "+", "z"}]], ")"}], 
RowBox[{"-", "b2"}]], 
RowBox[{
RowBox[{"1", "+", 
RowBox[{"Re", "[", "y", "]"}]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "z"}], "y"], ")"}], "b2"], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b2,n])},{Abs[x] Abs[z] (Abs[x y]+Abs[x-x y]+Abs[z]-Abs[x] Abs[z])<0&&Abs[x]<x Conjugate[x]&&1/Abs[x]+Abs[(1-y)/z]<1&&1/Abs[x]+Abs[y/z]<1&&Abs[y/(-1+y)]<1&&1+Abs[y/(-1+y)]<Abs[z/(1-y)],((-x)^-a1 x^(-m+n) (1-y)^-a2 ((1-y)/z)^n Gamma[-a1+b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a1+a2+b2-c+m,a2+n},{1+a2-b3+n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "z"], ")"}], "a2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"1", "+", 
RowBox[{"Re", "[", "z", "]"}]}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[a2,n] Pochhammer[1+a1+a2-c,m])/((m-n)! n! Gamma[b1] Gamma[b3] Gamma[-a1-a2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-b1 x^(-m+n) (1-y)^-a2 ((1-y)/z)^n Gamma[a1-b1] Gamma[-a2+b3] Gamma[c] HypergeometricPFQ[{1+a2+b1+b2-c+m,a2+n},{1+a2-b3+n},y/z] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "z"], ")"}], "a2"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"1", "+", 
RowBox[{"Re", "[", "z", "]"}]}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m])/((m-n)! n! Gamma[a1] Gamma[b3] Gamma[-a2-b1+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2-b3,n])+((-x)^-a1 x^(-m+n) (1-y)^-a2 ((1-y)/z)^n Gamma[-a1+b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{-a1-b2-b3+c-m,a2-b3-n},{-a1-b3+c-m},y/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "z"], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"1", "+", 
RowBox[{"Re", "[", "z", "]"}]}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+b3-c,m])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1-a2+b3,n])+((-x)^-b1 x^(-m+n) (1-y)^-a2 ((1-y)/z)^n Gamma[a1-b1] Gamma[a2-b3] Gamma[c] HypergeometricPFQ[{-b1-b2-b3+c-m,a2-b3-n},{-b1-b3+c-m},y/(-1+y)] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], "z"], ")"}], "b3"], 
RowBox[{
RowBox[{"Re", "[", "y", "]"}], ">=", 
RowBox[{"1", "+", 
RowBox[{"Re", "[", "z", "]"}]}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], ")"}], 
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
StripWrapperBoxes->True]\)) Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1-a2+b3,n])},{Abs[y/(-1+y)]<1&&Abs[(-1+z)/z]<1&&Abs[z/(x-x z)]<1&&Abs[x]>1,((1/x)^(m-n) (-x)^-a1 (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b3+c] HypergeometricPFQ[{a2,-a1-b2-b3+c-m},{-a1-b3+c-m},y/(-1+y)] Pochhammer[a1,m-n] Pochhammer[b3,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b3-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b3-c,m])+HypergeometricPFQ[{a2,a2-b2-n},{a2-n},y/(-1+y)] (((1-y)^-a2 (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "a1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "a1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[a1+a2+b3-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b3] Gamma[1-a1-a2-b3+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n])+((-x)^(a2+b3-c) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[-a1+b1] Gamma[1+a2+b3-c] Gamma[a1+a2+b3-c] Gamma[c] Gamma[-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a1"}], "-", "a2", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a1", "+", "a2", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[a1+a2+b3-c,m-2 n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b3] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n])+((1-y)^-a2 (-1+1/z)^(-a2+c) (1-z)^-b3 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[a2+b3-c] Gamma[c] Gamma[1-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{"-", "b1"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], "b1"], 
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a2+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b3] Gamma[1-a2-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n])+((-x)^(a2+b3-c) (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n (z/(x (-1+z)))^(m-n) Gamma[a1-b1] Gamma[1+a2+b3-c] Gamma[a2+b1+b3-c] Gamma[c] Gamma[-a2-b3+c] (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"x", " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
FractionBox["1", "z"]}], ")"}]}], ")"}], 
RowBox[{
RowBox[{"-", "a2"}], "-", "b1", "-", "b3", "+", "c"}]], 
RowBox[{
RowBox[{
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "z", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "z", "]"}]}]},
{
SuperscriptBox[
RowBox[{"(", 
FractionBox["z", 
RowBox[{"x", "-", 
RowBox[{"x", " ", "z"}]}]], ")"}], 
RowBox[{"a2", "+", "b1", "+", "b3", "-", "c"}]], 
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a2+b1+b3-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b3] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n]))+((1/x)^(m-n) (-x)^-b1 (1-y)^-a2 (-1+1/z)^b3 (1-z)^-b3 ((-1+z)/z)^n Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b3+c] HypergeometricPFQ[{a2,-b1-b2-b3+c-m},{-b1-b3+c-m},y/(-1+y)] Pochhammer[b1,m-n] Pochhammer[b3,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b3-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b3+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b3-c,m])},{Abs[z/(-1+z)]<1&&Abs[(-1+y)/y]<1&&Abs[y/(x-x y)]<1&&Abs[x]>1,((1/x)^(m-n) (-x)^-a1 (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[-a1+b1] Gamma[c] Gamma[-a1-a2-b2+c] HypergeometricPFQ[{a2,-a1-b2-b3+c-m},{-a1-b2+c-m},z/(-1+z)] Pochhammer[a1,m-n] Pochhammer[b2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[1+a1+b2-c,m])/((m-n)! n! Gamma[b1] Gamma[-a1-a2+c] Gamma[-a1-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2+b2-c,m])+HypergeometricPFQ[{a2,a2-b3-n},{a2-n},z/(-1+z)] (((-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (y/(x (-1+y)))^(m-n) (1-z)^-a2 Gamma[-a1+b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[a1+a2+b2-c,m-2 n])/((m-n)! n! Gamma[a2] Gamma[b1] Gamma[b2] Gamma[1-a1-a2-b2+c] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n])+((-x)^(a2+b2-c) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (y/(x (-1+y)))^(m-n) (1-z)^-a2 Gamma[-a1+b1] Gamma[1+a2+b2-c] Gamma[a1+a2+b2-c] Gamma[c] Gamma[-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[a1,m-n] Pochhammer[1-a2,n] Pochhammer[1+a1+a2-c,m-n] Pochhammer[a1+a2+b2-c,m-2 n])/((m-n)! n! Gamma[1-a1] Gamma[a1] Gamma[a2] Gamma[b1] Gamma[b2] Pochhammer[1+a1-b1,m-n] Pochhammer[1+a1+a2-c,m-2 n])+((-1+1/y)^(-a2+c) (1-y)^-b2 ((-1+y)/y)^n (y/(x (-1+y)))^(m-n) (1-z)^-a2 Gamma[a1-b1] Gamma[a2+b2-c] Gamma[c] Gamma[1-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a2+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[b2] Gamma[1-a2-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n])+((-x)^(a2+b2-c) (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (y/(x (-1+y)))^(m-n) (1-z)^-a2 Gamma[a1-b1] Gamma[1+a2+b2-c] Gamma[a2+b1+b2-c] Gamma[c] Gamma[-a2-b2+c] (\!\(\*
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
RowBox[{"Re", "[", "x", "]"}], "+", 
SuperscriptBox[
RowBox[{"Re", "[", "y", "]"}], "2"]}], ">", 
RowBox[{"Re", "[", "y", "]"}]}]},
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
StripWrapperBoxes->True]\)) Pochhammer[1-a2,n] Pochhammer[b1,m-n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[a2+b1+b2-c,m-2 n])/((m-n)! n! Gamma[a1] Gamma[a2] Gamma[1-b1] Gamma[b1] Gamma[b2] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1-c,m-2 n]))+((1/x)^(m-n) (-x)^-b1 (-1+1/y)^b2 (1-y)^-b2 ((-1+y)/y)^n (1-z)^-a2 Gamma[a1-b1] Gamma[c] Gamma[-a2-b1-b2+c] HypergeometricPFQ[{a2,-b1-b2-b3+c-m},{-b1-b2+c-m},z/(-1+z)] Pochhammer[b1,m-n] Pochhammer[b2,n] Pochhammer[1+a2+b1-c,m-n] Pochhammer[1+b1+b2-c,m])/((m-n)! n! Gamma[a1] Gamma[-a2-b1+c] Gamma[-b1-b2+c] Pochhammer[1-a1+b1,m-n] Pochhammer[1+a2+b1+b2-c,m])}};




(* ::Section:: *)
(*End package*)


End[]


EndPackage[]
