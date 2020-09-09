(* Wolfram Language Package *)

(***************************************************************************************************************************************************)
(**********************************************************Begin kappa gluon HE************************************************************************)
(***************************************************************************************************************************************************)
KGlu1sigXHE::usage = "KGlu1sigX"
KGlu2sigXHE::usage = "KGlu2sigX"
KappaGluGlu1sigHE::usage = "KappaGluGlu1sig"
KGlu1sigWXYZHE::usage = "KGlu1sigWXYZ"
KappaGluGlu2sigHE::usage = "KappaGluGlu2sig"
KGlu2sigWXYZHE::usage = "KGlu2sigWXYZ"

KGLUoneHE::usage = "KGluoneHE[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(gluon\)]\) when there is dependence only on one parameter. The argument ghWW is the hWW \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKGLUoneHE::usage="TableKGluoneHE[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_] \[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(gluon\)]\). The argument ghWW is the \
hWW coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. "

KGluHE::usage="KGluHE[ghtt_,ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(gluon\)]\), \
when there is dependence on two or more parameters. The arguments ghWW is the hWW \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKGluHE::usage="TableKGluHE[ghtt_,ghbb_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(gluon\)]\). The argument ghWW is the hWW, \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."

(***************************************************************************************************************************************************)
(**********************************************************End kappa gluon HE**************************************************************************)
(***************************************************************************************************************************************************)

Begin["`Package`"]
End[]

Begin["`PlotsKappaGluonHE`Private`"]

(***************************************************************************************************************************************************)
(**********************************************************Begin kappa gluon HE**************************************************************************)
(***************************************************************************************************************************************************)
Individual process;
kappa gluon HE

(*kappa gluon to 1\[Sigma] in the case in which there is dependence in one parameter*)

KGlu1sigXHE[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgluglu[ghtt, ghbb],kappaGluonSUP1sigHE,kappaGluonINF1sigHE},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)",
"U.L.[1\[Sigma]]", "L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->800,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->1,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa gluon to 2\[Sigma] in the case in which there is dependence in one parameter*)

KGlu2sigXHE[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgluglu[ghtt, ghbb],kappaGluonSUP2sigHE,kappaGluonINF2sigHE},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)",
"U.L.[2\[Sigma]]", "L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->800,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->1,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

KGLUoneHE[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:={
KGlu1sigXHE[ghtt,ghbb,x,xmin,xmax,xlabel],
KGlu2sigXHE[ghtt,ghbb,x,xmin,xmax,xlabel]
}

(*EXPORTING TABLES FOR KGLUone*)

TableKGLUoneHE[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGLUone_1sigma_HE.txt"}],Re[datagg1sigXYZHE[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGLUone_2sigma_HE.txt"}],Re[datagg2sigXYZHE[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*To 1\[Sigma]*)

KappaGluGlu1sigHE[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaGluonINF1sigHE<=kgluglu[ghtt,ghbb]<=kappaGluonSUP1sigHE},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{Style["\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)", Larger, Bold]}, {1, 0.5}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio->1]

KGlu1sigWXYZHE[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KappaGluGlu1sigHE[ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 2\[Sigma]*)

KappaGluGlu2sigHE[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaGluonINF2sigHE<=kgluglu[ghtt,ghbb]<=kappaGluonSUP2sigHE},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{Style["\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)", Larger, Bold]}, {1, 0.5}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio->1]

KGlu2sigWXYZHE[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KappaGluGlu2sigHE[ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KGluHE[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KGlu1sigWXYZHE[
ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KGlu2sigWXYZHE[
ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*KappaGluon2sig[
ghtt[c\[Alpha],Ztt,u], ghbb[c\[Alpha],Zbb,u],Ztt,u,0,1,500,2000,Ztt,u,c\[Alpha],Zbb,0.9,1,0.005,0,1,0.2]*)

(*TABLES FOR KGlu*)

dataKGlu1sigHE[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KGluINF1sigHE<= kgluglu[ghtt, ghbb] <=KGluSUP1sigHE,kgluglu[ghtt, ghbb],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKGlu2sigHE[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KGluINF2sigHE<= kgluglu[ghtt, ghbb] <=KGluSUP2sigHE,kgluglu[ghtt, ghbb],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR KGlu*)

TableKGluHE[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGlu_1sigma_HE.txt"}],
Re[
dataKGlu1sigHE[ghtt,ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGlu_2sigma_HE.txt"}],
Re[
dataKGlu2sigHE[ghtt,ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},"Table"
]
}

(***************************************************************************************************************************************************)
(**********************************************************End kappa gluon HE**************************************************************************)
(***************************************************************************************************************************************************)
End[] 