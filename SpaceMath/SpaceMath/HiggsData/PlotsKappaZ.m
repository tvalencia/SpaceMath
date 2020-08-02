(* Wolfram Language Package *)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF Kappa-Z************************************************************************************)
(******************************************************************************************************************************************************)
KZ1sigX::usage = "See later"
KZ2sigX::usage = "See later"
dataZZ1sigXY::usage = "See later"
dataZZ2sigXY::usage = "See later"
dataZZ1sigXYZ::usage = "See later"
dataZZ2sigXYZ::usage = "See later"
tableZ1sigXY::usage = "See later"
tableZ2sigXY::usage = "See later"
tableZ1sigXYZ::usage = "See later"
tableZ2sigXYZ::usage = "See later"
kappaz2sig::usage = "See later"
KZ2sigWXYZ::usage = "See later"
kappaz1sig::usage = "See later"
KZ1sigWXYZ::usage = "See later"

KZone::usage = "KZone[ghzz_,x_,xmin_,xmax_,xlabel_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\) when there is dependence only on one parameter. The argument ghzz is the hZZ \
coupling. Here, h represents to SM-like Higgs boson while Z is the Z gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKZone::usage="TableKZone[ghzz_,x_,xmin_,xmax_,xstep_] \[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\). The argument ghzz is the \
hZZ coupling. Here, h represents to SM-like Higgs boson while Z is the Z gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. "

KZ::usage="KZ[ghzz_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\), \
when there is dependence on two or more parameters. The arguments ghzz is the hZZ \
coupling. Here, h represents to SM-like Higgs boson while Z is the Z gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKZ::usage="TableKZ[ghzz_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\). The argument ghzz is the hZZ, \
coupling. Here, h represents to SM-like Higgs boson while Z is the Z gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************End OF Kappa-Z*******************************************************************************)
(******************************************************************************************************************************************************)

Begin["`Package`"]
End[]

Begin["`PlotsKappaZ`Private`"]

(***************************************************************************************************************************************************)
(**********************************************************Begin kappa Z****************************************************************************)
(***************************************************************************************************************************************************)
Individual process;
kappa Z

(*kappa Z to 1\[Sigma] in the case in which there is dependence in one parameter*)

KZ1sigX[ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{kZ[ghZZ],kappaZSUP1sig,kappaZINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa Z to 2\[Sigma] in the case in which there is dependence in one parameter*)

KZ2sigX[ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{kZ[ghZZ],kappaZSUP2sig,kappaZINF2sig},{x,xmin,xmax} 
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

KZone[ghZZ_,x_,xmin_,xmax_,xlabel_]:={KZ1sigX[ghZZ,x,xmin,xmax,xlabel],KZ2sigX[ghZZ,x,xmin,xmax,xlabel]}

(*KappaZX1sig[ghZZ[c\[Alpha],1],c\[Alpha],0,1,Subscript[c, \[Alpha]]]
KappaZX2sig[ghZZ[c\[Alpha],1],c\[Alpha],0,1,Subscript[c, \[Alpha]]]*)

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-Z}*)

dataZZ1sigXY[ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaZINF1sig<=kZ[ghZZ]<=kappaZSUP1sig, kZ[ghZZ],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-W}*)

dataZZ2sigXY[ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaZINF2sig<=kZ[ghZZ]<=kappaZSUP2sig, kZ[ghZZ],0]}, {x, xmin,xmax,xstep}]

(*Create a table of three columns to 1 \[Sigma] \[Rule] {x,y,kappa-W}*)

dataZZ1sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaZINF1sig<=kZ[ghZZ]<=kappaZSUP1sig, kZ[ghZZ],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*Create a table of three columns to 2 \[Sigma] \[Rule] {x,y,kappa-top}*)

dataZZ2sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaZINF2sig<=kZ[ghZZ]<=kappaZSUP2sig, kZ[ghZZ],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*databb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Do[list=Append[list={x,y,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0
]}], {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]*)

 (*With this commands a table is generated and saved inside the folder TABLE*)

TableKZone[ghZZ_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZone_1sigma.txt"}],Re[dataZZ1sigXYZ[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZone_2sigma.txt"}],Re[dataZZ2sigXYZ[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*************************************************************************************************************************************************************************************)

(*This are the commands used in the shell of mathematica*)

tableZ1sigXY[ghZZ_,x_,xmin_,xmax_,xstep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tableZ1sigXY.txt"}],Re[dataZZ1sigXY[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tableZ2sigXY[ghZZ_,x_,xmin_,xmax_,xstep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tableZ2sigXY.txt"}],Re[dataZZ2sigXY[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tableZ1sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tableZ1sigXYZ.txt"}],Re[dataZZ1sigXYZ[ghZZ,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tableZ2sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tableZ2sigXYZ.txt"}],Re[dataZZ2sigXYZ[ghZZ,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
,"Table"
]

(*tableZ2sigXY[ghZZ[c\[Alpha],1],c\[Alpha],0.7,1,0.025]
tableZ2sigXYZ[ghZZ[c\[Alpha],u],c\[Alpha],0.7,1,0.025,u,0.1,1,0.1]*)

(*To 2\[Sigma]*)

(*kappa W in the case in which there are dependence in two or more parameters*)
kappaz2sig[ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaZINF2sig<= kZ[ghZZ] <= kappaZSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Blue, Dashed, Thickness[0.003]]}, PlotStyle -> {{Blue, Opacity[0.3]}}, AspectRatio -> 0.6]

KZ2sigWXYZ[
ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaz2sig[ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

(*kappa W in the case in which there are dependence in two or more parameters*)
kappaz1sig[ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaZINF2sig<= kZ[ghZZ] <= kappaZSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Blue, Dashed, Thickness[0.003]]}, PlotStyle -> {{Blue, Opacity[0.3]}}, AspectRatio -> 0.6]

KZ1sigWXYZ[
ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaz1sig[ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KZ[
ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KZ1sigWXYZ[
ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KZ2sigWXYZ[
ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*KappaZ1sig[
ghZZ[c\[Alpha],u],c\[Alpha],u,0.8,1,1,2,calpha,U,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep]*)

(***************************************************************************************************************************************************)
(**********************************************************End kappa Z******************************************************************************)
(***************************************************************************************************************************************************)

End[]
