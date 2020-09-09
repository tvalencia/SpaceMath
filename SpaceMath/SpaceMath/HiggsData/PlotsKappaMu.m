(* Wolfram Language Package *)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF Kmu************************************************************************************)
(******************************************************************************************************************************************************)
Kmu1sigX::usage = "Kmu1sigX"
Kmu2sigX::usage = "Kmu2sigX"
datamumu1sigXY::usage = "datamumu1sigXY"
datamumu2sigXY::usage = "datamumu2sigXY"
datamumu1sigXYZ::usage = "datamumu1sigXYZ"
datamumu2sigXYZ::usage = "datamumu2sigXYZ"
tablemu1sigXY::usage = "tablemu1sigXY"
tablemu2sigXY::usage = "tablemu2sigXY"
tablemu1sigXYZ::usage = "tablemu1sigXYZ"
tablemu2sigXYZ::usage = "tablemu2sigXYZ"
kappamu2sig::usage = "kappamu2sig"
Kmu2sigWXYZ::usage = "Kmu2sigWXYZ"
kappamu1sig::usage = "kappamu1sig"
Kmu1sigWXYZ::usage = "Kmu1sigWXYZ"

dataKmu1sig::usage = "dataKmu1sig"
dataKmu2sig::usage = "dataKmu2sig"

KMUoneLHC::usage = usage = "\!\(\*
StyleBox[\"KMUoneLHC\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghmumu_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xlabel_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Mu]\)]\)\ when there is dependence only on one parameter. The argument ghtautau is h\[Mu]\[Mu]\ 
coupling. Here, h represents to SM-like Higgs boson while mu stands for mu-lepton. The label x indicates the parameter to constrain, while xmin and xmax are the \
initial and final values defined by users and xlabel is used for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."


TableKMUoneLHC::usage="\!\(\*
StyleBox[\"TableKMUoneLHC\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghmumu_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xstep_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table with two columns: KMUone-X. The output file will be saved as TableKMUone_1sigma_X.txt and TableKMUone_2sigma_X.txt
in $UserDocumentsDirectory. "

KmuLHC::usage="KmuLHC[ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\), \
when there is dependence on two or more parameters. The argument ghmumu is the  h\[mu]\[mu]\ coupling. Here, h represents to SM-like Higgs boson while mu is the mu-lepton. Labels x and y \
indicate the parameters to constrain, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKmuLHC::usage="TableKmuLHC[ghmumu_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[mu]\)]\). The arguments ghtt, ghbb and ghmumu are the htt, \
hbb and hmumu couplings. Here, h represents to SM-like Higgs boson while t, b and mu are the top and bottom quarks and the mu-lepton. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************End OF Kmu**********************************************************************************)
(******************************************************************************************************************************************************)

Begin["`Package`"]
End[]

Begin["`PlotsKappaMu`Private`"]

(******************************************************************************************************************************************************)
(*********************************************************Begin Kappa mu******************************************************************************)
(******************************************************************************************************************************************************)

Individual process;
kappa mu

(*kappa mu to 1\[Sigma] in the case in which there is dependence in one parameter*)

Kmu1sigX[ghmumu_,x_,xmin_,xmax_,xlabel_]:=Plot[{kmu[ghmumu],kappaMuSUP1sig,kappaMuINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)",
"U.L.[1\[Sigma]]", "L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->800,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->1,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa mu to 2\[Sigma] in the case in which there is dependence in one parameter*)

Kmu2sigX[ghmumu_,x_,xmin_,xmax_,xlabel_]:=Plot[{kmu[ghmumu],kappaMuSUP2sig,kappaMuINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)",
"U.L.[2\[Sigma]]", "L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->800,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->1,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

KMUoneLHC[ghmumu_,x_,xmin_,xmax_,xlabel_]:={
Kmu1sigX[ghmumu,x,xmin,xmax,xlabel],
Kmu2sigX[ghmumu,x,xmin,xmax,xlabel]
}
(*KappamuX1sig[ghmumu[0.9,0.1,u],u,500,2000,u[GeV]]
KappamuX2sig[ghmumu[0.9,0.1,u],u,500,2000,u[GeV]]*)

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-mu}*)

datamumu1sigXY[ghmumu_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaMuINF1sig<=kmu[ghmumu]<=kappaMuSUP1sig, kmu[ghmumu],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-mu}*)

datamumu2sigXY[ghmumu_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaMuINF2sig<=kmu[ghmumu]<=kappaMuSUP2sig, kmu[ghmumu],0]}, {x, xmin,xmax,xstep}]


(*Create a table of three columns to 1 \[Sigma] \[Rule] {x,y,kappa-mu}*)

datamumu1sigXYZ[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaMuINF1sig<=kmu[ghmumu]<=kappaMuSUP1sig, kmu[ghmumu],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*Create a table of three columns to 2 \[Sigma] \[Rule] {x,y,kappa-mu}*)

datamumu2sigXYZ[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappamuINF2sig<=kmu[ghmumu]<=kappamuSUP2sig, kmu[ghmumu],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*databb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Do[list=Append[list={x,y,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0
]}], {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]*)

TableKMUoneLHC[ghmumu_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKMUone_1sigma_LHC.txt"}],Re[datamumu1sigXY[ghmumu,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKMUone_2sigma_LHC.txt"}],Re[datamumu2sigXY[ghmumu,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

 (*With this commands a table is generated and saved inside the folder TABLE*)

(*************************************************************************************************************************************************************************************)

(*This are the commands used in the shell of mathematica*)

tablemu1sigXY[ghmumu_,x_,xmin_,xmax_,xstep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tablemu1sigXY.txt"}],Re[datamumu1sigXY[ghmumu,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tablemu2sigXY[ghmumu_,x_,xmin_,xmax_,xstep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tablemu2sigXY.txt"}],Re[datamumu2sigXY[ghmumu,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tablemu1sigXYZ[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tablemu1sigXYZ.txt"}],Re[datamumu1sigXYZ[ghmumu,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tablemu2sigXYZ[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
FileNameJoin[{$UserDocumentsDirectory,"tablemu2sigXYZ.txt"}],Re[datamumu2sigXYZ[ghmumu,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
,"Table"
]

(*tablemu2sigXYZ[ghmumu[c\[Alpha],Zmumu,1000],c\[Alpha],0,1,0.1,Zmumu,0,0.1,0.01]
tablemu1sigXY[ghmumu[c\[Alpha],0.05,1000],c\[Alpha],0,1,0.1]*)

(*************************************************************************************************************************************************************************************)
(*kappa mu in the case in which there are dependence in two or more parameters*)

(*To 2\[Sigma]*)

(*kappa mu to 2\[Sigma]*)
kappamu2sig[ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaMuINF2sig<= Abs[kmu[ghmumu]] <= kappaMuSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{Style["\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)", Larger, Bold]}, {1, 0.5}],
(* AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, *)
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Yellow, Dashed, Thickness[0.003]]}, PlotStyle -> {{Yellow, Opacity[0.8]}}, AspectRatio->1]

Kmu2sigWXYZ[
ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappamu2sig[ghmumu,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

(*kappa mu to 1\[Sigma]*)
kappamu1sig[ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaMuINF1sig<= Abs[kmu[ghmumu]] <= kappaMuSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{Style["\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Mu]\)]\)", Larger, Bold]}, {1, 0.5}],
(* AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Yellow, Dashed, Thickness[0.003]]}, PlotStyle -> {{Yellow, Opacity[0.3]}}, AspectRatio->1]

Kmu1sigWXYZ[
ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappamu1sig[ghmumu,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KmuLHC[
ghmumu_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
Kmu1sigWXYZ[
ghmumu,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
Kmu2sigWXYZ[
ghmumu,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*Kmu1sigWXYZ[ghmumu[-ArcCos[cba]+ArcTan[tb],tb],cba,tb,-1,1,0.1,50,c\[Beta]\[Alpha],t\[Beta],xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,50]*)

(*Kappamu2sig[ghmumu[c\[Alpha],Zmumu,u],u,Zmumu,500,2000,0,1
,u[GeV],Zmumu,c\[Alpha],yfor,0.9,0.99,0.01,yformin,yformax,yforstep]*)

(*TABLES FOR Kmu*)

dataKmu1sig[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    kappaMuINF1sig <= Kmu[ghmumu] <= kappaMuSUP1sig, Kmu[ghmumu],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKmu2sig[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    kappaMuINF2sig <= Kmu[ghmumu] <= kappaMuSUP2sig, Kmu[ghmumu],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR Kmu*)

TableKmuLHC[ghmumu_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKmu_1sigma_LHC.txt"}],
Re[
dataKmu1sig[ghmumu,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKmu_2sigma_LHC.txt"}],
Re[
dataKmu2sig[ghmumu,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
 "Table"
]
}

(******************************************************************************************************************************************************)
(*********************************************************End Kappa mu******************************************************************************)
(******************************************************************************************************************************************************)

End[] 
