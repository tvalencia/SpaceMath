(* Wolfram Language Package *)

RVone::usage = "\!\(\*
StyleBox[\"RVone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghZZ_\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule]  \
This command evaluates \!\(\*SubscriptBox[\(R\), \(V\)]\) with V=W, \
Z when there is dependence only on one parameter. The arguments ghtt, ghbb, ghVV are the h-top top, h-bottom bottom, h-VV \
couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the axis-X label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])"

RGAMone::usage = "\!\(\*
StyleBox[\"RGAMone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghWW_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"gCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"mCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\) when there is dependence only on one parameter. The arguments ghtt, ghbb, ghWW and gCH are the h-top top, h-bottom bottom, h-WW and h-\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) \
couplings. Here, h represents to SM-like Higgs boson. Labels mCH and x \
indicate the charged scalar mass and the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the axis-X label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])"

RBOTone::usage = "\!\(\*
StyleBox[\"RBOTone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule]  \
This command evaluates \!\(\*SubscriptBox[\(R\), \(b\)]\)\
when there is dependence only on one parameter. The arguments ghtt, ghbb are the h-top top \
and h-bottom bottom couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the axis-X label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])"

RTAUone::usage = "\!\(\*
StyleBox[\"RTAUone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtautau_\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule]  \
This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\)\ 
 when there is dependence only on one parameter. The arguments ghtt, ghbb and ghtautau are the h-top top \
, h-bottom bottom and h-tau tau couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the axis-X label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])"

TableRVone::usage="\!\(\*
StyleBox[\"TableRVone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghVV_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xstep_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(V\)]\), with V= W, Z. The arguments ghtt, ghbb and ghVV are the h-top top \
, h-bottom bottom and h-V V couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the intervales from xmin to xmax. "

TableRGAMone::usage="\!\(\*
StyleBox[\"TableRGAMone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghWW_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"gCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"mCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xstep_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW, gCH, gCH and mCH are the h-top top \
, h-bottom bottom, h-W W, \!\(\*SuperscriptBox[\(ghH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings, while mCH is the charged scalar boson mass that could be to contribute to process. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the intervales from xmin to xmax. "

TableRBOTone::usage="\!\(\*
StyleBox[\"TableRBOTone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xstep_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(b\)]\). The arguments ghtt are ghbb are the h-top top \
and h-bottom bottom couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the intervales from xmin to xmax. "

TableRTAUone::usage="\!\(\*
StyleBox[\"TableRTAUone\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtautau_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xstep_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\). The arguments ghtt are ghbb are the h-top top, \
h-bottom bottom and h-tau tau couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the intervales from xmin to xmax. "

RXALL::usage="\!\(\*
StyleBox[\"RXALL\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtt_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghbb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghZZ_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghWW_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ghtautau_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"gCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"mCH_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"x_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"y_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xmax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ymin_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ymax_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"xlabel_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"ylabel_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"PP_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\). The arguments ghtt are ghbb are the h-top top, \
h-bottom bottom and h-tau tau couplings. Here, h represents to SM-like Higgs boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the intervales from xmin to xmax. "

RV::usage="RV[ghtt_,ghbb_,ghVV_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(R\), \(V\)]\) with V=W, \
Z when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghVV are the h-top top, h-bottom bottom, h-VV \
couplings. Here, h represents to SM-like Higgs boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the axis-X label (axis-Y label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the intervale from xformin (yformin) to xformax (yformax), respectively. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])"

RXALL1sig::usage ="See later"

RXALL2sig::usage ="See later"

RZ1sigX::usage ="See later"

RZ2sigX::usage ="See later"

RZone::usage ="See later"

(*TABLES FOR RVone*)
dataRZone1sig::usage ="See later"
dataRZone2sig::usage ="See later"

(*EXPORTING TABLES FOR RVone*)
TableRZone::usage ="See later"

(*RZ to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
RZ2sig::usage ="See later"
RZ2sigWXYZ::usage ="See later"

(*RZ to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
RZ1sig::usage ="See later"
RZ1sigWXYZ::usage ="See later"
RZ::usage ="See later"

(*TABLES FOR RZ*)
dataRZ1sig::usage ="See later"
dataRZ2sig::usage ="See later"

(*EXPORTING TABLES FOR RZ*)
TableRZ::usage ="See later"

(* Individual process *)
(* R W *)
(*R W to 1\[Sigma] in the case in which there is dependence in one parameter*)
RW1sigX::usage ="See later"

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
RW2sigX::usage ="See later"
RWone::usage ="See later"

(*TABLES FOR RWone*)
dataRWone1sig::usage ="See later"
dataRWone2sig::usage ="See later"

(*EXPORTING TABLES FOR RVone*)
TableRWone::usage ="See later"

(*RW to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
RW2sig::usage ="See later"
RW2sigWXYZ::usage ="See later"

(*RW to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
RW1sig::usage ="See later"
RW1sigWXYZ::usage ="See later"
RW::usage ="See later"

(*TABLES FOR RW*)
dataRW1sig::usage ="See later"
dataRW2sig::usage ="See later"

(*EXPORTING TABLES FOR RZ*)
TableRW::usage ="See later"

(* Individual process *)
(* R Gamma *)
(*R Gamma to 1\[Sigma] in the case in which there is dependence in one parameter*)
RGam1sigX::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in one parameter*)
RGam2sigX::usage ="See later"


(*TABLES FOR RGAMone*)
dataRGAMone1sig::usage ="See later"
dataRGAMone2sig::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
Rgam2sig::usage ="See later"
RGam2sigWXYZ::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
Rgam1sig::usage ="See later"
RGam1sigWXYZ::usage ="See later"
RGam::usage ="See later"


(*R gamma to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(* Individual process *)
(* R botton *)
(*************************************************************************************************************************************************************************************)
(*R bottom to 1\[Sigma] in the case in which there is dependence in one parameter*)
Rb1sigX::usage ="See later"

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
Rb2sigX::usage ="See later"

(*TABLES FOR RBOTone*)
dataRBOTone1sig::usage ="See later"
dataRBOTone2sig::usage ="See later"

(*EXPORTING TABLES FOR RBOTone*)
Rb::usage ="See later"

(* Individual process *)
(* R tau *)
(*R tau to 1\[Sigma] in the case in which there is dependence in one parameter*)
Rtau1sigX::usage ="See later"

(*R tau to 2\[Sigma] in the case in which there is dependence in one parameter*)
Rtau2sigX::usage ="See later"

(*TABLES FOR RTAUone*)
dataRTAUone1sig::usage ="See later"
dataRTAUone2sig::usage ="See later"

Rtau::usage ="See later"

Intersection2sigRXX::usage ="See later"
Intersection1sigRXX::usage ="See later"
(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PlotsRXX`Private`"]

RXALL1sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RbbINF1sig <= Rbotbot[ghtt, ghbb] <= RbbSUP1sig, 
 RtautauINF1sig <= Rtata[ghtt, ghbb,ghtautau] <= RtautauSUP1sig, 
   RwwINF1sig <= RWW[ghtt, ghbb, ghWW] <= RwwSUP1sig, 
     RzzINF1sig <= RZZ[ghtt, ghbb, ghZZ] <= RzzSUP1sig, 
       RgammagammaINF1sig <= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <= RgammagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, PlotLegends -> 
 Placed[{Style["\!\(\*SubscriptBox[\(\[Mu]\), \(bb\)]\)", Larger, 
    Bold], Style["\!\(\*SubscriptBox[\(\[Mu]\), \(\[Tau]\[Tau]\)]\)", 
    Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(\[Mu]\), \(WW\)]\)", Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(\[Mu]\), \(ZZ\)]\)", Larger, Bold], Style["\!\(\*SubscriptBox[\(\[Mu]\), \(\[Gamma]\[Gamma]\)]\)", Larger, Bold]}, {1, 
   0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.3]}, {Pink, Opacity[0.3]}, {Yellow, 
   Opacity[0.3]}, {Blue, Opacity[0.3]}, {Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];
   
RXALL2sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RbbINF2sig <= Rbotbot[ghtt, ghbb] <= RbbSUP2sig, 
 RtautauINF2sig <= Rtata[ghtt, ghbb,ghtautau] <= RtautauSUP2sig, 
   RwwINF2sig <= RWW[ghtt, ghbb, ghWW] <= RwwSUP2sig, 
     RzzINF2sig <= RZZ[ghtt, ghbb, ghZZ] <= RzzSUP2sig, 
       RgammagammaINF2sig <= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <= RgammagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, PlotLegends -> 
 Placed[{Style["\!\(\*SubscriptBox[\(\[Mu]\), \(bb\)]\)", Larger, 
    Bold], Style["\!\(\*SubscriptBox[\(\[Mu]\), \(\[Tau]\[Tau]\)]\)", 
    Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(\[Mu]\), \(WW\)]\)", Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(\[Mu]\), \(ZZ\)]\)", Larger, Bold], Style["\!\(\*SubscriptBox[\(\[Mu]\), \(\[Gamma]\[Gamma]\)]\)", Larger, Bold]}, {1, 
   0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.3]}, {Pink, Opacity[0.3]}, {Yellow, 
   Opacity[0.3]}, {Blue, Opacity[0.3]}, {Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RXALL[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:={
RXALL1sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
RXALL2sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP]
};

(*RXXALL[ghtt[ArcCos[Cab]+ArcTan[tb], 1.415,Cab,tb], ghbb[ArcCos[Cab]+ArcTan[tb], 1,Cab,tb],ghZZ[Sqrt[1-Cab^2]],ghWW[Sqrt[1-Cab^2]],
ghtautau[ArcCos[Cab]+ArcTan[tb], 1,Cab,tb],0.01,500,tb,Cab,0,10,0,1,Subscript[t, \[Beta]],Subscript[c, \[Alpha]\[Beta]]]*)

(* Individual process *)
(* R Z *)
(*************************************************************************************************************************************************************************************)
(*R Z to 1\[Sigma] in the case in which there is dependence in one parameter*)
RZ1sigX[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{RZZ[ghtt,ghbb,ghZZ],RzzSUP1sig,RzzINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(ZZ\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(ZZ\)]\)"}, 
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange,Opacity[0.1]}
];

(*RZ to 2\[Sigma] in the case in which there is dependence in one parameter*)
RZ2sigX[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{RZZ[ghtt,ghbb,ghZZ],RzzSUP2sig,RzzINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(ZZ\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(ZZ\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange,Opacity[0.1]}
];

RZone[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:={
RZ1sigX[ghtt,ghbb,ghZZ,x,xmin,xmax,xlabel],
RZ2sigX[ghtt,ghbb,ghZZ,x,xmin,xmax,xlabel]
};

(*TABLES FOR RVone*)
dataRZone1sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RzzINF1sig<=RZZ[ghtt,ghbb,ghZZ]<=RzzSUP1sig, RZZ[ghtt,ghbb,ghZZ],0]}, {x, xmin,xmax,xstep}];
dataRZone2sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RzzINF2sig<=RZZ[ghtt,ghbb,ghZZ]<=RzzSUP2sig, RZZ[ghtt,ghbb,ghZZ],0]}, {x, xmin,xmax,xstep}];

(*EXPORTING TABLES FOR RVone*)
TableRZone[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRZone_1sigma.txt"}],Re[dataRZone1sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRZone_2sigma.txt"}],Re[dataRZone2sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
};

(*RZ to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
RZ2sig[ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RzzINF2sig <= Re[RZZ[ghtt, ghbb, ghZZ]] <= RzzSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(Z\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RZ2sigWXYZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
RZ2sig[ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*RZ to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
RZ1sig[ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RzzINF1sig <= Re[RZZ[ghtt, ghbb, ghZZ]] <= RzzSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(Z\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RZ1sigWXYZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
RZ1sig[ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

RZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
RZ1sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
RZ2sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
};

(*TABLES FOR RZ*)
dataRZ1sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    RzzINF1sig<=RZZ[ghtt,ghbb,ghZZ]<=RzzSUP1sig, RZZ[ghtt,ghbb,ghZZ],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}];

dataRZ2sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    RzzINF2sig<=RZZ[ghtt,ghbb,ghZZ]<=RzzSUP2sig, RZZ[ghtt,ghbb,ghZZ],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}];

(*EXPORTING TABLES FOR RZ*)
TableRZ[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRZ_1sigma.txt"}],
Re[
dataRZ1sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]
]
/. {{_,_,0} -> Sequence[]},
"Table",
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRZ_2sigma.txt"}],
Re[
dataRZ2sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]
]
/. {{_,_,0} -> Sequence[]},
 "Table"
};

(* Individual process *)
(* R W *)
(*************************************************************************************************************************************************************************************)
(*R W to 1\[Sigma] in the case in which there is dependence in one parameter*)

RW1sigX[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{RWW[ghtt,ghbb,ghWW],RwwSUP1sig,RwwINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(WW\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(WW\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Orange],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Orange]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange,Opacity[0.1]}
];

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
RW2sigX[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{RWW[ghtt,ghbb,ghWW],RwwSUP2sig,RwwINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(WW\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(WW\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Orange],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Orange]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange,Opacity[0.1]}
];

RWone[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:={
RW1sigX[ghtt,ghbb,ghWW,x,xmin,xmax,xlabel],
RW2sigX[ghtt,ghbb,ghWW,x,xmin,xmax,xlabel]
};

(*TABLES FOR RWone*)
dataRWone1sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RwwINF1sig<=RWW[ghtt,ghbb,ghWW]<=RwwSUP1sig, RWW[ghtt,ghbb,ghWW],0]}, {x, xmin,xmax,xstep}];
dataRWone2sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RwwINF2sig<=RWW[ghtt,ghbb,ghWW]<=RwwSUP2sig, RWW[ghtt,ghbb,ghWW],0]}, {x, xmin,xmax,xstep}];

(*EXPORTING TABLES FOR RVone*)
TableRWone[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRWone_1sigma.txt"}],Re[dataRWone1sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRWone_2sigma.txt"}],Re[dataRWone2sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
};

(*RW to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
RW2sig[ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RwwINF2sig <= Re[RWW[ghtt, ghbb, ghWW]] <= RwwSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(W\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Green, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RW2sigWXYZ[
ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
RW2sig[ghtt, ghbb,ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*RW to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
RW1sig[ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RwwINF1sig <= Re[RWW[ghtt, ghbb, ghWW]] <= RwwSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(W\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Green, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RW1sigWXYZ[
ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
RW1sig[ghtt, ghbb,ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

RW[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
RW1sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
RW2sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
};

(*TABLES FOR RW*)
dataRW1sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    RwwINF1sig<=RWW[ghtt,ghbb,ghWW]<=RwwSUP1sig, RWW[ghtt,ghbb,ghWW],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}];

dataRW2sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    RwwINF2sig<=RWW[ghtt,ghbb,ghWW]<=RwwSUP2sig, RWW[ghtt,ghbb,ghWW],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}];

(*EXPORTING TABLES FOR RZ*)
TableRW[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRW_1sigma.txt"}],
Re[
dataRW1sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]
]
/. {{_,_,0} -> Sequence[]},
"Table"
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRW_2sigma.txt"}],
Re[
dataRW2sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]
]
/. {{_,_,0} -> Sequence[]},
 "Table"
};
(*************************************************************************************************************************************************************************************)

(* Individual process *)
(* R Gamma *)
(*R Gamma to 1\[Sigma] in the case in which there is dependence in one parameter*)
RGam1sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{ Rgaga[ghtt, ghbb, ghWW, gCH, mCH],RgammagammaSUP1sig,RgammagammaINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Yellow],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Yellow]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Yellow,Opacity[0.1]}
];

(*R gamma to 2\[Sigma] in the case in which there is dependence in one parameter*)
RGam2sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{ Rgaga[ghtt, ghbb, ghWW, gCH, mCH],RgammagammaSUP2sig,RgammagammaINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Yellow],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Yellow]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Yellow,Opacity[0.1]}
];

RGAMone[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:={
RGam1sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel],
RGam2sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel]
};

(*TABLES FOR RGAMone*)
dataRGAMone1sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RgammagammaINF1sig<=Rgaga[ghtt, ghbb, ghWW, gCH, mCH]<=RgammagammaSUP1sig,Rgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, {x, xmin,xmax,xstep}];
dataRGAMone2sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RgammagammaINF2sig<=Rgaga[ghtt, ghbb, ghWW, gCH, mCH]<=RgammagammaSUP2sig, Rgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, {x, xmin,xmax,xstep}];

(*EXPORTING TABLES FOR RGAMone*)
TableRGAMone[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRGAMone_1sigma.txt"}],Re[dataRGAMone1sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRGAMone_2sigma.txt"}],Re[dataRGAMone2sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
};

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
Rgam2sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RgammagammaINF2sig<= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <=RgammagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Purple, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP];

RGam2sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Rgam2sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
Rgam1sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RgammagammaINF1sig<= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <=RgammagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Gamma]\[Gamma]\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

RGam1sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Rgam1sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP], {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

RGam[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
RGam1sigWXYZ[
ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
RGam2sigWXYZ[
ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
};

(*R gamma to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(* Individual process *)
(* R botton *)
(*************************************************************************************************************************************************************************************)
(*R bottom to 1\[Sigma] in the case in which there is dependence in one parameter*)
Rb1sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{Rbotbot[ghtt,ghbb],RbbSUP1sig,RbbINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(bb\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(bb\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
];

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
Rb2sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{Rbotbot[ghtt,ghbb],RbbSUP2sig,RbbINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(bb\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(bb\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
];

RBOTone[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:={
Rb1sigX[ghtt,ghbb,x,xmin,xmax,xlabel],
Rb2sigX[ghtt,ghbb,x,xmin,xmax,xlabel]
};

(*TABLES FOR RBOTone*)
dataRBOTone1sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RbbINF1sig<=Rbotbot[ghtt,ghbb]<=RbbSUP1sig,Rbotbot[ghtt,ghbb],0]}, {x, xmin,xmax,xstep}];
dataRBOTone2sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RbbINF2sig<=Rbotbot[ghtt,ghbb]<=RbbSUP2sig,Rbotbot[ghtt,ghbb],0]}, {x, xmin,xmax,xstep}];

(*EXPORTING TABLES FOR RBOTone*)
TableRBOTone[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRBOTone_1sigma.txt"}],Re[dataRBOTone1sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRBOTone_2sigma.txt"}],Re[dataRBOTone2sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
};

(*\[Mu]bb*)
Rb[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RbbINF2sig <= Rbotbot[ghtt, ghbb] <= RbbSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(\[Mu]\), \(b \
\*OverscriptBox[\(b\), \(_\)]\)]\)"},
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP];

(*Rbb[ghtt,ghbb,Ztt,Zbb,0,1,0,1,Subscript[Overscript[Z, ~], tt],Subscript[Overscript[Z, ~], bb]]*)

(*************************************************************************************************************************************************************************************)

(*ghtautau[tb_, cab_] := 
 Sin[ArcCos[cab] + ArcTan[tb]]*(
  mtau*Sqrt[1 - Sin[ArcCos[cab] + ArcTan[tb]]^2])/(
  vev*(cab - (Sin[ArcCos[cab] + ArcTan[tb]]*Sin[ArcTan[tb]])))

ghtt[tb_, cab_] := 
 Sin[ArcCos[cab] + ArcTan[tb]]*(
  mt*Sqrt[1 - Sin[ArcCos[cab] + ArcTan[tb]]^2])/(
  vev*(cab - (Sin[ArcCos[cab] + ArcTan[tb]]*Sin[ArcTan[tb]])))
ghbb[tb_, cab_] := 
 Sin[ArcCos[cab] + ArcTan[tb]]*(
  mb*Sqrt[1 - Sin[ArcCos[cab] + ArcTan[tb]]^2])/(
  vev*(cab - (Sin[ArcCos[cab] + ArcTan[tb]]*Sin[ArcTan[tb]])))*)
  
(* Individual process *)
(* R tau *)
(*R tau to 1\[Sigma] in the case in which there is dependence in one parameter*)
Rtau1sigX[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{Rtata[ghtt,ghbb,ghtautau],RtautauSUP1sig,RtautauINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Tau]\[Tau]\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(\[Tau]\[Tau]\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Green],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Green]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Green,Opacity[0.1]}
];

(*R tau to 2\[Sigma] in the case in which there is dependence in one parameter*)
Rtau2sigX[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{Rtata[ghtt,ghbb,ghtautau],RtautauSUP2sig,RtautauINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(R\), \(\[Tau]\[Tau]\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(R\), \(\[Tau]\[Tau]\)]\)"},
FrameStyle->Thickness[0.003],AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Green],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Green]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Green,Opacity[0.1]}
];

RTAUone[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:={
Rtau1sigX[ghtt,ghbb,ghtautau,x,xmin,xmax,xlabel],
Rtau2sigX[ghtt,ghbb,ghtautau,x,xmin,xmax,xlabel]
};

(*TABLES FOR RTAUone*)
dataRTAUone1sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RtautauINF1sig<=Rtata[ghtt,ghbb,ghtautau]<=RtautauSUP1sig,Rtata[ghtt,ghbb,ghtautau],0]}, {x, xmin,xmax,xstep}];
dataRTAUone2sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[RtautauINF2sig<=Rtata[ghtt,ghbb,ghtautau]<=RtautauSUP2sig,Rtata[ghtt,ghbb,ghtautau],0]}, {x, xmin,xmax,xstep}];

(*EXPORTING TABLES FOR RTAUone*)
TableRTAUone[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRTAUone_1sigma.txt"}],Re[dataRTAUone1sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableRTAUone_2sigma.txt"}],Re[dataRTAUone2sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
};

(*\[Mu]\[Tau]\[Tau]*)
Rtau[ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{RtautauINF2sig <= Rtata[ghtt, ghbb,ghtautau] <= RtautauSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(\[Mu]\), \(\[Tau]\[Tau]\)]\)"}, 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],PlotPoints->PP];

(*Rtau[ghtt[tb, cab], ghbb[tb, cab],ghtautau[tb, cab],cab,tb,-1,1,0.1,50,cab,tb]*)

(*R\[Tau]\[Tau][ghtt,ghbb,gh\[Tau]\[Tau],Ztt,Zbb,0,1,0,1,Subscript[Z, tt],Subscript[Z, bb]]*)

Intersection2sigRXX[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{RbbINF2sig <= Rbotbot[ghtt, ghbb] <= RbbSUP2sig&& 
    RtautauINF2sig <= Rtata[ghtt, ghbb,ghtautau] <= RtautauSUP2sig&& 
   RwwINF2sig <= RWW[ghtt, ghbb, ghWW] <= RwwSUP2sig&& 
    RzzINF2sig <= RZZ[ghtt, ghbb, ghZZ] <= RzzSUP2sig&& 
  RgammagammaINF2sig <= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <= RgammagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Larger, Bold]}, PlotLegends -> 
 Placed[{Style["Intersection", Larger, 
    Bold]}, {0.8, 0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800(*,PlotPoints->100000*),
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP
];

Intersection1sigRXX[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{RbbINF1sig <= Rbotbot[ghtt, ghbb] <= RbbSUP1sig&& 
    RtautauINF1sig <= Rtata[ghtt, ghbb,ghtautau] <= RtautauSUP1sig&& 
   RwwINF1sig <= RWW[ghtt, ghbb, ghWW] <= RwwSUP1sig&& 
    RzzINF1sig <= RZZ[ghtt, ghbb, ghZZ] <= RzzSUP1sig&& 
  RgammagammaINF1sig <= Rgaga[ghtt, ghbb, ghWW, gCH, mCH] <= RgammagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)", Larger, Bold]}, PlotLegends -> 
 Placed[{Style["Intersection", Larger, 
    Bold]}, {0.8, 0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800(*,PlotPoints->100*),
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP
];

End[]