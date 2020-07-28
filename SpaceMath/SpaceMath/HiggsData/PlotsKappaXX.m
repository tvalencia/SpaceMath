(* Wolfram Language Package *)

(******************************************************************************************************************************************************)
(*********************************************************Begin KV************************************************************************************)
(******************************************************************************************************************************************************)
KVone::usage = "\!\(\*
StyleBox[\"KVone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"xlabel_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule]  \
This command evaluates \!\(\*SubscriptBox[\(K\), \(V\)]\) with V=W, \
Z when there is dependence only on one parameter. The arguments ghtt, ghbb and ghVV are the htt, hbb, hVV \
couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKVone::usage="\!\(\*
StyleBox[\"TableKVone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(V\)]\), with V= W, Z. The arguments ghtt, ghbb and ghVV are the htt, hbb, hVV \
couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. The output file will be saved in $UserDocumentsDirectory."

KV::usage="KV[ghtt_,ghbb_,ghVV_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(V\)]\) with V=W, \
Z when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghVV are the htt, hbb, hVV \
couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKV::usage="TableKV[ghtt_,ghbb_,ghVV_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(V\)]\), with V= W, Z gauge bosons. The arguments ghtt, ghbb and ghVV are the htt, \
hbb and hVV couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax). The output file will be saved in $UserDocumentsDirectory."
(******************************************************************************************************************************************************)
(*********************************************************END OF KV************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF KGAMMA************************************************************************************)
(******************************************************************************************************************************************************)
KGAMone::usage = "\!\(\*
StyleBox[\"KGAMone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\) when there is dependence only on one parameter. The arguments ghtt, ghbb, ghWW and gCH are the htt, hbb, hWW and h\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) \
couplings. Here, h represents to SM-like Higgs boson, t and b the top and bottom quarks, \!\(\*SuperscriptBox[\(H\), \(-\)]\) the charged scalar boson. Labels mCH and x \
indicate the charged scalar mass and the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKGAMone::usage="\!\(\*
StyleBox[\"TableKGAMone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW, gCH and mCH are the htt \
, hbb, hWW, \!\(\*SuperscriptBox[\(hH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings, while mCH is the charged scalar boson mass that could to contributes to the process. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. The output file will be saved in $UserDocumentsDirectory."

KGAM::usage="KGAM[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\) \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghWW and gCH are the htt, hbb, hWW and h\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\)\
couplings while mCH stands for the charged scalar boson mass. Here, h represents to SM-like Higgs boson while t and b the top and bottom querkas. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKGAM::usage="TableKGAM[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW and gCH are the htt, \
hbb, hWW and h-\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax). The output file will be saved in $UserDocumentsDirectory."
(******************************************************************************************************************************************************)
(*********************************************************END OF KGAMMA************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF KTAU************************************************************************************)
(******************************************************************************************************************************************************)
KTAUone::usage = "\!\(\*
StyleBox[\"KTAUone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)\ when there is dependence only on one parameter. The arguments ghtt, ghbb and ghtautau are the htt,\ hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks and tau stands for tau-lepton. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKTAUone::usage="\!\(\*
StyleBox[\"TableKTAUone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\). The arguments ghtt, ghbb and ghtautau are the htt, \
hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks and tau stands for tau-lepton. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. The output file will be saved in $UserDocumentsDirectory."

KTAU::usage="KTAU[ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\), \
when there is dependence on two or more parameters. The arguments ghtt, ghbb and ghtautau are the htt, hbb \
and htautau couplings. Here, h represents to SM-like Higgs boson while t, b and tau are the top and bottom quarks and the tau-lepton. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKTAU::usage="TableKTAU[ghtt_, ghbb_,ghtautau_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\). The arguments ghtt, ghbb and ghtautau are the htt, \
hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t, b and tau are the top and bottom quarks and the tau-lepton. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax). The output file will be saved in $UserDocumentsDirectory."
(******************************************************************************************************************************************************)
(*********************************************************End OF KTAU************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF KBOTTOM************************************************************************************)
(******************************************************************************************************************************************************)
KBOTone::usage = "\!\(\*
StyleBox[\"KBOTone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
This command evaluates \!\(\*SubscriptBox[\(K\), \(b\)]\)\ when there is dependence only on one parameter. The arguments ghtt, ghbb are the htt \
and hbb couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKBOTone::usage="\!\(\*
StyleBox[\"TableKBOTone\",\nFontWeight->\"Bold\"]\)\!\(\*
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(b\)]\). The arguments ghtt are ghbb are the htt \
and hbb couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. The output file will be saved in $UserDocumentsDirectory."

Kb::usage="Kb[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(b\)]\), \
when there is dependence on two or more parameters. The arguments ghtt and ghbb are the htt, hbb \
couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKb::usage="TableKb[ghtt_, ghbb_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(b\)]\). The arguments ghtt, ghbb are the htt, \
hbb couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax). The output file will be saved in $UserDocumentsDirectory."
(******************************************************************************************************************************************************)
(*********************************************************End OF KBOTTOM************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF KXALL************************************************************************************)
(*****************************************************************************************************************************************************)

KXALL::usage="KXALL[ghtt_, ghbb_, ghZZ_, ghWW_, ghtautau_, gCH_, mCH_, x_, y_, xmin_, xmax_, ymin_, ymax_, xlabel_, ylabel_, xfor_, yfor_, xformin_, xformax_, xforstep_, yformin_, yformax_, yforstep_, PP_][[i]]\[Rule] This command evaluates all signal strengths, \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghZZ, ghWW, ghtautau and gCH are the htt, hbb, \
hZZ, hWW, htautau and \!\(\*SuperscriptBox[\(hH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks; V=Z, W are the gauge bosons, tau is the tau lepton and \!\(\*SuperscriptBox[\(H\), \(-\)]\) is a charged scalar boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively; mCH stands for the charged scalar boson. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

(******************************************************************************************************************************************************)
(*********************************************************End OF KXALL************************************************************************************)
(*****************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************End OF KXIntersection************************************************************************************)
(*****************************************************************************************************************************************************)
KXintersection::usage="KXintersection[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates the intersection of all signal strengths, \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghZZ, ghWW, ghtautau and gCH are the htt, hbb, \
hZZ, hWW, htautau and \!\(\*SuperscriptBox[\(hH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks; V=Z, W are the gauge bosons, tau is the tau lepton and \!\(\*SuperscriptBox[\(H\), \(-\)]\) is a charged scalar boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively; mCH stands for the charged scalar boson. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."
(******************************************************************************************************************************************************)
(*********************************************************End OF KXIntersection************************************************************************************)
(*****************************************************************************************************************************************************)

(*********************************************************************************************
*******************************All signal strenghts*************************************************************
*********************************************************************************************)
KXALL1sig::usage ="See later"

KXALL2sig::usage ="See later"

KZ1sigX::usage ="See later"

KZ2sigX::usage ="See later"

KZone::usage ="See later"

(*TABLES FOR KVone*)
dataKZone1sig::usage ="See later"
dataKZone2sig::usage ="See later"

(*EXPORTING TABLES FOR KVone*)
TableKZone::usage ="See later"

(*KZ to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
KZ2sig::usage ="See later"
KZ2sigWXYZ::usage ="See later"

(*KZ to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]ZZ*)
KZ1sig::usage ="See later"
KZ1sigWXYZ::usage ="See later"
KZ::usage ="See later"

(*TABLES FOR KZ*)
dataKZ1sig::usage ="See later"
dataKZ2sig::usage ="See later"

(*EXPORTING TABLES FOR KZ*)
TableKZ::usage ="See later"

(* Individual process *)
(* R W *)
(*R W to 1\[Sigma] in the case in which there is dependence in one parameter*)
KW1sigX::usage ="See later"

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
KW2sigX::usage ="See later"
KWone::usage ="See later"

(*TABLES FOR KWone*)
dataKWone1sig::usage ="See later"
dataKWone2sig::usage ="See later"

(*EXPORTING TABLES FOR KVone*)
TableKWone::usage ="See later"

(*KW to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
KW2sig::usage ="See later"
KW2sigWXYZ::usage ="See later"

(*KW to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]WW*)
KW1sig::usage ="See later"
KW1sigWXYZ::usage ="See later"
KW::usage ="See later"

(*TABLES FOR KW*)
dataKW1sig::usage ="See later"
dataKW2sig::usage ="See later"

(*EXPORTING TABLES FOR KZ*)
TableKW::usage ="See later"

(* Individual process *)
(* R Gamma *)
(*R Gamma to 1\[Sigma] in the case in which there is dependence in one parameter*)
KGAM1sigX::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in one parameter*)
KGAM2sigX::usage ="See later"


(*TABLES FOR KGAMone*)
dataKGAMone1sig::usage ="See later"
dataKGAMone2sig::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
KGAM2sig::usage ="See later"
KGAM2sigWXYZ::usage ="See later"

(*R gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)
(*\[Mu]\[Gamma]\[Gamma]*)
KGAM1sig::usage ="See later"
KGAM1sigWXYZ::usage ="See later"

(*R gamma to 1\[Sigma] in the case in which there is dependence in more than two parameters*)
(* Individual process *)
(* R botton *)
(*************************************************************************************************************************************************************************************)
(*R bottom to 1\[Sigma] in the case in which there is dependence in one parameter*)
Kb1sigX::usage ="See later"

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)
Kb2sigX::usage ="See later"

(*TABLES FOR KBOTone*)
dataKBOTone1sig::usage ="See later"
dataKBOTone2sig::usage ="See later"

(* Individual process *)
(* R tau *)
(*R tau to 1\[Sigma] in the case in which there is dependence in one parameter*)
KTAU1sigX::usage ="See later"

(*R tau to 2\[Sigma] in the case in which there is dependence in one parameter*)
KTAU2sigX::usage ="See later"

(*TABLES FOR KTAUone*)
dataKTAUone1sig::usage ="See later"
dataKTAUone2sig::usage ="See later"

Intersection2sigKXX::usage ="See later"
Intersection1sigKXX::usage ="See later"
(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PlotsKXX`Private`"]

(*********************************************************************************************
*******************************All signal strenghts*************************************************************
*********************************************************************************************)

KXALL1sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KbbINF1sig <= KBOTbot[ghtt, ghbb] <= KbbSUP1sig, 
 KTAUtauINF1sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP1sig, 
   KWwINF1sig <= KWW[ghtt, ghbb, ghWW] <= KWwSUP1sig, 
     KZzINF1sig <= KZZ[ghtt, ghbb, ghZZ] <= KZzSUP1sig, 
       KGAMmagammaINF1sig <= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= KGAMmagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, PlotLegends -> 
 Placed[{Style["\!\(\*SubscriptBox[\(K\), \(b\)]\)", Larger, 
    Bold], Style["\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)", 
    Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(K\), \(W\)]\)", Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(K\), \(Z\)]\)", Larger, Bold], Style["\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)", Larger, Bold]}, {1, 
   0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.3]}, {Pink, Opacity[0.3]}, {Yellow, 
   Opacity[0.3]}, {Blue, Opacity[0.3]}, {Orange, Opacity[0.3]}},PlotPoints->PP]


KXALL1[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KXALL1sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KXALL2sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KbbINF2sig <= KBOTbot[ghtt, ghbb] <= KbbSUP2sig, 
 KTAUtauINF2sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP2sig, 
   KWwINF2sig <= KWW[ghtt, ghbb, ghWW] <= KWwSUP2sig, 
     KZzINF2sig <= KZZ[ghtt, ghbb, ghZZ] <= KZzSUP2sig, 
       KGAMmagammaINF2sig <= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= KGAMmagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, PlotLegends -> 
 Placed[{Style["\!\(\*SubscriptBox[\(K\), \(b\)]\)", Larger, 
    Bold], Style["\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)", 
    Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(K\), \(W\)]\)", Larger, Bold], 
   Style["\!\(\*SubscriptBox[\(K\), \(Z\)]\)", Larger, Bold], Style["\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)", Larger, Bold]}, {1, 
   0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.3]}, {Pink, Opacity[0.3]}, {Yellow, 
   Opacity[0.3]}, {Blue, Opacity[0.3]}, {Orange, Opacity[0.3]}},PlotPoints->PP]


KXALL2[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KXALL2sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KXALL[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:={
KXALL1[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KXALL2[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*KXXALL[ghtt[ArcCos[Cab]+ArcTan[tb], 1.415,Cab,tb], ghbb[ArcCos[Cab]+ArcTan[tb], 1,Cab,tb],ghZZ[Sqrt[1-Cab^2]],ghWW[Sqrt[1-Cab^2]],
ghtautau[ArcCos[Cab]+ArcTan[tb], 1,Cab,tb],0.01,500,tb,Cab,0,10,0,1,Subscript[t, \[Beta]],Subscript[c, \[Alpha]\[Beta]]]*)

Individual process;
K Z

(*************************************************************************************************************************************************************************************)

(*R Z to 1\[Sigma] in the case in which there is dependence in one parameter*)

KZ1sigX[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{KZZ[ghtt,ghbb,ghZZ],KZzSUP1sig,KZzINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(Z\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(Z\)]\)"}, 
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Red],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Red]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange}
]

(*KZ to 2\[Sigma] in the case in which there is dependence in one parameter*)

KZ2sigX[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:=Plot[{KZZ[ghtt,ghbb,ghZZ],KZzSUP2sig,KZzINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(Z\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(Z\)]\)"}, 
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Red],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Red]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Orange}
]

KZone[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xlabel_]:={
KZ1sigX[ghtt,ghbb,ghZZ,x,xmin,xmax,xlabel],
KZ2sigX[ghtt,ghbb,ghZZ,x,xmin,xmax,xlabel]
}

(*TABLES FOR KVone*)

dataKZone1sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KZzINF1sig<=KZZ[ghtt,ghbb,ghZZ]<=KZzSUP1sig, KZZ[ghtt,ghbb,ghZZ],0]}, {x, xmin,xmax,xstep}]

dataKZone2sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KZzINF2sig<=KZZ[ghtt,ghbb,ghZZ]<=KZzSUP2sig, KZZ[ghtt,ghbb,ghZZ],0]}, {x, xmin,xmax,xstep}]

(*EXPORTING TABLES FOR KVone*)

TableKZone[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZone_1sigma.txt"}],Re[dataKZone1sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZone_2sigma.txt"}],Re[dataKZone2sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*KZ to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*KZZ*)
KZ2sig[ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KZzINF2sig <= Re[KZZ[ghtt, ghbb, ghZZ]] <= KZzSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(Z\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP]

KZ2sigWXYZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KZ2sig[ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KZ to 1\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]ZZ*)
KZ1sig[ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KZzINF1sig <= Re[KZZ[ghtt, ghbb, ghZZ]] <= KZzSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(Z\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP]

KZ1sigWXYZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KZ1sig[ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KZ[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KZ1sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KZ2sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*TABLES FOR KZ*)

dataKZ1sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KZzINF1sig<=KZZ[ghtt,ghbb,ghZZ]<=KZzSUP1sig, KZZ[ghtt,ghbb,ghZZ],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKZ2sig[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    KZzINF2sig<=KZZ[ghtt,ghbb,ghZZ]<=KZzSUP2sig, KZZ[ghtt,ghbb,ghZZ],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR KZ*)

TableKZ[ghtt_,ghbb_,ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZ_1sigma.txt"}],
Re[
dataKZ1sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep,y,ymin,ymax,ystep]]/. {{_,_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKZ_2sigma.txt"}],
Re[
dataKZ2sig[ghtt,ghbb,ghZZ,x,xmin,xmax,xstep,y,ymin,ymax,ystep]]/. {{_,_,0} -> Sequence[]},
"Table"
]
}

Individual process;
K W

(*************************************************************************************************************************************************************************************)

(*R W to 1\[Sigma] in the case in which there is dependence in one parameter*)

KW1sigX[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{KWW[ghtt,ghbb,ghWW],KWwSUP1sig,KWwINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(W\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(W\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple,Opacity[0.5]],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple,Opacity[0.5]]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Pink}
]

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)

KW2sigX[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{KWW[ghtt,ghbb,ghWW],KWwSUP2sig,KWwINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(W\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(W\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple,Opacity[0.5]],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple,Opacity[0.5]]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Pink}
]

KWone[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xlabel_]:={
KW1sigX[ghtt,ghbb,ghWW,x,xmin,xmax,xlabel],
KW2sigX[ghtt,ghbb,ghWW,x,xmin,xmax,xlabel]
}

(*TABLES FOR KWone*)

dataKWone1sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KWwINF1sig<=KWW[ghtt,ghbb,ghWW]<=KWwSUP1sig, KWW[ghtt,ghbb,ghWW],0]}, {x, xmin,xmax,xstep}]

dataKWone2sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KWwINF2sig<=KWW[ghtt,ghbb,ghWW]<=KWwSUP2sig, KWW[ghtt,ghbb,ghWW],0]}, {x, xmin,xmax,xstep}]

(*EXPORTING TABLES FOR KVone*)

TableKWone[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKWone_1sigma.txt"}],Re[dataKWone1sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKWone_2sigma.txt"}],Re[dataKWone2sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*KW to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]WW*)
KW2sig[ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KWwINF2sig <= Re[KWW[ghtt, ghbb, ghWW]] <= KWwSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(W\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Black, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Red, Opacity[0.8]}}, AspectRatio -> 0.6,PlotPoints->PP]

KW2sigWXYZ[
ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KW2sig[ghtt, ghbb,ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KW to 1\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]WW*)
KW1sig[ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KWwINF1sig <= Re[KWW[ghtt, ghbb, ghWW]] <= KWwSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(W\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Black, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Red, Opacity[0.8]}}, AspectRatio -> 0.6,PlotPoints->PP]

KW1sigWXYZ[
ghtt_, ghbb_,ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KW1sig[ghtt, ghbb,ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KW[
ghtt_, ghbb_,ghZZ_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KW1sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KW2sigWXYZ[
ghtt, ghbb,ghZZ,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*TABLES FOR KW*)

dataKW1sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KWwINF1sig<=KWW[ghtt,ghbb,ghWW]<=KWwSUP1sig, KWW[ghtt,ghbb,ghWW],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKW2sig[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    KWwINF2sig<=KWW[ghtt,ghbb,ghWW]<=KWwSUP2sig, KWW[ghtt,ghbb,ghWW],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR KW*)

TableKW[ghtt_,ghbb_,ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKW_1sigma.txt"}],
Re[
dataKW1sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep,y,ymin,ymax,ystep]]/. {{_,_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKW_2sigma.txt"}],
Re[
dataKW2sig[ghtt,ghbb,ghWW,x,xmin,xmax,xstep,y,ymin,ymax,ystep]]/. {{_,_,0} -> Sequence[]},
"Table"
]
}

(*************************************************************************************************************************************************************************************)

Individual process;
K Gamma

(*R Gamma to 1\[Sigma] in the case in which there is dependence in one parameter*)

KGAM1sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{ Kgaga[ghtt, ghbb, ghWW, gCH, mCH],KGAMmagammaSUP1sig,KGAMmagammaINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Cyan],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Cyan]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue}
]

(*K gamma to 2\[Sigma] in the case in which there is dependence on one parameter*)

KGAM2sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{ Kgaga[ghtt, ghbb, ghWW, gCH, mCH],KGAMmagammaSUP2sig,KGAMmagammaINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Cyan],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Cyan]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue}
]

KGAMone[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:={
KGAM1sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel],
KGAM2sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel]
}

(*TABLES FOR KGAMone*)

dataKGAMone1sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KGAMmagammaINF1sig<=Kgaga[ghtt, ghbb, ghWW, gCH, mCH]<=KGAMmagammaSUP1sig, 
Kgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, {x, xmin,xmax,xstep}]

dataKGAMone2sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KGAMmagammaINF2sig<=Kgaga[ghtt, ghbb, ghWW, gCH, mCH]<=KGAMmagammaSUP2sig, 
Kgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, {x, xmin,xmax,xstep}]


(*EXPORTING TABLES FOR KGAMone*)

TableKGAMone[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGAMone_1sigma.txt"}],Re[dataKGAMone1sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGAMone_2sigma.txt"}],Re[dataKGAMone2sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*K gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]\[Gamma]\[Gamma]*)
KGAM2sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KGAMmagammaINF2sig<= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <=KGAMmagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Purple, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP]

KGAM2sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KGAM2sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*K gamma to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]\[Gamma]\[Gamma]*)
KGAM1sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KGAMmagammaINF1sig<= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <=KGAMmagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
  ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\)"}, {1,0.6}],
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Purple, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP]

KGAM1sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KGAM1sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KGAM[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KGAM1sigWXYZ[
ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KGAM2sigWXYZ[
ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*TABLES FOR KW*)

dataKGAM1sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KGAMmagammaINF1sig<= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <=KGAMmagammaSUP1sig,Kgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKGAM2sig[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KGAMmagammaINF2sig<= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <=KGAMmagammaSUP2sig,Kgaga[ghtt, ghbb, ghWW, gCH, mCH],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR KGAM*)

TableKGAM[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGAM_1sigma.txt"}],
Re[
dataKGAM1sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKGAM_2sigma.txt"}],
Re[
dataKGAM2sig[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},"Table"
]
}

(*************************************************************************************************************************************************************************************)

Individual process;
K botton

(*************************************************************************************************************************************************************************************)

(*K bottom to 1\[Sigma] in the case in which there is dependence in one parameter*)

Kb1sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{KBOTbot[ghtt,ghbb],KbbSUP1sig,KbbINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(b\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(b\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)

Kb2sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{KBOTbot[ghtt,ghbb],KbbSUP2sig,KbbINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(b\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(b\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]


KBOTone[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:={
Kb1sigX[ghtt,ghbb,x,xmin,xmax,xlabel],
Kb2sigX[ghtt,ghbb,x,xmin,xmax,xlabel]
}

(*TABLES FOR KBOTone*)

dataKBOTone1sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KbbINF1sig<=KBOTbot[ghtt,ghbb]<=KbbSUP1sig, 
KBOTbot[ghtt,ghbb],0]}, {x, xmin,xmax,xstep}]

dataKBOTone2sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KbbINF2sig<=KBOTbot[ghtt,ghbb]<=KbbSUP2sig,
 KBOTbot[ghtt,ghbb],0]}, {x, xmin,xmax,xstep}]


(*EXPORTING TABLES FOR KBOTone*)

TableKBOTone[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKBOTone_1sigma.txt"}],Re[dataKBOTone1sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKBOTone_2sigma.txt"}],Re[dataKBOTone2sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*************************************************************************************************************************************************************************************)

(*Kb to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*\[Mu]bb*)
Kb2sig[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KbbINF2sig <= KBOTbot[ghtt, ghbb] <= KbbSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(K\), \(b\)]\)"},
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[1]}}, AspectRatio -> 0.6,PlotPoints->PP]

Kb2sigWXYZ[
ghtt_,ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Kb2sig[ghtt,ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*Kb to 1\[Sigma] in the case in which there is dependence in more than two parameters*)

(*Kb*)
Kb1sig[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KbbINF1sig <= KBOTbot[ghtt, ghbb] <= KbbSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(K\), \(b\)]\)"},
 AxesLabel -> {Style["x", Larger, Bold], 
 Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
 GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
 Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[1]}}, AspectRatio -> 0.6,PlotPoints->PP]

Kb1sigWXYZ[
ghtt_,ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Kb1sig[ghtt,ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

Kb[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
Kb1sigWXYZ[
ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
Kb2sigWXYZ[
ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*TABLES FOR Kb*)

dataKb1sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KbbINF1sig <= KBOTbot[ghtt, ghbb] <= KbbSUP1sig, KBOTbot[ghtt, ghbb],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKb2sig[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KbbINF2sig <= KBOTbot[ghtt, ghbb] <= KbbSUP2sig, KBOTbot[ghtt, ghbb],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR Kb*)

TableKBOTone[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKBOTone_1sigma.txt"}],
Re[dataKBOTone1sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKBOTone_2sigma.txt"}],Re[dataKBOTone2sig[ghtt,ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

TableKb[ghtt_,ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKb_1sigma.txt"}],
Re[
dataKb1sig[ghtt,ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKb_2sigma.txt"}],
Re[
dataKb2sig[ghtt,ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
}

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

Individual process;
K tau

(*K tau to 1\[Sigma] in the case in which there is dependence in one parameter*)

KTAU1sigX[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{Ktata[ghtt,ghbb,ghtautau],KTAUtauSUP1sig,KTAUtauINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Green],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Green]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Yellow}
]

(*K tau to 2\[Sigma] in the case in which there is dependence on one parameter*)

KTAU2sigX[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{Ktata[ghtt,ghbb,ghtautau],KTAUtauSUP2sig,KTAUtauINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Green],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Green]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Yellow}
]

KTAUone[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xlabel_]:={
KTAU1sigX[ghtt,ghbb,ghtautau,x,xmin,xmax,xlabel],
KTAU2sigX[ghtt,ghbb,ghtautau,x,xmin,xmax,xlabel]
}


(*TABLES FOR KTAUone*)

dataKTAUone1sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KTAUtauINF1sig<=Ktata[ghtt,ghbb,ghtautau]<=KTAUtauSUP1sig, 
Ktata[ghtt,ghbb,ghtautau],0]}, {x, xmin,xmax,xstep}]

dataKTAUone2sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    KTAUtauINF2sig<=Ktata[ghtt,ghbb,ghtautau]<=KTAUtauSUP2sig, 
Ktata[ghtt,ghbb,ghtautau],0]}, {x, xmin,xmax,xstep}]

(*EXPORTING TABLES FOR KTAUone*)

TableKTAUone[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKTAUone_1sigma.txt"}],Re[dataKTAUone1sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKTAUone_2sigma.txt"}],Re[dataKTAUone2sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*KTAU to 2\[Sigma] in the case in which there is dependence in more than two parameters*)

(*KTAU*)
KTAU2sig[ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KTAUtauINF2sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)"}, 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Magenta, Dashed, Thickness[0.003]]}, PlotStyle -> {{Black, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP]

KTAU2sigWXYZ[
ghtt_,ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KTAU2sig[ghtt,ghbb,ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KTAU to 1\[Sigma] in the case in which there is dependence in more than two parameters*)

(*KTAU*)
KTAU1sig[ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{KTAUtauINF1sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(K\), \(\[Tau]\)]\)"}, 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],
BoundaryStyle -> {1 -> Directive[Magenta, Dashed, Thickness[0.003]]},
 PlotStyle -> {{Black, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP]

KTAU1sigWXYZ[
ghtt_,ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KTAU1sig[ghtt,ghbb,ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KTAU[
ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
KTAU1sigWXYZ[
ghtt, ghbb,ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
KTAU2sigWXYZ[
ghtt, ghbb,ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*TABLES FOR KTAU*)

dataKTAU1sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KTAUtauINF1sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP1sig, Ktata[ghtt, ghbb,ghtautau],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKTAU2sig[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    KTAUtauINF1sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP1sig, Ktata[ghtt, ghbb,ghtautau],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR KTAU*)

TableKTAU[ghtt_,ghbb_,ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKTAU_1sigma.txt"}],
Re[
dataKTAU1sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKTAU_2sigma.txt"}],
Re[
dataKTAU1sig[ghtt,ghbb,ghtautau,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
 "Table"
]
}

(*********************************************************************************************************************)
(*********************************************INTERSECTION*********************************************************)
(*********************************************************************************************************************)

Intersection2sigKXX[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{KbbINF2sig <= KBOTbot[ghtt, ghbb] <= KbbSUP2sig&& 
    KTAUtauINF2sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP2sig&& 
   KWwINF2sig <= KWW[ghtt, ghbb, ghWW] <= KWwSUP2sig&& 
    KZzINF2sig <= KZZ[ghtt, ghbb, ghZZ] <= KZzSUP2sig&& 
  KGAMmagammaINF2sig <= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= KGAMmagammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, PlotLegends -> 
 Placed[{Style["Intersection", Larger, 
    Bold]}, {0.8, 0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600(*,PlotPoints->100000*),
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Black, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Blue, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP
]

Inter2sigma[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Intersection2sigKXX[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

Intersection1sigKXX[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{KbbINF1sig <= KBOTbot[ghtt, ghbb] <= KbbSUP1sig&& 
    KTAUtauINF1sig <= Ktata[ghtt, ghbb,ghtautau] <= KTAUtauSUP1sig&& 
   KWwINF1sig <= KWW[ghtt, ghbb, ghWW] <= KWwSUP1sig&& 
    KZzINF1sig <= KZZ[ghtt, ghbb, ghZZ] <= KZzSUP1sig&& 
  KGAMmagammaINF1sig <= Kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= KGAMmagammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, PlotLegends -> 
 Placed[{Style["Intersection", Larger, 
    Bold]}, {0.8, 0.6}], 
 AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600(*,PlotPoints->100000*),
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Black, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Blue, Opacity[0.5]}}, AspectRatio -> 0.6,PlotPoints->PP
]

Inter1sigma[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Intersection1sigKXX[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

KXintersection[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
Inter1sigma[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
,
Inter2sigma[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

End[]