(* Definitions; *)

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
(*********************************************************Begin OF RGamma************************************************************************************)
(******************************************************************************************************************************************************)
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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\) when there is dependence only on one parameter. The arguments ghtt, ghbb, ghWW and gCH are the htt, hbb, hWW and h\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) \
couplings. Here, h represents to SM-like Higgs boson, t and b the top and bottom quarks, \!\(\*SuperscriptBox[\(H\), \(-\)]\) the charged scalar boson. Labels mCH and x \
indicate the charged scalar mass and the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW, gCH and mCH are the htt \
, hbb, hWW, \!\(\*SuperscriptBox[\(hH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings, while mCH is the charged scalar boson mass that could to contributes to the process. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax."

RGam::usage="RGam[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\) \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghWW and gCH are the htt, hbb, hWW and h\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\)\
couplings while mCH stands for the charged scalar boson mass. Here, h represents to SM-like Higgs boson while t and b the top and bottom querkas. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableRGam::usage="TableRGam[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW and gCH are the htt, \
hbb, hWW and h-\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************END OF RGamma************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF KGamma************************************************************************************)
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
to indicates the steps from xmin to xmax."

KGam::usage="KGam[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\) \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghWW and gCH are the htt, hbb, hWW and h\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\)\
couplings while mCH stands for the charged scalar boson mass. Here, h represents to SM-like Higgs boson while t and b the top and bottom querkas. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKGam::usage="TableRGam[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(K\), \(\[Gamma]\)]\). The arguments ghtt, ghbb, ghWW and gCH are the htt, \
hbb, hWW and h-\!\(\*SuperscriptBox[\(H\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b the top and bottom quarks. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************END OF KGamma************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF RTau************************************************************************************)
(******************************************************************************************************************************************************)
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
This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\)\ when there is dependence only on one parameter. The arguments ghtt, ghbb and ghtautau are the htt,\ hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks and tau stands for tau-lepton. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

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
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\). The arguments ghtt, ghbb and ghtautau are the htt, \
hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks and tau stands for tau-lepton. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. "

Rtau::usage="Rtau[ghtt_, ghbb_,ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\), \
when there is dependence on two or more parameters. The arguments ghtt, ghbb and ghtautau are the htt, hbb \
and htautau couplings. Here, h represents to SM-like Higgs boson while t, b and tau are the top and bottom quarks and the tau-lepton. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableRtau::usage="TableRtau[ghtt_, ghbb_,ghtautau_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of the signal strength \!\(\*SubscriptBox[\(R\), \(\[Tau]\)]\). The arguments ghtt, ghbb and ghtautau are the htt, \
hbb and htautau couplings. Here, h represents to SM-like Higgs boson while t, b and tau are the top and bottom quarks and the tau-lepton. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************End OF RTau************************************************************************************)
(******************************************************************************************************************************************************)


(******************************************************************************************************************************************************)
(*********************************************************Begin OF RXALL************************************************************************************)
(*****************************************************************************************************************************************************)

KappaALL::usage="KappaALL[ghtt_, ghbb_, ghZZ_, ghWW_, ghtautau_, gCH_, mCH_, x_, y_, xmin_, xmax_, ymin_, ymax_, xlabel_, ylabel_, xfor_, yfor_, xformin_, xformax_, xforstep_, yformin_, yformax_, yforstep_, PP_][[i]]\[Rule] This command evaluates all \[Kappa]'s, \
when there is dependence on two or more parameters. The arguments ghtt, ghbb, ghZZ, ghWW, ghtautau and gCH are the htt, hbb, \
hZZ, hWW, htautau and \!\(\*SuperscriptBox[\(hH\), \(-\)]\)\!\(\*SuperscriptBox[\(H\), \(+\)]\) couplings. Here, h represents to SM-like Higgs boson while t and b are the top and bottom quarks; V=Z, W are the gauge bosons, tau is the tau lepton and \!\(\*SuperscriptBox[\(H\), \(-\)]\) is a charged scalar boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively; mCH stands for the charged scalar boson. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

(******************************************************************************************************************************************************)
(*********************************************************End OF Kappa-ALL************************************************************************************)
(*****************************************************************************************************************************************************)
(******************************************************************************************************************************************************)
(*********************************************************Begin OF Kappa-Bottom************************************************************************************)
(******************************************************************************************************************************************************)
Kbone::usage = "Kbone[ghbb_,x_,xmin_,xmax_,xlabel_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\) when there is dependence only on one parameter. The argument ghbb is the hbb \
coupling. Here, h represents to SM-like Higgs boson while b is the bottom quark. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKbone::usage="TableKbone[ghbb_,x_,xmin_,xmax_,xstep_] \[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\). The argument ghbb is the \
hbb coupling. Here, h represents to SM-like Higgs boson while b is the bottom quark. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. "

Kb::usage="Kb[ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\), \
when there is dependence on two or more parameters. The arguments ghbb is the hbb \
coupling. Here, h represents to SM-like Higgs boson while b is the bottom quark. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKb::usage="TableKb[ghbb_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\). The argument ghbb is the hbb, \
coupling. Here, h represents to SM-like Higgs boson while b is the bottom quark. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************End OF Kappa-Bottom************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF Kappa-Z************************************************************************************)
(******************************************************************************************************************************************************)
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
(*********************************************************End OF Kappa-Z************************************************************************************)
(******************************************************************************************************************************************************)

(******************************************************************************************************************************************************)
(*********************************************************Begin OF Kappa-W************************************************************************************)
(******************************************************************************************************************************************************)
KWone::usage = "KWone[ghWW_,x_,xmin_,xmax_,xlabel_][[i]]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\) when there is dependence only on one parameter. The argument ghWW is the hWW \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xlabel is used \
for indicates the X axis label. Finally, [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma])."

TableKWone::usage="TableKWone[ghWW_,x_,xmin_,xmax_,xstep_] \[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\). The argument ghWW is the \
hWW coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. The label x \
indicates the parameter to constraint, while xmin and xmax are the \
initial and final values defined by users and xstep is used \
to indicates the steps from xmin to xmax. "

KW::usage="KW[ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]\[Rule] This command evaluates \!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\), \
when there is dependence on two or more parameters. The arguments ghWW is the hWW \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users. Argument xlabel (ylabel) is used \
for indicates the X axis label (Y axis label). The arguments xfor (yfor), xformin (yformin), xforstep (yforstep) represent an additional parameter to constraint, namely: initial value, final value and the steps from xformin (yformin) to xformax (yformax), respectively. Label [[i]] stands for confidence level, i=1 (2) indicates 1\[Sigma] (2\[Sigma]), Finally, PP is an option for plotting functions that specifies how many initial sample points to use."

TableKW::usage="TableKW[ghWW_, x_, xmin_, xmax_, xstep_, y_, ymin_, ymax_, ystep_]\[Rule] This command generates a table of \!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\). The argument ghWW is the hWW, \
coupling. Here, h represents to SM-like Higgs boson while W is the W gauge boson. Labels x and y \
indicate the parameters to constraint, while xmin (ymin) and xmax (ymax) are the \
initial and final values defined by users and xstep (ystep) is used \
to indicates the steps from xmin to xmax (ymin to ymax)."
(******************************************************************************************************************************************************)
(*********************************************************End OF Kappa-W************************************************************************************)
(******************************************************************************************************************************************************)

(*ghtt[\[Alpha]_, Att_,Cab_,tb_]:=g/2 (mt/mW)((-Cos[\[Alpha]]/tb*Cos[ArcTan[tb]]) +(Sqrt[2] Cab/(g*tb*Cos[ArcTan[tb]]) (mW/mt)*(mt/vev)*Att))
ghbb[\[Alpha]_, Abb_,Cab_,tb_]:= g/2 (mb/mW)((-Sin[\[Alpha]]*tb/Sin[ArcTan[tb]]) +(Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mb)*(mb/vev)*Abb))
ghmumu[\[Alpha]_, Amumu_,Cab_,tb_]:= g/2 (mmu/mW)((-Sin[\[Alpha]]*tb/Sin[ArcTan[tb]]) +(Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mmu)*(mmu/vev)*Amumu))
ghtautau[\[Alpha]_, Atautau_,Cab_,tb_]:= g/2 (mtau/mW)((-Sin[\[Alpha]]*tb/Sin[ArcTan[tb]]) +(Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mmu)*(mtau/vev)*Atautau))
ghtaumu[Ataumu_,Cab_,tb_]:=((Cab*tb)/(Sqrt[2]*Sin[ArcTan[tb]]))*((Sqrt[mmu*mtau]/vev)*Ataumu)
ghtautau[\[Alpha]_, Atata_,Cab_,tb_] := g/2 (mtau/mW)((-Sin[\[Alpha]]*tb/Sin[ArcTan[tb]]) +(Sqrt[2] (Cab*tb)/(g*Sin[ArcTan[tb]]) (mW/mtau)*(mtau/vev)*Atata))
ghWW[sab_]:= g*mW*sab
ghZZ[sab_] := g*mZ*sab*)
(*THDM-I couplings
ghtt[a_,tb_]:=(g/2) (mt/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghbb[a_,tb_]:=(g/2) (mb/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghtautau[a_,tb_]:=(g/2) (mtau/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghWW[cab_]:=gw*mW*Sqrt[1-cab^2]
ghZZ[cab_]:=gz*mZ*Sqrt[1-cab^2]*)

Begin["`Package`"]
End[]

Begin["`PlotsKappaXXv2`Private`"]

All processes;

(*********************************************************************************************
*******************************All kappa's*************************************************************
*********************************************************************************************)

(*To 2\[Sigma]*)

KappaX2sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,
xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaBotINF2sig <= kb[ghbb] <= kappaBotSUP2sig(*,
kappaTopINF2sig<=ktop[ghtt]<=kappaTopSUP2sig*), 
    kappaTauINF2sig <= Abs[ktau[ghtautau]] <= kappaTauSUP2sig, 
   kappaWINF2sig<= kW[ghWW] <= kappaWSUP2sig, 
    kappaZINF2sig<= kZ[ghZZ] <= kappaZSUP2sig, 
  kappaGammaINF2sig <=Abs[kgaga[ghtt, ghbb, ghWW, gCH, mCH]] <=  kappaGammaSUP2sig,
kappaGluonINF2sig<=Abs[kgluglu[ghtt,ghbb]]<=kappaGluonSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)",(*
"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)",*)"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)",
"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)","\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)",
"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)","\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*)
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35,PlotPoints->PP, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]],
  6 -> Directive[Purple, Dashed, Thickness[0.003]],
  7 -> Directive[Cyan, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.1]}, {Pink, Opacity[0.1]}, {Yellow, 
   Opacity[0.1]}, {Blue, Opacity[0.1]}, {Orange, Opacity[0.1]}, {Purple, Opacity[0.1]}, {Cyan, Opacity[0.1]}}, AspectRatio -> 1]

K2sigAll[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,
yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_
]:=Manipulate[
KappaX2sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep
}];

(*To 1\[Sigma]*)

KappaX1sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,
xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaBotINF1sig <= kb[ghbb] <= kappaBotSUP1sig(*,
kappaTopINF2sig<=ktop[ghtt]<=kappaTopSUP2sig*), 
    kappaTauINF1sig <= Abs[ktau[ghtautau]] <= kappaTauSUP1sig, 
   kappaWINF1sig<= kW[ghWW] <= kappaWSUP1sig, 
    kappaZINF1sig<= kZ[ghZZ] <= kappaZSUP1sig, 
  kappaGammaINF1sig <=Abs[kgaga[ghtt, ghbb, ghWW, gCH, mCH]] <=  kappaGammaSUP1sig,
kappaGluonINF1sig<=Abs[kgluglu[ghtt,ghbb]]<=kappaGluonSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)",(*
"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)",*)"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)",
"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)","\!\(\*SubscriptBox[\(\[Kappa]\), \(Z\)]\)",
"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)","\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35,PlotPoints->PP, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Green, Dashed, Thickness[0.003]], 
  2 -> Directive[Pink, Dashed, Thickness[0.003]], 
  3 -> Directive[Yellow, Dashed, Thickness[0.003]], 
  4 -> Directive[Blue, Dashed, Thickness[0.003]], 
  5 -> Directive[Orange, Dashed, Thickness[0.003]],
  6 -> Directive[Purple, Dashed, Thickness[0.003]],
  7 -> Directive[Cyan, Dashed, Thickness[0.003]]}, PlotStyle -> {{Green, Opacity[0.1]}, {Pink, Opacity[0.1]}, {Yellow, 
   Opacity[0.1]}, {Blue, Opacity[0.1]}, {Orange, Opacity[0.1]}, {Purple, Opacity[0.1]}, {Cyan, Opacity[0.1]}}, AspectRatio -> 0.6]

K1sigAll[
ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,
yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_
]:=Manipulate[
KappaX1sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep
}];

KappaALL[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,
yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
{
K1sigAll[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,
yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP
],
K2sigAll[
ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,
yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP
]
}

(*************************************************************************************************************************************************************************************)

(*KappaALL[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,
yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]*)

(*K2sigAll[
ghtt[c\[Alpha],Ztt,u], ghbb[c\[Alpha],Zbb,u],ghZZ[c\[Alpha]],ghWW[c\[Alpha]],ghtautau[c\[Alpha],Ztautau,u],0,mCH,Ztt,Zbb,0,30,0,5,Ztt,Zbb,u,
c\[Alpha],500,2000,100,0.9,0.99,0.01
]*)

(*************************************************************************************************************************************************************************************)

Individual process;
kappa botton

(*************************************************************************************************************************************************************************************)

(*kappa bottom to 1\[Sigma] in the case in which there is dependence in one parameter*)

Kb1sigX[ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kb[ghbb],kappaBotSUP1sig,kappaBotINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)",
"U.L.[1\[Sigma]]","L.L.[1\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa bottom to 2\[Sigma] in the case in which there is dependence in one parameter*)

Kb2sigX[ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kb[ghbb],kappaBotSUP2sig,kappaBotINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)",
"U.L.[2\[Sigma]]","L.L.[2\[Sigma]]"},{1,0.5}],ImageSize->1000,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003](*,AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]}*),LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Black],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

Kbone[ghbb_,x_,xmin_,xmax_,xlabel_]:={
Kb1sigX[ghbb,x,xmin,xmax,xlabel],
Kb2sigX[ghbb,x,xmin,xmax,xlabel]
}

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-bottom}*)

dataKBOT1sig[ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaBotINF1sig<=kb[ghbb]<=kappaBotSUP1sig, kb[ghbb],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-bottom}*)

dataKBOT2sig[ghbb_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0]}, {x, xmin,xmax,xstep}]

(*EXPORTING TABLES FOR RBOTone*)

TableKbone[ghbb_,x_,xmin_,xmax_,xstep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKappaBOTone_1sigma.txt"}],Re[dataKBOT1sig[ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
],
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKappaBOTone_2sigma.txt"}],Re[dataKBOT2sig[ghbb,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},
"Table"
]
}

(*************************************************************************************************************************************************************************************)

(*************************************************************************************************************************************************************************************)

(*Kappa_b to 2\[Sigma] in the case in which there is dependence on more than two parameters*)

(*To 2\[Sigma]*)

kappaBOTTOM2sig[ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaBotINF2sig <= Abs[kb[ghbb]] <=kappaBotSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"}, {1,0.8}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[1]}}, AspectRatio -> 0.6,PlotPoints->PP]

Kb2sigWXYZ[
ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaBOTTOM2sig[ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

kappaBOTTOM1sig[ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaBotINF1sig <= Abs[kb[ghbb]] <=kappaBotSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> {"\!\(\*SubscriptBox[\(\[Kappa]\), \(b\)]\)"},
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, *)AspectRatio -> 1, 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 600,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed],BoundaryStyle -> {1 -> Directive[Red, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[1]}}, AspectRatio -> 0.6,PlotPoints->PP]

Kb1sigWXYZ[
ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaBOTTOM1sig[ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

Kb[ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:={
Kb1sigWXYZ[
ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP],
Kb2sigWXYZ[
ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,PP]
}

(*Kb[ghbb[-ArcCos[cba]+ArcTan[tb],tb],cba,tb,-1,1,0.1,50,c\[Beta]\[Alpha],t\[Beta],xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,50][[2]]*)

(*TABLES FOR \[Kappa]b*)

dataKb1sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    kappaBotINF1sig <= Abs[kb[ghbb]] <=kappaBotSUP1sig, Abs[kb[ghbb]],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

dataKb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=
Table[
{x,y,If[
    kappaBotINF2sig <= Abs[kb[ghbb]] <=kappaBotSUP2sig, Abs[kb[ghbb]],0]}, 
{x, xmin,xmax,xstep}, {y, ymin,ymax,ystep}]

(*EXPORTING TABLES FOR Rb*)

TableKb[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:={
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKappaBOT_1sigma.txt"}],
Re[
dataKb1sig[ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
,
Export[
FileNameJoin[{$UserDocumentsDirectory,"TableKappaBOT_2sigma.txt"}],
Re[
dataKb2sig[ghbb,x,xmin,xmax,xstep,y,ymin,ymax,ystep]
]/. {{_,_,0} -> Sequence[]},
"Table"
]
}

Individual process;
kappa top

(*kappa top to 2\[Sigma] in the case in which there is dependence in one parameter*)

Kt2sigX[ghtt_,x_,xmin_,xmax_,xlabel_]:=Plot[{ktop[ghtt],kappaTopSUP2sig,kappaTopINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*KappatopX1sig[ghtt[0.9,10,u],u,500,2000,u[GeV]]
KappatopX2sig[ghtt[0.9,10,u],u,500,2000,u[GeV]]*)

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-top}*)

datatt1sigXY[ghtt_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaTopINF1sig<=ktop[ghtt]<=kappaTopSUP1sig, ktop[ghtt],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-top}*)

datatt2sigXY[ghtt_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaTopINF2sig<=ktop[ghtt]<=kappaTopSUP2sig, ktop[ghtt],0]}, {x, xmin,xmax,xstep}]

(*Create a table of three columns to 1 \[Sigma] \[Rule] {x,y,kappa-top}*)

datatt1sigXYZ[ghtt_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaTopINF1sig<=ktop[ghtt]<=kappaTopSUP1sig, ktop[ghtt],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*Create a table of three columns to 2 \[Sigma] \[Rule] {x,y,kappa-top}*)

datatt2sigXYZ[ghtt_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaTopINF2sig<=ktop[ghtt]<=kappaTopSUP2sig, ktop[ghtt],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*databb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Do[list=Append[list={x,y,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0
]}], {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]*)

 (*With this commands a table is generated and saved inside the folder TABLE*)

(*************************************************************************************************************************************************************************************)

(*This are the commands used in the shell of mathematica*)

tableTop1sigXY[ghtt_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTop1sigXY.txt",Re[datatt1sigXY[ghtt,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tableTop2sigXY[ghtt_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTop2sigXY.txt",Re[datatt2sigXY[ghtt,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tableTop1sigXYZ[ghtt_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTop1sigXYZ.txt",Re[datatt1sigXYZ[ghtt,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tableTop2sigXYZ[ghtt_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTop2sigXYZ.txt",Re[datatt2sigXYZ[ghtt,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
,"Table"
]

(*tableTop1sigXYZ[ghtt[c\[Alpha],Ztt,1000],c\[Alpha],0,1,0.1,Ztt,0,10,1]*)

(*************************************************************************************************************************************************************************************)

kappaT2sig[ghtt_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaTopINF2sig<=ktop[ghtt]<=kappaTopSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->60, FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Pink, Dashed, Thickness[0.003]]}, PlotStyle -> {{Pink, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->200]

Kt2sigWXYZ[
ghtt_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaT2sig[ghtt,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

(*kappa top to 1\[Sigma]*)
kappaT1sig[ghtt_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaTopINF1sig<=ktop[ghtt]<=kappaTopSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->60, FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(t\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Pink, Dashed, Thickness[0.003]]}, PlotStyle -> {{Pink, Opacity[0.3]}}, AspectRatio -> 0.6]

Kt1sigWXYZ[
ghtt_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaT1sig[ghtt,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KappaTOP1sig[ghtt[c\[Alpha],Ztt,u],u,Ztt,500,2000,0,20,u[GeV],Subscript[
Overscript[Z, ~], tt],c\[Alpha],yfor,0.95,0.99,0.01,yformin,yformax,yforstep
]*)

Individual process;
kappa tau

(*kappa tau to 1\[Sigma] in the case in which there is dependence in one parameter*)

Ktau1sigX[ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{ktau[ghtautau],kappaTauSUP1sig,kappaTauINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa tau to 2\[Sigma] in the case in which there is dependence in one parameter*)

Ktau2sigX[ghtautau_,x_,xmin_,xmax_,xlabel_]:=Plot[{ktau[ghtautau],kappaTauSUP2sig,kappaTauINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)","\!\(\*
StyleBox[\"SpaceMath\",\nFontSlant->\"Italic\"]\)"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*KappatauX1sig[ghtautau[0.9,0.1,u],u,500,2000,u[GeV]]
KappatauX2sig[ghtautau[0.9,0.1,u],u,500,2000,u[GeV]]*)

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-tau}*)

datatautau1sigXY[ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaTauINF1sig<=ktau[ghtautau]<=kappaTauSUP1sig, ktau[ghtautau],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-tau}*)

datatautau2sigXY[ghtautau_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaTauINF2sig<=ktau[ghtautau]<=kappaTauSUP2sig, ktau[ghtautau],0]}, {x, xmin,xmax,xstep}]

(*Create a table of three columns to 1 \[Sigma] \[Rule] {x,y,kappa-tau}*)

datatautau1sigXYZ[ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaTauINF1sig<=ktau[ghtautau]<=kappaTauSUP1sig, ktau[ghtautau],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*Create a table of three columns to 2 \[Sigma] \[Rule] {x,y,kappa-tau}*)

datatautau2sigXYZ[ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaTauINF2sig<=ktau[ghtautau]<=kappaTauSUP2sig, ktau[ghtautau],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*databb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Do[list=Append[list={x,y,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0
]}], {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]*)

 (*With this commands a table is generated and saved inside the folder TABLE*)

(*************************************************************************************************************************************************************************************)

(*This are the commands used in the shell of mathematica*)

tableTau1sigXY[ghtautau_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTau1sigXY.txt",Re[datatautau1sigXY[ghtautau,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tableTau2sigXY[ghtautau_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTau2sigXY.txt",Re[datatautau2sigXY[ghtautau,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tableTau1sigXYZ[ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTau1sigXYZ.txt",Re[datatautau1sigXYZ[ghtautau,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tableTau2sigXYZ[ghtautau_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableTau2sigXYZ.txt",Re[datatautau2sigXYZ[ghtautau,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
,"Table"
]

(*tableTau2sigXYZ[ghtautau[c\[Alpha],Ztautau,1000],c\[Alpha],0,1,0.1,Ztautau,0,0.1,0.01]
tableTau1sigXY[ghtautau[c\[Alpha],0.05,1000],c\[Alpha],0,1,0.1]*)

(*************************************************************************************************************************************************************************************)

(*To 2\[Sigma]*)

(*kappa tau to 2\[Sigma]*)
kappaTau2sig[ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaTauINF2sig<= Abs[ktau[ghtautau]] <= kappaTauSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)"}, {1,0.7}],
(* AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]}, *)
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Yellow, Dashed, Thickness[0.003]]}, PlotStyle -> {{Yellow, Opacity[0.8]}}, AspectRatio -> 0.6]

Ktau2sigWXYZ[
ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaTau2sig[ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

(*kappa tau to 1\[Sigma]*)
kappaTau1sig[ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaTauINF1sig<= Abs[ktau[ghtautau]] <= kappaTauSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Large, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Tau]\)]\)"}, {1,0.6}],
(* AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Yellow, Dashed, Thickness[0.003]]}, PlotStyle -> {{Yellow, Opacity[0.3]}}, AspectRatio -> 0.6]

Ktau1sigWXYZ[
ghtautau_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaTau1sig[ghtautau,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*Ktau1sigWXYZ[ghtautau[-ArcCos[cba]+ArcTan[tb],tb],cba,tb,-1,1,0.1,50,c\[Beta]\[Alpha],t\[Beta],xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,50]*)

(*KappaTAU2sig[ghtautau[c\[Alpha],Ztautau,u],u,Ztautau,500,2000,0,1
,u[GeV],Ztautau,c\[Alpha],yfor,0.9,0.99,0.01,yformin,yformax,yforstep]*)

(******************************************************************************************************************************************************)
(*********************************************************Begin KW*************************************************************************************)
(******************************************************************************************************************************************************)

Individual process;
kappa W

(*kappa W to 1\[Sigma] in the case in which there is dependence in one parameter*)

KW1sigX[ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{kW[ghWW],kappaWSUP1sig,kappaWINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa W to 2\[Sigma] in the case in which there is dependence in one parameter*)

KW2sigX[ghWW_,x_,xmin_,xmax_,xlabel_]:=Plot[{kW[ghWW],kappaWSUP2sig,kappaWINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

KWone[ghWW_,x_,xmin_,xmax_,xlabel_]:={KW1sigX[ghWW,x,xmin,xmax,xlabel],KW2sigX[ghWW,x,xmin,xmax,xlabel]}

(*KappaWX1sig[ghWW[c\[Alpha]],c\[Alpha],0,1,Subscript[c, \[Alpha]]]
KappaWX2sig[ghWW[c\[Alpha]],c\[Alpha],0,1,Subscript[c, \[Alpha]]]*)

(*************************************************************************************************************************************************************************************)

 (*With this commands a table is created*)

(*Create a table of two columns to 1 \[Sigma] \[Rule] {x,kappa-W}*)

dataWW1sigXY[ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaWINF1sig<=kW[ghWW]<=kappaWSUP1sig, kW[ghWW],0]}, {x, xmin,xmax,xstep}]

(*Create a table of two columns to 2 \[Sigma] \[Rule] {x,kappa-W}*)

dataWW2sigXY[ghWW_,x_,xmin_,xmax_,xstep_]:=Table[{x,If[
    kappaWINF2sig<=kW[ghWW]<=kappaWSUP2sig, kW[ghWW],0]}, {x, xmin,xmax,xstep}]

(*Create a table of three columns to 1 \[Sigma] \[Rule] {x,y,kappa-W}*)

dataWW1sigXYZ[ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaWINF1sig<=kW[ghWW]<=kappaWSUP1sig, kW[ghWW],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*Create a table of three columns to 2 \[Sigma] \[Rule] {x,y,kappa-top}*)

dataWW2sigXYZ[ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Table[{x,y,If[
    kappaWINF2sig<=kW[ghWW]<=kappaWSUP2sig, kW[ghWW],0]}, {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]

(*databb2sig[ghbb_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Do[list=Append[list={x,y,If[
    kappaBotINF2sig<=kb[ghbb]<=kappaBotSUP2sig, kb[ghbb],0
]}], {x, xmin,xmax,xstep},{y, ymin,ymax,ystep}]*)

 (*With this commands a table is generated and saved inside the folder TABLE*)

(*************************************************************************************************************************************************************************************)

(*This are the commands used in the shell of mathematica*)

tableW1sigXY[ghWW_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableW1sigXY.txt",Re[dataWW1sigXY[ghWW,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tableW2sigXY[ghWW_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableW2sigXY.txt",Re[dataWW2sigXY[ghWW,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tableW1sigXYZ[ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableW1sigXYZ.txt",Re[dataWW1sigXYZ[ghWW,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tableW2sigXYZ[ghWW_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableW2sigXYZ.txt",Re[dataWW2sigXYZ[ghWW,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
,"Table"
]

(*tableW2sigXY[ghWW[c\[Alpha]],c\[Alpha],0.7,1,0.01]*)

(*************************************************************************************************************************************************************************************)

(*To 2\[Sigma]*)

(*kappa W in the case in which there are dependence in two or more parameters*)
kappaw2sig[ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaWINF2sig<= kW[ghWW] <= kappaWSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Blue, Dashed, Thickness[0.003]]}, PlotStyle -> {{Blue, Opacity[0.3]}}, AspectRatio -> 0.6]

KW2sigWXYZ[
ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaw2sig[ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 1\[Sigma]*)

(*kappa W in the case in which there are dependence in two or more parameters*)
kappaw1sig[ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaWINF2sig<= kW[ghWW] <= kappaWSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(W\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Blue, Dashed, Thickness[0.003]]}, PlotStyle -> {{Blue, Opacity[0.3]}}, AspectRatio -> 0.6]

KW1sigWXYZ[
ghWW_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
kappaw1sig[ghWW,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KappaW2sig[
ghWW[c\[Alpha],u],c\[Alpha],u,0.8,1,1,2,calpha,U,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep]*)

(******************************************************************************************************************************************************)
(*********************************************************End KW***************************************************************************************)
(******************************************************************************************************************************************************)

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
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableZ1sigXY.txt",Re[dataZZ1sigXY[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} -> Sequence[]},"Table"
]

tableZ2sigXY[ghZZ_,x_,xmin_,xmax_,xstep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableZ2sigXY.txt",Re[dataZZ2sigXY[ghZZ,x,xmin,xmax,xstep]]/. {{_,0} ->Sequence[]}
,"Table"
]

tableZ1sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableZ1sigXYZ.txt",Re[dataZZ1sigXYZ[ghZZ,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. { {_,_,0}-> Sequence[]},"Table"
]

tableZ2sigXYZ[ghZZ_,x_,xmin_,xmax_,xstep_,y_,ymin_,ymax_,ystep_]:=Export[
"/home/maau/Dropbox/SPACEMATH/Higgs_Data/Tables/tableZ2sigXYZ.txt",Re[dataZZ2sigXYZ[ghZZ,x,xmin,xmax,xstep,y, ymin,ymax,ystep]]/. {{_,_,0} ->Sequence[]}
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

(*KappaZ1sig[
ghZZ[c\[Alpha],u],c\[Alpha],u,0.8,1,1,2,calpha,U,xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep]*)

(***************************************************************************************************************************************************)
(**********************************************************End kappa Z******************************************************************************)
(***************************************************************************************************************************************************)

(***************************************************************************************************************************************************)
(**********************************************************Begin kappa gamma************************************************************************)
(***************************************************************************************************************************************************)
Individual process;
kappa gamma

(*kappa \[Gamma] to 1\[Sigma] in the case in which there is dependence in one parameter*)

KGam1sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgaga[ghtt, ghbb, ghWW, gCH, mCH],kappaGammaSUP1sig,kappaGammaINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa \[Gamma] to 2\[Sigma] in the case in which there is dependence in one parameter*)

KGam2sigX[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgaga[ghtt, ghbb, ghWW, gCH, mCH],kappaGammaSUP2sig,kappaGammaINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)",
"Upper Limit [2\[Sigma]]","Lower Limit [2\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

KGAMone[ghtt_,ghbb_,ghWW_,gCH_,mCH_,x_,xmin_,xmax_,xlabel_]:={KGam1sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel],KGam2sigX[ghtt,ghbb,ghWW,gCH,mCH,x,xmin,xmax,xlabel]}

(*To 1\[Sigma]*)

(*kappa \[Gamma]*)
Kappagaga1sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{  kappaGammaINF1sig <= kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= kappaGammaSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, 
PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6]

KGam1sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Kappagaga1sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 2\[Sigma]*)

(*kappa \[Gamma]*)
Kappagaga2sig[ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{  kappaGammaINF2sig <= kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= kappaGammaSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(\[Gamma]\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6,PlotPoints->PP]

KGam2sigWXYZ[
ghtt_, ghbb_,ghWW_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Kappagaga2sig[ghtt, ghbb,ghWW,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KGam1sigWXYZ[ghtt[-ArcCos[cba]+ArcTan[tb],tb],ghbb[-ArcCos[cba]+ArcTan[tb],tb],ghWW[cba],0,mCH,cba,tb,-1,1,0.1,50,c\[Beta]\[Alpha],t\[Beta],xfor,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep,50]*)

(*KappaGaGa1sig[
ghtt[0.99,Ztt,u], ghbb[0.99,0,u],ghWW[0.99],0,mCH,Ztt,u,0,1,500,2000,Subscript[Overscript[Z, ~], tt],
Subscript[Overscript[Z, ~], bb],u,yfor,xformin,xformax,xforstep,yformin,yformax,yforstep]*)

(***************************************************************************************************************************************************)
(**********************************************************End kappa gamma**************************************************************************)
(***************************************************************************************************************************************************)



Individual process;
kappa gluon

(*kappa gluon to 1\[Sigma] in the case in which there is dependence in one parameter*)

KGlu1sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgluglu[ghtt, ghbb],kappaGluonSUP1sig,kappaGluonINF1sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*kappa gluon to 2\[Sigma] in the case in which there is dependence in one parameter*)

KGlu2sigX[ghtt_,ghbb_,x_,xmin_,xmax_,xlabel_]:=Plot[{kgluglu[ghtt, ghbb],kappaGluonSUP2sig,kappaGluonINF2sig},{x,xmin,xmax}
,PlotLegends->Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)",
"Upper Limit [1\[Sigma]]","Lower Limit [1\[Sigma]]"},{1,0.5}],ImageSize->1300,Frame->True,
FrameLabel->{xlabel,"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)","SpaceMath"},
FrameStyle->Thickness[0.003],(*AxesLabel->{Style["x",Large,Bold,Bold],
Style["y",Large,Bold,Bold]},*)LabelStyle->30,PlotStyle->{Directive[AbsoluteThickness[3.3],
Red],Directive[AbsoluteThickness[3.3],Dashing[{0.09,0.04}],Purple],Directive[AbsoluteThickness[3.3],
Dashing[{0.03,0.03,0.003}],Purple]},GridLinesStyle->Directive[Black,Dashed],GridLines->Automatic,
AspectRatio->0.6,Filling->{3->{2}},FillingStyle->{Blue,Opacity[0.1]}
]

(*To 1\[Sigma]*)

KappaGluGlu1sig[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaGluonINF1sig<=kgluglu[ghtt,ghbb]<=kappaGluonSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio -> 0.6]

KGlu1sigWXYZ[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KappaGluGlu1sig[ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*To 2\[Sigma]*)

KappaGluGlu2sig[ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=
RegionPlot[{kappaGluonINF2sig<=kgluglu[ghtt,ghbb]<=kappaGluonSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Medium, Bold]}, 
 PlotLegends -> Placed[{"\!\(\*SubscriptBox[\(\[Kappa]\), \(g\)]\)"}, {1,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio -> 0.6]

KGlu2sigWXYZ[
ghtt_, ghbb_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
KappaGluGlu2sig[ghtt, ghbb,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP
],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*KappaGluon2sig[
ghtt[c\[Alpha],Ztt,u], ghbb[c\[Alpha],Zbb,u],Ztt,u,0,1,500,2000,Ztt,u,c\[Alpha],Zbb,0.9,1,0.005,0,1,0.2]*)

(*COMAND FOR CALL FOR ALL KAPPAS *)

Intersection;

(*To 1\[Sigma]*)

Intersectionkappa1sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{kappaBotINF1sig <= kb[ghbb] <= kappaBotSUP1sig&&
kappaTopINF1sig<=ktop[ghtt]<=kappaTopSUP1sig&& 
    kappaTauINF1sig <= ktau[ghtautau] <= kappaTauSUP1sig&& 
   kappaWINF1sig<= kW[ghWW] <= kappaWSUP1sig&& 
    kappaZINF1sig<= kZ[ghZZ] <= kappaZSUP1sig&& 
  kappaGammaINF1sig <= kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= kappaGammaSUP1sig&&
kappaGluonINF1sig<=kgluglu[ghtt,ghbb]<=kappaGluonSUP1sig},
 {x, xmin, xmax}, {y, ymin, ymax}
 ,PlotPoints->PP, FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Larger, Bold]}, 
 PlotLegends -> Placed[{"Intersection"}, {0.8,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*) 
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 1000,(*PlotPoints->500,*)
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Orange, Dashed, Thickness[0.003]]}, PlotStyle -> {{Orange, Opacity[0.3]}}, AspectRatio -> 0.6]

InterKappa1sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Intersectionkappa1sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}]

(*To 2\[Sigma]*)

Intersectionkappa2sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{kappaBotINF2sig <= kb[ghbb] <= kappaBotSUP2sig&&
    kappaTauINF2sig <= ktau[ghtautau] <= kappaTauSUP2sig&& 
   kappaWINF2sig<= kW[ghWW] <= kappaWSUP2sig&& 
    kappaZINF2sig<= kZ[ghZZ] <= kappaZSUP2sig&& 
  kappaGammaINF2sig <= kgaga[ghtt, ghbb, ghWW, gCH, mCH] <= kappaGammaSUP2sig&&
kappaGluonINF2sig<=kgluglu[ghtt,ghbb]<=kappaGluonSUP2sig},
 {x, xmin, xmax}, {y, ymin, ymax}
,PlotPoints->PP , FrameLabel -> {Style[xlabel, Larger, Bold], 
   Style[ylabel, Larger, Bold], 
   Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)", Larger, Bold]}, 
 PlotLegends -> Placed[{"Intersection"}, {0.8,0.6}],
 (*AxesLabel -> {Style["x", Larger, Bold], 
   Style["y", Larger, Bold]},*)
 FrameStyle ->  Thickness[0.004], LabelStyle -> 35, ImageSize -> 800,
  GridLines -> Automatic, GridLinesStyle -> 
   Directive[Black, 
     Dashed], BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.003]]}, PlotStyle -> {{Purple, Opacity[0.3]}}, AspectRatio -> 1]

InterKappa2sig[ghtt_, ghbb_,ghZZ_,ghWW_,ghtautau_,gCH_,mCH_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,yfor_,xformin_,xformax_,xforstep_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
Intersectionkappa2sig[ghtt, ghbb,ghZZ,ghWW,ghtautau,gCH,mCH,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}]
 
 End[]