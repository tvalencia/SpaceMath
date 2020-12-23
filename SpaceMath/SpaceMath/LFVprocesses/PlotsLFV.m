(* Wolfram Language Package *)

ParamSpace::usage="See later"

LFVall::usage="See later"

aMU::usage="See later"

DeltaAMU::usage="See later"

InterParam::usage="See later"

LFVintersection::usage="See later"

Begin["`Package`"]
End[]

Begin["`PlotsLFV`Private`"]

(*The ParameterSpace command show all constraints*)

(*ALL PROCESSES*)

ParamSpace[ghee_,ghmue_,ghmumu_,ghtaumu_,ghtautau_,gHee_,gHmue_,gHmumu_,gHtaumu_,gHtautau_,gAee_,gAmue_,gAmumu_,gAtaumu_,gAtautau_,gAtt_,ghtt_,gHtt_,mh_,mH_,mA_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{0<BRhtaumu[ghtaumu]<BRHtoTAUMU
,aMUInf<=amu[ghmumu, ghtaumu, gHmumu, gHtaumu, gAmumu, gAtaumu, mh,mH, mA]<=aMUSup
,0<BRtauto3muons[ghmumu, ghtaumu, gHmumu, gHtaumu, gAmumu, gAtaumu, mH, mA]<BRTAUtoMUMUMU
,0<BRmuto3electrons[ghee,ghmue,gHee,gHmue,gAee,gAmue,mH,mA]<BRMUtoEEE
,0<BRtautomugamma[ghtaumu,ghtautau,gAtaumu,gAtautau,gHtaumu,gHtautau,ghtt,gHtt,gAtt,mh,mH,mA]<BRTAUtoMUgamma
(*,dmuINF<dmu[ghtaumu,gHtaumu,gAtaumu,mh,mH,mA]<dmuSUP*)},{x,xmin,xmax},{y,ymin,ymax}
,FrameLabel->{Style[xlabel,Larger,Bold],Style[ylabel,Larger,Bold],Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)",Medium,Bold]},PlotLegends->Placed[{"h\[Rule] \[Tau]\[Mu]","\!\(\*SubscriptBox[\(\[Delta]a\), \(\[Mu]\)]\)","\[Tau]\[Rule] 3\[Mu]","\[Tau]\[Rule] \[Mu]\[Gamma]","\!\(\*SubscriptBox[\(d\), \(\[Mu]\)]\)"},{1,0.5}],AxesLabel -> {Style["x", Larger, Bold], 
Style["y", Larger, Bold]}, AspectRatio -> 1, FrameStyle ->  Thickness[0.004], LabelStyle -> 35,ImageSize->800,GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
  Dashed], PlotStyle -> {{Blue, Opacity[0.1]}, {Green, 
   Opacity[0.1]}, {Red, Opacity[0.1]},{Cyan, Opacity[0.3]},{Magenta, Opacity[10]}},BoundaryStyle -> {1 -> 
   Directive[Blue, Dashed, Thickness[0.002], Opacity[10]], 
  2 -> Directive[Green, Dashed, Thickness[0.004], Opacity[10]], 
  3 -> Directive[Red, Dashed, Thickness[0.002], Opacity[10]],4 -> Directive[Cyan, Dashed, Thickness[0.002],Opacity[10]]
  ,5 -> Directive[Magenta, Dashed, Thickness[0.002], Opacity[10]]},PlotPoints->PP]

LFVall[
ghmumu_,ghtaumu_,ghtautau_,gHmumu_,gHtaumu_,gHtautau_,gAmumu_,gAtaumu_,gAtautau_,gAtt_,ghtt_,gHtt_,mh_,mH_,mA_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,xformin_,xformax_,xforstep_,yfor_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
ParamSpace[ghmumu,ghtaumu,ghtautau,gHmumu,gHtaumu,gHtautau,gAmumu,gAtaumu,gAtautau,gAtt,ghtt,gHtt,mh,mH,mA,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

(*INDIVIDUAL PROCESS*)

(*Delta amu*)

aMU[ghtaumu_,x_,xmin_,xmax_,y_,ymin_,ymax_,xlabel_,ylabel_]:=RegionPlot[0<BRhtaumu[ghtaumu]<BRHtoTAUMU,{x,xmin,xmax},{y,ymin,ymax}
,FrameLabel->{Style[xlabel,Larger,Bold],Style[ylabel,Larger,Bold](*,Style["Subscript[\[Delta]a, \[Mu]]",Larger,Bold]*)},PlotLegends->{"h\[Rule]\[Tau]\[Mu]"},AxesLabel -> {Style["x", Larger, Bold], 
Style["y", Larger, Bold]}, AspectRatio -> 1, FrameStyle ->  Thickness[0.004], LabelStyle -> 35,ImageSize->600,GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
  Dashed], PlotStyle -> {{Blue, Opacity[0.5]}, {Green, 
   Opacity[50]}, {Red, Opacity[0.5]},{Yellow, Opacity[0.5]}},BoundaryStyle -> {1 -> Directive[Purple, Dashed, Thickness[0.002]]},PlotPoints->200]

DeltaAMU[ghtaumu_,x_,xmin_,xmax_,y_,ymin_,ymax_,xlabel_,ylabel_,xfor_,xformin_,xformax_,xforstep_,yfor_,yformin_,yformax_,yforstep_]:=Manipulate[
aMU[ghtaumu,x,xmin,xmax,y,ymin,ymax,xlabel,ylabel],{xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
]

(*RegionPlot[{aMUInf<=amu[ghmumu[a,b,1], ghtaumu[a,b,1], gHmumu[a,b,1], 
gHtaumu[a,b,1], gAmumu[a,b,1], gAtaumu[a,b,1],125, 300,1000]<=aMUSup},{a,-1,1},{b,-1,1}
,FrameLabel->{Style[a,Larger,Bold],Style[b,Larger,Bold],Style["\!\(\*SubscriptBox[\(\[Chi]\), \(ij\)]\)=1",Larger,Bold]},PlotLegends->{"\!\(\*SubscriptBox[\(\[Delta]a\), \(\[Mu]\)]\)"},AxesLabel -> {Style["x", Larger, Bold], 
Style["y", Larger, Bold]}, AspectRatio -> 1, FrameStyle ->  Thickness[0.004], LabelStyle -> 35,ImageSize->1000,GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
  Dashed], PlotStyle -> {{Green, Opacity[0.3]}, {Green, 
   Opacity[50]}, {Red, Opacity[0.5]},{Yellow, Opacity[0.5]}},BoundaryStyle -> {1 -> 
   Directive[Green, Dashed, Thickness[0.002]], 
  2 -> Directive[Orange, Dashed, Thickness[0.002], Opacity[10]], 
  3 -> Directive[Green, Dashed, Thickness[0.002]],4 -> Directive[Red, Dashed, Thickness[0.002]],5 -> Directive[Purple, Dashed, Thickness[0.002]]},PlotPoints->100]*)

(*{s\[Alpha]=Sqrt[1-c\[Alpha]^2],c\[Alpha]=0.99,Z\[Mu]\[Mu]=0.0001,Z\[Tau]\[Tau]=0.01,Ztt=0.01}*)

(*, Subscript[m, Subscript[H, F]]=1 TeV, Subscript[m, Subscript[A, F]]=0.5 TeV*)

(*RegionPlot[{0<BR\[Tau]to\[Mu]\[Gamma][gh\[Tau]\[Mu],gh\[Tau]\[Tau],gA\[Tau]\[Mu],gA\[Tau]\[Tau],gH\[Tau]\[Mu],gH\[Tau]\[Tau],ghtt,gHtt,gAtt,500,500]<BRexp\[Tau]to\[Mu]\[Gamma]},{u,500,2000},{Z\[Tau]\[Mu],0,1}
,FrameLabel->{Style[u[GeV],Larger,Bold],Style[Subscript[Overscript[Z, ~], \[Tau]\[Mu]],Larger,Bold],Style["\!\(\*SubscriptBox[\(c\), \(\[Alpha]\)]\)=0.99, \!\(\*SubscriptBox[\(m\), SubscriptBox[\(H\), \(F\)]]\)=1 TeV, \!\(\*SubscriptBox[\(m\), SubscriptBox[\(A\), \(F\)]]\)=0.5 TeV",Larger,Bold]},PlotLegends->{"\[Tau]\[Rule]\[Mu]\[Gamma]"},AxesLabel -> {Style["x", Larger, Bold], 
Style["y", Larger, Bold]}, AspectRatio -> 1, FrameStyle ->  Thickness[0.004], LabelStyle -> 35,ImageSize->600,GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
  Dashed], PlotStyle -> {{Green, Opacity[0.3]}, {Green, 
   Opacity[50]}, {Red, Opacity[0.5]},{Yellow, Opacity[0.5]}},BoundaryStyle -> {1 -> 
   Directive[Green, Dashed, Thickness[0.002]], 
  2 -> Directive[Orange, Dashed, Thickness[0.002], Opacity[10]], 
  3 -> Directive[Green, Dashed, Thickness[0.002]],4 -> Directive[Red, Dashed, Thickness[0.002]],5 -> Directive[Purple, Dashed, Thickness[0.002]]}]*)

(*INTERSECTION*)

InterParam[ghmumu_,ghtaumu_,ghtautau_,gHmumu_,gHtaumu_,gHtautau_,gAmumu_,gAtaumu_,gAtautau_,gAtt_,ghtt_,gHtt_,mh_,mH_,mA_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,PP_]:=RegionPlot[
{0<BRhtaumu[ghtaumu]<BRHtoTAUMU
(*&&aMUInf<=amu[ghmumu, ghtaumu, gHmumu, gHtaumu, gAmumu, gAtaumu, mh,mH, mA]<=aMUSup*)
&&0<BRtauto3muons[ghmumu, ghtaumu, gHmumu, gHtaumu, gAmumu, gAtaumu, mH, mA]<BRTAUtoMUMUMU
&&0<BRtautomugamma[ghtaumu,ghtautau,gAtaumu,gAtautau,gHtaumu,gHtautau,ghtt,gHtt,gAtt,mh,mH,mA]<BRTAUtoMUgamma
(*&&dmuINF<dmu[ghtaumu,gHtaumu,gAtaumu,mh,mH,mA]<dmuSUP*)},{x,xmin,xmax},{y,ymin,ymax}
,FrameLabel->{Style[xlabel,Larger,Bold],Style[ylabel,Larger,Bold],Style["\!\(\*
StyleBox[\"SpaceMath\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)",Medium,Bold]},PlotLegends->Placed[{"h\[Rule] \[Tau]\[Mu]","\!\(\*SubscriptBox[\(\[Delta]a\), \(\[Mu]\)]\)","\[Tau]\[Rule] 3\[Mu]","\[Tau]\[Rule] \[Mu]\[Gamma]","\!\(\*SubscriptBox[\(d\), \(\[Mu]\)]\)"},{1,0.5}],AxesLabel -> {Style["x", Larger, Bold], 
Style["y", Larger, Bold]}, AspectRatio -> 1, FrameStyle ->  Thickness[0.004], LabelStyle -> 35,ImageSize->800,GridLines -> Automatic, GridLinesStyle -> 
 Directive[Black, 
  Dashed], PlotStyle -> {{Blue, Opacity[0.1]}, {Green, 
   Opacity[0.1]}, {Red, Opacity[0.1]},{Yellow, Opacity[0.3]},{Magenta, Opacity[10]}},BoundaryStyle -> {1 -> 
   Directive[Blue, Dashed, Thickness[0.002], Opacity[10]], 
  2 -> Directive[Green, Dashed, Thickness[0.004], Opacity[10]], 
  3 -> Directive[Red, Dashed, Thickness[0.002], Opacity[10]],4 -> Directive[Yellow, Dashed, Thickness[0.002],Opacity[10]]
  ,5 -> Directive[Magenta, Dashed, Thickness[0.002], Opacity[10]]},PlotPoints->PP]

LFVintersection[
ghmumu_,ghtaumu_,ghtautau_,gHmumu_,gHtaumu_,gHtautau_,gAmumu_,gAtaumu_,gAtautau_,gAtt_,ghtt_,gHtt_,mh_,mH_,mA_,x_,y_,xmin_,xmax_,ymin_,ymax_,xlabel_,ylabel_,xfor_,xformin_,xformax_,xforstep_,yfor_,yformin_,yformax_,yforstep_,PP_]:=
Manipulate[
InterParam[ghmumu,ghtaumu,ghtautau,gHmumu,gHtaumu,gHtautau,gAmumu,gAtaumu,gAtautau,gAtt,ghtt,gHtt,mh,mH,mA,x,y,xmin,xmax,ymin,ymax,xlabel,ylabel,PP],
 {xfor,xformin,xformax,xforstep},{yfor,yformin,yformax,yforstep}
];

End[]




