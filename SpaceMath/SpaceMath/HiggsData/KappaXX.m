(*THIS PROGRAM EVALUATE THE \[Kappa]-FACTOR,from \
https://arxiv.org/abs/1809.10733*)


(* Scalar boson decays into fermion pair; *)

(* Definitions; *)

\[Tau]fi::usage ="See later"
\[Tau]fj::usage ="See later"
WidthHff::usage ="See later"
ft::usage ="See later"
fb::usage ="See later"
gt::usage ="See later"
gb::usage ="See later"
At::usage ="See later"
Ab::usage ="See later"
Ft::usage ="See later"
Fb::usage ="See later"
AHgg::usage ="See later"
WidthHgg::usage ="See later"
Aht::usage ="See later"
Ahb::usage ="See later"
Af::usage ="See later"
fW::usage ="See later"
gW::usage ="See later"
AW::usage ="See later"
FW::usage ="See later"
AhW::usage ="See later" 
fH::usage ="See later"
gH::usage ="See later"
AH::usage ="See later"
FH::usage ="See later"
AHc::usage ="See later"
Ahgaga::usage ="See later"
WidthHgaga::usage ="See later"
RTW::usage ="See later"
RTZ::usage ="See later"
\[Delta]Z::usage ="See later"
WidthHWW::usage ="See later"
WidthHZZ::usage ="See later"
ktau::usage ="See later"
ktop::usage ="See later"
kW::usage ="See later"
kZ::usage ="See later"
kgaga::usage ="See later"
kgluglu::usage ="See later"

(************************************************************************************************************************************************************************************************************************************************************************************************************************)

Begin["`Package`"]
End[]

Begin["`KappaXX`Private`"]

Scalar boson decays into fermion pair;

Definitions;

\[Tau]fi[mi_,mS_] := (2 mi/mS)^2
\[Tau]fj[mj_,mS_] := (2 mj/mS)^2

Decay width of the Scalar boson into fermion pair;



WidthHff[ghfifj_, Nc_, mi_, mj_,mS_] := (((ghfifj^2) Nc mS)/(128 \[Pi]))*((4-(Sqrt[\[Tau]fi[mi,mS]]+Sqrt[\[Tau]fj[mj,mS]])^2)^(3/2)) (Sqrt[(4-(Sqrt[\[Tau]fi[mi,mS]]-Sqrt[\[Tau]fj[mj,mS]])^2)])

(************************************************************************************************************************************************************************************************************************************************************************************************************************)

Scalar boson decay into gluon pair at one - loop level;

Definitions;



ft[mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mt^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mt^2))^-1)])]-I \[Pi])^2;
fb[mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mb^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mb^2))^-1)])]-I \[Pi])^2;
gt[mS_]:=ArcSin[Sqrt[(mS^2)/(4mt^2)]]^2
gb[mS_]:=ArcSin[Sqrt[(mS^2)/(4mb^2)]]^2



At[mS_]:=If[((mS^2)/4mt^2)<=1,gt[mS],ft[mS]]
Ab[mS_]:=If[((mS^2)/4mb^2)<=1,gb[mS],fb[mS]]



Ft[mS_]:=2*((mS^2/(4mt^2))+(((mS^2/(4mt^2))-1)*At[mS]))*((mS^2/(4mt^2))^-2); 
Fb[mS_]:=2*((mS^2/(4mb^2))+(((mS^2/(4mb^2))-1)*At[mS]))*((mS^2/(4mb^2))^-2);



AHgg[ghtt_,ghbb_,mS_]:=2*mW ((ghtt/(ghtt*mt) Ft[mS])+(ghbb/(ghbb*mb) Fb[mS]))(*We consider the contribution of bottom and top quarks inside the loop*)

WidthHgg[ghtt_,ghbb_,mS_]:=((GF*\[Alpha]s^2*mS^3)/(36 Sqrt[2] \[Pi]^3 ))*
Abs[3/4 AHgg[ghtt,ghbb,mS]]^2

(************************************************************************************************************************************************************************************************************************************************************************************************************************)

(*Higgs boson decay into photon pair*)

{Qt=(2/3),Qb=(-1/3)};

(*Main fermion contribution come from top and bottom quark*)

Aht[ghtt_,mS_]:=3 Qt^2 ghtt Ft[mS]
Ahb[ghbb_,mS_]:=3 Qb^2 ghbb Fb[mS]
Af[ghtt_,ghbb_,mS_]:=Aht[ghtt,mS]+Ahb[ghbb,mS]
(**)

(*W contribution*)

fW[mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mW^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mW^2))^-1)])]-I \[Pi])^2;
gW[mS_]:=ArcSin[Sqrt[mS^2/(4mW^2)]]^2;
AW[mS_]:=If[(mS^2/(4mW^2))<=1,gW[mS],fW[mS]];
FW[mS_]:=-((2*(mS^2/(4mW^2))^2)+(3*(mS^2/(4mW^2)))+((3*(2(mS^2/(4mW^2))-1))*AW[mS]))*(mS^2/(4mW^2))^(-2);
AhW[ghWW_,mS_]:=(ghWW) FW[mS] 

(*Charged scalar contribution*)

fH[mCH_,mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mCH^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mCH^2))^-1)])]- I \[Pi])^2 ;
gH[mCH_,mS_]:=ArcSin[Sqrt[mS^2/(4mCH^2)]]^2 ;
AH[mCH_,mS_]:=If[(mS^2/(4mCH^2))<=1,gH[mCH,mS],fH[mCH,mS]];
FH[mCH_,mS_]:=-((mS^2/(4mCH^2))-AH[mCH,mS])*(mS^2/(4mCH^2))^-2
AHc[gCH_,mCH_,mS_]:=(mW^2 gCH)/(2CW^2 mCH^2) FH[mCH,mS]



Ahgaga[ghtt_,ghbb_,ghWW_,mS_,gCH_,mCH_]:=Af[ghtt,ghbb,mS]+AhW[ghWW,mS]+AHc[gCH,mCH,mS]

(*Decay width of Scalar boson into photon-photon*)

WidthHgaga[ghtt_,ghbb_,ghWW_,gCH_,mCH_,mS_]:=
((GF)*(\[Alpha]em^2)*(mS^3))/(128 Sqrt[2] \[Pi]^3 )*Abs[Ahgaga[ghtt,ghbb,ghWW,mS,gCH,mCH]]^2




(************************************************************************************************************************************************************************************************************************************************************************************************************************)

(*Scalar boson decay into vector pair*)(*Definitions*)

RTW[mS_]:=-(((1-mW^2/mS^2) (47 (mW^4/mS^4)-(13 mW^2/mS^2)+2))/(2 mW^2/mS^2))-3/2 (4 (mW^4/mS^4)-6 (mW^2/mS^2)+1)*(Log[mW^2/mS^2])+
(3 (20 (mW^4/mS^4)- 8 (mW^2/mS^2)+1))/Sqrt[4 (mW^2/mS^2)-1]*ArcCos[(3 (mW^2/mS^2)-1)/(2 (mW^3/mS^3))];

RTZ[mS_]:=RTW[mS]/.mW-> mZ;

\[Delta]Z=7-(40/(3 SW^2))+160/(9 SW^4);

(*Decay width of Higgs boson into WW pair*)

WidthHWW[ghWW_,mS_]:=((ghWW^2) mS)/(512 (\[Pi]^3) (mW^4)) RTW[mS]
(**)

(*Decay width of Scalar boson into ZZ pair*)

WidthHZZ[ghZZ_,mS_]:=((ghZZ^2) mS)/(2048 (\[Pi]^3) mZ^4) \[Delta]Z RTZ[mS]

(************************************************************************************************************************************************************************************************************************************************************************************************************************)

(*\[Kappa]-factors*)

(*THDM-I couplings*)
(*ghtt[a_,tb_]:=(g/2) (mt/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghbb[a_,tb_]:=(g/2) (mb/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghtautau[a_,tb_]:=(g/2) (mtau/mW) (Cos[a]/(tb*Cos[ArcTan[tb]]))
ghWW[cba_]:=gw*mW*Sqrt[1-cba]
ghZZ[cba_]:=gz*mZ*Sqrt[1-cba]*)

(*Kappab*)

kb[ghbb_]:=Sqrt[WidthHff[ghbb, 3, mb, mb,125]/WidthHff[g mb/(2 mW), 3, mb, mb,125]]

(*RegionPlot[kappaBotINF2sig\[LessEqual] kb[ghbb[-ArcCos[c] + ArcTan[t], t]]\[LessEqual] kappaBotSUP2sig,{c,-1,1},{t,0.01,10},PlotPoints\[Rule]60]*)

(*Kappatau*)

ktau[ghtautau_]:=Sqrt[WidthHff[ghtautau, 1, mtau, mtau,125]/WidthHff[g mtau/(2 mW), 1, mtau, mtau,125]]

(*RegionPlot[kappaTauINF2sig\[LessEqual] ktau[ghtautau[-ArcCos[c] + ArcTan[t], t]]\[LessEqual] kappaTauSUP2sig,{c,-1,1},{t,0.01,10},PlotPoints\[Rule]60]*)

(*Kappatop*)

ktop[ghtt_]:=Sqrt[WidthHff[ghtt, 3, mt, mt,125]/WidthHff[g mt/(2 mW), 3, mt, mt,125]]

(*RegionPlot[kappaTopINF2sig\[LessEqual] ktop[ghtt[-ArcCos[c] + ArcTan[t], t]]\[LessEqual] kappaTopSUP2sig,{c,-1,1},{t,0.01,10},PlotPoints\[Rule]60]*)

(*KappaW*)

kW[ghWW_]:=Sqrt[WidthHWW[ghWW,125]/WidthHWW[gw*mW,125]]

(*Plot[{kappaWINF2sig, kW[ghWW[c]], kappaWSUP2sig},{c,-1,1},PlotPoints\[Rule]60]*)

(*KappaZ*)

kZ[ghZZ_]:=Sqrt[WidthHZZ[ghZZ,125]/WidthHZZ[gz*mZ,125]]

(*Plot[{kappaZINF2sig, kZ[ghZZ[c]], kappaZSUP2sig},{c,-1,1},PlotPoints\[Rule]60]*)

(*Kappa\[Gamma]*)

kgaga[ghtt_,ghbb_,ghWW_,gCH_,mCH_]:=Sqrt[WidthHgaga[ghtt,ghbb,ghWW,gCH,mCH,125]/WidthHgaga[g mt/(2 mW),g mb/(2 mW),gw*mW,0,mCH,125]]

(*RegionPlot[kappaGammaINF2sig\[LessEqual] kgaga[ghtt[-ArcCos[c] + ArcTan[t], t],ghbb[-ArcCos[c] + ArcTan[t], t],ghWW[c],0,mCH]\[LessEqual] kappaGammaSUP2sig,{c,-1,1},{t,0.01,100},PlotPoints\[Rule]60]*)

(*Kappag*)

kgluglu[ghtt_,ghbb_]:=Sqrt[WidthHgg[ghtt,ghbb,125]/WidthHgg[g mt/(2 mW),g mb/(2 mW),125]]

(*RegionPlot[kappaGluonINF2sig\[LessEqual] kgluglu[ghtt[-ArcCos[c] + ArcTan[t], t],ghbb[-ArcCos[c] + ArcTan[t], t]]\[LessEqual] kappaGluonSUP2sig,{c,-1,1},{t,0.01,10},PlotPoints\[Rule]60]*)

(************************************************************************************************************************************************************************************************************************************************************************************************************************)
End[]