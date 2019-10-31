(* Wolfram Language Package *)

(* THIS PROGRAM EVALUATE THE SIGNAL STRENGHT FROM LHC *)
(*https://arxiv.org/abs/1809.10733*)

(*Scalar boson decays into fermion pair;*)
(*Definitions;*)
\[Tau]fi::usage ="See later"
\[Tau]fj::usage ="See later"

(*Decay width of the Scalar boson into fermion pair*)
WidthHff::usage ="See later"

(*Scalar boson decay into gluon pair at one-loop level*)
(*Definitions*)
ft::usage ="See later"
fb::usage ="See later"
gt::usage ="See later"
gb::usage ="See later"

At::usage ="See later"
Ab::usage ="See later"

Ft::usage ="See later"
Fb::usage ="See later"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`RXX`Private`"]

(*Scalar boson decays into fermion pair;*)
(*Definitions;*)
\[Tau]fi[mi_,mS_] := (2 mi/mS)^2;
\[Tau]fj[mj_,mS_] := (2 mj/mS)^2;

(*Decay width of the Scalar boson into fermion pair*)
WidthHff[ghfifj_, Nc_, mi_, mj_,mS_] := (((ghfifj^2) Nc mS)/(128 \[Pi]))*((4-(Sqrt[\[Tau]fi[mi,mS]]+Sqrt[\[Tau]fj[mj,mS]])^2)^(3/2)) (Sqrt[(4-(Sqrt[\[Tau]fi[mi,mS]]-Sqrt[\[Tau]fj[mj,mS]])^2)])

(*Scalar boson decay into gluon pair at one-loop level*)
(*Definitions*)
ft[mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mt^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mt^2))^-1)])]-I \[Pi])^2;
fb[mS_]:=-(1/4) (Log[(1+Sqrt[1-(((mS^2)/(4mb^2))^-1)])/(1-Sqrt[1-(((mS^2)/(4mb^2))^-1)])]-I \[Pi])^2;
gt[mS_]:=ArcSin[Sqrt[(mS^2)/(4mt^2)]]^2;
gb[mS_]:=ArcSin[Sqrt[(mS^2)/(4mb^2)]]^2;

At[mS_]:=If[((mS^2)/4mt^2)<=1,gt[mS],ft[mS]];
Ab[mS_]:=If[((mS^2)/4mb^2)<=1,gb[mS],fb[mS]];

Ft[mS_]:=2*(mS^2/(4mt^2)+((mS^2/(4mt^2)-1)*At[mS]))*((mS^2/(4mt^2))^-2); 
Fb[mS_]:=2*(mS^2/(4mb^2)+((mS^2/(4mb^2)-1)*At[mS]))*((mS^2/(4mb^2))^-2);

End[]