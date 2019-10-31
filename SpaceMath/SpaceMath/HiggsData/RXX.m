(* Wolfram Language Package *)

(* THIS PROGRAM EVALUATE THE SIGNAL STRENGHT FROM LHC *)
(*https://arxiv.org/abs/1809.10733*)

(*Scalar boson decays into fermion pair;*)
(*Definitions;*)
\[Tau]fi::usage ="See later"
\[Tau]fj::usage ="See later"

(*Decay width of the Scalar boson into fermion pair*)
WidthHff::usage ="See later"

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


End[]