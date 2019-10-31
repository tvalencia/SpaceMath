(* Wolfram Language Package *)


(*value of masses(units given in GeV )*)
(*Reference: M. Tanabashi et al. (Particle Data Group), Phys. Rev. D 98, 030001 (2018).*)

mtau::usage = "tau mass"
mmu::usage="muon mass"
me::usage="electron mass"
mu::usage="up quark mass"

md::usage="down quark mass" 
mc::usage="charm quark mass"
ms::usage="strange quark mass"
mt::usage="top quark mass"
mb::usage="bottom quark"
mh::usage="Higgs boson mass"
mW::usage="W gauge boson mass"
mZ::usage="Z gauge boson mass"

(*value of constants*)
vev::usage="vacumm expectation value"
\[Alpha]em::usage="structure fine constant"
ee::usage="electric charge"
g::usage="2 mW/vev"
\[Alpha]s::usage="strong alpha"
CW::usage="cosine of Weinberg angle"
SW::usage="sine of Weinberg angle"
gw::usage="Weak constant coupling (W)"
gz::usage="Weak constant coupling (Z)"
ge::usage="electric charge"
GF::usage="Fermi constant"

(*value of bounds*)
(* Higgs data *)
(*Epsilon to 2\[Sigma]*)
(*Reference: P. P. Giardino, K. Kannike, I. Masina, M. Raidal, and A. Strumia, J. High Energy Phys. 05 (2014) 046.*)
EpstopSUP::usage="See later."
EpstopINF::usage="See later."
EpsbotSUP::usage="See later."
EpsbotINF::usage="See later."
EpstauSUP::usage="See later."
EpstauINF::usage="See later."
EpsZSUP::usage="See later."
EpsZINF::usage="See later."
EpsWSUP::usage="See later."
EpsWINF::usage="See later."

(*Signal Strengths*)
(*Reference: ARXIV:1809.10733*)
(*central values for gluon production*)
Rbb::usage="See later."
Rtautau::usage="See later."
Rww::usage="See later."
Rzz::usage="See later."
Rgammagamma::usage="See later."

(*Signal Strengths to 2\[Sigma]*)
(*Reference: ARXIV:1809.10733*)
RbbSUP2sig::usage="See later."
RbbINF2sig::usage="See later."
RtautauSUP2sig::usage="See later."
RtautauINF2sig::usage="See later."
RwwSUP2sig::usage="See later."
RwwINF2sig::usage="See later."
RzzSUP2sig::usage="See later."
RzzINF2sig::usage="See later."
RgammagammaINF2sig::usage="See later."
RgammagammaSUP2sig::usage="See later."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Values`Private`"]

mtau = 1.77686; (*tau mass*)
mmu = 0.1056583745; (*muon mass*) 
me = 0.0005109989461; (*electron mass*)
mu = 0.0022; (*up quark mass*)
md = 0.0047; (*down quark mass*) 
mc = 1.275; (*charm quark mass*)
ms = 0.095; (*strange quark mass*)
mt = 173.21; (*top quark mass*) 
mb = 4.18; (*bottom quark*) 
mh = 125.18; (*Higgs boson mass*)
mW = 80.379; (*W gauge boson mass*)
mZ = 91.1876; (*Z gauge boson mass*)

(*value of constants*)
vev = 246; (*vacumm expectation value*)
\[Alpha]em =1/137; (*structure fine constant*)
ee=Sqrt[4*\[Pi]*\[Alpha]em ]; (*electric charge*)
g= 2 mW/vev;
\[Alpha]s=0.11; (*strong alpha*)
CW=mW/mZ; (*cosine of Weinberg angle*)
SW=Sqrt[1-(CW^2)]; (*sine of Weinberg angle*)
gw=ge/SW; (*Weak constant coupling (W)*)
gz=gw/CW; (*Weak constant coupling (Z)*)
ge=Sqrt[4 \[Pi] \[Alpha]em]; (*electric charge*)
GF=1.16637^-5; (*Fermi constant*)

(*value of bounds*)
(* Higgs data *)
(*Epsilon to 2\[Sigma]*)
(*Reference: P. P. Giardino, K. Kannike, I. Masina, M. Raidal, and A. Strumia, J. High Energy Phys. 05 (2014) 046.*)
EpstopSUP=0.01;
EpstopINF=-0.43;
EpsbotSUP=-0.19+0.28;
EpsbotINF=-0.19-0.28;
EpstauSUP=-0.03+0.17;
EpstauINF=-0.03-0.17;
EpsZSUP=0+0.1;
EpsZINF=0-0.1;
EpsWSUP=-0.2+0.13;
EpsWINF=-0.2-0.13;

(*Signal Strengths*)
(*Reference: ARXIV:1809.10733*)
(*central values for gluon production*)
Rbb = 1.02;
Rtautau = 1.11;
Rww = 1.08;
Rzz = 1.19;
Rgammagamma = 1.10;

(*Signal Strengths to 2\[Sigma]*)
(*Reference: ARXIV:1809.10733*)
RbbSUP2sig=1.32;
RbbINF2sig=0.72;
RtautauSUP2sig=1.45;
RtautauINF2sig=0.77;
RwwSUP2sig=1.4202;
RwwINF2sig=0.739804;
RzzSUP2sig=1.42007;
RzzINF2sig=0.959928;
RgammagammaINF2sig=0.909912;
RgammagammaSUP2sig=1.29009;


End[]