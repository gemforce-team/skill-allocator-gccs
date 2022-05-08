(* ::Package:: *)

BeginPackage["GC2`"];


(* Gem growths *)
gl=Log2[1.38];
gb=Log2[1.09];
gm=0.625; (* 8c *)

gc=Log2[1.38];
gd=Log2[1.58];
gk=1.41; (* 11c *)

gM=1/(1-gm);

(* Talismans *)
ta=15;


Begin["`Private`"];


GC2`TCp[tc_]:=1+0.03*(tc+ta);
GC2`TCs[tc_]:=1/2(1+(1/10)*Quotient[tc+ta,3]);
GC2`TCd[tc_]:=(1.2+(1/10)*Quotient[tc+ta,3]);
GC2`Component[x_]:=(1+4/100*(x+ta))*(1+5/100*Quotient[x+ta,3]);
GC2`Leech[l_]:=Component[l];
GC2`Bbound[b_]:=Component[b];
GC2`Crit[c_]:=Component[c];
GC2`Traps[t_]:=(1.8+5/100(t+ta));
GC2`Reson[r_]:=(1+4.5/100(r+ta));
GC2`AmpsSpecial[a_]:=0.15+0.004*Quotient[a+ta,3];
GC2`AmpsDamage[a_]:=0.20+0.004*Quotient[a+ta,3];
GC2`AmpsSpeed[a_]:=0.10+0.004*(a+ta);


(* We assume the player uses the mg 2048 spec at Amps 120 *)
GC2`Sla = 45.314303; (* Leech value of the amps *)
GC2`Slg = 27.517235; (* Leech value of the gem *)
GC2`Sbg = 2.724008; (* Bbound value of the gem *)
GC2`Gal = 2.5; (* The average mana amp sees 2.5 gems, seems reasonable *)
GC2`Qal = 2; (* There are 2 amps for every gem, one each side, more or less *)
GC2`AmpsMana[tc_, a_] := 1 + Qal * Gal * AmpsSpecial[a] * TCp[tc]/TCs[tc] * Sla/Slg;

(* We assume the player uses the kg 2048 spec at Amps 120 *)
GC2`Sda = 27.742752; (* Damage value of the amps *)
GC2`Sdg = 179.786041; (* Damage value of the gem *)
GC2`Sca = 11.719542; (* Damage value of the amps *)
GC2`Scg = 23.138496; (* Damage value of the gem *)
GC2`Sbg = 2.780091; (* Bbound value of the gem *)
GC2`Gac = 1; (* Single gem in tower *)
GC2`Qac = 8; (* Surrounded by 8 amps *)
GC2`AmpsKill[tc_, a_] := (1 + Qac * Gac * AmpsDamage[a] * TCp[tc]/TCd[tc] * Sda/Sdg)*
(1 + Qac * Gac * AmpsSpecial[a] * TCp[tc]/TCs[tc] * Sca/Scg);


GC2`Mana[tc_, b_, l_, t_, a_] := TCs[tc]^2*Bbound[b]*Leech[l]*Traps[t]*AmpsMana[tc, a];
GC2`Kill[tc_, b_, c_, r_, a_] := TCd[tc]*TCs[tc]^3*Bbound[b]^2*Crit[c]*Reson[r]*AmpsKill[tc, a];
GC2`GlobalPower[tc_, b_, l_, t_, c_, r_, a_] := Mana[tc, b, l, t, a]^(gM*gk)*Kill[tc, b, c, r, a];


End[]; (* Private *)
EndPackage[];
