
ods html; ods graphics on;
*ffact_inj.sas
* A fractional factorial design (2^8-4);
option ls=75 ps=55 nocenter nodate formdlim='*';
data chem;
input v1 $ v2 $ v3 $ v4 $ v5 $ Y;
cards;
L L L H H 0.78
H L H H H 1.10
H H L L L 1.70
H L H L L 1.28
L H L L H 0.97
L L H L H 1.47
L H L H L 1.85
H H H H L 2.10
L H H H H 0.76
H H L H H 0.62
L L H H L 1.09
L L L L L 1.13
H L L L H 1.25
H H H L H 0.98
H L L H L 1.36
L H H L L 1.18
run;
proc print;
run;
proc glm;
class v1-v5;
model Y = v1-v5 v1*v2 v1*v3 v1*v4 v1*v5 v2*v3 v2*v4 v2*v5 v3*v4 v3*v5 v4*v5;
LSMEANS v1-v5 v1*v2 v1*v3 v1*v4 v1*v5 v2*v3 v2*v4 v2*v5 v3*v4 v3*v5 v4*v5;
estimate 'v1' v1 1 -1;
estimate 'v2' v2 1 -1;
estimate 'v3' v3 1 -1;
estimate 'v4' v4 1 -1;
estimate 'v5' v5 1 -1;
estimate 'v1*v2' v1*v2 1 -1 -1 1;
estimate 'v1*v3' v1*v3 1 -1 -1 1;
estimate 'v1*v4' v1*v4 1 -1 -1 1;
estimate 'v1*v5' v1*v5 1 -1 -1 1;
estimate 'v2*v3' v2*v3 1 -1 -1 1;
estimate 'v2*v4' v2*v4 1 -1 -1 1;
estimate 'v2*v5' v2*v5 1 -1 -1 1;
estimate 'v3*v4' v3*v4 1 -1 -1 1;
estimate 'v3*v5' v3*v5 1 -1 -1 1;
estimate 'v4*v5' v4*v5 1 -1 -1 1;
run;
ods graphics off;ods html close;
