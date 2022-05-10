
ODS HTML; ODS GRAPHICS ON;
option ls=75 ps=55 nocenter nodate;
title 'Problem 2 - Assignment 7 - Spring 2022';

data PROB66;
array Y Y1-Y4; INPUT S  $ F $ P $ Y1-Y4 @@;  
TRT1 = COMPRESS (S) || (F);
TRT = COMPRESS (TRT1) || (P);
do over Y;
W=Y;
output; end;
      drop  Y1-Y4;
      label S  = 'SURFACE TRT' F = 'FILLERS' P = 'PROPORTION FILLER'
            W = 'WEIGHT LOSS';
cards;
S1 F1 25 194 208 197 205
S1 F1 50 233 241 235 239
S1 F1 75 260 279 266 270
S1 F2 25 229 197 200 212
S1 F2 50 224 243 228 235
S1 F2 75 249 232 236 245
S2 F1 25 155 173 160 168
S2 F1 50 198 177 185 192
S2 F1 75 235 219 230 225
S2 F2 25 137 160 142 155
S2 F2 50 184 163 175 177
S2 F2 75 212 189 195 204
run;
TITLE 'EFFECTS MODEL ANALYSIS';
proc glm order=data;
class F S P;
model W = S F P F*S S*P F*P F*S*P/SS3;
lsmeans  S|F|P/stderr pdiff ;
lsmeans  S|F|P/stderr pdiff adjust=Tukey;
run;
TITLE 'CELL MEANS MODEL ANALYSIS';
proc glm order=data;
class TRT;
model W = TRT/SS3;
means TRT/hovtest=bf; 
lsmeans TRT/stderr;
CONTRAST  'LIN-S1F1'   TRT    -1   0  1;
CONTRAST  'LIN-S1F2'   TRT     0   0  0 -1  0  1;
CONTRAST  'LIN-S2F1'   TRT     0   0  0  0  0  0 -1  0  1;
CONTRAST  'LIN-S2F2'   TRT     0   0  0  0  0  0  0  0  0  -1 0 1;
CONTRAST  'QUA-S1F1'   TRT     1  -2  1;
CONTRAST  'QUA-S1F2'   TRT     0   0  0  1 -2  1;
CONTRAST  'QUA-S2F1'   TRT     0   0  0  0  0  0  1  -2  1;
CONTRAST  'QUA-S2F2'   TRT     0   0  0  0  0  0  0   0  0  1 -2 1;

CONTRAST  'LIN-F1'   TRT      -1   0  1  0  0  0  -1 0 1;
CONTRAST  'LIN-F2'   TRT       0   0  0 -1  0  1   0  0  0  -1  0  1;
CONTRAST  'QUA-F1'   TRT       1  -2  1  0  0  0   1 -2  1;
CONTRAST  'QUA-F2'   TRT       0   0  0  1 -2  1   0  0  0   1 -2  1;
run;
output out=ASSUMP r=RESID p=MEANS;
means trt/hovtest=bf;
proc gplot; plot resid*means;
proc univariate def=5 plot normal; var RESID;
run;
proc glimmix order=data;
class F S P;
model W = F|S|P;
lsmeans F*S*P / plot = meanplot(sliceby=F plotby=S join);
lsmeans F*S*P / plot = meanplot(sliceby=F plotby=P join);
lsmeans F*S*P / plot = meanplot(sliceby=S plotby=F join);
lsmeans F*S*P / plot = meanplot(sliceby=S plotby=P join);
lsmeans F*S*P / plot = meanplot(sliceby=P plotby=F join);
lsmeans F*S*P / plot = meanplot(sliceby=P plotby=S join);
run;
ODS GRAPHICS OFF; ODS HTML CLOSE;
