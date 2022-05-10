ods html; ods graphics on;
option ls=90 ps=50 nocenter nodate formdlim='*';
title 'Assignment 10 - Problem I ';
data NEW;
INPUT F $ NI $ D $ TRT $ Y @@;
CARDS;
1  N E  CE  21.4 1  N O CO   50.8 1  N L CL   53.2
1 .5 E  E   54.8 1 .5 O O    56.9 1 .5 L L  57.7
2  N E  CE  11.3 2  N O CO   42.7 2  N L CL  44.8
2 .5 E  E   47.9 2 .5 O O    46.8 2 .5 L L 54.0
3  N E  CE  34.9 3  N O CO   61.8 3  N L CL 57.8
3 .5 E  E   40.1 3 .5 O O    57.9 3 .5 L L 62.0

RUN;
proc MIXED CL ORDER=DATA METHOD=TYPE3;
class NI D F;
model Y = NI D NI*D/residuals;
random F;
LSMEANS NI D NI*D/ADJUST=TUKEY;
run;

proc MIXED CL ORDER=DATA;
class TRT F;
model Y =TRT/residuals;
random F;
LSMEANS TRT/ADJUST=TUKEY;
run;
ods graphics off; ods html close;
