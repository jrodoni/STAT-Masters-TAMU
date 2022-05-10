ods html;ods graphics on;
* RCBD_SOYBEAN.sas;
* the following program computes AOV for RCBD ;
option ls=100 ps=60 nocenter nodate;
title 'ANALYSIS OF RB DESIGN';

data soybean;
INPUT TRT $ FIELD $ NP @@;   
      label TRT = 'TYPE OF TREATMENT' NP = 'NUMBER OF PLANTS';
cards;
CON 1F 8 CON 2F 11 CON 3F 12 CON  4F 13
AVA 1F 2 AVA 2F  5 AVA 3F  7 AVA  4F 11
SPE 1F 4 SPE 2F 10 SPE 3F  9 SPE  4F  8
SEM 1F 3 SEM 2F  6 SEM 3F  9 SEM  4F 10
FER 1F 9 FER 2F  3 FER 3F  5 FER  4F  5
run;
TITLE 'RANDOMIZED BLOCK ANALYSIS ';
proc MIXED data=soybean ORDER=DATA METHOD=TYPE3;
class TRT FIELD;
model NP = TRT/DDFM=SAT;
RANDOM FIELD;
contrast 'c1' trt   -4  1  1  1 1 ;
contrast 'c2' trt    0 -1 -1  1 1 ;
contrast 'c3' trt    0 -1  1  0 0 ;
contrast 'c4' trt    0  0  0 -1 1 ;
LSMEANS TRT/ADJUST=TUKEY;
LSMEANS TRT/ADJUST=DUNNETTL;
run;
proc plot data=soybean;
plot np*trt=field;
run;
ods graphics off;ods html close;
