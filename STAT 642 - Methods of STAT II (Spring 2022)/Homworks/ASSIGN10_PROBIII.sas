OPTIONS PS=50 LS=90 nocenter nodate;

ODS HTML; ODS GRAPHICS ON;

TITLE 'ASSIGNMENT 10 - PROBLEM III ';
DATA NEW;
INPUT BLOCK $ VOLT  $ Y @@;
X=LOG(Y);
CARDS;

B1 V24 .      B1 V28 .      B1 V32 38.19 B1 V36 .     B1 V40 5.44 B1 V44 1.96 B1 V48  0.55
B2 V24 220.22 B2 V28 .      B2 V32 .     B2 V36 7.66  B2 V40 .    B2 V44 2.54 B2 V48  0.67
B3 V24 270.85 B3 V28 200.67 B3 V32 .     B3 V36 .     B3 V40 6.24 B3 V44 .    B3 V48  0.76
B4 V24 360.14 B4 V28 170.52 B4 V32 45.43 B4 V36 .     B4 V40 .    B4 V44 3.22 B4 V48  .
B5 V24 .      B5 V28 220.12 B5 V32 56.74 B5 V36 9.32  B5 V40 .    B5 V44 .    B5 V48  0.61
B6 V24 300.66 B6 V28 .      B6 V32 55.34 B6 V36 10.41 B6 V40 7.19 B6 V44 .    B6 V48  .
B7 V24 .      B7 V28 190.78 B7 V32 .     B7 V36 8.74  B7 V40 6.92 B7 V44 2.21 B7 V48  .
RUN;
PROC GLM ;
CLASS BLOCK VOLT;
MODEL X = BLOCK VOLT ;
RANDOM BLOCK;
means block volt; 
LSMEANS  VOLT/ADJUST=TUKEY;
CONTRAST 'LINEAR' VOLT -3 -2 -1 0 1 2 3;
ESTIMATE 'LINEAR' VOLT -3 -2 -1 0 1 2 3;
RUN;
PROC GLM ;
CLASS BLOCK VOLT;
MODEL X = VOLT BLOCK ;
RANDOM BLOCK;
means block volt; 
LSMEANS  VOLT/ADJUST=TUKEY;
run;
PROC MIXED CL ALPHA=.05 METHOD=TYPE3;
CLASS BLOCK VOLT;
MODEL X = VOLT /RESIDUALS;
RANDOM BLOCK;
LSMEANS  VOLT/ADJUST=TUKEY;
CONTRAST 'LINEAR' VOLT -3 -2 -1 0 1 2 3;
ESTIMATE 'LINEAR' VOLT -3 -2 -1 0 1 2 3;
RUN;
ODS GRAPHICS OFF; ODS HTML CLOSE;
