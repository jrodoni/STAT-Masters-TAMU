ODS HTML;ODS GRAPHICS ON;
option ls=80 ps=55 nocenter nodate; 
title 'ASSIGNMENT 10 - PROBLEM IV';

data yields; 
INPUT W $ N $ F $ P $ Y @@; 
LABEL W = 'WATER' N = 'NITROGEN' F = 'FIELD' P = 'PHOSPHORUS' Y = 'EFFICIENCY';
cards; 
16  0  F1  P1  8.1    
16  0  F1  P2  9.7
16 130 F1  P1 36.0  
16 130 F1  P2 34.2
16 260 F1  P1 34.6  
16 260 F1  P2 34.0
16  0  F2  P1  8.6   
16  0  F2  P2 15.5
16 130 F2  P1 34.5  
16 130 F2  P2 33.1
16 260 F2  P1 40.7  
16 260 F2  P2 39.3
22  0  F1  P1 10.0  
22  0  F1  P2  6.2
22 130 F1  P1 21.5  
22 130 F1  P2 19.7
22 260 F1  P1 30.7  
22 260 F1  P2 28.9
22  0  F2  P1  5.1   
22  0  F2  P2 10.9
22 130 F2  P1 19.9  
22 130 F2  P2 21.9
22 260 F2  P1 26.4  
22 260 F2  P2 25.7
28  0  F1  P1 10.6  
28  0  F1  P2  6.3
28 130 F1  P1 19.4  
28 130 F1  P2 19.7
28 260 F1  P1 23.2  
28 260 F1  P2 23.0
28  0  F2  P1  4.5   
28  0  F2  P2 10.4
28 130 F2  P1 21.7  
28 130 F2  P2 19.9
28 260 F2  P1 19.4  
28 260 F2  P2 23.2

RUN;
proc mixed cl alpha=.05 method=type3; 
class P N W F;
model  Y =  P W N W*N  W*P N*P  W*N*P/RESIDUALS;
RANDOM F F*P;
LSMEANS P|W|N/ADJUST=TUKEY;
run;
proc mixed cl alpha=.05 method=reml; 
class P N W F;
model  Y =  P W N W*N  W*P N*P  W*N*P/RESIDUALS;
RANDOM F F*P;
LSMEANS P|W|N/ADJUST=TUKEY;
run;
ODS GRAPHICS OFF;ODS HTML CLOSE;
