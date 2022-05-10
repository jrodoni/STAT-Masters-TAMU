* fishnet-random.sas;
*This is the SAS code for the fish net example;
ods rtf file = "u:/meth2/output/fishnet.rtf";
ods html; ods graphics on;
option ls=80 ps=55 nocenter nodate; 
title 'One-way ANOVA with Random Factor';

data fishnet; 
array Y Y1-Y5;
input MACH $ Y1-Y5; do over Y; S=Y; output; end;
      drop Y1-Y5;
      label MACH = 'MACHINE' S = 'STRENGTH OF MATERIAL';
cards;
M1 128 127 129 126 128
M2 121 120 123 122 125   
M3 126 125 127 125 124   
M4 125 126 129 128 127   
RUN;
title 'ANALYSIS USING PROC MIXED-TYPE3';
PROC MIXED METHOD=TYPE3 CL;
CLASS MACH;
MODEL  S = / COVB RESIDUALS;                    
RANDOM MACH/ CL ALPHA=.05;
RUN;
ods rtf close;
ods graphics off; ods html close;

