ods html;ods graphics on;
option ls=80 ps=50 nocenter nodate;
title 'PROBLEM 1 Assignment 7 Spring 2022';

data old;
array Y Y1-Y4; INPUT C $ N $  Y1-Y4;  
TRT=COMPRESS (C) || COMPRESS (N); 
     label C = 'CROP' N = 'NITROGEN' R = 'ACE-REDUCTION' X = 'LOGARITHM OF ACE-REDUCTION'
            TRT = 'CROP-NITRATE';
do over Y;
R=Y;
W=Y;
X=log(R);
output; end;
      drop  Y1-Y4;
cards;
A 0    2.6 1.1 0.9 1.2    
A 50   1.5 1.8 0.7 2.2
A 100  0.6 1.3 1.9 2.6
S 0    6.5 2.6 3.9 4.3
S 50   0.6 0.6 0.3 0.8
S 100  0.5 0.1 0.1 0.3
G 0    0.5 0.9 0.7 0.7
G 50   0.3 0.5 0.4 0.4
G 100  0.2 0.1 0.1 0.2
M 0    0.8 0.9 2.2 1.2
M 50   0.7 0.7 0.5 0.6
M 100  0.3 0.4 0.2 0.2

Run;
data new;set old;
I1=0;I2=0;I3=0;I4=0;I5=0;I6=0;I7=0;I8=0;I9=0;I10=0;I11=0;
If TRT='A0' then I1=1;
If TRT='S0' then I2=1;
If TRT='G0' then I3=1;
If TRT='M0' then I4=1;
If TRT='A50' then I5=1;
If TRT='S50' then I6=1;
If TRT='G50' then I7=1;
If TRT='M50' then I8=1;
If TRT='A100' then I9=1;
If TRT='S100' then I10=1;
If TRT='G100' then I11=1;
run;
proc glm data=old order=data;
class C N;
model R = C N  C*N/ss3;
lsmeans C N C*N/stderr pdiff adjust=tukey;
output out=ASSUMP r=RESID p=MEANS;
PROC PLOT; PLOT RESID*MEANS=C/vref=0;
PROC PLOT; PLOT MEANS*N=C;
proc univariate def=5 plot normal; var RESID;

PROC GLM data=old order=data;
CLASS TRT;
MODEL R = TRT/SS3;
CONTRAST  'LIN-A VS S' TRT    -1  0  1  1  0 -1  0  0  0  0 0  0;
CONTRAST  'LIN-A VS G'  TRT   -1  0  1  0  0  0  1  0 -1  0 0  0;
CONTRAST  'LIN-A VS M'  TRT   -1  0  1  0  0  0  0  0  0  1 0 -1;
CONTRAST  'LIN-S VS G'  TRT    0  0  0 -1  0  1  1  0 -1  0 0  0;
CONTRAST  'LIN-S VS M' TRT     0  0  0 -1  0  1  0  0  0  1 0 -1;
CONTRAST  'LIN-G VS M'  TRT    0  0  0  0  0  0 -1  0  1  1 0 -1;
MEANS TRT/hovtest=bf;
RUN;
proc glm data=new order=data;
class C N;
model X = C N  C*N/ss3;
lsmeans C N C*N/stderr pdiff;
output out=ASSUMP r=RESID p=MEANS;
PROC PLOT; PLOT RESID*MEANS=C/vref=0;
PROC PLOT; PLOT MEANS*N=C;
proc univariate def=5 plot normal; var RESID;

PROC GLM data=new order=data;
CLASS TRT;
MODEL X = TRT/SS3;
CONTRAST  'LIN-A VS S' TRT    -1  0  1  1  0 -1  0  0  0  0 0  0;
CONTRAST  'LIN-A VS G'  TRT   -1  0  1  0  0  0  1  0 -1  0 0  0;
CONTRAST  'LIN-A VS M'  TRT   -1  0  1  0  0  0  0  0  0  1 0 -1;
CONTRAST  'LIN-S VS G'  TRT    0  0  0 -1  0  1  1  0 -1  0 0  0;
CONTRAST  'LIN-S VS M' TRT     0  0  0 -1  0  1  0  0  0  1 0 -1;
CONTRAST  'LIN-G VS M'  TRT    0  0  0  0  0  0 -1  0  1  1 0 -1;
MEANS TRT/hovtest=bf;
RUN;
proc transreg details data=new ss2
                 plots=(transformation(dependent) obp);
      model BoxCox(R / convenient lambda=-2 to 2 by 0.001) =
            identity(I1 I2 I3 I4 I5 I6 I7 I8 I9 I10 I11);
   run;
   ods graphics off;ods html close;
