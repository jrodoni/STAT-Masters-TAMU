*ods html;ods graphics on;
* crab,genmod.sas
 The relationship between different habitats and the population densities of
Hermit crabs. There are 6 sites. At each site 25 transects are run and the
number of crabs are counted. Analyze using an overdispersed Poisson Model ;

option ls=120 ps=50 nocenter nodate;
title 'Hermit Crab Density';
data count;
infile 'u:\meth2\kuehl\expl4-1.dat';
input Y Site;
label Y = 'Crab Count';

title "Poisson Regression on Hermit Crab Data";
proc genmod data=count ;
class Site;
model  Y = Site/Dist=P link=log;
output out=outdata stdresdev = stdresdev p = predicted;
proc PLOT data = outdata;
plot stdresdev*Site;
run;
Title "Overdispersed Poisson Regression on Hermit Crab Data";
proc genmod data=count;
class Site;
model  Y = Site/dist=P link=log  scale = pearson;

contrast 'S2 vs S1' Site -1  1  0  0  0  0;
contrast 'S2 vs S3' Site  0  1 -1  0  0  0;
contrast 'S2 vs S4' Site  0  1  0 -1  0  0;
contrast 'S2 vs S5' Site  0  1  0  0 -1  0;
contrast 'S2 vs S6' Site  0  1  0  0  0 -10;
output out=outdata2 stdresdev = stdresdev2 p = predicted;
proc PLOT data = outdata2;
plot stdresdev2*Site;
run;
run;
*ods graphics off;
*ods html close;
