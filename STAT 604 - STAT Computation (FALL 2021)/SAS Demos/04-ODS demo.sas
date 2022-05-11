ods html close;
ods pdf (ID=both) file='both.pdf';
ods pdf (ID=one) file='one.pdf';

proc freq data=sashelp.class;
   tables sex;
run;

ods pdf (one) close;

proc means data=sashelp.class;
   var age;
run;

ods pdf (both) close;
ods html path="%qsysfunc(pathname(work))";

proc print data=sashelp.vstyle;
run;


ods html close;
title "Create a Table of Contents";
options nodate; /* Turns off printing of date at top of page */

ods pdf file="MyDefaultToc.pdf" contents=yes bookmarklist=hide
/*	pdftoc=1*/
/*	startpage=no*/
	;

/*ods proclabel "Car Origins by Type";*/

proc freq data=sashelp.cars;
   tables origin*type;
run;

/*ods proclabel "Car List";*/

proc print data=sashelp.cars;
run;

ods pdf close;

