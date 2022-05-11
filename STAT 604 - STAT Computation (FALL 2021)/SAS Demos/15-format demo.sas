libname mylib 'C:\Users\kinchelf\Documents\STAT604-FA20\mylib';

proc format;
	value gender 
		1 = 'Male'
		2 = 'Female';

	value $col
		'W' = 'Moon White'
		'B' = 'Sky Blue'
		'Y' = 'Sunburst Yellow'
		'G' = 'Rain Cloud Gray';

	value bmi
		0-15 ='Low'
		16-25='Medium'
		26-50='High';
run;

data mylib.carsurvey;
	set cert.cars;
	format sex gender. color $col. income dollar8.;
run;

proc print data=mylib.carsurvey;
run;

proc print data=sashelp.bmimen;
	format bmi bmi.;
run;

/***** Close SAS.  Reopen.  Run libname for mylib   ****/
/***** View permanent data set after viewing slides ****/

proc print data=mylib.carsurvey;
run;

options nofmterr; ** Run the print again;

/* Create a permanent format */
proc format library=mylib;
	value gender 
		1 = 'Male'
		2 = 'Female';

	value $col
		'W' = 'Moon White'
		'B' = 'Sky Blue'
		'Y' = 'Sunburst Yellow'
		'G' = 'Rain Cloud Gray';
run;

options fmtsearch=(mylib);

proc print data=mylib.carsurvey;
run;

/* Document formats with viewer and PROC */
proc format library=mylib fmtlib;
	select $col;
run;
