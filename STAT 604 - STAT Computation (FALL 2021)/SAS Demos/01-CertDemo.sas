/* Prep Guide Page 29 */
proc contents data=cert._all_ nods;
run;

/* View contents of automatic sashelp library */
proc contents data=sashelp._all_ nods;
run;

/* Prep Guide Page 31 */
proc contents data=cert.amounts varnum;
run;

/* Create data set in the work library (page 9)*/
data work.admit2;
	set cert.admit;
	where age > 39;
run;

/* examine descriptor portion of new data set */
proc contents data=admit2;
run; 

/* examine data portion of new data set (page 10)*/
proc print data=work.admit2;
run; 

/* examine descriptor portion of data set with index*/
proc contents data=sashelp.feeder;
run; 

