
proc sort data=cert.admit
	     out=work.admit;
   by actlevel descending age;
run;

proc print data=work.admit noobs;
	var actlevel age height weight;
	by actlevel;
	sum Fee;
	pageby actlevel;
run;
