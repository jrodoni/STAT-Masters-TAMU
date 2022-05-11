

filename cclass "C:\Users\kinchelf\Documents\STAT604Data\base-guide-practice-data\cert\class.txt";

/* run the code below and observe the log and the table */
proc import datafile=cclass
	dbms=tab
	out=class_import
	replace;
	delimiter='09'x;
run;

/* Look at the file in a text editor and see if we can fix the code */
proc import datafile=cclass
	dbms=dlm
	out=class_import
	replace;
	delimiter= ' ';
run;

/* Use an actual tab delimited file */
proc import datafile='C:\Users\kinchelf\Documents\PC SAS\Prog1\Data\data\storm_damage.tab'
	dbms=tab
	out=damage
	replace;
	delimiter='09'x;
run;

/* See Excel slide before running this section */

filename xlin "C:\Users\kinchelf\Documents\STAT604Data\base-guide-practice-data\cert\boots.xlsx" ;
proc import datafile= xlin
            dbms=xlsx    
            out=work.bootsales replace;
    sheet=boot;
run;

*options MSGLEVEL=I;
