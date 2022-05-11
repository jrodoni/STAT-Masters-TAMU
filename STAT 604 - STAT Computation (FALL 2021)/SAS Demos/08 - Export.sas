/* assign a fileref.  Does file exist in OS folder?*/
filename myxlf  "/home/u58919850/STAT604/cars.xlsx";

/* export a data set to the file (Does not accept where statement)*/
proc export data=sashelp.cars /*(where =(make='Honda'))*/
			outfile=myxlf
			dbms= xlsx
			replace;
		sheet=allcars;
		
run;

/* assign a libref to the excel file */
libname myxl xlsx "/home/u58919850/STAT604/cars.xlsx";
 
/* use a DATA step to write to Excel library */
data myxl.asiacars;
    set sashelp.cars;
    where origin='Asia';
run;

/* use the contents procedure to explore library */
proc contents data=myxl._all_ ;
run;


libname myxl clear;
