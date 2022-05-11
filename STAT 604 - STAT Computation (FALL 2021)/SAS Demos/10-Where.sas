***********************************************************;
*  Filtering Rows with Basic Operators                    *;
***********************************************************;
*  Syntax and Example                                     *;
*                                                         *;
*    WHERE expression;                                    *;
*                                                         *;
*    Basic Operators:                                     *;
*         = , EQ                                          *;
*         ^= , ~= , NE                                    *;
*         > , GT                                          *;
*         < , LT                                          *;
*         >= , GE                                         *;
*         <= , LE                                         *;
*    SAS Date Constant                                    *;
*         "ddmmmyyyy"d ("01JAN2015"d)                     *;
***********************************************************;

proc print data=sashelp.cars;
	where Type="SUV" and MSRP <= 30000;
/*	where MSRP between 28000 and 45000;*/
/*	where model contains 'convertible' and origin ne 'USA';*/
/*	where model like 'Town%';*/
/*	where model contains 'Town';*/
/*	where type in ('Truck' 'SUV');*/
/*	where origin='Europe' and type='Sedan' or MSRP > 70000;*/
run;	

***********************************************************;
*  Filtering Rows Using Macro Variables                   *;
***********************************************************;
*  Syntax and Example                                     *;
*                                                         *;
*    %LET macrovar=value;                                 *; 
*                                                         *;
*    Usage:                                               *;
*    WHERE numvar=&macrovar;                              *;
*    WHERE charvar="&macrovar";                           *;
*    WHERE datevar="&macrovar"d;                          *;
***********************************************************;

%let CarType=Wagon;

data &CarType;
	set sashelp.cars;
	where Type="&CarType";
run;

Title 'Analysis of &CarType Data';
proc print data=&CarType;
	var Type Make Model MSRP;
run;

proc means data=&CarType;
	var MSRP MPG_Highway;
run;

%let price = 25000;

data cheapcars;
	set sashelp.cars;
	where msrp < &price;
run;
