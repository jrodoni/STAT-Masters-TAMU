********** Create Data **********;
data EmpsAUUS;
   input First $ Gender $ Country $;
   datalines;
Togar	M	AU
Kylie	F	AU
Stacey	F	US
Gloria	F	US
James	M	US
;
run;

data PhoneO;
   input Country $ Phone $15.;
   datalines;
AU +61(2)5555-1500
AU +61(2)5555-1600
AU +61(2)5555-1700
US +1(305)555-1500
US +1(305)555-1600
;
run;

********** Many-to-Many (Self-Study) **********;
data EmpsOfc;
   merge EmpsAUUS PhoneO;
   by Country;
run;

proc print data=EmpsOfc;
run;

proc sql;
   create table EmpsOfc as
   select First, Gender, PhoneO.Country, Phone
   from EmpsAUUS, PhoneO
   where EmpsAUUS.Country=PhoneO.Country;
quit;

proc print data=EmpsOfc;
run;

/*  Purpose: Demonstrate the merge of 3 data sets                               */

libname mylib 'C:\Users\kinchelf\Documents\';

/*---------------------------------------------------------------------------*/
/* Merge 3 years of payroll data into a single data set keeping most recent  */
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* Payroll06 - 347 obs */
/* Payroll07 - 330 obs */
/* Payroll08 - 308 obs */
/*---------------------------------------------------------------------------*/

data payroll_hist;
	merge mylib.payroll06 mylib.payroll07 mylib.payroll08(drop=marital_status);
	by employee_id;
run;

/*---------------------------------------------------------------------------*/
/* Find out the diffence between 2006 and 2007 data sets  */
/*---------------------------------------------------------------------------*/
data diff;
	merge mylib.payroll06(in=a) mylib.payroll07(in=b) ;
	by employee_id;
	if not a or not b;
	if a then inset=2006;
	else inset=2007;
run;
