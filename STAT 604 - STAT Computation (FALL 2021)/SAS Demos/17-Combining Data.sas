proc sort data=cert.measure out=measure;
	by id;
run;

data one2one;
	set cert.patients;
	if age < 60 ;
	set measure;
run;

data one2one ;
	set cert.patients;
	putlog "First Set" _all_;
	if age < 60 ;
	set measure;
	putlog 'Second Set' _all_;
run;

data one2onem;
	merge cert.patients measure;
run;

********** Create Data **********;
data EmpsAU;
   input First $ Gender $ EmpID;
   datalines;
Togar	M	121150
Kylie	F	121151
Birin	M	121152
;
run;

data Phones;
   input EmpID Type $ Phone $15.;
   datalines;
121150 Home +61(2)5555-1793
121150 Work +61(2)5555-1794
121151 Home +61(2)5555-1849
121152 Work +61(2)5555-1850
121152 Home +61(2)5555-1665
121152 Cell +61(2)5555-1666
;
run;

********** One-to-Many Merge **********;
data Empphones;
   merge EmpsAU Phones;
   by EmpID;
run;

proc print data=Empphones;
run;
