title1 "Heart Rates for Patients with";
title3 "Increased Stress Tolerance";
footnote1 "Data from Treadmill Tests";
Footnote3 "1st Quarter Admissions";

proc print data=cert.stress;
   var resthr maxhr rechr;
   where tolerance="I";
run; 

/* From page 99.  Run separately  */
footnote;
proc means data=cert.stress;
   where tolerance="I";
   var resthr maxhr ;
run;
	
