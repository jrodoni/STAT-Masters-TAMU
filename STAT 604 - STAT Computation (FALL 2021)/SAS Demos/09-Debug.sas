/* Know thy data */
proc contents data=cert.invent;
run;

/* Use the debugger to examine the PDV in DATA Step operations */
data work.update; */ debug;
	set cert.invent;
	total=instock+backorg;
	saleprice=(CostPerUnit*.65) + CostPerUnit;
	format CostPerUnit saleprice dollar6.2;
run;
*******Return to slide deck before running the code below ******;
/* Compare Put with Putlog */
data _null_;
   file print;
   put 'This will go to the default output destination';
   putlog 'This should only appear in the log';
   put 'ERROR: This is not really an error!';
   putlog 'ERROR: Errors are Red';
   putlog 'NOTE: Notes are Blue';
   putlog 'WARNING: Warnings are Green with PUTLOG, too';
run;

/* Use Putlog to examine the PDV in DATA Step operations */
data work.update; 
	putlog 'Iteration ' _n_;
	putlog 'PDV before SET';
	putlog _all_;
	set cert.invent (obs=2);
	putlog 'PDV after SET';
	putlog _all_;
	total=instock+backord;
	saleprice=(CostPerUnit*.65) + CostPerUnit;
	format CostPerUnit saleprice dollar6.2;
run;
