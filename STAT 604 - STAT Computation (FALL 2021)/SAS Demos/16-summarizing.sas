/* Run the program and examine results.  Troubleshoot with debugger */
data fees;
	set cert.admit;
	ytd_fees=ytd_fees+Fee;
run;

/* Activity:  Add a statement to fix the problem */
data fees;
	set cert.admit;

	ytd_fees=ytd_fees+Fee;
run;

/* Test concept on different data */
data amounts;
	set cert.amounts;
	retain tot_amount 0;
	tot_amount=tot_amount+amount;
run;

/* Rewrite step with a single expression */
data amounts;
	set cert.amounts;
	tot_amount+amount;
run;

/**** Identifying First. and Last. ****/
proc sort data=cert.admit out=admit_sort;
	by actlevel;
run;

data admit_by /debug;
	set admit_sort;
	by actlevel;
run;

/* Output only the last observation per group */
data fees;
	set admit_sort;
	by actlevel;
	ytd_fees+Fee;
	if last.actlevel;
run;

/* move this line to the program above */
	if first.actlevel then ytd_fees=0;

/* Compare dedup with sort vs. data step 
	2. nodupkey
	3. remove date
	4. restore date, remove nodup     */

proc sort data=cert.amounts out=amounts_nodup noduprec;
	by name date;
run;

data amounts_first;
	set amounts_nodup;
	by ???;
	if first.???;
run;


data amounts_last;
	set amounts_nodup;
	by name;
	if last.name;
run;


/**** Demonstrate Multiple By variables ****/
proc sort data=cert.admit out=admit_mf;
	by actlevel sex;
run;

data admit_by ;
	set admit_mf;
	by actlevel sex;
	putlog actlevel= sex= first.actlevel= last.actlevel= first.sex= last.sex=;
run;

/* Output only the last observation per group */
data fees_mf;
	set admit_mf;
	by actlevel sex;
	if first.actlevel then ytd_fees=0;
	if first.sex then mf_fees=0;
	ytd_fees+Fee;
	mf_fees+Fee;
	if last.sex;
run;

/* How can we effeciently remove the unwanted columns? */

data male female;
	set cert.admit (drop=sex height weight);
	if sex='F' then output female;
	else if sex='M' then output male;
run;
