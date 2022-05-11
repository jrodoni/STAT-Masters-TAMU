/* View slides about formats */
/* Look at formats applied to the wrong interval */
proc print data=datetimes;
	format time3n dtn3 dtn4 date9.;
run;


/* Demonstrate lengths of different date formats 3, 10, 17, 31*/
proc print data = cert.aprbills;
	format datein weekdate3. dateout worddate3.;
run;


/* Create Conversion Data set*/
data convtest;
	Number=4;
	Character='4';
	BigNum=1234567.89;
	format Bignum dollar13.2;
	BigNum2Char=put(BigNum, dollar13.2);
run;


proc print data=convtest;
	where number = character;
	*where number=input(character,1.);
run;

proc print data=convtest;
	where character=put(number,1.);
	format bignum 13.2;
run;

/* Compare automatic and explicit conversion to character */
data newtemp;
	set cert.temp;
	region=substr(site,1,2);
	assignment1 = site||'/'||dept;
	assignment2 = put(site,2.)||'/'||dept;
run;

/* Can we change the type of an existing variable */
data convtest2;
	set convtest;
	character=input(character, 1.);
run;

/* demonstrate date functions */
data temptimes;
	set cert.temp;
	keep startdate enddate days: birthdate startday age: p_:;
	format startdate enddate birthdate  date9.;

	/* What day of week did they start */
	startday=weekday(startdate);

	/* How many days between start and end? */
	days2=datdif(startdate,enddate, '30/360');
	days3=datdif(startdate,enddate, 'ACT/ACT');
	daysa=enddate-startdate;

	/* How old when they started */
	age=yrdif(birthdate, startdate, 'AGE');
	age2=intck('year', birthdate, startdate, 'C');

	/* When does probationary period end */
	p_first=intnx('month', startdate, 3,'beginning');
	p_end=intnx('month', startdate, 3,'end');
	p_same=intnx('month', startdate, 3,'same');
	format p_: date9.;
run;

/* Comparison of yrdif basis */
data _null_;
   sdate='16oct1998'd;
   edate='16feb2010'd;
   y30360=yrdif(sdate, edate, '30/360');
   yactact=yrdif(sdate, edate, 'ACT/ACT');
   yact360=yrdif(sdate, edate, 'ACT/360');
   yact365=yrdif(sdate, edate, 'ACT/365');
   yage=yrdif(sdate, edate, 'AGE');
   put y30360= / yactact= / yact360= / yact365= / yage=;
run;
