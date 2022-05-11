/* Demonstrate automatic Conversion */
data convtest;
	Number=4;
	Character='4';
	BigChar='1,234';
	Char2Num=Character*1;
	BigChar2Num=BigChar*1;
run;

proc print data=convtest;
	where number='4';
run;

proc print data=convtest;
	where character=4;
run;

/* Go back to slide show */
/* Experiment with date time values */
data datetimes;
	time1 = '6:45 PM';
	time2 = '18:45';
	time3 = '18.45';
	time1n = input(time1, time.);
	time2n = input(time2, time.);
	time3n = input(time3, time.);
	dt1='20Oct2020 6:45 PM';
	dtn1=input(dt1, datetime18.);
	dt2='20Oct2020 18:45:00';
	dtn2=input(dt2, datetime18.);
	dt3='10/20/2020 6:45 PM';
	dtn3=input(dt3, anydtdte18.);
	dt4='10/20/2020 6:45 PM';
	dtn4=input(dt4, anydtdtm18.);

run;

