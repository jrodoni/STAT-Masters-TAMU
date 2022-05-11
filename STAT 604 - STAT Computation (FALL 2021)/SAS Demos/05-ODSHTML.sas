ods _all_ close;
ods html path='c:\demo2020Fall\'
		file='onepage.html';

proc print data=sashelp.cars;
run;

proc means data=sashelp.cars;
   var mpg_city;
run;

ods html close;
ods html path="%qsysfunc(pathname(work))";

ods html close;
title "Create a Table of Contents";
ods html path='c:\demo2020Fall\' /*(url=none)*/
		body='framedpage.html' /*(url='framedpage.html')*/
		contents= 'toc.html' /*(url='toc.html')*/
		frame='frame.html' ;

proc print data=sashelp.cars;
run;

proc means data=sashelp.cars;
   var mpg_city;
run;

ods html close;
ods html path="%qsysfunc(pathname(work))";
