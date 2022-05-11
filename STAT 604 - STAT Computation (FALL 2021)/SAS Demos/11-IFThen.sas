***********************************************************;
*  Conditional Processing with IF-THEN/ELSE               *; 
***********************************************************;
*  Syntax and Example                                     *;
*                                                         *;
*  IF expression THEN statement;                          *;
*  ELSE IF expression THEN statement;                     *;
*  ELSE statement;                                        *;
*                                                         *;
*  Setting length for new character columns:              *;
*   LENGTH charvar $ length;                              *;
***********************************************************;

data cars2;
    set sashelp.cars;
    if MSRP<20000 then Cost_Group=1;
    else if MSRP<40000 then Cost_Group=2;
    else if MSRP<60000 then Cost_Group=3;
    else Cost_Group=4;
	keep Make Model Type MSRP Cost_Group;
run;

/* Demo of length statement */
data cars2;
    set sashelp.cars;

    if MSRP<60000 then CarType="Basic";
    else CarType="Luxury";
    keep Make Model MSRP CarType;

run;

    length CarType $ 6;

/* Multiple actions on one condition */
data under40 over40;
    set sashelp.cars;
	keep Make Model MSRP Cost_Group;
    if MSRP<20000 then do;
       Cost_Group=1;
       output under40;
    end;
    else if MSRP<40000 then do;
       Cost_Group=2;
       output under40;
    end;
    else do;
       Cost_Group=3;
       output over40;
    end;
run;
