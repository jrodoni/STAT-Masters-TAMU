***********************************************************;
*  Activity 15-1                                          *;
*  1) Highlight the PROC PRINT step and run the selection.*;
*     Note how many rows are in the SASHELP.CLASS table.  *;
*  2) Highlight the PROC TRANSPOSE step and run the       *;
*     selection. Answer the following questions:          *;
*     Which columns from the input table are transposed   *;
*     into rows?                                          *;
*     What does each column in the output table represent?*;
*     What is the name of the output table?               *;
***********************************************************;

proc print data=sashelp.class;
run;

/* Create a temporary copy with a label */
data class;
	set sashelp.class;
	label sex='Gender';
run;

proc transpose data=class;
run;

***********************************************************;
*  Activity   15-2                                        *;
*  1) Add the OUT= option on the PROC TRANSPOSE statement *;
*     (in the program above) to create an output table    *;
*     named CLASS_T.                                      *;
*  2) Add the following ID statement and run the step.    *;
*     What changes in the results?                        *;
*        id Name;                                         *;
*  3) Add the following VAR statement and run the step.   *;
*     What changes in the results?                        *;
*        var Height Weight;                               *;
*  4) Add sex to the var statement and compare.		      *;
*  5) Move sex to the ID statement and observe.			  *;
***********************************************************;

proc transpose data=class;
run;


***********************************************************;
*  Creating a Split Table with PROC TRANSPOSE             *;
***********************************************************;
*  Syntax                                                 *;
*                                                         *;
*    PROC TRANSPOSE DATA=input-table OUT=output-table     *;
*                   <PREFIX=column> <NAME=column>;        *;
*        <VAR columns(s)>;                                *;
*        <ID column>;                                     *;
*        <BY column(s)>;                                  *;
*    RUN;                                                 *;
***********************************************************;

***********************************************************;
*  Demo                                                   *;
*  1) Run the PROC TRANSPOSE step and examine the error   *;
*     in the log. The step fails because the values of ID *;
*     are not unique.                                     *;
*  2) Add a BY statement to transpose the values within   *;
*     the groups of Season, Basin, and Name. Run the      *;
*     program.                                            *;
*  3) Notice that the unique values of WindRank (1, 2, 3, *;
*     and 4) are assigned as the column names for the     *;
*     transposed values of WindMPH.                       *;
*  4) To give the transposed columns standard names, add  *;
*     the PREFIX=Wind option in the PROC TRANSPOSE        *;
*     statement. To rename the _name_ column that         *;
*     identifies the source column for the transposed     *;
*     values, add the NAME=WindSource option as well. Run *;
*     the step.                                           *;
*  5) Delete the NAME= option and add the DROP= data set  *;
*     option on the output table to drop the _name_       *;
*     column. Run the step.                               *;
***********************************************************;
libname pg2 'C:\Users\kinchelf\Documents\PC SAS\Prog2\Data\data';

/* KNOW YOUR DATA FIRST*/
proc transpose data=pg2.storm_top4_narrow out=wind_rotate;
	var WindMPH;
	id WindRank;
run;

/* Add grouping */
proc transpose data=pg2.storm_top4_narrow out=wind_rotate;
	var WindMPH;
	id WindRank;
	by Season Basin Name ;
run;

*** Does it make any difference if we remove ID?;
proc transpose data=pg2.storm_top4_narrow out=wind_rotate;
	var WindMPH;
	by Season Basin Name ;
run;

*** What if our ranks were in different order?;
/* KNOW DATA */
proc transpose data=pg2.storm_top4_random out=wind_rotate;
	var WindMPH;
	id WindRank; ** What if we remove this?;
	by Season Basin Name ;
run;

***********************************************************;
*  Activity 15-3                                         *;
*  1) Run the program. Notice that, by default, PROC      *;
*     TRANSPOSE transposes all the numeric columns,       *;
*     Wind1-Wind4.                                        *;
*  2) Add a VAR statement in PROC TRANSPOSE to transpose  *;
*     only the Wind1 and Wind2 columns. Run the program.  *;
*  3) What are the names of the columns that contain the  *;
*     column names and values that have been transposed?  *;
***********************************************************;

title "Storm Wide";
proc print data=pg2.storm_top4_wide(obs=5);
run;

proc transpose data=pg2.storm_top4_wide out=storm_top4_narrow;
	by Season Basin Name;
	*Add a VAR statement;
run;

title "Storm Narrow";
proc print data=storm_top4_narrow(obs=10);
run;
title;
