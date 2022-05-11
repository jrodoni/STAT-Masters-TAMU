***********************************************************;
*  Executing an Iterative DO Loop                         *;
***********************************************************;
*  Syntax                                                 *;
*                                                         *;
*    DATA output-table;                                   *;
*        . . .                                            *;
*        DO index-column = start TO stop <BY increment>;  *;
*           . . . repetitive code . . .                   *;
*        END;                                             *;
*        . . .                                            *;
*    RUN;                                                 *;
***********************************************************;

***********************************************************;
*  Demo                                                   *;
*  1) Run the program and view the Forecast output table. *;
*     Notice that there are three rows (Year 1, 2, and 3) *;
*     for each combination of Region, Product, and        *;
*     Subsidiary.                                         *;
*  2) Return to the Program tab and click the Toggle DATA *;
*     Step Debugger toolbar button to enable debugging in *;
*     the program if it is not already enabled. Click the *;
*     Debugger icon next to the DATA statement. The DATA  *;
*     Step Debugger window appears.                       *;
*  3) Click the Step execution to next line toolbar       *;
*     button to execute the highlighted SET statement.    *;
*  4) Click the button again to execute the highlighted   *;
*     DO statement. Notice that the Year value has been   *;
*     set to 1.                                           *;
*  5) Click the button three times to execute the         *;
*     statements inside the DO loop and the END           *;
*     statement. Notice that the Year value has been      *;
*     incremented to 2 and that processing returns to the *;
*     inside of the DO loop.                              *;
*  6) Continue to click the button to execute the         *;
*     highlighted statements inside the DO loop. Observe  *;
*     the changing of values in the PDV.                  *;
*  7) At the end of third iteration of the DO loop,       *;
*     notice that the Year value is incremented to 4 and  *;
*     that processing does not return to the inside of    *;
*     the DO loop.                                        *;
*  8) Close the DATA Step Debugger.                       *;
*  9) Change the range to a list and rerun 				  *;
***********************************************************;

data forecast /debug;
    set sashelp.shoes(rename=(Sales=ProjectedSales));
    do Year = 1 to 3;
       ProjectedSales=ProjectedSales*1.05;
       output;
    end;
    keep Region Product Subsidiary Year ProjectedSales;
    format ProjectedSales dollar10.;
run;

***********************************************************;
*  Activity 14.01                                          *;
*  1) Run the program. How much is in savings at month 12?*;
*  2) Delete the OUTPUT statement and run the program     *;
*     again.                                              *;
*  3) How many rows are created?                          *;
*  4) What is the value of Month?                         *;
*  5) What is the value of Savings?                       *;
***********************************************************;

data YearlySavings;
   Amount=200;
   do Month=1 to 12;
      Savings+Amount;
	   Savings+(Savings*0.02/12); 
	  output;
   end;
   format Savings 12.2;
run;

/* Demo of While loop */
data YearlySavingW;
   Amount=200;
   month=0;
   do while (savings < 1000);
   	  month + 1;
      Savings+Amount;
	   Savings+(Savings*0.02/12); 
	  output;
   end;
   format Savings 12.2;
run;

/* Demo of Until loop */
data YearlySavingU;
   Amount=200;
   month=0;
   do until (savings >= 1000);
   	  month + 1;
      Savings+Amount;
	   Savings+(Savings*0.02/12); 
	  output;
   end;
   format Savings 12.2;
run;

/* Modify the programs above to include interative loops with conditions */

***********************************************************;
*  Activity 14.02                                          *;
*  1) Examine the DATA step code and run the program.     *;
*     Uncomment the RETAIN statement and run the program  *;
*     again. Why is the RETAIN statement necessary?       *;
*  2) Add a subsetting IF statement to include only the   *;
*     last row per student in the output table. Run the   *;
*     program.                                            *;
*  3) What must be true of the input table for the DATA   *;
*     step to work?                                       *;
***********************************************************;
libname mylib 'C:\Users\kinchelf\Documents\';

data class_wide;
	set mylib.class_test_narrow;
	by Name;

	keep Name Math Reading;
	if TestSubject="Math" then Math=TestScore;
	else if TestSubject="Reading" then Reading=TestScore;
run;


	*retain Name Math Reading;
