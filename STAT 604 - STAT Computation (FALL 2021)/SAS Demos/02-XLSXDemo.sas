libname certxl xlsx "C:\Users\kinchelf\Documents\STAT604Data\base-guide-practice-data\cert\exercise.xlsx";

proc contents data=certxl.ActivityLevels;
run;

libname certxl clear;
