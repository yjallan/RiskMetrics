libname CRSP "C:\Users\yjallan3\OneDrive - Georgia Institute of Technology\YASHO\Acads_Gatech\4th Sem - Fall 2017\MGT 6090 Management of Financial Instituions\Datasets\CRSP";
%let path = C:\Users\yjallan3\OneDrive - Georgia Institute of Technology\YASHO\Acads_Gatech\4th Sem - Fall 2017\MGT 6090 Management of Financial Instituions\HWs\HW 7\R Code;

/*My generated random year is 2001 in the R code */
/*Getting the DSF Data between 2000 and 2010*/
data DSF;
set CRSP.dsf (keep = date PERMNO RET);
if 2000 <= year(date) <= 2010;	
run;

/*Sorting the data*/
proc sort data = DSF;
by permno date;
run;

/*Creating a dataset of all distinct permno*/
proc sql;
create table distinct_PERMNO as 
select distinct permno
from DSF;
quit;

/*Random Sampling 100 permnos*/
proc surveyselect data=distinct_PERMNO
   method=srs n=100 out=hundred_permno;
run;

/*Using the Random Sample of 100 Permnos to subset the DSF data and storing it in my Folder*/
proc sql;
create table Required_DSF as
select A.*
from DSF as A,hundred_permno as B
where A.permno=B.permno
order by permno,date;
quit;

proc export data=Required_DSF
   outfile='C:\Users\yjallan3\OneDrive - Georgia Institute of Technology\YASHO\Acads_Gatech\4th Sem - Fall 2017\MGT 6090 Management of Financial Instituions\HWs\HW 7\R Code\Subsetted_DSF.csv'
   dbms=csv
   replace;
run;

