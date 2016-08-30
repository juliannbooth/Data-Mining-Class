*Juliann Booth
Data Mining HW 28
SAS - Cows ;

*Problem 1;
*(a)Import the data;
data cows;
    infile 'C:\Users\000680344\Desktop\cows.txt' dlm=',';
    input MILK FARM $;

proc print data=cows;
run;

*Find average milk production by farm;

proc means data=cows;
by Farm;
run;

*Obtain histogram and qqplot of milk prod values at each farm;
proc univariate data=cows;
histogram MILK/normal;
qqplot MILK;
by FARM;
run;

*(b) Perform an ANOVA to test whether the average milk 
production at the three farms is the same;

proc anova data=cows;
class farm;
model milk=farm;
run;

*(c)Test using PROC GLM;
proc glm data=cows;
class farm;
model milk=farm;
run;