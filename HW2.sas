*Juliann Booth - Data Mining II - SAS HW 2
*
*Linear regression and model diagnostics;


*Importing data;


data math5305lab6;
    infile 'C:\Users\000680344\Desktop\math5305Lab6Data.txt' dlm=',';
    input Y X1 X2 X3;
proc print data=math5305lab6;
run;

*(a) Fit the regression model;
*(b) get the estimates;
*(c) get the t-stat and p=vals;
proc reg data=math5305lab6;
    model Y=X1 X2 X3;
run;

*(d) Find an appropriate test-stat and p-val to test
H0: B_1 = B_2 = B_3 = 0;

*(e) Find an R^2 for this model;

*(f) Investigate normality of the residuals using a 
qq plot and shapiro-wilk test;
proc reg data=math5305lab6;
    model Y=X1 X2 X3;
	output out=myoutput
	r=myresiduals;
run;

proc print data=myoutput;
run;


*Testing normality of residuals;
proc univariate data=myoutput normal;
var myresiduals;
run;

*Histogram and qqplot for residuals;
proc univariate data=myoutput;
histogram myresiduals/normal;
qqplot myresiduals;
run;
**Do Shapiro-Wilk test!!;

*(g) Use the /SPEC option;
*Testing homoscedasticity of residuals (constancy of error variance);
proc reg data=math5305lab6;
    model Y=X1 X2 X3/SPEC;
run;


*(h) Produce the following plots: Y vs. X_j, Y vs. Yhat
e vs. X_j, e vs. Yhat;

*Plotting with built in residuals and predicted values (yhat values);
proc reg data=math5305lab6;
    model Y=X1 X2 X3;
	output out=myoutput
	r=myresiduals;
	plot Y*X1 
		 Y*X2 
		 Y*X3
		 Y*predicted.
		 residual.*X1 
		 residual.*X2 
		 residual.*X3 
		 residual.*predicted.;
run; 

*(i) Answer the question;