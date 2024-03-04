ods pdf file="C:\Users\HP\OneDrive\Desktop\BS 805\project_output.pdf";

libname bs805 "C:\Users\HP\OneDrive\Desktop\BS 805\";

/**linking baseline and clinical data for cohort 1**/
/**used match-merging technique to link the datasets**/
proc sort data=bs805.baseline_cohort1;
	by id;
run;
proc sort data=bs805.clinical_cohort1;
	by id;
run;
data cohort1;
	merge bs805.baseline_cohort1 bs805.clinical_cohort1;
	by id;
run;

/**linking baseline and clinical data for cohort 2**/
/**used match-merging technique to link the datasets**/
proc sort data=bs805.baseline_cohort2;
	by id;
run;
proc sort data=bs805.clinical_cohort2;
	by id;
run;
data cohort2;
	merge bs805.baseline_cohort2 bs805.clinical_cohort2;
	by id;
run;

/**combining the two cohort datasets into a temp dataset**/
data combined_cohort;
	set cohort1  cohort2 ;
	by id;
run;


/**creating a new combined dataset for analyses**/
proc format;
	value BMI_catf 1='Normal'
				   2='Overweight'
				   3='Obese';
	value MALEf    0='female'
				   1='male';
run;
data new_combined_cohort;
	set combined_cohort;
	weight_kg = weight*(0.453592);/*converting weight in lbs to kg*/
	height_m = height * (0.0254);/*converting height in inchies to meters*/
	BMI = (weight_kg/(height_m*height_m) ); /*creating a BMI variable*/

	if BMI < 19 then delete;/*excluding BMI values less than 19*/

	if BMI=. then BMI_cat=.;/*grouping BMI into 3 categories*/
	else if BMI < 25 then BMI_cat = 1;
	else if 25 <= BMI < 30 then BMI_cat =2;
	else if BMI >= 30 then BMI_cat = 3;

	if (19 <= BMI < 25) then BMI1=BMI;/*created 3 piecewise variables for each BMI interval as defined in the BMI cat variable*/
	else if BMI >=25 then BMI1=25;

	if(19 <= BMI < 25) then BMI2=25;
	else if (25 <= BMI < 30) then BMI2=BMI;
	else if BMI >= 30 then BMI2=30;

	if (19 <= BMI < 30) then BMI3=30;
	else if BMI >=30 then BMI3=BMI;

	waist_hip_ratio = ((waist)/(hip));/*created a waist to hip ratio variables in meters*/

	Mean_SBP = mean(of BP_1S BP_2S);/*mean systolic blood pressure*/

	glyhb_ln = log(glyhb);/*natural log of HbA1c*/

	if gender='male' then MALE=1; else MALE = 0;/*dummy variable with male as 1 and female as 0*/
	
	label MALE="Sex"
          chol="Total Cholestrol"
          weight_kg="Weight in kilograms"
          height_m ="Height in meters"
      	  stab_glu="Blood Glucose Level"
          glyhb="hemoglobin A1c"
      	  waist="waist circumference in inches"
          hip="hip circumference in inches"
          BMI="Body Mass Index"
          BMI_cat="Categorical BMI variable "     
          waist_hip_ratio="Waist to Hip Ratio"
          glyhb_ln="Natural logarithm of HbA1c"
          Mean_sbp="Mean systolic blood pressure";

	format BMI_cat BMI_catf. MALE MALEf.;
run;

proc means data=new_combined_cohort;
	var MEAN_SBP;
run;

/**Descriptive Statistics for all study variables**/
/**table displays distribution of variables by BMI categories**/

/*Checking for normality of continuous variables within BMI categories to know which measures of central tendency and dispersion to report*/
proc univariate data=new_combined_cohort noprint;
	histogram BMI waist_hip_ratio age waist hip Mean_SBP glyhb chol stab_glu weight_kg height_m;
	class BMI_cat;
	title'checking for normality and skewness of continuous variables';
run;
title;

/*reporting means and standard deviation for these variables because data distribution looks normal*/
proc means data=new_combined_cohort maxdec=2 mean std;
	var  waist_hip_ratio age waist hip chol weight_kg height_m;
	class BMI_cat;
	title 'descriptive statistics for continuous variables with normal distribution';
run;
title;

/*reporting median and IQR for these variables because data distribtuion looks skewed*/
proc means data=new_combined_cohort maxdec=2 median Q1 Q3;
	var stab_glu glyhb BMI Mean_SBP;
	class BMI_cat;
	title 'descriptive statistics for continuous variables with skewed distribution';
run;
title;
/*for categorical variables*/
proc freq data=new_combined_cohort;
	tables MALE * BMI_cat / norow nocol nocum ;
	title 'descriptive statistics for categorical variable (sex)';
run;
title;

/**One-way ANOVA to to test a common mean of HbA1C across BMI categories using log vs non-log version of HbA1C variable**/

/*one-way anova for non log version*/
proc glm data=new_combined_cohort;
	class BMI_cat (ref='Normal');
	model glyhb= BMI_cat/solution;
	title'one-way ANOVA across BMI categories for non log version of HbA1C';
run;
title;

/*one-way anova for log version*/
proc glm data=new_combined_cohort;
	class BMI_cat (ref='Normal');
	model glyhb_ln= BMI_cat/solution;
	lsmeans BMI_cat/adjust=tukey;
	title'one-way ANOVA across BMI categories for log version of HbA1C';
run;
title;
	

/**best approach modeling approach to assess relationship between BMI and HbA1C**/

/**a. creating dummy variables for BMI**/
data new_combined_cohort;
	set new_combined_cohort;
	**creating dummy variables for BMI categories;
	** Normal (1) is the reference group;
	if BMI_cat=2 then over_weight=1; else over_weight=0;
	if BMI_cat =3 then obese=1; else obese=0;
run;
proc glm data=new_combined_cohort;
	model glyhb_ln=over_weight obese/solution;
	title'Linear regression model for log transformed HbA1C using dummy variables for BMI';
run;
title;
/**b. BMI categories as an ordinal variable**/
proc glm data=new_combined_cohort;
	model glyhb_ln=BMI_cat/solution;
	title'Linear regression model for log transformed  HbA1C using BMI categories as an ordinal variable';
run;
title;

/**c. BMI as a continuous variable**/
proc reg data=new_combined_cohort;
	title1'Linear regression model for log transformed HbA1C using BMI as a continuous variable';
	model glyhb_ln=BMI;
run;
title;

/**d. Piecewise linear model using piecewise variables*/
proc reg data=new_combined_cohort;
	model glyhb_ln= BMI1 BMI2 BMI3/stb;
	test BMI1 = BMI2;
	test BMI2 = BMI3;
	output out=piece pred=pred_glyhb_ln;
	title'Piecewise linear model for log transformed HbA1C using piecewise variables for BMI';
run;
title;
proc sort data=piece;
	by BMI;
run;
symbol1 value =dot color=black;
symbol2 line=1 interpol =join color=black;

proc gplot data=piece;
	plot  glyhb_ln*BMI  pred_glyhb_ln*BMI / overlay;
	title 'A plot of the slopes of the piecewise model with observed values of log transformed HbA1C and BMI';
run;
title;

/*checking for linearity assumption between log transformed HbA1C and BMI*/
proc gplot data=new_combined_cohort;
	plot glyhb_ln * BMI;
	title' Log transformed HbA1C with BMI plot';
run;
title;



/**Assessing effect modifiers on relationship between log transformed HbA1C and BMI**/

/**Age as an effect modifier using BMI as a categorical variable**/
proc glm data=new_combined_cohort;
	 class BMI_cat (ref='Normal');
	 model glyhb_ln = BMI_cat age BMI_cat*age/solution;
	 title' Assessing age as an effect modifier between log transformed HbA1C and BMI';
run;
title;

/**Sex as an effect modifier using BMI as a categorical variable**/
proc glm data=new_combined_cohort;
	 class BMI_cat (ref='Normal') MALE (ref='female');
	 model glyhb_ln = BMI_cat MALE BMI_cat*MALE/solution;
	 title' Assessing age as an effect modifier between log transformed HbA1C and BMI';
run;
title;


/**A. Best predictor out of waist, hip, waist to hip ratio**/

/**waist**/
proc reg data=new_combined_cohort;
	model glyhb_ln = waist;
	title'Linear regression of log-transformed HbA1C and waist';
run;
title;

/**waist to hip ratio**/
proc reg data=new_combined_cohort;
	model glyhb_ln = waist_hip_ratio;
	title'Linear regression of log-transformed HbA1C and waist to hip ratio';
run;
title;

/**hip**/
proc reg data=new_combined_cohort;
	model glyhb_ln = hip;
	title'Linear regression of log-transformed HbA1C and hip';
run;
title;

/* Means Plots for each of the three variables to assess linearity with log transformed HbA1C*/
proc rank groups=10 data=new_combined_cohort out=three;
 	var waist waist_hip_ratio hip;
 	ranks rwaist rwaist_hip_ratio rhip;
run;
/**waist**/
proc sort data=three;
 	by rwaist;
run;
proc means data=three noprint;
 	by rwaist;
 	var glyhb_ln waist;
 	output out=waiststr mean=glyhb_lnmean waistmean;
run;
proc plot data=waiststr;
 	plot glyhb_lnmean*waistmean='W';
 	title 'Plot of Decile Means for Waist';
run;
title;

/**hip**/
proc sort data=three;
 	by rhip;
run;
proc means data=three noprint;
 	by rhip;
 	var glyhb_ln hip;
 	output out=hipstr mean=glyhb_lnmean hipmean;
run;
proc plot data=hipstr;
	 plot glyhb_lnmean*hipmean='H';
 	title1 'Plot of Decile Means for Hip';
run;

/**waist to hip ratio**/
proc sort data=three;
 	by rwaist_hip_ratio;
run;
proc means data=three noprint;
	by rwaist_hip_ratio;
 	var glyhb_ln waist_hip_ratio;
 	output out=waist_hip_ratiostr mean=glyhb_lnmean waist_hip_ratiomean;
run;
proc plot data=waist_hip_ratiostr;
 	plot glyhb_lnmean*waist_hip_ratiomean='W';
 	title 'Plot of Decile Means for Waist to Hip ratio';
run;
title;

/**multi-collinearity analysis, outliers and influence points**/
proc reg data=new_combined_cohort;
 	model glyhb_ln=waist waist_hip_ratio hip/ tol vif collinoint r;
	id;
	title'Multicollinearity analysis of waist, hip, and waist to hip ratio';
	output out=two pred=p_glyhb_ln student=str_glyhb_ln residual=resid_glyhb_ln cookd=cooksd;
 run;
 title;

proc print data=two;
	id id;
	where abs(str_glyhb_ln)>=3;
	var str_glyhb_ln p_glyhb_ln resid_glyhb_ln waist waist_hip_ratio hip;
	title'outliers among waist, hip and waist to hip ratio';
run;
title;

proc print data=two;
	id id;
	where cooksd >= 4/386;
	var str_glyhb_ln p_glyhb_ln resid_glyhb_ln cooksd waist waist_hip_ratio hip;
	title'influence points among waist, hip and waist to hip ratio';
run;
title;


 /**b**/
/**assessing for confounding**/
/*Crude model*/
 proc glm data=new_combined_cohort;
 	class BMI_cat(ref='Normal');
	model glyhb_ln = BMI_cat/solution;
	title'Crude model of log transformed HbA1C and BMI categories';
run;
title;

/*Adjusted model*/
 proc glmselect data=new_combined_cohort;
 	class BMI_cat(ref='Normal') MALE(ref='female');
	model glyhb_ln = BMI_CAT age chol waist MALE Mean_SBP/selection=none stb;
	title'Adjusted model of log transformed HbA1C and BMI categories with other predictors';
run;
title;


/**Assessing whether the multivariate regression model is over-fit or under-fit**/

/**LASSO based linear regression**/
proc glmselect data=new_combined_cohort;
	class BMI_cat(ref='Normal') MALE(ref='female');
 	model glyhb_ln=BMI_cat age chol waist MALE Mean_SBP / selection=lasso (stop=none choose=aic);
	title'LASSO based linear regression for the multivariate linear regression model';
run;
title;

/**Backward selection based linear regression**/
proc glmselect data=new_combined_cohort;
	class BMI_cat(ref='Normal') MALE(ref='female');
 	model glyhb_ln=BMI_cat age chol waist MALE Mean_SBP / selection=backward (choose=aic stop=sl) sle=0.05 sls=0.05 select=aic;
	title'Backward selection linear regression for the multivariate linear regression model';
run;
title;
ods pdf close;


