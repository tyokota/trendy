### E-mail correspondence with CDC-INFO about fitting orthogonal polynomials

> Subject: Question about methodology 

> Your Question: 

> Hi, I recently came across this article. I'm interested in knowing how to replicate the method. I had created the orthogonal polynomials as stated in step 1. However, I noticed that linear and quadratic were significant. In the article, the author stopped after including a quadratic trend. Can someone please explain this step better to me? 

> http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf 

> Thank you, <br>
> Thomas

Thomas,
 
The pdf you reference includes directions for testing cubic as well as linear and quadratic trends, so I apologize that  I’m not certain what you are requesting be clarified.
 
However, I hope the following example helps.
 
Say you wanted to test for cubic trends in a particular health risk behavior (e.g., using tobacco in the past 30 days) and you had data from all 13 YRBS surveys from 1991 – 2015. You could use a model similar to the following (and only interpret the cubic term):
 
Health Risk Behavior of interest = sex race grade T13L  T13Q  T13C ;
 
In this example, T13L,  T13Q, and T13C are the linear, quadratic, and cubic time trend variables coded with orthogonal coefficients from PROC IML (for all 13 YRBS datasets).
 
If the cubic trend was statistically significant, you would produce adjusted prevalence and standard error estimates for the particular health risk behavior of interest (e.g., current tobacco Use) for each survey year using the PREDMARG statement in SUDAAN. Then, you would import those values into Joinpoint to determine which 2 points in time (years) define the cubic trend.  
 
Let’s say the “Joinpoint” years for the health risk behavior of interest were 1999 and 2011. You would then perform 3 linear trend regression analyses.
 
The first linear trend regression would include 5 years of YRBS data and cover the years 1991-1999.
 
For example:  Health Risk Behavior = sex race grade T5L_L.   In this example, T5L_L is coded with PROC IML linear coefficients for the years 1991, 1993, 1995, 1997, and 1999 and tests the left segment for linear trend.
 
The second linear regression would include 7 years of data and cover the years 1999-2011.
 
For example: Health Risk Behavior = sex race grade T7L_M. In this example, T7L_M is coded with PROC IML linear coefficients for the years 1999, 2001, 2003, 2005, 2007, 2009, and 2011 and tests the middle segment for linear trend.
 
The final linear regression would include 3 years of data and cover the years 2011-2015.
 
For example: Health Risk Behavior = sex race grade T3L_R. In this example, T3L_R is coded with PROC IML linear coefficients for the years 2011, 2013, and 2015 and tests the right segment for linear trends.
 
You then describe the significant cubic trend in terms of what was happening from 1991-1999 (perhaps a linear increase) followed by what happened from 1999-2011 (perhaps a linear decrease) and finally what happened from 2011-2015 (perhaps no change).
 
I hope this helps,
 
Richard Lowry
 
Medical Officer <br>
Division of Adolescent and School Health <br>
National Center for HIV/AIDS, Viral Hepatitis, STD, and TB Prevention <br>
Centers for Disease Control and Prevention
