# reproduce the center for disease control & prevention's linear trend analysis
# on complex sample survey data (the youth risk behavioral surveillance system)
# http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf


# this script was written by
# thomas yokota
# thomasyokota@gmail.com

# with contributions from
# anthony joseph damico
# ajdamico@gmail.com

# thanks to dr. thomas lumley for creating the svypredmeans() function
# to replicate SUDAAN's PREDMARG command for this specific purpose

# thanks to dr. richard lowry at the cdc for methodological assistance in
# interpreting and reproducing that yrbss trend publication


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#################################################################################################################################
# prior to running this analysis script, the yrbss 1991-2011 single-year files must all be loaded as r data files (.rda) on the #
# local machine. running the download automation script will create the appropriate files for your pleasurable convenience      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/asdfree/blob/master/Youth%20Risk%20Behavior%20Surveillance%20System/download%20all%20microdata.R  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will create files like "yrbs2011.rda" in C:/My Directory/YRBSS or wherever the working directory was set          #
#################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# uncomment this line by removing the `#` at the front..
# setwd( "C:/My Directory/YRBSS" )


# remove the # in order to run this install.packages line only once
# install.packages( c( "downloader" , "plyr" , "survey" ) )


library(downloader)	# downloads and then runs the source() function on scripts from github
library(plyr) 		# contains the rbind.fill() function, which stacks two data frames even if they don't contain the same columns.  the rbind() function does not do this
library(survey) 	# load survey package (analyzes complex design surveys)


# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN
# SAS uses "remove" instead of "adjust" by default,
# the table target replication was generated with SAS,
# so if you want to get closer to that, use "remove"


# load dr. thomas lumley's `svypremeans` function, which replicates SUDAAN's PREDMARG command
source_url( "https://raw.githubusercontent.com/tyokota/trendy/master/svypredmeans.R" , prompt = FALSE )
# for more detail about this method, see https://gist.github.com/tslumley/2e74cd0ac12a671d2724


# initiate an empty `y` object
y <- NULL

# loop through each year of YRBSS microdata
for ( year in seq( 1991 , 2011 , 2 ) ){

	# load the current year
	load( paste0( "yrbs" , year , ".rda" ) )
	
	# tack on a `year` column
	x$year <- year
	
	# stack that year of data alongside the others,
	# ignoring mis-matching columns
	y <- rbind.fill( x , y )
	
	# clear the single-year of microdata from RAM
	rm( x )
	
}

# remove all unnecessary columns from the 1991-2011 multi-year stack
y <- y[ c( "q2" , "q3" , "q4" , "q23" , "q26" , "q27" , "q28" , "q29" , "year" , "psu" , "stratum" , "weight" , "raceeth" ) ]

# convert every column to numeric type
y[ , ] <- sapply( y[ , ] , as.numeric )

# construct year-specific recodes so that
# "ever smoked a cigarette" // grade // sex // race-ethnicity align across years
y <-
	transform(
		
		y ,
		
		smoking = 
			as.numeric(
				ifelse( year == 1991 , q23 ,
				ifelse( year %in% c( 1993 , 2001:2009 ) , q28 ,
				ifelse( year %in% 1995:1997 , q26 ,
				ifelse( year %in% 1999 , q27 ,
				ifelse( year %in% 2011 , q29 , NA ) ) ) ) ) 
			) ,
				
		raceeth = 
			
			ifelse( year %in% 1991:1997 ,
				ifelse( q4 %in% 1:3 , q4 , ifelse( q4 %in% 4:6 , 4 , NA ) ) ,
			
			ifelse( year %in% 1999:2005 ,
				ifelse( q4 %in% 6 , 1 ,
				ifelse( q4 %in% 3 , 2 ,
				ifelse( q4 %in% c( 4 , 7 ) , 3 ,
				ifelse( q4 %in% c( 1 , 2 , 5 , 8 ) , 4 , NA ) ) ) ) ,
				
			ifelse( year %in% 2007:2011 ,
				ifelse( raceeth %in% 5 , 1 ,
				ifelse( raceeth %in% 3 , 2 ,
				ifelse( raceeth %in% c( 6 , 7 ) , 3 ,
				ifelse( raceeth %in% c( 1 , 2 , 4 , 8 ) , 4 , NA ) ) ) ) ,
				
				NA ) ) ) ,
				
		grade = ifelse( q3 == 5 , NA , as.numeric( q3 ) ) ,
		
		sex = ifelse( q2 %in% 1:2 , q2 , NA )
		
	)
	

# again remove unnecessary variables, keeping only the complex sample survey design columns
# plus independent/dependent variables to be used in the regression analyses
y <- y[ c( "year" , "psu" , "stratum" , "weight" , "smoking" , "raceeth" , "sex" , "grade" ) ]

# set female to the reference group
y$sex <- relevel( factor( y$sex ) , ref = "2" )

# set ever smoked=yes // white // 9th graders as the reference groups
for ( i in c( 'smoking' , 'raceeth' , 'grade' ) ) y[ , i ] <- relevel( factor( y[ , i ] ) , ref = "1" )


# let us introduce you to a new friend
# ?contr.poly
# this function implements polynomials used in complex survey data.  for more detail on this subject, see page 216 of 
# Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences By Jacob Cohen, Patricia Cohen, Stephen G. West, Leona S. Aiken
# "The polynomials we have used as predictors to this point are natural polynomials, generated from the linear predictor by centering and the powering the linear predictor."
# https://www.google.com/search?q=The+polynomials+we+have+used+as+predictors+to+this+point+are+natural+polynomials%2C+generated+from+the+linear+predictor+by+centering+and+the+powering+the+linear+predictor.&ie=utf-8&oe=utf-8


# extract a linear contrast vector of length eleven,
# because we have eleven distinct years of yrbss data `seq( 1999 , 2011 , 2 )`
c11l <- contr.poly( 11 )[ , 1 ]

# also extract a quadratic (squared) contrast vector
c11q <- contr.poly( 11 )[ , 2 ]

# just in case, extract a cubic contrast vector
c11c <- contr.poly( 11 )[ , 3 ]

# for each record in the data set, tack on the linear, quadratic, and cubic contrast value
# these contrast values will serve as replacement for the linear `year` variable in any regression.
y$t11l <- c11l[ match( y$year , seq( 1999 , 2011 , 2 ) ) ]
y$t11q <- c11q[ match( y$year , seq( 1999 , 2011 , 2 ) ) ]
y$t11c <- c11c[ match( y$year , seq( 1999 , 2011 , 2 ) ) ]

# construct a complex sample survey design object
des <- 
	svydesign(
		id = ~psu , 
		strata = ~interaction( stratum , year ) ,
		data = y , 
		weights = ~weight , 
		nest = TRUE
	)
	
# immediately remove records with missing smoking status
des_ns <- subset( des_ns , !is.na( smoking ) )
	

# calculate unadjusted, un-anythinged "ever smoked" rates by year
# note that this reproduces the unadjusted "ever smoked" statistics at the top of
# pdf page 6 of http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf
svyby( ~ smoking , ~ year , svymean , design = des_ns )


# # # calculate the *maximum number of joinpoints* needed # # #

# calculate the "ever smoked" binomial regression,
# adjusted by sex, age, race-ethnicity, and
# a linear year contrast
summary(
	svyglm(
		I( smoking == 1 ) ~ sex + raceeth + grade + t11l , 
		design = subset( des_ns , smoking %in% 1:2 ) , 
		family = quasibinomial
	)
)
# the linear year-contrast variable is hugely significant here
# therefore, there is probably going to be some sort of trend.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# a linear trend only needs a maximum of zero joinpoints. #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# calculate the "ever smoked" binomial regression,
# adjusted by sex, age, race-ethnicity, and
# both linear and quadratic year contrasts
summary(
	svyglm(
		I( smoking == 1 ) ~ sex + raceeth + grade + t11l + t11q , 
		design = subset( des_ns , smoking %in% 1:2 ) , 
		family = quasibinomial 
	)
)
# the linear year-contrast variable is hugely significant here
# but the quadratic year-contrast variable is also significant.
# therefore, we must use joinpoint software for this analysis.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# a quadratic trend needs a maximum of one joinpoints.  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# calculate the "ever smoked" binomial regression,
# adjusted by sex, age, race-ethnicity, and
# linear, quadratic, and cubic year contrasts
summary(
	svyglm(
		I( smoking == 1 ) ~ sex + raceeth + grade + t11l + t11q + t11c , 
		design = subset( des_ns , smoking %in% 1:2 ) , 
		family = quasibinomial 
	)
)
# the cubic year-contrast term is *not* significant in this model.
# therefore, we should stop testing the shape of this line.
# in other words, we can stop at a quadratic trend and *do not* need a cubic trend.

# note: if the cubic trend *were* significant, then we would increase
# the maximum number of joinpoints to *two* instead of *one*
# but since the cubic term is not significant, we should stop with the previous regression.

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# a cubic trend needs a maximum of two joinpoints.    #
# if we keep getting significant trends, we keep      #
# increasing the number of maximum joinpoints.        #
# year^4 requires a maximum of three joinpoints.      #
# year^5 requires a maximum of four joinpoints. etc.  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # end of *maximum number of joinpoints* calculations # # #



# calculate the survey-year-independent predictor effects
# and store these results into a separate object
marginals <- 
	svyglm(
		formula = I( smoking == 1 ) ~ sex + raceeth + grade ,
		design = des_ns , 
		family = quasibinomial
	)

# run these marginals through dr. thomas lumley's schnazzy new `svypredmeans` function
( means_for_joinpoint <- svypredmeans( marginals , ~factor( year ) ) )
# now you've got means and standard errors matching SUDAAN's PREDMARG command

# coerce these to a data.frame object
means_for_joinpoint <- as.data.frame( means_for_joinpoint )

# extract the row names as the survey year
means_for_joinpoint$year <- rownames( means_for_joinpoint )

# output this data.frame object into your current working directory
# in a format readable by the national cancer institute's joinpoint software
# write.table( means_for_joinpoint , "means for joinpoint.txt" , sep = "\t" , row.names = FALSE , col.names = TRUE ) 


# # # # external software to calculate which years to use as joinpoints # # # #

# # the national cancer institute's joinpoint software
# # ( a free download from https://surveillance.cancer.gov/joinpoint/download )
# # will import the text file and (if you follow the settings used in `joinpoint settings.png`)
# # https://raw.githubusercontent.com/tyokota/trendy/master/joinpoint%20settings.png

# # except: see the red square in that picture?  look for the red square.
# # the maximum number of joinpoints should be determined based on the
# # linear / quadratic / cubic / etc. experiments conducted above.
# # in this example, quadratic was the final significant term,
# # so we should use maximum joinpoints = 1; however, if in your data
# # cubic is the final significant term, use maximum joinpoints = 2.

# # using a maximum joinpoint = 1 in this example, the software
# # determines that a joinpoint should be constructed at the year 1999.
# # this tells you that you ought to re-do the previous svyglm() analyses twice.
# # once using all years prior to and including 1999
# # then again starting at 1999 and ending at the final year of data
# # note that re-running this analysis two more times
# # requires re-integrating the year-contrast values for a five-timepoint span (1991, 1993, 1995, 1997, 1999)
# # and, separately, a seven-timepoint span (1999, 2001, 2003, 2005, 2007, 2009, 2011)

# # # # end of external software joinpoint instructions # # # #


# calculate a five-timepoint linear contrast vector
c5l <- contr.poly( 5 )[ , 1 ]

# calculate a seven-timepoint linear contrast vector
c7l <- contr.poly( 7 )[ , 1 ]

# tack the five-timepoint linear contrast vectors onto the current survey design object
des_ns <- update( des_ns , t5l = c5l[ match( year , seq( 1991 , 1999 , 2 ) ) ] )

# tack the seven-timepoint linear contrast vectors onto the current survey design object
des_ns <- update( des_ns , t7l = c7l[ match( year , seq( 1999 , 2011 , 2 ) ) ] )


# reproduce the sentence on pdf page 6 of http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf
# In this example, T5L_L had a p-value=0.52261 and beta=0.03704. Therefore, there was "no significant change in the prevalence of ever smoking a cigarette during 1991-1999."
summary(
	svyglm(
		I( smoking == 1 ) ~ sex + raceeth + grade + t5l ,
		design = subset( des_ns , smoking %in% 1:2 & year <= 1999 ) , 
		family = quasibinomial
	)
)

# reproduce the sentence on pdf page 6 of http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf
# In this example, T7L_R had a p-value<0.0001 and beta=-0.99165. Therefore, there was a "significant linear decrease in the prevalence of ever smoking a cigarette during 1999-2011."
summary(
	svyglm(
		I( smoking == 1 ) ~ sex + raceeth + grade + t7l ,
		design = subset( des_ns , smoking %in% 1:2 & year >= 1999 ) , 
		family = quasibinomial
	)
)


# fini