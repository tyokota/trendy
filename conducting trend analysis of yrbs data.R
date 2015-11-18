# reproduce the center for disease control & prevention's linear trend analysis
# on complex sample survey data (the youth risk behavioral surveillance system)
# http://www.cdc.gov/healthyyouth/yrbs/pdf/yrbs_conducting_trend_analyses.pdf


# this script was written by
# thomas yokota
# thomasyokota@gmail.com

# vito michele rosario muggeo
# vito.muggeo@unipa.it 

# anthony joseph damico
# ajdamico@gmail.com

# thanks to dr. thomas lumley for creating the svypredmeans() function
# to replicate SUDAAN's PREDMARG command for this specific purpose

# thanks to dr. richard lowry at the cdc for methodological assistance in
# interpreting and reproducing that yrbss trend publication


# # # this example displays only linearized designs (created with the ?svydesign function)
# # # for more detail about how to reproduce this analysis with a replicate-weighted design (created with the ?svrepdesign function)
# # # see the methods note below the `svydesign` block about how to best reproduce this analysis on a replication design


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
# install.packages( c( "downloader" , "plyr" , "survey" , "segmented" ) )

# Muggeo V. (2008) Segmented: an R package to fit regression models with broken-line relationships. R News, 8, 1: 20-25.
library(segmented)	# determine segmented relationships in regression models

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
# stacking multiple years and accounting for `year` in the nested strata
des <- 
	svydesign(
		id = ~psu , 
		strata = ~interaction( stratum , year ) ,
		data = y , 
		weights = ~weight , 
		nest = TRUE
	)


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# methods note about how to stack replication designs #

# this is only relevant if you are trying to create a `des` like the object above but just have replicate weights and do not have the clustering information (psu)

# it is quite straightforward to construct a replication design using a linearized design (see ?as.svrepdesign)
# however, for privacy reasons, going in the opposite direction is much more challenging (see http://www.asdfree.com/2014/09/how-to-provide-variance-calculation-on.html)
# therefore, you'll need to do some dataset-specific homework on how to best *stack* multiple years of a replicate-weighted design before
# before you construct a multiple-year-stacked survey design like the object above.

# these publicly-available survey data sets include both replicate weights (for a replication design) and, separately, clustering information (for a linearized design):
# the medical expenditure panel survey, the national health and nutrition examination survey, the consumer expenditure survey

# in most cases, omitting the `year` variable from the `des` construction above will make your standard errors larger (conservative)
# -> ergo -> you can probably just rbind( repweight_year_one , repweight_year_two , ... ) so long as the survey design has not changed over the period.
# once you have the rbound replicate weights object for every year, you could just construct one huge multi-year svrepdesign.
# make sure you include scales, rscales, rho, and whatever else the svrepdesign() call asks for.
# if you are worried you missed something, check attributes( your_single_year_replication_design_object )

# # the above solution is likely all you need in most cases. # #

# # if you need to be very conservative with your computation of trend statistical significance # #

# attempt to re-construct fake clusters yourself using a regression.  search for "malicious" in
# https://github.com/ajdamico/asdfree/blob/master/Confidentiality/how%20to%20create%20de-identified%20replicate%20weights.R
# the purpose this, though, isn't to identify individuals in the dataset, it's to get a variable like `psu` above that gives you reasonable standard errors.

# see the object `your.replicate.weights` in that script?
# you could reconstruct a fake psu for each record in your data set with something as easy as
# # fake_psu <- kmeans( your.replicate.weights , 20 )
# where 20 is the (completely made up) number of clusters x strata.
# hopefully the methodology documents (or the people who wrote them) will at least tell you how many clusters there were in the original sample,
# even if the clusters themselves were not disclosed.

# fake_psu should be a one-record-per-person vector object that can immediately be appended onto your data set.

# # at the point you've made fake clusters, they will surely be worse than the real clusters (i.e. conservative standard errors)
# # and you can construct a multiple-year survey design with
# # des <- svydesign( id = ~ your_fake_psus , strata = ~ year , data = y , weights = ~ weight , nest = TRUE )

# this approach will probably be conservative probably

# end of note about how to stack replication designs. #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

	
# immediately remove records with missing smoking status
des_ns <- subset( des , !is.na( smoking ) )


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
means_for_joinpoint$year <- as.numeric( rownames( means_for_joinpoint ) )

# must be sorted, just in case it's not already
means_for_joinpoint <- means_for_joinpoint[ order( means_for_joinpoint$year ) , ]


# # joinpoint analysis # #
# # vito wrote this part # #

# look at what you've got
means_for_joinpoint

# rename columns so they do not conflict with variables in memory
names( means_for_joinpoint ) <- c( 'mean' , 'se' , 'yr' )
# the above line is only because the ?segmented function does not work if an object of the same name is also in memory.

# plot what you've got
with(means_for_joinpoint, plot(yr,mean)) #with() specifies the means_for_joinpointataframe where the variables 'y' and 'year' are stored

# since we have information about variance of each datum, display circles proportional to '1/sqrt(variance)'
# using the 'cex' argument.  the smaller the circle, the larger the variance (and the lower the weight)

with( means_for_joinpoint , plot( yr , mean , cex = 2 / (se * 100 ) ) ) 
# 2 / ( se * 100 ) is only relevant here because it averages near 1 so the plot looks nice

# since a joinpoint analysis requires modeling the log(mean),
# compute the corresponding variance var( log( mean ) ) = var( mean ) / mean^2
# thus the new weights will be ( 1 / var( log( mean ) ) )
# create that weight variable
means_for_joinpoint$wgt <- with( means_for_joinpoint, ( mean / se ) ^ 2 ) 

# fit a piecewise linear regression
# estimate the 'starting' linear model with the usual "lm" function using the log values and the weights

o <- lm( log( mean ) ~ yr , weights = wgt , data = means_for_joinpoint )

# add a segmented variable (`yr` in this example) with 1 breakpoint
os <- segmented( o , ~yr )

# `os` is now a `segmented` object, which means it includes information on the fitted model,
# such as parameter estimates, standard errors, residuals.  check it out!
summary( os )

# did you see the `Estimated Break-Point(s)` in that result?
# figuring out the breakpoint year was the purpose of this joinpoint analysis.
( your_breakpoint <- round( as.vector( os$psi[, "Est." ] ) ) )
# so.  that's a joinpoint.  that's where the two line segments join.  okay?


# # methods notes about joinpoints with `segmented` # #

# obtain the annual percent change (APC=) estimates for each time point
slope( os , APC = TRUE )

# the returned CIs for the APC may be different from the ones returned by joinpoint; for further details, check out
# Muggeo V. (2010) A Comment on `Estimating average annual per cent change in trend analysis' by Clegg et al.,
# Statistics in Medicine; 28, 3670-3682. Statistics in Medicine, 29, 1958-1960.

# in general, `plot.segmented()` can be used to plot the fitted lines along with observations, but here
# we need to reconvert the fitted values (which are on the log scale) on the original scale.
# use a "fine" grid (e.g. 100 values, lots of pixels) to better display the fitted lines

year100 <- seq( 1991 , 2011 , length.out = 100 )

fit100 <- predict( os , newdata = data.frame( yr = year100 ) )

lines( year100 , exp( fit100 ) , col = 2 , lwd = 2 )

# in the next version of the segmented package, plot.segmented() will include an option to transform the fitted values before plotting.

# the estimated breakpoint along its (approximate) standard error is
os$psi

# note that the above number is not an integer! the `segmented` package uses an iterative procedure (described in the article below)
# and therefore even between-year solutions are returned. the joinpoint software implements two estimating algorithms: 
# the grid-search and the Hudson algorithm, the latter returning also non-integer solutions like segmented.

# Muggeo V. (2003) Estimating regression models with unknown break-points. Statistics in Medicine, 22: 3055-3071.


# in this cdc replication example, a more parsimonious model is supported by data:
# as the left slope (data points prior to 1999) is almost zero, it makes sense to cordon it off.

o0 <- lm( log( mean ) ~ yr , weights = wgt , data = means_for_joinpoint) # no covariate
os0 <- segmented( o0 , ~yr )
fit100 <- predict( os0 , newdata = data.frame( year = year100 ) )
lines( year100 , exp( fit100 ) , col = 3 , lwd = 2)
slope(os0, APC=TRUE) #a somewhat narrower confidence interval..



# # end of joinpoint analysis # #

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