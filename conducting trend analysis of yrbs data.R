library(stringr)
library(plyr)

setwd( "C:/My Directory/YRBSS" )

anv <- c( "q2" , "q3" , "q4" , "q23" , "q26" , "q27" , "q28" , "q29" , "year" , "psu" , "stratum" , "weight" , "raceeth" )

y <- NULL

for ( year in seq( 1991 , 2011 , 2 ) ){

	load( paste0( "yrbs" , year , ".rda" ) )
	x$year <- year
	y <- rbind.fill( x , y )
	rm( x )
	
}

y <- y[ anv ]

y[ , ] <- sapply( y[ , ] , as.numeric )


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
	

y <- y[ c( "year" , "psu" , "stratum" , "weight" , "smoking" , "raceeth" , "sex" , "grade" ) ]

for ( i in c( 'smoking' , 'raceeth' , 'grade' ) ) y[ , i ] <- relevel( factor( y[ , i ] ) , ref = "1" )

y$sex <- relevel( factor( y$sex ) , ref = "2" )

c5 <- contr.poly( 5 )[ , 1 ]

c7 <- contr.poly( 7 )[ , 1 ]

y$T5L <- c5[ match( y$year , seq( 1991 , 1999 , 2 ) ) ]

y$T7L <- c7[ match( y$year , seq( 1999 , 2011 , 2 ) ) ]


library(survey)

options(survey.lonely.psu='adjust')

des <- svydesign(id=~psu, strata=~interaction(stratum,year) ,data=subset(y, !is.na(smoking)), weights=~weight, nest=TRUE)

svyby(~smoking, ~year, svymean, design=des, na.rm=TRUE)


summary(svyglm(formula=I(smoking==1)~sex+raceeth+grade+T5L,
               design = subset(des, smoking %in% c(1,2) & year <= 1999), family = quasibinomial))

summary(svyglm(formula=I(smoking==1)~sex+raceeth+grade+T7L,
               design = subset(des, smoking %in% c(1,2) & year >= 1999), family = quasibinomial))


