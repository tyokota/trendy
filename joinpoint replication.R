



#We illustrate code to perform trend analysis in R via the segmented package [1]
#(rather than using the joinpoint software.)

#Import data in R
d<-read.table("d.txt", sep="\t", header=TRUE)

# A quick look
head(d) #annual data,
#y = ....................
#s = the standard deviation of each observation,
#year = observation year

#Plot data
with(d, plot(year,y)) #with() specifies the dataframe where the variables 'y' and 'year' are stored

#Even better: as we have information about variance of each datum,
#   we display circles proportional to '1/sqrt(variance)'
#   via the 'cex' argument (the smaller the circle, the larger the variance (and the lower the weight.)
with(d, plot(year,y, cex=2/(s*100))) #2/(s*100) just leads to values around 1..

# We reproduce analysis reported in [2 - .............. ANTHONY PUT HERE THE REFERENCE___], and since we are going to model
# log(y), we have to compute the corresponding variance var(log(y)) = var(y)/y^2, approximately
# thus the new weights (=1/var(log(y))) will be

d$w<- with(d, (y/s)^2) #build the weight variable

#To carry out a trend analysis and to fit a piecewise linear regression in
# R via the segmented package, we first estimate the 'starting' linear model via
# the usual "lm" function using the log values and the weights,

o<-lm(log(y)~year, weights=w, data=d) #note the response and the weights..

#and then we 'update' it, by adding a segmented variable (year in this example)
# with 1 breakpoint

library(segmented)
os<-segmented(o, ~year)

#'os' is a "segmented" object including information on the fitted model, such as
# parameter estimates, standard errors, residuals..

summary(os)

#The annual per cent change (APC) estimates for each time segment are obtained
# via the slope() function by specifying 'APC=TRUE'

slope(os, APC=TRUE)

# The returned CIs for the APC may be different from the ones returned by joinpoint; see [3] for details

# In general plot.segmented() can be used to plot the  fitted lines along with observations, but here
# we need to reconvert the fitted values (which are on the log scale) on the original scale..
# We use a "fine" grid (e.g. 100 values, it's enough), to better display the fitted lines

year100<-seq(1991,2011,l=100)
fit100<-predict(os, newdata=data.frame(year=year100))
lines(year100, exp(fit100), col=2, lwd=2)

#In next version of segmented plot.segmented() will include an option to transform the fitted values before plotting.

#The estimated breakpoint along its (approximate) standard error is
os$psi

# Note that is not an integer! In fact 'segmented' uses an iterative procedure [4] and therefore even
# intra-annual (better, most of times ) solutions are returned. The joinpoint software implements two
# estimating algorithms: the grid-search and the Hudson algorithm, the latter returning also non-integer
# solutions like segmented..

# A more parsimonious model is supported by data: as the left slope is almost zero, it could be useful to
# constrain the left slope to be zero. It suffices not to include the 'year' covariate in the starting model.
# Here code to fit the model and plot the data

o0<-lm(log(y)~1, weights=w, data=d) #no covariate
os0<-segmented(o0, ~year)
fit100<-predict(os0, newdata=data.frame(year=year100))
lines(year100, exp(fit100), col=3, lwd=2)

slope(os0, APC=TRUE) #a somewhat narrower confidence interval..



# [1] Muggeo V. (2008) Segmented: an R package to fit regression models with broken-line relationships. R News, 8, 1: 20-25.
# [2] ....................
# [3] Muggeo V. (2010) A Comment on `Estimating average annual per cent change in trend analysis' by Clegg et al.,
  # Statistics in Medicine; 28, 3670-3682. Statistics in Medicine, 29, 1958-1960.
# [4] Muggeo V. (2003) Estimating regression models with unknown break-points. Statistics in Medicine, 22: 3055-3071.
