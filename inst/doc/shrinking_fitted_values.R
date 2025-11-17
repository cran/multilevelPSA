## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(multilevelPSA)

## -----------------------------------------------------------------------------
getSimulatedData <- function(nvars=3,
							 ntreat=100, treat.mean=.6, treat.sd=.5,
							 ncontrol=1000, control.mean=.4, control.sd=.5) {
	if(length(treat.mean) == 1) { treat.mean = rep(treat.mean, nvars) }
	if(length(treat.sd) == 1) { treat.sd = rep(treat.sd, nvars) }
	if(length(control.mean) == 1) { control.mean = rep(control.mean, nvars) }
	if(length(control.sd) == 1) { control.sd = rep(control.sd, nvars) }
	
	df <- c(rep(0, ncontrol), rep(1, ntreat))
	for(i in 1:nvars) {
		df <- cbind(df, c(rnorm(ncontrol, mean=control.mean[i], sd=control.sd[i]),
						  rnorm(ntreat, mean=treat.mean[i], sd=treat.sd[i])))
	}
	df <- as.data.frame(df)
	names(df) <- c('treat', letters[1:nvars])
	return(df)
}

## ----message=FALSE, results = 'hide'------------------------------------------
test.df2 <- getSimulatedData(
	treat.mean=.6, control.mean=.4)

## ----results = 'hide'---------------------------------------------------------
psranges2 <- psrange(test.df2, test.df2$treat, treat ~ .,
					 samples=seq(100,1000,by=100), nboot=20)

## ----fig.height = 7, out.width='100%', fig.width = 6.5------------------------
plot(psranges2)

