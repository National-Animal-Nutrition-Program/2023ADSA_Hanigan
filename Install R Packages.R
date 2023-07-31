# installing/loading the latest installr package:
install.packages("installr") # install 
#setInternet2(TRUE) # only for R versions older than 3.3.0
installr::updateR() # updating R.
installr::install.rtools()  #this interferes with ACSLx operation
	
#if the above has problems install R-tools from the following URL using Web browser
#Select 'Edit Path' during the install
#https://cran.r-project.org/bin/windows/Rtools/

install.packages("beepr")  #notifies you with a sound when a process has completed.
install.packages("codetools")
install.packages("caret")
install.packages("checkmate")
install.packages("corrplot")
install.packages("data.table")
install.packages("DT")
install.packages("DHARMa")
install.packages("EnvStats")
install.packages("epiR")
install.packages("fBasics")
install.packages("FME")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("gridExtra")
install.packages("HLMdiag")
install.packages("leaps")
install.packages("lme4")
install.packages("lmerTest")
install.packages("mefa")
install.packages("merTools")
install.packages("metafor")
install.packages("mlbench")
install.packages("MuMIn")
install.packages("nlme")
install.packages("openxlsx") #does not require java. use instead of xlsx
install.packages("pbapply")  #apply function with a progress bar for process completion
install.packages("pbmcapply") #mcapply function with a progress bar
install.packages("PerformanceAnalytics")
install.packages("perturb")
install.packages("plyr")
install.packages("sjPlot")
install.packages("stringi")
install.packages("tidyr")
install.packages("XLConnect") #requires java which we do not have
#install.packages("xlsx") #requires java which we do not have

install.packages("rstan")
install.packages("brms") #installing this will install all of the following
install.packages("backports")
install.packages("httpuv")
install.packages("Rcpp")
#install.packages("reshape2") #depracated in 2020.  switch to tidyr
