library(plumber)

# Set the working directory
#setwd("~/ProjectsTSB/amarex/amarex-webtool/rabimo-api/")

source("plumber1.R")

mcInt <- pr("plumber1.R")
pr_run(mcInt)
