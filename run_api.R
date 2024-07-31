library(plumber)

source("endpoints.R")

mcInt <- pr("endpoints.R")
pr_run(mcInt)
