library(plumber)

mcInt <- plumber::pr("endpoints.R") |>
  plumber::pr_mount("/bgi", plumber::pr("endpoints_bgi.R"))

plumber::pr_run(mcInt)
