library(plumber)
library(jsonlite)
library(kwb.rabimo)

#* @apiTitle  Rabimo Result
#* @apiDescription An API That Computes Rabimo Result

#* @get /rabimo_result
calculate <- function() {
  # Load old Berlin data from the R-wrapper package kwb.abimo
  old_data <- kwb.abimo::abimo_input_2019

  # Convert the original Abimo inputs to the new format
  new_inputs <- kwb.rabimo::prepare_berlin_inputs(
    data = old_data,
    config_file = kwb.abimo::default_config()
  )

  # Run the kwb.rabimo::run_rabimo function
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = new_inputs$data,
    config = new_inputs$config
  )

  return(rabimo_result)
}
