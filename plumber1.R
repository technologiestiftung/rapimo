library(plumber)
library(jsonlite)
library(kwb.rabimo)

#* @apiTitle  Rabimo Result
#* @apiDescription An API That Computes Rabimo Result
#* @get /rabimo_result
rabimoResult <- function() {
  # Load Berlin data from the R-wrapper package kwb.abimo
  data <- kwb.abimo::abimo_input_2019

  # Provide Abimo's default configuration
  abimo_config <- kwb.abimo:::read_config()
  config <- kwb.rabimo::abimo_config_to_config(abimo_config)

  # Run the kwb.rabimo::run_rabimo function
  rabimo_result <- kwb.rabimo::run_rabimo(data, config)

  # Convert the result to JSON format
  json_result <- toJSON(rabimo_result, pretty = FALSE)

  # Return the JSON result
  json_result
}
