library(dplyr)
library(jsonlite)
library(kwb.rabimo)
library(logger)
library(plumber)


#* @apiTitle  Rabimo Result
#* @apiDescription An API That Computes Rabimo Result

# OBSOLETE
#* @get /calculate_all
calculateAll <- function() {
  # Load Berlin data and config
  data <- kwb.rabimo::rabimo_inputs_2020$data
  config <- kwb.rabimo::rabimo_inputs_2020$config

  # Run the kwb.rabimo::run_rabimo function
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = data,
    config = config
  )

  return(rabimo_result)
}

calculateMultiblock <- function(req) {
  # Convert JSON to dataframe
  input <- fromJSON(req$postBody)

  # Get features and targets
  features <- input$features
  targets <- input$targets

  targets_map <- c(
    new_green_roof = "green_roof",
    new_to_swale = "to_swale",
    new_unpaved = "unpaved"
  )

  # Rename the list keys
  names(targets) <- targets_map[names(targets)]

  # Validate data
  data_urban <- kwb.rabimo:::check_or_convert_data_types(
    data = features,
    types = kwb.rabimo:::get_expected_data_type(),
    convert = TRUE
  )

  # Load default configuration
  config <- kwb.rabimo::rabimo_inputs_2020$config

  # Run abimo calculations
  output_urban <- kwb.rabimo::run_rabimo_with_measures(
    blocks = data_urban,
    measures = targets,
    config = config
  )

  # Transform the data to its natural equivalent
  type <- "undeveloped"
  data_natural <- kwb.rabimo::data_to_natural(data = data_urban, type = type)

  # Run abimo calculations for the natural scenario
  output_natural <- kwb.rabimo::run_rabimo(
    data = data_natural,
    config = config
  )

  # Calculate Delta-W
  delta_w <- kwb.rabimo::calculate_delta_w(natural = output_natural, urban = output_urban)

  # Add Delta-W to the Abimo output for the urban scenario
  merged_output <- merge(output_urban, delta_w, by = "code", all.x = TRUE)

  return(merged_output)
}