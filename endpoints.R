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


# OBSOLETE
#* @get /peek_input
#* @param n_records:int Number of records to show
#* @serializer json list(na="string") # Ensure NA values are handled appropriately
peekInput <- function(n_records) {
  # Load Berlin data and config
  data <- kwb.rabimo::rabimo_inputs_2020$data

  # Check if n_records is greater than the number of available records
  if (n_records > nrow(data)) {
    n_records <- nrow(data) # Limit to available records
  }

  # Select the first n_records from the dataframe
  result <- head(data, n_records)

  return(result)
}


#* @post /calculate_multiblock
#* @serializer json
#* @param input:object Input should be a JSON with the following format:
#* {
#*     features:
#*         [
#*             {
#*                 feature_1: x1,
#*                 feature_2: x2,
#*                 feature_3: x3
#*             },
#*             {
#*                 feature_1: x1,
#*                 feature_2: x2,
#*                 feature_3: x3
#*             }
#*         ],
#*     targets:
#*         {
#*           green_roof: 0.35,
#*           to_swale: 0.2,
#*           unpaved: 0.17,
#*         }
#*     },
#* ]
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


# OBSOLETE
#* @get /calculate_all_delta_w
calculateAllDeltaW <- function() {
  # Load Berlin data and config
  input_urban <- kwb.rabimo::rabimo_inputs_2020$data
  config <- kwb.rabimo::rabimo_inputs_2020$config

  type <- "undeveloped"
  input_natural <- kwb.rabimo::data_to_natural(data = input_urban, type = type)

  # Get abimo outputs for urban and natural scenarios
  output_urban <- kwb.rabimo::run_rabimo(
    data = input_urban,
    config = config
  )

  output_natural <- kwb.rabimo::run_rabimo(
    data = input_natural,
    config = config
  )

  # Calculate Delta-W
  delta_w <- kwb.rabimo::calculate_delta_w(natural = output_natural, urban = output_urban)

  return(delta_w)
}


# OBSOLETE
#* @post /calculate_multiblock_delta_w
#* @serializer json
#* @param input:object Input should be a JSON of nested arrays,
# each representing a block with fields such as code, prec_yr, prec_s,
# epot_yr, epot_s, district, total_area, and other related attributes.
#* @response 200 Returns the processed data
calculateMultiblockDeltaW <- function(req) {
  # Load Berlin data and config
  input_urban <- fromJSON(req$postBody)
  config <- kwb.rabimo::rabimo_inputs_2020$config

  # Validate data
  input_urban <- kwb.rabimo:::check_or_convert_data_types(
    data = input_urban,
    types = kwb.rabimo:::get_expected_data_type(),
    convert = TRUE
  )

  # Transform the data to its natural equivalent
  type <- "undeveloped"
  input_natural <- kwb.rabimo::data_to_natural(data = input_urban, type = type)

  # Get abimo outputs for urban and natural scenarios
  output_urban <- kwb.rabimo::run_rabimo(
    data = input_urban,
    config = config
  )

  output_natural <- kwb.rabimo::run_rabimo(
    data = input_natural,
    config = config
  )

  # Calculate Delta-W
  delta_w <- kwb.rabimo::calculate_delta_w(natural = output_natural, urban = output_urban)

  return(delta_w)
}
