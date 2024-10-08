library(dplyr)
library(jsonlite)
library(kwb.rabimo)
library(logger)
library(plumber)


#* @apiTitle  Rabimo Result
#* @apiDescription An API That Computes Rabimo Result

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
#*           new_green_roof: 0.35,
#*           new_to_swale: 0.2,
#*           new_pvd: 0.17,
#*         }
#*     },
#* ]
calculateMultiblock <- function(req) {
  # Convert JSON to dataframe
  input <- fromJSON(req$postBody)

  # Initialize an empty list to hold the new input
  new_input <- list()

  # Iterate over each row in the input data frame
  for (i in 1:nrow(input$features)) {
    # Access the i-th row of the features data frame
    features_row <- input$features[i, ]

    # Update features with values in targets
    features_row$green_roof <- input$targets$new_green_roof
    features_row$to_swale <- input$targets$new_to_swale
    features_row$pvd <- input$targets$new_pvd

    # Append the modified features to the new_inputs list
    new_input[[i]] <- features_row
  }

  # Convert the list of modified features to a data frame
  new_input <- do.call(rbind, new_input)

  # Validate data
  new_input <- kwb.rabimo:::check_or_convert_data_types(
    data = new_input,
    types = kwb.rabimo:::get_expected_data_type(),
    convert = TRUE
  )

  # Load default configuration
  config <- kwb.rabimo::rabimo_inputs_2020$config

  # Run abimo calculations
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = new_input,
    config = config
)

  return(rabimo_result)
}


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
