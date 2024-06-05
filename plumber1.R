library(dplyr)
library(jsonlite)
library(kwb.rabimo)
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
#* @param new_values_json:json New values
calculateMultiblock <- function(new_values_json) {
# Load Berlin data and config
data <- kwb.rabimo::rabimo_inputs_2020$data
config <- kwb.rabimo::rabimo_inputs_2020$config

# Convert JSON to dataframe
new_values <- fromJSON(new_values_json)

# Create a copy of the original data frame for manipulation
updated_data <- data

# Override data with new values
updated_data <- left_join(updated_data, new_values, by = "code", suffix = c("", ".new_values")) %>%
  mutate(green_roof = coalesce(green_roof.new_values, green_roof)) %>%
  select(-green_roof.new_values)  # Remove the temporary column

# Run abimo calculations
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = updated_data,
    config = config
)

  return(rabimo_result)
}
