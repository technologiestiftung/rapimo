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
#* @param input:json Input for a selection of blocks in JSON format
calculateMultiblock <- function(input) {
  # Convert JSON to dataframe
  input <- fromJSON(input)

  # Define the required column names
  required_columns <- c("code", "prec_yr", "prec_s", "epot_yr", "epot_s",
                        "district", "total_area", "area_main", "area_rd",
                        "main_fraction", "roof", "green_roof", "swg_roof",
                        "pvd", "swg_pvd", "srf1_pvd", "srf2_pvd", "srf3_pvd",
                        "srf4_pvd", "srf5_pvd", "road_fraction", "pvd_rd",
                        "swg_pvd_rd", "srf1_pvd_rd", "srf2_pvd_rd", "srf3_pvd_rd",
                        "srf4_pvd_rd", "sealed", "to_swale", "gw_dist", "ufc30",
                        "ufc150", "land_type", "veg_class", "irrigation", "block_type")

  # Check if all required columns are present
  missing_columns <- setdiff(required_columns, names(input))

  if (length(missing_columns) > 0) {
    # Return a 400 Bad Request error response
    response <- list(
      error = "400 - Bad Request",
      message = paste("Missing column(s):", paste(missing_columns, collapse = ", "))
    )
    return(response)
  }

  # Convert specific columns to the appropriate data types
  input <- input %>%
    mutate(
      code = as.character(code),
      prec_yr = as.integer(prec_yr),
      prec_s = as.integer(prec_s),
      epot_yr = as.integer(epot_yr),
      epot_s = as.integer(epot_s),
      district = as.character(district),
      total_area = as.numeric(total_area),
      area_main = as.numeric(area_main),
      area_rd = as.numeric(area_rd),
      main_fraction = as.numeric(main_fraction),
      roof = as.numeric(roof),
      green_roof = as.numeric(green_roof),
      swg_roof = as.numeric(swg_roof),
      pvd = as.numeric(pvd),
      swg_pvd = as.numeric(swg_pvd),
      srf1_pvd = as.numeric(srf1_pvd),
      srf2_pvd = as.numeric(srf2_pvd),
      srf3_pvd = as.numeric(srf3_pvd),
      srf4_pvd = as.numeric(srf4_pvd),
      srf5_pvd = as.numeric(srf5_pvd),
      road_fraction = as.numeric(road_fraction),
      pvd_rd = as.numeric(pvd_rd),
      swg_pvd_rd = as.numeric(swg_pvd_rd),
      srf1_pvd_rd = as.numeric(srf1_pvd_rd),
      srf2_pvd_rd = as.numeric(srf2_pvd_rd),
      srf3_pvd_rd = as.numeric(srf3_pvd_rd),
      srf4_pvd_rd = as.numeric(srf4_pvd_rd),
      sealed = as.numeric(sealed),
      to_swale = as.numeric(to_swale),
      gw_dist = as.numeric(gw_dist),
      ufc30 = as.numeric(ufc30),
      ufc150 = as.numeric(ufc150),
      land_type = as.character(land_type),
      veg_class = as.integer(veg_class),
      irrigation = as.integer(irrigation),
      block_type = as.character(block_type)
    )

  # Load default configuration
  config <- kwb.rabimo::rabimo_inputs_2020$config

  # Run abimo calculations
  rabimo_result <- kwb.rabimo::run_rabimo(
    data = input,
    config = config
)

  return(rabimo_result)
}
