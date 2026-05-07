library(dplyr)
library(jsonlite)
library(kwb.rabimo)
library(logger)
library(plumber)


#* @apiTitle  Rabimo Result - BGI-Planer
#* @apiDescription An API That Computes Rabimo Results for the BGI-Planer Tool

# ── BGI-PLANER ENDPOINTS ─────────────────────────────────────────────────────

#* @post /calculate_multiblock
#* @serializer json
calculateMultiblock <- function(req) {
  # Convert JSON to dataframe
  input <- fromJSON(req$postBody)

  # Get features and targets
  features <- input$features
  targets_input <- input$targets

  if (is.null(targets_input)) {
    stop("Missing 'targets' in request body.")
  }

  # Accept both key styles: new_* and canonical keys.
  targets <- list(
    green_roof = if (!is.null(targets_input$new_green_roof)) targets_input$new_green_roof else targets_input$green_roof,
    to_swale = if (!is.null(targets_input$new_to_swale)) targets_input$new_to_swale else targets_input$to_swale,
    unpaved = if (!is.null(targets_input$new_unpaved)) targets_input$new_unpaved else targets_input$unpaved
  )

  missing_targets <- names(targets)[vapply(targets, is.null, logical(1))]
  if (length(missing_targets) > 0) {
    stop(sprintf("Missing target values: %s", paste(missing_targets, collapse = ", ")))
  }

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
