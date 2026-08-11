library(kwb.smartwater)
library(dplyr)
library(jsonlite)
library(logger)
library(plumber)

#* @apiTitle  Rabimo Result - BGI-Planer
#* @apiDescription An API That Computes Rabimo Results for the BGI-Planer Tool

# ── BGI-PLANER ENDPOINTS ─────────────────────────────────────────────────────

#* @post /calculate_multiblock
#* Calculate water balance for multiple block areas with and without measures
#* Runs R-ABIMO for a set of block areas in two scenarios: the original state
#* (no user-defined measures) and a modified state with the provided measures
#* applied. Returns per-block results under water_balance.status_quo and
#* water_balance.with_measures, plus statistics with area-weighted summaries,
#* runoff_reduction_percent, and water_quality_indicators.
#* @param blocks:[data.frame] Array of R-ABIMO block objects. Each block must
#*   include fields: code, prec_yr, prec_s, epot_yr, epot_s, district,
#*   total_area, roof, green_roof, swg_roof, pvd, swg_pvd, srf1_pvd–srf5_pvd,
#*   to_swale, gw_dist, ufc30, ufc150, land_type, veg_class, irrigation.
#* @param measures:[data.frame] Array of measure objects, one per block. Each
#*   object must have a "code" field matching a block, plus numeric fields for
#*   each measure type in m2 (e.g. green_roof_ext, unpaving, to_swale,
#*   to_tree_pit, to_cistern). See /get_measure_info for the full list of
#*   supported measure field names.
#* @serializer json
calculateMultiblock <- function(req, res) {
  input    <- fromJSON(req$postBody, simplifyDataFrame = TRUE)
  blocks   <- input$blocks
  measures <- input$measures

  if (is.null(blocks) || nrow(blocks) == 0) {
    res$status <- 400
    return(list(error = "Missing or empty 'blocks' in request body."))
  }
  if (is.null(measures)) {
    res$status <- 400
    return(list(error = "Missing 'measures' in request body."))
  }

  missing_codes <- setdiff(measures$code, blocks$code)
  if (length(missing_codes) > 0) {
    res$status <- 400
    return(list(error = paste(
      "measures contain codes not present in blocks:",
      paste(missing_codes, collapse = ", ")
    )))
  }

  result <- tryCatch(
    kwb.smartwater::calculate_water_balance(blocks, measures, convert_types = TRUE),
    error = function(e) {
      res$status <<- 500
      list(error = paste("calculate_water_balance failed:", conditionMessage(e)))
    }
  )
  if (!is.null(result$error)) {
    return(result)
  }

  result
}

#* @get /get_measure_info
#* Get info on measures supported by kwb.smartwater
#* @param type optional. One or more of: "green_roof", "pavement", "trees", "infiltration", "retention"
#* @param field_name_only optional. If TRUE, returns only the field_name per measure
#* @serializer unboxedJSON
getMeasureInfo <- function(type = character(0), field_name_only = FALSE) {
  kwb.smartwater::get_measure_info(type, as.logical(field_name_only))
}

#* @get /plot_effect_of_disconnect
#* Plot the effect of disconnecting surfaces (runoff reduction)
#* @param runoff_reduction runoff_reduction in percent, as returned by /calculate_multiblock in statistics.runoff_reduction_percent
#* @param type one of "critical_hours", "unpleasant_hours", "critical_events", "negative_deviation"
#* @serializer contentType list(type="image/png")
plotEffectOfDisconnect <- function(runoff_reduction, type, res) {
  file <- tryCatch(
    kwb.smartwater::plot_effect_of_disconnect(
      surface_reduction = as.numeric(runoff_reduction),
      type              = type,
      output_dir        = tempdir()
    ),
    error = function(e) {
      res$status <<- 400
      NULL
    }
  )
  if (is.null(file)) {
    return(NULL)
  }
  readBin(file, "raw", n = file.info(file)$size)
}