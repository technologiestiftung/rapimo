library(dplyr)
library(jsonlite)
# library(kwb.rabimo)  # TODO: enable once library is available
library(logger)
library(plumber)


#* @apiTitle  Rabimo Result - BGI-Planer
#* @apiDescription An API That Computes Rabimo Results for the BGI-Planer Tool

# ── PLACEHOLDERS (replace with kwb.rabimo calls once library is ready) ────────

# TODO: replace with kwb.rabimo::run_rabimo(data = blocks, config = config)
.placeholder_run_rabimo <- function(blocks) {
  lapply(seq_len(nrow(blocks)), function(i) {
    b <- blocks[i, ]
    impervious <- b$roof + b$pvd
    runoff  <- round(b$prec_yr * impervious * 0.90, 3)
    evapor  <- round(b$prec_yr * (1 - impervious) * 0.60, 3)
    infiltr <- round(b$prec_yr - runoff - evapor, 3)
    list(code = b$code, area = round(b$total_area, 3),
         runoff = runoff, infiltr = infiltr, evapor = evapor)
  }) |> bind_rows()
}

# TODO: replace with kwb.rabimo::run_rabimo_with_measures(blocks, measures, config)
.placeholder_run_rabimo_with_measures <- function(blocks, measures) {
  measure_cols <- setdiff(names(measures), "code")
  lapply(seq_len(nrow(blocks)), function(i) {
    b <- blocks[i, ]
    m <- measures[measures$code == b$code, measure_cols, drop = FALSE]
    impervious <- b$roof + b$pvd
    runoff  <- round(b$prec_yr * impervious * 0.90, 3)
    evapor  <- round(b$prec_yr * (1 - impervious) * 0.60, 3)
    infiltr <- round(b$prec_yr - runoff - evapor, 3)
    if (nrow(m) > 0) {
      # Small mock reduction proportional to average measure intensity (0–100)
      avg_intensity <- mean(unlist(m), na.rm = TRUE) / 100
      reduction     <- avg_intensity * 0.05
      runoff  <- round(runoff  * (1 - reduction), 3)
      infiltr <- round(infiltr * (1 + reduction * 0.5), 3)
      evapor  <- round(evapor  * (1 + reduction * 0.5), 3)
    }
    list(code = b$code, area = round(b$total_area, 3),
         runoff = runoff, infiltr = infiltr, evapor = evapor)
  }) |> bind_rows()
}

# TODO: replace with kwb.rabimo::calculate_delta_w(natural = ..., urban = ...)
.placeholder_calculate_delta_w <- function(blocks, urban_wb) {
  # Natural scenario proxy: low impervious fraction
  lapply(seq_len(nrow(blocks)), function(i) {
    b       <- blocks[i, ]
    nat_runoff <- round(b$prec_yr * 0.10, 3)  # ~10 % runoff for undeveloped land
    urb_row <- urban_wb[urban_wb$code == b$code, ]
    delta_w <- round((urb_row$runoff - nat_runoff) / b$prec_yr * 100, 1)
    list(code = b$code, delta_w = delta_w)
  }) |> bind_rows()
}

# Computes area-weighted means across all blocks for a single water-balance data frame.
.summarise_water_balance <- function(wb) {
  total_area <- sum(wb$area)
  list(
    total_area_m2 = jsonlite::unbox(round(total_area, 3)),
    runoff        = jsonlite::unbox(round(sum(wb$runoff  * wb$area) / total_area, 3)),
    infiltr       = jsonlite::unbox(round(sum(wb$infiltr * wb$area) / total_area, 3)),
    evapor        = jsonlite::unbox(round(sum(wb$evapor  * wb$area) / total_area, 3)),
    delta         = jsonlite::unbox(round(sum(wb$delta_w * wb$area) / total_area, 1))
  )
}

# ── BGI-PLANER ENDPOINTS ─────────────────────────────────────────────────────

#* @post /calculate_multiblock
#* @serializer json
calculateMultiblock <- function(req) {
  input    <- fromJSON(req$postBody, simplifyDataFrame = TRUE)
  blocks   <- input$blocks
  measures <- input$measures

  if (is.null(blocks)) {
    stop("Missing 'blocks' in request body.")
  }
  if (is.null(measures)) {
    stop("Missing 'measures' in request body.")
  }

  # Original water balance (no measures)
  wb_original <- .placeholder_run_rabimo(blocks)

  # Water balance with measures applied
  wb_with_measures <- .placeholder_run_rabimo_with_measures(blocks, measures)

  # Delta-W for both scenarios
  dw_original      <- .placeholder_calculate_delta_w(blocks, wb_original)
  dw_with_measures <- .placeholder_calculate_delta_w(blocks, wb_with_measures)

  wb_original      <- merge(wb_original,      dw_original,      by = "code")
  wb_with_measures <- merge(wb_with_measures, dw_with_measures, by = "code")

  # Summary statistic: total runoff reduction
  runoff_reduction_pct <- round(
    (sum(wb_original$runoff) - sum(wb_with_measures$runoff)) /
      sum(wb_original$runoff) * 100,
    3
  )

  list(
    water_balance_with_measures = wb_with_measures,
    water_balance_original      = wb_original,
    summary = list(
      original      = .summarise_water_balance(wb_original),
      with_measures = .summarise_water_balance(wb_with_measures)
    ),
    statistics = list(
      runoff_reduction_percent = runoff_reduction_pct
    )
  )
}
