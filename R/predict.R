#' Run Tidal Current Prediction
#'
#' Executes the SISCORAR prediction executable for a specific date.
#' On macOS/Linux, runs the Windows executable via Wine.
#' On Windows, runs natively.
#'
#' @param date Date object or `"YYYY-MM-DD"` string.
#' @param area Character: bay name. One of: `"guanabara"`, `"sepetiba"`,
#'   `"paranagua"`, `"santos"`, `"baiatos"`. Default: `"guanabara"`.
#' @param daylight_saving Logical: apply Brazilian daylight saving time
#'   adjustment. Default: `FALSE`.
#'
#' @return Logical: `TRUE` if prediction completed successfully.
#'
#' @details
#' The prediction writes results to Grade.bin in the area directory.
#' Use [read_predictions()] to parse the results.
#'
#' @seealso [predict_currents()] for a combined run+read workflow
#'
#' @examples
#' \dontrun{
#' run_prediction(Sys.Date(), "guanabara")
#' run_prediction("2025-06-15", "santos")
#' }
#'
#' @export
run_prediction <- function(date, area = "guanabara", daylight_saving = FALSE) {
  date <- as.Date(date)
  area <- .validate_area(area)
  area_path <- get_area_path(area)
  exec_name <- get_exec_name(area)

  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  dst_flag <- if (daylight_saving) "1" else "0"

  cli::cli_inform("Running prediction for {.val {area}} on {.val {date}}...")

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(area_path)

  if (.Platform$OS.type == "windows") {
    cmd_result <- system2(
      exec_name,
      args = c(year, month, day, dst_flag),
      stdout = FALSE, stderr = FALSE
    )
  } else {
    wine_cmd <- .wine_command_for_area(area)
    cmd_result <- system2(
      wine_cmd,
      args = c(exec_name, year, month, day, dst_flag),
      stdout = FALSE, stderr = FALSE,
      env = "WINEDEBUG=-all"
    )
  }

  grade_file <- file.path(area_path, "Grade.bin")
  if (file.exists(grade_file) && file.info(grade_file)$mtime > Sys.time() - 60) {
    cli::cli_alert_success("Prediction completed.")
    return(invisible(TRUE))
  }

  cli::cli_warn(
    "Prediction may have failed -- {.path Grade.bin} was not updated."
  )
  invisible(FALSE)
}

#' Predict Currents for a Single Date
#'
#' Main entry point: runs the prediction executable and reads the binary results.
#'
#' @inheritParams run_prediction
#'
#' @return data.table with prediction results (see [read_predictions()]).
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents(Sys.Date(), "guanabara")
#' dt[which.max(velocity_cm_s)]
#' }
#'
#' @seealso [predict_currents_range()] for multiple dates
#' @export
predict_currents <- function(date, area = "guanabara", daylight_saving = FALSE) {
  date <- as.Date(date)
  area <- .validate_area(area)
  success <- run_prediction(date, area, daylight_saving)
  if (!success) {
    cli::cli_warn("Prediction failed for {.val {area}} on {.val {date}}")
    return(data.table())
  }
  read_predictions(area, date)
}

#' Predict Currents for a Date Range
#'
#' Runs predictions for multiple consecutive dates and combines results.
#'
#' @param start_date Start date (Date or `"YYYY-MM-DD"` string).
#' @param end_date End date (Date or `"YYYY-MM-DD"` string).
#' @inheritParams run_prediction
#'
#' @return data.table with all dates combined, plus a `date` column.
#'
#' @examples
#' \dontrun{
#' dt <- predict_currents_range("2025-06-01", "2025-06-07", "guanabara")
#' dt[, .(max_speed = max(velocity_cm_s)), by = date]
#' }
#'
#' @export
predict_currents_range <- function(start_date, end_date, area = "guanabara",
                                   daylight_saving = FALSE) {
  area <- .validate_area(area)
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  results <- lapply(dates, function(d) {
    cli::cli_h3("{d}")
    dt <- predict_currents(d, area, daylight_saving)
    if (nrow(dt) > 0L) dt[, date := d]
    dt
  })

  rbindlist(results)
}
