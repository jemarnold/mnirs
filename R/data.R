#' @name artinis_intervals.xlsx
#'
#' @title 10 Hz Artinis Oxysoft export
#'
#' @description Exported from Artinis Oxysoft, recorded on Oxymon MKIII at
#'   50 Hz and exported at 10 Hz. Containing two 5-minute cycling work
#'   intervals and an ischaemic occlusion, placed on the vastus lateralis
#'   muscle site.
#'
#' @docType data
#'
#' @format .xlsx file with header metadata and with four columns and 20919 rows.
#'
#'   - `nirs_channels = c(O2Hb = 2, HHb = 3)`
#'   - `time_channel = c(sample = 1)`
#'   - `event_channel = c(event = 4)`
#'
NULL


#' @name moxy_intervals.csv
#'
#' @title 0.5 Hz Moxy onboard export
#'
#' @description Exported from Moxy onboard recording at 0.5 Hz no smoothing.
#'   Containing four 5-minute cycling work intervals, placed on the vastus
#'   lateralis muscle site.
#'
#' @docType data
#'
#' @format .csv file with seven columns and 974 rows.
#'
#'   - `nirs_channels = c("SmO2 Live", "SmO2 Averaged", "THb")`
#'   - `time_channel = c("hh:mm:ss")`
#'
NULL


#' @name moxy_ramp.xlsx
#'
#' @title 2 Hz PerfPro export of Moxy data
#'
#' @description Exported from PerfPro Studio software, recorded at 0.5 Hz no
#'   smoothing and exported at 2 Hz. Containing a ramp incremental cycling
#'   protocol, placed on bilateral vastus lateralis muscle sites. With data
#'   errors (outliers, invalid data, missing `NA` values) introduced to
#'   demonstrate *{mnirs}* package functionality.
#'
#' @docType data
#'
#' @format .xlsx file with six columns and 2203 rows.
#'
#'   - `nirs_channels = c("SmO2 Live", "SmO2 Live(2)")`
#'   - `time_channel = c("hh:mm:ss")`
#'   - `event_channel = c("Lap")`
#'
NULL


#' @name train.red_intervals.csv
#'
#' @title 10 Hz Train.Red App export
#'
#' @description Exported from Train.Red app, recorded at 10 Hz. Containing
#'   two 5-minute cycling work intervals, placed on bilateral vastus lateralis
#'   muscle sites.
#'
#' @docType data
#'
#' @format .csv file with header metadata and with 18 columns and 12001 rows.
#'
#'   - `nirs_channels = c("SmO2", "HBDiff", "O2HB unfiltered", "HHb unfiltered", "THb unfiltered")`
#'   - `time_channel = c("Timestamp (seconds passed)")`
#'   - `event_channel = c("Lap/Event")`
#'
NULL
