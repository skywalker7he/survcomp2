#' Kapan-Meier Plots and Log-rank Test
#' @description
#' Draws Kaplan-Meier plots of the survival outcome of interest, and compares the levels of one categorical variable within each level of another categorical variable globally via log-rank tests.
#' @param dat a data frame
#' @param patid an integer vector of the subject IDs
#' @param f1 the name of the first categorical variable
#' @param f2 the name of the second categorical variable
#' @param dt_start the name of the date of the starting point
#' @param dt_outcome the name of the date of the survival outcome
#' @param dt_end the name of the date of the endpoint
#' @param unit the unit of measurement for the time variable; options are: "day", "month", "year"
#'
#' @return Multiple Kaplan-Meier plots with a display of the p-values from log-rank tests.
#' @export
survcomp_plot <- function(dat, patid, f1, f2, dt_start, dt_outcome, dt_end,
                          unit = "month") {
  cols_specified <- c(patid, f1, f2, dt_start, dt_outcome, dt_end)
  return(cols_specified)
}
