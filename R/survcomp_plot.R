#' Kapan-Meier Plots and Log-rank Test
#' @description
#' Draws Kaplan-Meier plots of the survival outcome of interest, and compares the levels of one categorical variable within each level of another categorical variable globally via log-rank tests.
#' @param dat a data frame.
#' @param patid an integer vector of the subject IDs.
#' @param f1 the name of the first categorical variable.
#' @param f2 the name of the second categorical variable.
#' @param dt_start the name of the date of the starting point.
#' @param dt_outcome the name of the date of the survival outcome.
#' @param dt_end the name of the date of the endpoint.
#' @param unit the unit of measurement for the time variable; options are: "day", "month", "year".
#'
#' @return Multiple Kaplan-Meier plots with a display of the p-values from log-rank tests.
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom dplyr %>% select
#' @importFrom lubridate ymd
#' @importFrom stats as.formula
#' @importFrom ggplot2 theme element_blank element_line
#' @importFrom survminer surv_fit ggsurvplot arrange_ggsurvplots
#' @importFrom grDevices rainbow
survcomp_plot <- function(dat, patid, f1, f2, dt_start, dt_outcome, dt_end,
                          unit = "month") {
  cols_specified <- c(patid, f1, f2, dt_start, dt_outcome, dt_end)
  dat_da <- dat %>% select(
    all_of(cols_specified)
  )
  length_f1 <- length(f1)
  if (!(length_f1 == 1)) {
    stop("Please enter only one categorical variable name for the argument f1!")
  }
  if (!is.factor(dat_da[, f1])) {
    dat_da[, f1] <- as.factor(dat_da[, f1] - 1)
  }
  length_f2 <- length(f2)
  if (!(length_f2 == 1)) {
    stop("Please enter only one categorical variable name for the argument f2!")
  }
  if (!is.factor(dat_da[, f2])) {
    dat_da[, f2] <- as.factor(dat_da[, f2] - 1)
  }
  num_start_date <- length(dt_start)
  if (!(num_start_date == 1)) {
    stop("There can only be one starting point!")
  }
  num_outcome_date <- length(dt_outcome)
  if (!(num_outcome_date == 1)) {
    stop("There can only be one column for the outcome's date!")
  }
  num_end_date <- length(dt_end)
  if (!(num_end_date == 1)) {
    stop("There can only be one column for the date of last follow-up!")
  }
  dat_da[, dt_start] <- ymd(dat_da[, dt_start])
  dat_da[, dt_outcome] <- ymd(dat_da[, dt_outcome])
  dat_da[, dt_end] <- ymd(dat_da[, dt_end])
  dat_da$status <- ifelse(!is.na(dat_da[, dt_outcome]), 1, 0)
  dat_da$dt_status <- as.Date(ifelse(dat_da$status == 1,
                                     dat_da[, dt_outcome],
                                     dat_da[, dt_end]))
  if (unit == "day") {
    dat_da$time <- as.numeric(difftime(dat_da$dt_status,
                                       dat_da[, dt_start],
                                       "day"))
  } else if (unit == "month") {
    dat_da$time <- round(as.numeric(difftime(dat_da$dt_status,
                                             dat_da[, dt_start],
                                             "days")) / 30.4375, 2)
  } else if (unit == "year") {
    dat_da$time <- round(as.numeric(difftime(dat_da$dt_status,
                                             dat_da[, dt_start],
                                             "days")) / 365.25, 2)
  } else {
    stop("No such a unit of measurement for the time variable!")
  }
  survfit_fmla <- as.formula(paste("Surv(time, status) ~", f1))
  f1_levels <- levels(dat_da[, f1])
  f2_levels <- levels(dat_da[, f2])
  surv_fit_object <- list()
  surv_curves <- list()
  theme <- theme(axis.line = element_line(colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
  for (i in 1:length(f2_levels)) {
    surv_fit_object[[i]] <- surv_fit(survfit_fmla,
                                     data = dat_da[dat_da[, f2] == f2_levels[i],])
    surv_curves[[i]] <- ggsurvplot(
      fit = surv_fit_object[[i]],
      linetype = "strata",
      risk.table="abs_pct",
      risk.table.height = 0.30,
      risk.table.col = "strata",
      risk.table.fontsize = 3.5,
      censor = FALSE,
      ggtheme = theme,
      palette = rainbow(5),
      xlim = c(0, 150),
      xlab = "Time, months",
      ylab = "Survival probability",
      pval = TRUE,
      pval.method = TRUE,
      legend.title = paste(f2, f2_levels[i], sep = " = "),
      legend = "top")
    surv_curves[[i]]$table <- surv_curves[[i]]$table +
      theme(axis.line = element_blank())
  }
  return(arrange_ggsurvplots(surv_curves, print = TRUE,
                             ncol = length(f2_levels), nrow = 1))
}