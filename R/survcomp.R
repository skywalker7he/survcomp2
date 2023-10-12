#' Post-hoc Within Group Comparisons of Differences in a Survival Outcome
#' @description
#' Performs pairwise comparisons of the levels of one categorical variable within each level of another categorical variable adjusting for multiplicity.
#' @param dat a data frame.
#' @param patid an integer vector of the subject IDs.
#' @param x_c the name(s) of categorical variable(s).
#' @param x_n the name(s) of numeric variable(s).
#' @param dt_start the name of the date of the starting point.
#' @param dt_outcome the name of the date of the survival outcome.
#' @param dt_end the name of the date of the endpoint.
#' @param unit the unit of measurement for the time variable; options are: "day", "month", "year".
#'
#' @return a summary_emm object and a data frame of pairwise comparisons, confidence intervals, test statistics, and p-values.
#' @export
#'
#' @importFrom emmeans emmeans
#'
#' @examples
#' # Read in the sample data
#' data(dat_da)
#' emm_contrasts <- survcomp(
#' dat = dat_da, patid = "ID..",
#' x_c = c("Groups", "Breast_surgery_code"),
#' x_n = "AGE_agedx", dt_start = "DT_dxdate2",
#' dt_outcome = "DT_dod",
#' dt_end = "DT_date_last_seen"
#' )
#' # Only keep the statistically significant pairwise comparisons
#' emm_contrasts[emm_contrasts$p.value < 0.05, ]

survcomp <- function(dat, patid, x_c, x_n, dt_start, dt_outcome, dt_end,
                     unit = "month") {
  # Combine variable names entered
  cols_specified <- c(patid, x_c, x_n, dt_start, dt_outcome, dt_end)
  # Select variables based on the names entered
  dat_da <- dat[, cols_specified]
  num_categorical <- length(x_c)
  num_numeric <- length(x_n)
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
  for (i in 1:num_categorical) {
  # Check whether every categorical variable entered is indeed a factor; if not,
  # convert it to a factor and make the reference level starting from 0
    if (!is.factor(dat_da[, x_c[i]])) {
      dat_da[, x_c[i]] <- as.factor(dat_da[, x_c[i]] - 1)
    } else {
  # If it is a factor, make its reference level starting from 0 by converting it
  # to numeric first, subtract 1, and then, convert it back to a factor
      dat_da[, x_c[i]] <- as.factor(as.numeric(dat_da[, x_c[i]]) - 1)
    }
  }
  # Convert the date variables to a Date type
  dat_da[, dt_start] <- ymd(dat_da[, dt_start])
  dat_da[, dt_outcome] <- ymd(dat_da[, dt_outcome])
  dat_da[, dt_end] <- ymd(dat_da[, dt_end])
  # Obtain the status from the missingness or nonmissingness of the outcome date variable:
  # if it is not missing, then the outcome had occured during the study period (status == 1);
  # otherwise, the subject had been censored (status == 0)
  dat_da$status <- ifelse(!is.na(dat_da[, dt_outcome]), 1, 0)
  # Combine the dates of the outcome and the dates of last follow-up to
  # form a vector of status (e.g., death or censored) dates
  dat_da$dt_status <- as.Date(ifelse(dat_da$status == 1,
                                     dat_da[, dt_outcome],
                                     dat_da[, dt_end]),
                              origin = "1970-01-01")
  # Calculate the differences between the starting dates and the status dates rowwise
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
  # Fit a Cox proportional hazards model with interaction of the categorical variables entered
  coxph_fmla <- as.formula(paste("Surv(time, status) ~",
                                 paste(x_c, collapse = "*")))
  coxph_fit <- coxph(coxph_fmla, data = dat_da)
  emm_specs <- as.formula(paste("pairwise ~", paste(x_c, collapse = "|")))
  emm <- emmeans(coxph_fit, specs = emm_specs, type = "response")
  return(emm$contrasts %>% summary(infer = TRUE))
}
