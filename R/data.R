#' Data to showcase differences in overall survival among subgroups of breast cancer survivors
#'
#' Contains the grouping variable, and essential information needed to derive the overall survival time and status of each breast cancer survivor.
#'
#'
#' @format A data frame with 1598 rows and 12 variables
#'  \describe{
#'      \item{ID..}{Subject ID}
#'      \item{pts_sex}{Patient sex}
#'      \item{race}{Race}
#'      \item{ER}{ER status}
#'      \item{PR}{PR status}
#'      \item{HER2}{Her2 status}
#'      \item{Groups}{5 groups of breast cancer}
#'      \item{Breast_surgery_code}{Surgery type}
#'      \item{AGE_agedx}{Age at cancer diagnosis}
#'      \item{DT_dxdate2}{Date of cancer diagnosis}
#'      \item{DT_dod}{Date of death}
#'      \item{DT_date_last_seen}{Date of last follow-up}
#'      }
#'
#' @examples
#' data(dat_da)
"dat_da"
