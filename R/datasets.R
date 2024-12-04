#' List of Longitduinal datasets
#'
#' Datasets are names "datX" where "X" is a number. Currently there are 19 individual longitudinal datasets. "data_meta" and "data_info" contain meta data and info 
#'
#' @format A tibble with 243 rows and 10 variables:
#' \describe{
#'   \item{Year}{dbl Year price was recorded}
#'   \item{Month}{dbl Month price was recorded. Ranges from 1-12 for January - December} 
#'   \item{`0-15`}{dbl denoting monthly price for the 0-15/lb. size class}
#'   \item{`15-20`}{dbl denoting monthly price for the 15-20/lb. size class}
#'   \item{`20-25`}{dbl denoting monthly price for the 20-25/lb. size class}
#'   \item{`25-30`}{dbl denoting monthly price for the 25-30/lb. size class}
#'   \item{`30-40`}{dbl denoting monthly price for the 30-40/lb. size class}
#'   \item{`40-50`}{dbl denoting monthly price for the 40-50/lb. size class}
#'   \item{`50-67`}{dbl denoting monthly price for the 50-67/lb. size class}
#'   \item{Pieces}{dbl denoting monthly price of pieces of shrimp totalling a pound}
#' }
#' @source \url{https://www.pnas.org/content/114/7/1512/tab-figures-data}
"shrimp"