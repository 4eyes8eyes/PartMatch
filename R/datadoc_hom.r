#' Homoscedastic data
#'
#' Sample homoscedastic data
#' for partially matched samples
#'
#' @docType data
#'
#' @usage data(homNA)
#'
#' @format An object of class \code{"cross"}
#'
#' @keywords datasets
#'
#' @references Kuan and Huang. (2013) Stat Med 32(19): 3247-3259
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3717400/}{PubMed})
#'
#' @examples
#' data(homNA)
#' t = homNA[,1]
#'  [1] -0.4625594  0.5849841  2.9118589  1.3546332  0.8006880 -0.7483867  0.3439456
#'  [8] -1.0196846  1.2392074 -0.7007505  0.3980049 -1.8159799  0.2925999  0.6369279
#'  [15] -0.2525269 -1.4409351 -1.4967681         NA         NA         NA
#' n = homNA[,2]
#'  [1] -0.4928560 -1.0150960  0.5434571  1.5258284 -1.4978565 -0.5262878  2.3662263
#'  [8]  1.0018894 -0.5976729 -0.6430285         NA         NA         NA         NA
#'  [15]         NA         NA         NA  2.1944258  0.5233598 -0.1910694
#' \donttest{plot(t,n)}
"homNA"