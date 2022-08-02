#' Heteroscedastic data
#'
#' Sample heteroscedastic data
#' for partially matched samples
#'
#' @docType data
#'
#' @usage data(hetNA)
#'
#' @format An object of class \code{"cross"}
#'
#' @keywords datasets
#'
#' @references Kuan and Huang. (2013) Stat Med 32(19): 3247-3259
#' (\href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3717400/}{PubMed})
#'
#' @examples
#' data(hetNA)
#' t = hetNA[,1]
#' [1]  1.57040278  2.13648570  0.81215347  2.78592165  0.19550634 -1.45570149
#' [7]  1.33967679 -0.92073306  0.95880898 -1.82215630  0.37800955  1.56714382
#' [13]  0.02588191  0.41610619  0.36030595  0.68844113 -0.67294267          NA
#' [19]          NA          NA
#' n = hetNA[,2]
#' [1] -0.5306183  1.2159488 -1.4315441  0.7408119  1.4269980 -2.2245374  1.5603345
#' [8]  0.6192872  0.4623979 -1.2176234         NA         NA         NA         NA
#' [15]         NA         NA         NA -0.5093132  0.5180292  0.7362109
#' 
#' \donttest{plot(t,n)}
"hetNA"