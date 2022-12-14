% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kim.R
\name{kim}
\alias{kim}
\title{Kim et al.'s modified t-statistic}
\usage{
kim(x, y, alternative = c("two.sided", "less", "greater"))
}
\arguments{
\item{x}{first sample}

\item{y}{second sample}

\item{alternative}{comparison in alternative hypothesis (two-sided, less, or greater)}
}
\value{
Vector of length two whose first element is the test statistic and second element is the p-value
}
\description{
Calculates Kim et al.'s modified t-statistic and corresponding p-value for partially matched samples.
}
\examples{
kim(hetNA[,1],hetNA[,2],alternative="two.sided")

}
