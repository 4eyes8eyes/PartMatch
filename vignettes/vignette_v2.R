## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----hetNA--------------------------------------------------------------------
data(hetNA, package = "partmatch")
## The heteroscedastic data
hetNA
## The tumor samples
hetNA[,1]
## The normal samples
hetNA[,2]

## ----homNA--------------------------------------------------------------------
data(homNA, package = "partmatch")
## The homoscedastic data
homNA
## The tumor samples
homNA[,1]
## The normal samples
homNA[,2]

## ----liptak-------------------------------------------------------------------
## liptak(x, y, alternative=c("greater","less","two.sided"))
## where:    x is the vector of tumor samples
##           y is the vector of normal samples
##           alternative is the comparison in the alternative hypothesis. For example, if the alternative hypothesis is H1 > mu0, 
##            then alternative should be "greater". This argument is optional; the default is "greater".

## ----liptak_ex----------------------------------------------------------------
library(partmatch)
test = liptak(hetNA[,1], hetNA[,2], alternative="less")
test

## ----kim----------------------------------------------------------------------
## kim(x, y, alternative=c("two.sided","less","greater"))
## where:    x is the vector of tumor samples
##           y is the vector of normal samples
##           alternative is the comparison in the alternative hypothesis. For example, if the alternative hypothesis is H1 > mu0, 
##            then alternative should be "greater". This argument is optional; the default is "two.sided".

## ----kim_ex-------------------------------------------------------------------
test = kim(hetNA[,1], hetNA[,2], alternative="less")
test[1]
test[2]

## ----looneyjones--------------------------------------------------------------
## looneyjones(x, y, alternative=c("two.sided","less","greater"))
## where:    x is the vector of tumor samples
##           y is the vector of normal samples
##           alternative is the comparison in the alternative hypothesis. For example, if the alternative hypothesis is H1 > mu0, 
##            then alternative should be "greater". This argument is optional; the default is "greater".

## ----looneyjones_ex-----------------------------------------------------------
test = looneyjones(hetNA[,1], hetNA[,2], alternative="less")
test[1]
test[2]

## ----linstivers---------------------------------------------------------------
## linstivers(x, y, alternative=c("two.sided","less","greater"))
## where:    x is the vector of tumor samples
##           y is the vector of normal samples
##           alternative is the comparison in the alternative hypothesis. For example, if the alternative hypothesis is H1 > mu0, 
##            then alternative should be "greater". This argument is optional; the default is "two.sided".

## ----linstivers_ex------------------------------------------------------------
test = linstivers(hetNA[,1], hetNA[,2], alternative="less")
test[1]
test[2]

## ----ekbohm-------------------------------------------------------------------
## ekbohm(x, y, alternative=c("two.sided","less","greater"))
## where:    x is the vector of tumor samples
##           y is the vector of normal samples
##           alternative is the comparison in the alternative hypothesis. For example, if the alternative hypothesis is H1 > mu0, 
##            then alternative should be "greater". This argument is optional; the default is "two.sided".

## ----ekbohm_ex----------------------------------------------------------------
test = ekbohm(hetNA[,1], hetNA[,2], alternative="less")
test[1]
test[2]

