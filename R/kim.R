#' Kim et al.'s modified t-statistic
#'
#' Calculates Kim et al.'s modified t-statistic and corresponding p-value for partially matched samples.
#'
#' @param x first sample
#' @param y second sample
#' @param alternative comparison in alternative hypothesis (two-sided, less, or greater)
#'
#' @return Vector of length two whose first element is the test statistic and second element is the p-value
#'
#' @examples
#' kim(hetNA[,1],hetNA[,2],alternative="two.sided")
#' 
#' @export
kim = function(x,y,alternative=c("two.sided","less","greater")){
  ## Creating the vectors for each sample, as in the Liptak weighted z-test
  alternative = match.arg(alternative)
  n1 = 0; n2 = 0; n3 = 0;
  len = length(x)
  ## Counting the number of samples in each group
  for(i in 1:len){
    if(!is.na(x[i]) & !is.na(y[i])){	## both groups
      n1 = n1+1
    } else if(!is.na(x[i]) & is.na(y[i])){	## first group only
      n2 = n2+1
    } else if(is.na(x[i]) & !is.na(y[i])){	## second group only
      n3=n3+1
    }
    ## ignoring the case where both groups are NA
  }
  ## Put the n1 paired samples into two paired vectors
  ## and the n2 and n3 samples into independent vectors
  group1a = rep(NA,n1); group1b = rep(NA,n1);
  group2 = rep(NA,n2); group3 = rep(NA,n3)
  for(i in 1:length(x)){
    if((!is.na(x[i])) & (!is.na(y[i]))){	## both groups
      group1a[min(which(is.na(group1a)))] = x[i]
      group1b[min(which(is.na(group1b)))] = y[i]
    } else if(!is.na(x[i]) & is.na(y[i])){	## first group only
      group2[min(which(is.na(group2)))] = x[i]
    } else if(is.na(x[i]) & !is.na(y[i])){	## second group only
      group3[min(which(is.na(group3)))] = y[i]
    }
  }
  if(length(x)>length(y))stop("not enough 'y' observations")
  if(length(x)<length(y))stop("not enough 'x' observations")
  D = (group1a-group1b)
  Dbar = mean(D)
  Tbar = mean(group2)
  Nbar = mean(group3)
  Sd = sd(D)
  St = sd(group2)
  Sn = sd(group3)
  nH = 2 / ((1/n2)+(1/n3))
  score = ((n1*Dbar)+(nH*(Tbar-Nbar))) / sqrt((n1*Sd^2)+(nH^2)*((Sn^2)/n3 + (St^2)/n2))
  ## The null distribution of t3 is approximated by a standard Gaussian distribution
  if(alternative=="two.sided"){
    if(score > 0){
      p.value = 2*(1-pnorm(score,0,1))
    } else{
      p.value = 2*pnorm(score,0,1)
    }
  } else if(alternative=="less"){
    p.value = pnorm(score,0,1)
  } else if(alternative=="greater"){
    p.value = 1-pnorm(score,0,1)
  }
  
  results = c(score,p.value)
  return(results)
}