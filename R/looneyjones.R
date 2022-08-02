#' Looney and Jones' corrected Z-test
#'
#' Calculates Looney and Jones' corrected Z-test test statistic and corresponding p-value for partially matched samples.
#'
#' @param x first sample
#' @param y second sample
#' @param alternative comparison in alternative hypothesis (two-sided, less, or greater)
#'
#' @return Vector of length two whose first element is the test statistic and second element is the p-value
#'
#' @examples
#' looneyjones(hetNA[,1],hetNA[,2],alternative="two.sided")
#' 
#' @export
looneyjones = function(x,y,alternative=c("two.sided","less","greater")){
  alternative = match.arg(alternative)
  ## This has to be done for every test
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
  n12 = append(group1a,group2)
  n13 = append(group1b,group3)
  Tbar = mean(n12)
  Nbar = mean(n13)
  ST2 = (sd(n12))^2
  SN2 = (sd(n13))^2
  STN1 = cov(group1a,group1b)
  score = (Tbar-Nbar) / sqrt((ST2/(n1+n2))+(SN2/(n1+n3))+((2*n1*STN1)/((n1+n2)*(n2+n3))))
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