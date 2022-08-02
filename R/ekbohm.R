#' Ekbohm's MLE based test
#'
#' Calculates Ekbohm's MLE based test statistic and corresponding p-value for partially matched samples.
#'
#' @param x first sample
#' @param y second sample
#' @param alternative comparison in alternative hypothesis (two-sided, less, or greater)
#'
#' @return Vector of length two whose first element is the test statistic and second element is the p-value
#'
#' @examples
#' ekbohm(hetNA[,1],hetNA[,2],alternative="two.sided")
#' 
#' @export
ekbohm = function(x,y,alternative=c("two.sided","less","greater")){
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
  Tbar = mean(group2)
  Nbar = mean(group3)
  T1bar = mean(group1a)
  N1bar = mean(group1b)
  ST1 = sd(group1a)
  SN1 = sd(group1b)
  ST = sd(group2)
  SN = sd(group3)
  STN1 = cov(group1a,group1b)
  r = STN1 / (ST1*SN1)
  g = n1*(n1+n2+n3*r)*((n1+n2)*(n1+n3)-(n2*n3*r^2))^(-1)
  f = n1*(n1+n3+n2*r)*((n1+n2)*(n1+n3)-(n2*n3*r^2))^(-1)
  sigma = (ST1^2*(n1-1)+SN1^2*(n1-1)+(1+r^2)*(ST^2*(n2-1)+SN^2*(n3-1)))/(2*(n1-1)+(1+r^2)*(n2+n3-2))
  V1 = sigma*((2*n1*(1-r)+(n2+n3)*(1-r^2))/((n1+n2)*(n1+n3)-(n2*n3*r^2)))
  score = (f*(T1bar-Tbar)-g*(N1bar-Nbar)+Tbar-Nbar)/sqrt(V1)
  ## Under the null hypothesis, ZLS is approximately distributed
  ## as t with n1 degrees of freedom
  if(alternative=="two.sided"){
    p.value = 2*(1-pt(score,n1))
  } else if(alternative=="less"){
    p.value = 1-pt(score,n1,lower.tail=TRUE)
  } else if(alternative=="greater"){
    p.value = 1-pt(score,n1,lower.tail=FALSE)
  }
  
  results = c(score,p.value)
  return(results)
}