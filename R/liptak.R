#' Liptak's weighted z-test
#'
#' Calculates Liptak's weighted z-test p-value for partially matched samples.
#'
#' @param x first sample
#' @param y second sample
#' @param alternative comparison in alternative hypothesis (two-sided, less, or greater)
#'
#' @return P-value for the test
#'
#' @examples
#' liptak(hetNA[,1],hetNA[,2],alternative="two.sided")
#' 
#' @export
liptak = function(x,y,alternative=c("greater","less","two.sided")){
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
  ## Calculating the weights
  w1 = sqrt(2*n1)
  w2 = sqrt(n2+n3)
  ## p_1i is the p-value for biomarker i computed from the n1 paired samples
  ## p_2i is the corresponding p-value computed from the independent groups with n2 and n3 samples per group
  dif = group1a-group1b
  if(length(x)>length(y))stop("not enough 'y' observations")
  if(length(x)<length(y))stop("not enough 'x' observations")
  if(alternative=="greater"){
    p_1i = t.test(group1a,group1b,alternative="greater",paired=TRUE)$p.value
    ## For group 2 and group 3, also check for equal variance
    if(var.test(group2,group3)$p.value>0.05){
      p_2i = t.test(group2,group3,alternative="greater",var.equal=TRUE)$p.value
    } else{
      p_2i = t.test(group2,group3,alternative="greater",var.equal=FALSE)$p.value
    }
    ## Now compute Z_1i and Z_2i
    Z_1i = qnorm(1-p_1i)
    Z_2i = qnorm(1-p_2i)
  } else{
    p_1i = t.test(group1a,group1b,alternative="less",paired=TRUE)$p.value
    ## For group 2 and group 3, also check for equal variance
    if(var.test(group2,group3)$p.value>0.05){
      p_2i = t.test(group2,group3,alternative="less",var.equal=TRUE)$p.value
    } else{
      p_2i = t.test(group2,group3,alternative="less",var.equal=FALSE)$p.value
    }
  }
  ## Now compute Z_1i and Z_2i
  Z_1i = qnorm(1-p_1i)
  Z_2i = qnorm(1-p_2i)
  p.ci = 1-pnorm(((w1*Z_1i)+(w2*Z_2i))/sqrt(w1^2 + w2^2))
  if(p.ci<(1/2)){
    p.ci.2 = 2*p.ci
  } else{
    p.ci.2=2*(1-p.ci)
  }
  if(alternative=="two.sided"){
    return(p.ci.2)
  } else{
    return(p.ci)
  }
}	