#### Take Cronbach's alpha estimate and 95% CI ####
## object should be from saved psych::alpha() function 

format.alpha <- function(object){
  # Isolate each number and round to 2 decimals. Then drop the leading zero for APA. 
  alpha <- round(object$boot.ci[2], 2)
  
  # To protect against negative values, we need to check if alpha or the lower bound is less than .00
  # If it is, replace with .00, following Henson (2001)
  if (sign(alpha) == 1){
    alpha <- str_sub(as.character(alpha), 2, 4)
  } else if (sign(alpha) == -1){
    alpha <- ".00"
  }
  
  lower <- round(object$boot.ci[1], 2)
  
  if (sign(lower) == 1){
    lower <- str_sub(as.character(lower), 2, 4)
  } else if (sign(lower) == -1){
    lower <- ".00"
  }  
  
  # Not necessary for upper limit. This should not be able to be negative 
  upper <- round(object$boot.ci[3], 2)
  upper <- str_sub(as.character(upper), 2, 4)
  
  # Combine output into APA style reporting 
  output <- paste0(alpha, ", 95% CI = [", lower, ", ", upper, "]")
  return(output)
}

#### Take Afex ANOVA object and report each effect ####
## Requires saved afex object and effect to subset the data 

format.afex_anova <- function(anova_object, effect){
  # Isolate each element for APA style. Round to 2 decimals apart from eta squared
  df1 <- as.numeric(round(anova_object$anova_table$`num Df`[effect], 2))
  df2 <- as.numeric(round(anova_object$anova_table$`den Df`[effect], 2))
  fval <- as.numeric(format(round(anova_object$anova_table$F[effect], digits=2), nsmall = 2)) 
  # format retains the trailing zeros
  effect_size <- as.numeric(round(anova_object$anova_table$ges[effect], 3))
  
  # Set rules for formatting p values. 
  # If p > .001, leave as is and remove leading zero. 
  pval <- as.numeric(round(anova_object$anova_table$`Pr(>F)`[effect], 3))
  if (pval >= 0.001){
    pval <- str_sub(as.character(pval), 2, 5)
    pval <-  paste0(", $p$ = ", pval)
  # If p < .001, manually format as p < .001
  } else if (pval < 0.001){
    pval <- ", $p$ < .001"
  }
  
  # Do the same for generalised eta squared 
  effect_size <- as.numeric(round(anova_object$anova_table$ges[effect], 3))
  if (effect_size >= 0.001){
    effect_size <- str_sub(as.character(effect_size), 2, 5)
    effect_size <-  paste0(", $\\hat{\\eta}^2_G$ = ", effect_size)
    
  } else if (effect_size < 0.001){
    effect_size <- ", $\\hat{\\eta}^2_G$ < .001"
  }
  
  # There are some examples of super small F values, so give that a minimum value of .01. 
  if (fval >= 0.01){
    # keep as is if it is 
    fval <- fval
    
    # If it's smaller than 0.01, just format as 0.01. 
  } else if (fval < 0.01){
    fval <- "0.01"
  }
  
  # Combine result into nice APA style 
  result <- paste0("$F$ (", df1, ", ", df2, ") = ", fval, pval, effect_size)
  return(result)
}

#### Take TOST object and report it as a t-test ####
# In both scenarios, we need the second t-test as it's the largest p value. 

format.TOST <- function(object){
  # Isolate each element and round to 2 decimals
  t.stat <- as.numeric(round(object$TOST_t2, 2))
  df <- as.numeric(round(object$TOST_df, 2))
  
  # Set rules for formatting p values. 
  # If p > .001, leave as is and remove leading zero. 
  pval <- as.numeric(round(object$TOST_p2, 3))
  
  if (pval >= 0.001){
    pval <- str_sub(as.character(pval), 2, 5)
    pval <-  paste0(", $p$ = ", pval)
    # If p < .001, manually format as p < .001
  } else if (pval < 0.001){
    pval <- ", $p$ < .001"
  }
  
  # Combine each element into nice APA style
  result <- paste0("$t$ (", df, ") = ", t.stat, pval)
  return(result)
}


#### Take object from splithalf package and report in APA ####
# Requires the condition (200 or 500), but have to subset in code anyway

format.splithalf <- function(object, condition){
  # Subset data based on condition 
  data <- subset(object$final_estimates, condition == condition)
  
  # Isolate and format r. Remove leading zeros. 
  r <- object$final_estimates$spearmanbrown
  
  if (r >= 0.001){
    r <- str_sub(as.character(r), 2, 4)
    # If r is negative, manually set to 0. 
  } else if (r < 0.001){
    r <- ".00"
  }
  
  # Isolate and format lower CI. Remove leading zeros.
  CI_LL <- object$final_estimates$SB_low
  
  if (CI_LL >= 0.001){
    CI_LL <- str_sub(as.character(CI_LL), 2, 4)
    # If lower CI is negative, manually set to 0. 
  } else if (CI_LL < 0.001){
    CI_LL <- ".00"
  }
  
  # Leave upper CI as is, as it should never be negative. Just remove leading zero. 
  CI_UL <- object$final_estimates$SB_high
  CI_UL <- str_sub(as.character(CI_UL), 2, 4)
  
  result <- paste0("$r$ = ", r, ", 95% CI = [", CI_LL, ", ", CI_UL, "]")
  return(result)
}

#### format Mean and SD to help table reporting. Presents in format Mean (SD) ####
# Takes a vector of numbers
mean_sd <- function(data){
  
  # Take mean and SD, round to 2 decimals 
  mean <- round(mean(data, na.rm = T), 2)
  sd <- round(sd(data, na.rm = T), 2)
  
  # return the combined mean (SD) to help reporting 
  return(paste0(mean, " (", sd, ")"))
}

#### format median and IQR to help table reporting. Presents in format Median (IQR)
median_IQR <- function(data){
  
  # Take mean and SD, round to 2 decimals 
  med <- round(median(data, na.rm = T), 2)
  iqr <- round(IQR(data, na.rm = T), 2)
  
  # return the combined median (IQR) to help reporting 
  return(paste0(med, " (", iqr, ")"))
}
