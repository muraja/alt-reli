crelisens <- function(data, items.until, listwise = TRUE, mean = TRUE, det = TRUE, sto = TRUE, msto = F, round = TRUE){
  # Defining imputation functions
  #dependencies
  library(mice)
  library(haven)
  
  # Listwise deletion
  lwd <- function(data){data %>% na.omit}
  # Mean imputation
  mean_imp <- function(data){
    data %>% 
      mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 
  }
  # deterministic
  det_imp <- function(data){
    data %>% 
      zap_labels %>% 
      mice(method = "norm.predict", m=1) %>% 
      complete
  }
  # stochastic linear regression imputation
  sto_imp <- function(data){
    data %>% 
      zap_labels %>% 
      mice(method = "norm.nob", m = 1) %>% 
      complete
  }
  # multiple imputation (stochastic linear regression)
  msto_imp <- function(data){
    data %>% 
      zap_labels %>% 
      mice(method = "norm.nob", m = 5) %>% 
      complete
  }
  
  # function to calculate the reliabilities
  reliabilities <- function(data, items.until, imputation){
    sink("NUL")
    # dependencies
    library(psych)
    library(reliacoef)
    library(MBESS)
    
    D <- imputation(data)
    
    ## obtain tau-equivalent reliability (alpha)
    rho_T <- psych::alpha(D)
    rho_T <- rho_T$total
    rho_T <- rho_T$raw_alpha
    
    ## obtain unidimensional congeneric reliability
    rho_C <- MBESS::ci.reliability(D)
    rho_C <- rho_C$est
    
    if(items.until < ncol(D)){
      ## obtain multidimensional tau-equivalent reliability
      rho_MT <- reliacoef::stratified_alpha(D, until = items.until)
      rho_MT <- rho_MT$rel
      
      ## obtain second-order factor reliability
      rho_SOF <- second_order(D, nobs = nrow(D), until = items.until)
      rho_SOF <- rho_SOF$rel
      
      ## obtain bifactor reliability
      rho_BF <- bifactor(D, nobs = nrow(D), until = items.until)
      rho_BF <- rho_BF$rel
      
      ## obtain correlated factors reliability
      rho_CF <- correlated_factors(D, nobs = nrow(D), until = items.until)
      rho_CF <- rho_CF$rel
    } else {
      rho_MT <- NA
      rho_SOF <- NA
      rho_BF <- NA
      rho_CF <- NA
    }
    
    relis <- data.frame(rho_T, rho_MT, rho_C, rho_BF, rho_SOF, rho_CF)
    
    # adding a column telling the imputation method used
    relis$ImputationMethod <- as.character(substitute(imputation))
    relis <- relis %>% select(ImputationMethod, everything())
    sink()
    relis
  }

  # binding the reliability results with each imputation method used into one dataframe 
  DF <- rbind(if(listwise == TRUE){reliabilities(data, items.until, lwd)},
              if(mean == TRUE){reliabilities(data, items.until, mean_imp)},
              if(det == TRUE){reliabilities(data, items.until, det_imp)},
              if(sto == TRUE){reliabilities(data, items.until, sto_imp)},
              if(msto == TRUE){reliabilities(data, items.until, msto_imp)},
              make.row.names = FALSE)

  if (round == TRUE){DF <- DF %>% format(digits = 3)}

  return(DF)
}
