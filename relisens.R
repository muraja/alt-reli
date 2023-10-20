relisens <- function(data, n.factors, method = "score", rot = "oblimin", fam = "ml",
                     listwise = TRUE, mean = TRUE, det = TRUE, sto = TRUE, msto = F, 
                     round = TRUE){
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
      mice(method = "norm.predict", m=1, printFlag = FALSE) %>% 
      complete
  }
  # stochastic linear regression imputation
  sto_imp <- function(data){
    data %>% 
      zap_labels %>% 
      mice(method = "norm.nob", m = 1, printFlag = FALSE) %>% 
      complete
  }
  # multiple imputation (stochastic linear regression)
  msto_imp <- function(data){
    data %>% 
      zap_labels %>% 
      mice(method = "norm.nob", m = 5, printFlag = FALSE) %>% 
      complete
  }
  
  # function to calculate the reliabilities
  reliabilities <- function(data, n.factors, imputation, method){
    D <- imputation(data)
    # calculating correlation matrix
    R <- cor(D) 
    # calculating means and standard deviations
    M <- cbind(mean=sapply(D, mean), stddev=sapply(D, sd))
    # defining a target for rotation
    A <- fa(na.omit(data), nfactors=n.factors, rotate=rot, fm = fam)$loadings[] 
    # factor analysis with varimax rotation
    A. <- fa(D, nfactors=n.factors, rotate=rot, fm = fam)$loadings[]
    # rotating factor loadings if there are multiple factors
    B <- if(n.factors > 1){
      GPArotation::targetT(A., Target=A)$loadings[]
    } else {A.} 
    # computing factor score weights
    W <- solve(R) %*% B 
    colnames(W) <- paste0("Score",1:ncol(W))
    # computing phi
    orthogonal <- c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor")
    oblique <- c("Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster")
    if(rot %in% orthogonal || n.factors == 1){
      phi <- diag(ncol(B))
    }
    if(rot %in% oblique && n.factors > 1){
      phi <- fa(D, nfactors = n.factors, rotate = rot, fm = fam)$Phi[]
    }
    # Calculate reliabilities using the chosen method (sum|image|score)
    if(method == "sum"){
      df <- reli(R,B,O=phi,M=M[,"stddev"],sum=TRUE,scores=FALSE) %>% 
        as.data.frame %>% 
        rownames_to_column("ReliMethod")
      # removing reliabilities of the factor images
      df <- df[!grepl('ML',df$ReliMethod),] # this solution only works with fam="ml"
      # adding rho_MC; weighted sum
      rho_MC <- psych::omega(B, n.factors, Phi = phi, two.ok = TRUE)
      rho_MC <- rho_MC$omega.tot
      df <- cbind(df, rho_MC)
      # adding a column telling the imputation method used
      df$ImputationMethod <- as.character(substitute(imputation))
      df %>% select(ImputationMethod, everything())
    }
    else if (method == "image"){
      df <- reli(R,B,O=phi,M=M[,"stddev"],sum=FALSE,scores=FALSE) %>% 
        as.data.frame %>% 
        rownames_to_column("ReliMethod")
      # adding a column telling the imputation method used
      df$ImputationMethod <- as.character(substitute(imputation))
      df %>% select(ImputationMethod, everything())
    }
    else if(method == "score"){
      df <- reli(R,B,W,O=phi,M=M[,"stddev"]) %>% 
        as.data.frame %>% 
        rownames_to_column("ReliMethod")
      # adding a column telling the imputation method used
      df$ImputationMethod <- as.character(substitute(imputation))
      df %>% select(ImputationMethod, everything())
    }
  }
  # binding the reliability results with each imputation method used into one dataframe 
  DF <- rbind(if(listwise == TRUE){reliabilities(data, n.factors, lwd, method)},
              if(mean == TRUE){reliabilities(data, n.factors, mean_imp, method)},
              if(det == TRUE){reliabilities(data, n.factors, det_imp, method)},
              if(sto == TRUE){reliabilities(data, n.factors, sto_imp, method)},
              if(msto == TRUE){reliabilities(data, n.factors, msto_imp, method)},
              make.row.names = FALSE)

  if (round == TRUE){DF <- DF %>% format(digits = 3)}

  return(DF)
}
