
#-------------------------------------
# EE substudies analysis 

# configure data directories
# source base functions
# load libraries
#-------------------------------------

library(tidyverse)
library(haven)
library(washb)
library(foreign)
library(data.table)
library(tmle)
library(SuperLearner)
library(devtools)
library(kableExtra)
library(here)
library(caret)




theme_ki<-function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

theme_set(theme_ki())

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F",
               "#BCBD22","#17BECF")


# df=wbb
# Ws=NULL
# outcome="diar7d"
# exposure="sth"
# family="binomial"


washb_sth_glm <- function(df, Ws=NULL, outcome="pos", exposure,  family="binomial"){

  cat("N before dropping missing: ", nrow(df),"\n")
  
  df$Y <- df[[outcome]]
  df <- df %>% filter(!is.na(Y))
  #print(summary(df$Y))
  
  df$X <- df[[exposure]]
  df <- df %>% filter(!is.na(X))
  #print(summary(df$exposure))
  
  Wvars<-NULL
  minN<-NA
  
  if(length(unique(df$Y))<=2){
    if(length(unique(df$Y))>1 & length(unique(df$X))>1){
      if(length(unique(df$Y))>1 & length(unique(df$X))==2){
        minN <- min(table(df$Y, df$X))
      }else{
        minN <- min(table(df$Y))
      }
    }else{
      minN <- 0
    }
  }
  cat(minN,"\n")
  
  #cat(minN>=10 | length(unique(df$Y)) > 2)
  if(minN>=10 | length(unique(df$Y)) > 2){
    
    if(!is.null(Ws)){
      Wdf <- df %>% ungroup() %>% select(any_of(Ws)) %>% select_if(~sum(!is.na(.)) > 0)
      
      if(ncol(Wdf)>0){
        #drop covariates with near zero variance
        if(length(nearZeroVar(Wdf))>0){
          Wdf <- Wdf[,-nearZeroVar(Wdf)]
        }
        if(family=="neg.binom"){
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family="gaussian", print=T)
        }else{
          Wvars <- washb_prescreen(Y=df$Y, W=Wdf, family=family, print=T)
        }
        if(identical(Wvars, character(0)) ){
          Wvars <- NULL
        }
        if(family!="gaussian" & !is.null(Wvars)){
          nY<-floor(min(table(df$Y))/10) -1 #minus one because 10 variables needed to estimate coef. of X
          if(nY>=1){
            if(length(Wvars)>nY){
              Wvars<-Wvars[1:nY]
            }        
          }else{
            Wvars=NULL
          }
        }
      }else{
        Wvars <- NULL
      }
      df <- df %>% subset(., select =c("Y","X","clusterid", Wvars))
      df <- df[complete.cases(df),]
      cat("Covariates selected: ", Wvars,"\n")
      cat("N after dropping missing: ", nrow(df),"\n")
    }else{
      df <- df %>% subset(., select =c("Y","X","clusterid"))
      cat("N before dropping missing: ", nrow(df),"\n")
      df <- df[complete.cases(df),]
      cat("N after dropping missing: ", nrow(df),"\n")
    }
    
    #model formula
    f <- ifelse(is.null(Wvars),
                "Y ~ X",
                paste0("Y ~ X + ", paste(Wvars, collapse = " + ")))
    #fit model
    fit <- mpreg(formula = as.formula(f), df = df, vcv=FALSE, family=family)
    coef <- as.data.frame(t(fit[2,]))
    if(family=="gaussian"){
      res <- data.frame(Y=outcome,
                        X = exposure,
                        coef=coef$Estimate,
                        RR=NA,
                        se=coef$`Std. Error`,
                        Zval=coef$`z value`,
                        pval=coef$`Pr(>|z|)`)
      
      res$ci.lb <- res$coef - 1.96*res$se
      res$ci.ub <- res$coef + 1.96*res$se
    }else{
      res <- data.frame(Y=outcome,
                        X = exposure,
                        coef=coef$Estimate,
                        RR=exp(coef$Estimate),
                        se=coef$`Std. Error`,
                        Zval=coef$`z value`,
                        pval=coef$`Pr(>|z|)`)
      
      res$ci.lb <- exp(res$coef - 1.96*res$se)
      res$ci.ub <- exp(res$coef + 1.96*res$se) 
    }
  }else{
    res <- data.frame(Y=outcome,
                      X = exposure,
                      coef=NA,
                      RR=NA,
                      se=NA,
                      Zval=NA,
                      pval=NA,
                      ci.lb=NA,
                      ci.ub=NA,
                      minN=minN)
  }
  if(length(unique(df$Y))<=2){
    res$minN <- minN
    res$n<-sum(df$Y, na.rm=T)
  }
  
  res$N<-nrow(df)
  res$W <-ifelse(is.null(Wvars), "unadjusted", paste(Wvars, sep="", collapse=", "))

  #print(res)
  return(res)
}




mpreg <- function(formula, df, vcv=FALSE, family) {
  # modified Poisson regression formula
  # dataset used to fit the model	
  if(family!="neg.binom"){
    fit <- glm(as.formula(formula),family=family,  data=df)
  }else{
    fit <- MASS::glm.nb(as.formula(formula), data=df)
  }
  vcovCL <- cl(df=df, fm=fit, cluster=df$clusterid)
  rfit <- coeftest(fit, vcovCL)
  #print(summary(fit))
  #cat("\n\nRobust, Sandwich Standard Errors Account for Clustering:\n")
  #print(rfit) 
  if(vcv==FALSE) {
    return(rfit)
  } else {
    list(fit=rfit,vcovCL=vcovCL)
  }
}



cl   <- function(df,fm, cluster){
  # df: data used to fit the model
  # fm : model fit (object)
  # cluster : vector of cluster IDs
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum)) ;
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}


