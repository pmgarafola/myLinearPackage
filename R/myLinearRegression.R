#' Perform Linear Regression on user provided dependent variable Y and 
#' covariates X, i.e. Y = B0+B1x1+...+BnXn
#'
#' If the number of covariates is 5 or fewer, scatter plots of
#' Y input and each pair of covariates will be produced. Otherwise the
#' function will return a character string "Too may variables to plot. Maximum is 5."
#'
#' @param Y A vector of outcomes. Numeric.
#' @param X A matrix of covariates. Numeric.
#'   Columns correspond to covariates and rows correspond to
#'   values. Maximum 5 covariates allowed.
#' @param sub a set of integers corresponding to rows in X.
#' Only the specified rows will be used for modeling and
#' plotting.
#'
#' @return A list with three elements:
#' 1. plot: A plot object containing the pairwise scatter plots 
#' or a character string indicating there are too many covariates to 
#' plot.
#' 2. coefficients: A vector of model coefficients
#' 3. pvals: A vector of p-values for each coeffcient in the model
#' 
#' @export
#' @examples
#' #Compute a 2-covariate linear regression y~x1+x2, for rows 1-100
#' #inclusive in y, x1, x2, and then print the pairwise scatter plot 
#' #returned by the function.
#' #  
#' index<-1:100
#' myData<-data.frame(cbind(y,x1,x2)
#' model<-myLinearRegression(myData$y,myData[,2:3],index)
#' model$plot
#' #
#' #
#' #Compute a 1-covariate linear regression y~x1, for rows with selected 
#' #color category and then print the coefficients returned.
#' #  
#' colors<-rep(c("Red","Blue","Green","Yellow"),25)
#' myData<-data.frame(cbind(y,x1,colors))
#' index<-index<-as.numeric(rownames(subset(myData,color=="Red")))
#' xout<-myLinearRegression(myData$y,myData[,2:3],index)
#' xout$coefficients
#
myLinearRegression = function (Y,X,sub){
# Test arguments. If only one X is specified, skip plotting
# if more than 5 X is specified, skip plotting
    if(!is.null(ncol(X))) {  #check for whether only one covariate
        x<-X[sub,]
        y<-Y[sub]
        df<-data.frame(cbind(y,x))
   if (ncol(X)>5){
      myplot<-"Too may variables to plot. Maximum is 5."
  } else {
      myplot<-GGally::ggpairs(df)
  }
    } else {
      x<-X[sub]# Only one X specified, ncol = NULL
      y<-Y[sub]
      df<-data.frame(cbind(y,x))
      myplot<-GGally::ggpairs(df)
    }
  #
  # Run the model
  #
  lmyx<-lm(y~.,df)
  # Extract model outputs
  coefs<-lmyx$coefficients
  pvals<-summary(lmyx)$coefficients[,4]
  return(list("plot"=myplot,"coefficients"=coefs,"pvals"=pvals))
}

