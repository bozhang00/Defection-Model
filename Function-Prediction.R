predict.ChurnR <- function(object, dumpDate) {
  #Load all required packages
  for (i in c("AUC","lift","randomForest")) {
    if (!require(i,character.only=TRUE,quietly=TRUE)) {
      install.packages(i,
                       repos='http://cran.rstudio.com',quiet=TRUE)
      require(i,
              character.only=TRUE,
              quietly=TRUE)
    }
  }
  #Make sure all variables in the readAndPrepareData
  #enclosing environment (where it was defined) are removed
  #to avoid unexpected results
  environment(object$readAndPrepareData) <- environment()
  #browser()
  basetable <- object$readAndPrepareData(train=FALSE,
                                         end_ind= as.Date(dumpDate, object$f),
                                         length_ind=object$length_ind)
  cat("Predicting: ")
  time <- Sys.time()
  ans <- data.frame(CustomerID=basetable$CustomerID,
                    Score=predict(object=object$rf,
                                  newdata=basetable,
                                  type="prob")[,2])
  ans <- ans[order(ans$Score, decreasing=TRUE),]
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  
  ans
}
pred <-
  predict(object=ChurnModel,
          dumpDate="02/03/2011")

