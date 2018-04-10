rm(list = ls())         
ChurnR <- function(
                           start_ind,
                           end_ind,
                           start_dep,
                           end_dep,
                           evaluate=TRUE){
  f <- "%d/%m/%Y"
  t1 <- as.Date(start_ind, f)
  t2 <- as.Date(end_ind, f)
  t3 <- as.Date(start_dep, f)
  t4 <- as.Date(end_dep, f)
  length_ind <- t2 - t1
  
  if (!require("randomForest")) {
    install.packages('randomForest',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('randomForest')
  }
  ；；；；
  if (!require("AUC")) {
    install.packages('AUC',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('AUC')
  }
  
  if (!require("lift")) {
    install.packages('lift',
                     repos="https://cran.rstudio.com/", 
                     quiet=TRUE) 
    require('lift')
  }
  
  readAndPrepareData <- function(train=TRUE,...){
    ## Input data
    cat("Input data:")
    time <- Sys.time()
    f <- "%d/%m/%Y"
    setClass("fDate")
    setAs(from="character", to="fDate", def=function(from) as.Date(from, format="%d/%m/%Y"))
    
    # READING IN THE DATA
    
    ## Complaints
    complaints <- read.table(paste0("http://ballings.co/hidden/aCRM/data/chapter6/","complaints.txt"), sep = ";", header = T, colClasses = c(rep("character",2),
                                                                                                 "factor",
                                                                                                 "fDate",
                                                                                                 "factor",
                                                                                                 "factor",
                                                                                                 "factor"))
    ## Credit
    credit <- read.table(paste0("http://ballings.co/hidden/aCRM/data/chapter6/","credit.txt"), sep = ";", header = T, colClasses = c(rep("character",2),
                                                                                         "factor",
                                                                                         "fDate",
                                                                                         "factor",
                                                                                         "numeric",
                                                                                         "integer"))
    ## Customers
    customers <- read.table(paste0("http://ballings.co/hidden/aCRM/data/chapter6/","customers.txt"), sep = ";", header = T, colClasses = c("character",
                                                                                               "factor",
                                                                                               "fDate",
                                                                                               "factor",
                                                                                               "character",
                                                                                               "character"))
    ## Subscriptions
    subscriptions <- read.table(paste0("http://ballings.co/hidden/aCRM/data/chapter6/","subscriptions.txt"), sep = ";",header = T, colClasses = c(rep("character",2),
                                                                                                      rep("factor",2),
                                                                                                      rep("fDate",2),
                                                                                                      rep("integer",2),
                                                                                                      "fDate",
                                                                                                      rep("factor",2),
                                                                                                      "fDate",
                                                                                                      "integer",
                                                                                                      rep("numeric",8)))
    subscriptions <- subscriptions[-which(is.na(subscriptions$GrossFormulaPrice)),]
    
    ## Formula
    formula <- read.table(paste0("http://ballings.co/hidden/aCRM/data/chapter6/","formula.txt"), sep = ";" ,header = T, colClasses = c("character",
                                                                                           "integer",
                                                                                           "factor",
                                                                                           "integer"))
    
    
    
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    ###############################################################
    cat("data preparation:")
    time <- Sys.time()
    
    
    if (train==FALSE){
      dots <- list(...) # list(...) evaluates all arguments and
      # returns them in a named list
      t2 <- dots$end_ind
      t1 <- t2 - dots$length_ind
      rm(dots)
    }
    
    # ONLY SELECTING WHAT WE NEED
    
    complaints <- complaints[,"CustomerID"]
    #complaints$CustomerID <- 
    credit$ProcessingDate <- credit$CreditID <- credit$NbrNewspapers <- NULL
    customers <- customers[,c("CustomerID", "Gender", "ZIP")]
    
    formula <- formula[,c("FormulaID", "FormulaType", "Duration")]
    
    # FORMATTING INDIVIDUAL TABLES 
    
    ## Complaints Table
    # Number of Complaints
    complaints <- as.data.frame(table(complaints), stringsAsFactors = F)
    colnames(complaints) <- c("CustomerID", "NbrComplaints")
    
    customers <- merge(customers, complaints, by = "CustomerID", all.x = T)
    customers[,4][which(is.na(customers[,4]) == TRUE)] <- 0
    
    ## Credit
    # Number of Credits
    NbrCredits <- as.data.frame(table(credit$SubscriptionID), stringsAsFactors = F)
    colnames(NbrCredits) <- c("SubscriptionID", "NbrCredits")
    credit$SubscriptionID <- as.character(credit$SubscriptionID)
    credit <- merge(credit, NbrCredits, by = "SubscriptionID", all.x = T)
    
    # Total Amount of the Credits combined
    CreditAmount <- aggregate(credit$Amount, by = list(credit$SubscriptionID), sum)
    colnames(CreditAmount) <- c("SubscriptionID", "TotalCreditAmount")
    
    credit <- merge(credit, CreditAmount, by = "SubscriptionID", all.x = T)
    
    credit$Amount <- NULL
    
    ## Formula
    formula$Duration <- ifelse(formula$Duration >= 3, 1, 0)
    formula$Duration <- as.factor(as.integer(formula$Duration))
    
    subscriptions[,8][which(is.na(subscriptions[,8]) == TRUE)] <- 0
    
    subscriptions$PaymentDate <- NULL
    
    # MERGING ALL TABLES
    subscriptions$SubscriptionID <- as.character(subscriptions$SubscriptionID)
    subscriptions <- merge(subscriptions, credit, by = "SubscriptionID", all = T)
    
    subscriptions$ActionType <- as.factor(ifelse(is.na(subscriptions$ActionType)==TRUE,0,subscriptions$ActionType))
    subscriptions$CreditSource <- as.factor(ifelse(is.na(subscriptions$CreditSource)==TRUE,0,subscriptions$CreditSource))  
    
    
    for (i in 23:24){
      subscriptions[,i][which(is.na(subscriptions[,i]) == TRUE)] <- 0
    }
    
    subscriptions <- merge(subscriptions, formula, by = "FormulaID", all.x = T)
    subscriptions$CustomerID <- as.character(subscriptions$CustomerID)
    
    subscriptions <- merge(subscriptions, customers, by = "CustomerID", all.x = T)
    
    
    
    # Split the data into independent and dependebt data
    INT <- subscriptions[which(subscriptions$StartDate <= t2 & subscriptions$StartDate >= t1),]
    
    if (train == TRUE){
      DET <- subscriptions[which(subscriptions$RenewalDate <= t4 & subscriptions$RenewalDate > t3),]
    }
    ## make sure to only select subscription ID who did not end subscription before t2
    renew_before_t2 <- unique(INT[INT$RenewalDate<=t2, "SubscriptionID"])
    INT <- INT[!INT$SubscriptionID%in%renew_before_t2,]
    
    
    if (train == TRUE){
      dependent <- merge(data.frame(SubscriptionID=unique(INT[,"SubscriptionID"]),stringsAsFactors = FALSE),
                         data.frame(DET[,"SubscriptionID",drop=FALSE],Churn=0),by="SubscriptionID",
                         all.x=TRUE)
      dependent$Churn[is.na(dependent$Churn)] <- 1
      dependent$Churn <- as.factor(dependent$Churn)
      
    }
    if (train == TRUE){
      ## merge INT and dependent into basetable
      basetable <- merge(INT, dependent, by="SubscriptionID")
      basetable$RenewalDate <- NULL
      ## extract the first two digits of zip code as new zip code
      basetable$ZIP2 <- substr(basetable$ZIP,1,2)
      basetable$ZIP <- NULL
      colnames(basetable)[29] <- "ZIP"
      basetable$ZIP <- as.factor(basetable$ZIP)
      basetable$FormulaID <- as.character(basetable$FormulaID)
      basetable$ProductID <- as.factor(basetable$ProductID)
      basetable$StartDate <- basetable$EndDate <- NULL
      
    }else{
      basetable <- INT
      basetable$RenewalDate <- NULL
      ## extract the first two digits of zip code as new zip code
      basetable$ZIP2 <- substr(basetable$ZIP,1,2)
      basetable$ZIP <- NULL
      colnames(basetable)[28] <- "ZIP"
      basetable$ZIP <- as.factor(basetable$ZIP)
      basetable$FormulaID <- as.character(basetable$FormulaID)
      basetable$ProductID <- as.factor(basetable$ProductID)
      basetable$StartDate <- basetable$EndDate <- NULL
      basetable$Churn <- NULL
      
    }
   
    
    if (train==TRUE){
      basetable$SubscriptionID <- basetable$CustomerID <-  NULL
      Churn <- basetable$Churn
      basetable$Churn <- NULL
    }
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    #Our basetable is now ready and we can
    #move on the modeling phase
    if (train==TRUE){
      return(list(predictors=basetable, Churn=Churn))
    } else {
      return(basetable)
    }
  }
  basetable <- readAndPrepareData()
  ## Modeling 
  if (evaluate==TRUE){
    cat("Evaluating model:")
    time <- Sys.time()
    #We'll be using random forest and hence we will
    #not require a validation set.
    #Split the data in a train and test set
    ind <- 1:nrow(basetable$predictors)
    indTRAIN <- sample(ind,round(0.5*length(ind)))
    indTEST <- ind[-indTRAIN]
    #Fit the random forest on the training set
    rf <- randomForest(x=basetable$predictors[indTRAIN,],
                       y=basetable$Churn[indTRAIN])
    #Deploy the forest on the test set
    pred <- predict(rf,basetable$predictors[indTEST,],
                    type="prob")[,2]
    cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
        attr(Sys.time()- time,"units"), "\n")
    cat(" Number of predictors:", ncol(basetable$predictors[indTRAIN,]),
        "predictors\n")
    cat(" AUROC:",
        round(auc(roc(pred,basetable$Churn[indTEST])),4),"\n")
    cat(" Top decile lift of:",
        round(TopDecileLift(pred,basetable$Churn[indTEST]),4),"\n")
  }
  cat("Creating model:")
  time <- Sys.time()
  #Build model on all data
  rf <- randomForest(x=basetable$predictors,
                     y=basetable$Churn)
  cat(format(round(as.numeric(Sys.time()- time),1),nsmall=1,width=4),
      attr(Sys.time()- time,"units"), "\n")
  l <- list(rf = rf,
            readAndPrepareData = readAndPrepareData,
            f = f,
            length_ind = length_ind)
  class(l) <- "ChurnR"
  return(l)
  
}

ChurnModel <-
  ChurnR(
           start_ind="02/01/2006",
           end_ind="05/01/2010",
           start_dep="06/01/2010",
           end_dep="02/03/2011",
           evaluate=TRUE)







