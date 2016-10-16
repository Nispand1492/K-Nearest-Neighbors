match_acuracy = function(testrow,pred_class,acc_ctr)
{
  lenc = ncol(testrow)
  if(testrow[lenc]==pred_class)
  {
    acc_ctr = acc_ctr + 1
  }
  return(acc_ctr)
}

calc_accuracy = function(rows,ac_ctr)
{
  return((ac_ctr/rows) * 100)
}
setwd("F:/Machine Learning/Assignment2")
KFold <- dget("K_Fold.R")
partitions = KFold()
len = length(partitions[1,1,])
k = readline("Enter value of K for KNN algorithm :")
k = as.integer(k)
euclidian <- dget("Euclidean.R")
sink("Op2.txt")
for (j in 1:10)
{
  # j corresponds to each partition which is taken as test partition
  cat("Testing Partition : ",j,"\n")
  cat("--\n")
  test = partitions[j,,]
  training = partitions[-j,,]
  dim(training) = c(9*nrow(partitions[1,,]),ncol(test))
  training = cbind(training,distance="NA")
  training = na.omit(training)
  test = na.omit(test)
  l = 1
  ac_ctr = 0
      for(p in 1:nrow(test))
      {
        #p corresponds to each row
        #compare each row with every other row of training data.
        l = 1
        cat("Predicting class for row:")
        cat(p," of partition ",j,"\n")
        while(l < nrow(training))
          {
            if(is.element("NA",test[p,]))
            {
              break
            }
            else
            {
              distance = euclidian(as.numeric(test[p,1:(len-1)]),as.numeric(training[l,1:(len-1)]))
              training[l,len+1] =distance 
              l = l + 1
            }
          }
        #prepare data set for the prediction of class
        d <- as.data.frame(training,stringsAsFactors = FALSE)
        d[,len+1]<-as.numeric(d[,len+1])
        d = d[order(d[,9]),]
        cat("Neighbors of row ")
        cat(p," :\n ")
        neighbors = d[1:k,]
        print.data.frame(neighbors,row.names = FALSE)
        cat("\n")
        #predict class of that row
        cls = names(which.max(table(neighbors[,len])))
        #check for accuracy.
        #print("--")
        #cat("Row : ",test[p,])
        #print("-")
        if(as.character(test[p,len])==cls)
        {
          cat("True..Increasing accuracy counter:")
          ac_ctr = ac_ctr + 1
          cat(ac_ctr,"\n")
        }
        else
        {
          cat("False,Not Increasing accuracy:",ac_ctr,"\n")
        }
        #ac_ctr = match_acuracy(test[p,],cls,ac_ctr)
        cat("Actual Class :: Predicted Class\n")
        cat(as.character(test[p,len]),"::",as.character(cls),"\n")
        cat("\n--\n")
        #print(training)
      }
  ac_ctr = calc_accuracy(nrow(test),ac_ctr)
  cat("\n---\n")
  cat("\nNo of rows : ")
  cat(nrow(test),"\n")
  cat("\nAccuracy for Test partition ",j,":")
  cat(ac_ctr,"\n")
  ac_ctr = 0
}
sink()




