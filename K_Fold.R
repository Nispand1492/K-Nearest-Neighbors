t#Name   : Nispand Mehta
#UTA ID : 1001163146
#Machine Learning Assignment 1
#Output will be stored in Op.txt file which will be created in your working directory.

#set your folder path here as working directory.

function()
{
  setwd("F:/Machine Learning/Assignment2")
  #store file in your working directory
  csvdata <- read.csv(file="ecoli.csv",head=TRUE)
  colmnames=colnames(csvdata)
  l=length(colmnames)
  x = csvdata[1:nrow(csvdata),1:ncol(csvdata)-1]
  fx = csvdata[,ncol(csvdata)]
  #sink("Op.txt")
  print("Data Structure to store data of csvfile.")
  print(x)
  print("Data Structure to store class of csvfile.")
  print(fx)
  
  #Normalization Process
  for (col in 1:(ncol(csvdata)-1))
  {
    column = csvdata[1:nrow(csvdata),col]
    maxValue = max(column)
    minValue = min(column)
    for (row in 1:nrow(csvdata))
    {
      newValue = (column[row]-minValue)/(maxValue - minValue)
      x[row,col] = newValue 
    }
  }
  print("After Normalization")
  print(x)
  k = readline("Enter value of K for Kfold Classification :")
  k = as.integer(k)
  
  #Creating K Partitions with each partition size being row/k.
  partition = array(NA,dim=c(k,ceiling(nrow(csvdata)/k),ncol(csvdata)))
  r = sample(1:k,size=k,replace = FALSE)
  i = 1
  s = 1
  rowindex= array(1,dim=k)
  for(row in 1:nrow(csvdata)) 
  {
    if(i == k + 1)
    {
      i=1
      r = sample(1:k,size=k,replace = FALSE)
    }
    for(col in 1:ncol(x))
    {
      partition[r[i],rowindex[i],col]=x[row,col]
    }
    partition[r[i],rowindex[i],ncol(csvdata)] = as.character(fx[row])
    rowindex[i] = rowindex[i] + 1
    i = i+1
  }
  
  for (j in 1:k)
  {
    cat("Partition ",j)
    print(partition[j,,])
  }
  #sink()
  return(partition)
}