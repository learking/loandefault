#read in data
##path needs to be changed
setwd("/home/kwang2/play/loanDefault/data")
library(data.table)
require(bit64)
rawData = fread("train_v2.csv")
numData = nrow(rawData)

#create binary version of response
y = as.numeric(rawData$loss != 0)
rawData$y = y

#perform logistic on all features
##deal with formula first
xnam = colnames(rawData)[2:770]
fmla = as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
ld.logr <- glm(fmla, data=rawData, family=binomial("logit"))

#check results
