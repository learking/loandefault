# YaTa code for AUC 0.94, feature used f274, f528
library(caTools)
setwd("C:/Users/Yuelong\ G/Desktop/loadDefault")
rawData = read.csv("train_v2.csv")
tt=rawData[,c('f274','f528','loss')] # ,'f527'
set.seed(20140219)
tt=tt[sample(nrow(tt)),]
train=tt[1:(0.7*nrow(tt)),]
test=tt[-(1:(0.7*nrow(tt))),]
train[,ncol(train)]=ifelse(train[,ncol(train)]>0,1,0)
test[,ncol(test)]=ifelse(test[,ncol(test)]>0,1,0)
model= glm(as.factor(train[,ncol(train)])~.,data=train[,-ncol(train)],family=binomial)
pr = predict(model, test[,-ncol(test)],type="response")

pr=ifelse(is.na(pr),mean(!is.na(pr)),pr)
cat('AUC:',colAUC(pr,test[,ncol(test)]),'\n')

# test set prediction
pr_train <- ifelse(pr>0.5, 1, 0)
test_1 <- test[pr_train==1,'loss']
test_0 <- test[pr_train==0,'loss']

# find median in test 30% portion
test=tt[-(1:(0.7*nrow(tt))),]
test_sub<-test[which(pr_train==1),]
median(test_sub[,'loss'])

# prepare test set for submission
testData = read.csv("test_v2.csv")
test2 <- testData[,c('f274', 'f528')]
pr_test <- predict(model, test2, type="response")
pr_test=ifelse(is.na(pr_test),mean(!is.na(pr)),pr_test)
pr_pred <- ifelse(pr_test > 0.5, 1, 0)
testData$pr_pred <- pr_pred
submit <- testData[,c('id', 'pr_pred')]
colnames(submit) <- c('id', 'loss')
write.csv(submit, 'submit1.csv', row.names=FALSE)