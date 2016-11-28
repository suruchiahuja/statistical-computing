##############################
###### Question 5
##############################

rm(list = ls())
graphics.off()
#install.packages("ElemStatLearn")
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(ElemStatLearn)
library(base)
library(nnet)

spam<- read.table(file.choose(),header = F,sep = ",")

set.seed(12345)
test <- sample(1:nrow(spam), .20*nrow(spam))
spam.test <- spam[test,]
spam.train <- spam[-test,]

n<-colnames(spam[,-58])
n1<-paste(n,collapse="+")
formula <- paste("V58~",n1)

nnet.fit <- neuralnet(as.formula(formula), data = spam.train,hidden =1, err.fct = 'ce', linear.output = FALSE)
names(nnet.fit)
nnet.fit$result.matrix
out <- cbind(as.matrix(nnet.fit$covariate), nnet.fit$net.result[[1]])
head(out)

