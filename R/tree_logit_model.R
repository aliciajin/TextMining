#############################
# < Yiqian Jin >
# < Nov.25 >
# < 05 - 6, 7 >


############# use code from 04, obtain 4 dtm ##############
#(a)
#setwd()
install.packages('tm')
install.packages('SnowballC')
library(tm)
library('SnowballC')

source('prepare.R')  ## using modified code
preprocess.directory('fp_hamilton_train')
preprocess.directory('fp_hamilton_test')
preprocess.directory('fp_madison_train')
preprocess.directory('fp_madison_test')

#(b)
hamilton.train=read.directory('fp_hamilton_train_clean')
hamilton.test=read.directory('fp_hamilton_test_clean')
madison.train=read.directory('fp_madison_train_clean')
madison.test=read.directory('fp_madison_test_clean')

#(c)
full_list=c(hamilton.train, hamilton.test, madison.train, madison.test)
length(full_list)  ## output 77
mydictionary=make.sorted.dictionary.df(full_list)

#(d)
dtm.hamilton.train=make.document.term.matrix(hamilton.train, mydictionary)
dtm.hamilton.test=make.document.term.matrix(hamilton.test, mydictionary)
dtm.madison.train=make.document.term.matrix(madison.train, mydictionary)
dtm.madison.test=make.document.term.matrix(madison.test, mydictionary)

################### end reference#############
label_h_train=rep(1, nrow(dtm.hamilton.train))
label_m_train=rep(0, nrow(dtm.madison.train))
label_h_test=rep(1, nrow(dtm.hamilton.test))
label_m_test=rep(0, nrow(dtm.madison.test))

dtm.hamilton.train.labeled=cbind(dtm.hamilton.train, label_h_train)
dtm.madison.train.labeled=cbind(dtm.madison.train, label_m_train)
dtm.hamilton.test.labeled=cbind(dtm.hamilton.test, label_h_test)
dtm.madison.test.labeled=cbind(dtm.madison.test, label_m_test)

train.df=data.frame(rbind(dtm.hamilton.train.labeled, dtm.madison.train.labeled))
test.df=data.frame(rbind(dtm.hamilton.test.labeled, dtm.madison.test.labeled))

col_labels=as.vector(mydictionary$word)
col_labels[length(col_labels)+1]='y'  ### the length is changed to 4876
colnames(train.df)=col_labels
colnames(test.df)=col_labels

############### two data frame created #################

###### (a) 
library(rpart)
attach(train.df)
tree.Gini=rpart(y~., data=train.df, method='class')
par(mfrow=c(1,1), xpd = NA)
plot(tree.Gini)
text(tree.Gini, use.n=TRUE)
title('Tree_Gini impurity')

pred.test.Gini=predict(tree.Gini, test.df, type='class')
pred.test.Gini
summary(pred.test.Gini)
table(pred.test.Gini, test.df$y)


###### (b)
tree.info=rpart(y~. , train.df, method='class', parms=list(split='information'))
par(mfrow=c(1,1), xpd=NA)
plot(tree.info)
text(tree.info, use.n=TRUE)
title('Tree_information gain')

pred.test.info=predict(tree.info, test.df, type='class')
pred.test.info
summary(pred.test.info)
table(pred.test.info, test.df$y)


###### ############################################################
####  <07>  ##
####  center and scale it, except y

num=nrow(mydictionary)
#### ##method 1
# train.df.scale=scale(train.df[, 1:num])
# test.df.scale=scale(test.df[, 1:num])

######  method  2  
total.df=rbind(train.df, test.df)
total.df.scale=scale(total.df[, 1:num])
train.df.scale=total.df.scale[ 1:nrow(train.df),  ]
test.df.scale=total.df.scale[ -(1:nrow(train.df)),   ]

######  method 3  test - train.mean and train.sd
train.df.scale=scale(train.df[, 1:num])mean.train=apply(train.df, 2, mean)
sd.train=apply(train.df, 2, sd)

test.df.center=sweep(test.df[, 1:num],  2,  mean.train )
test.df.scale=sweep(test.df.center, 2,  sd.train, FUN="/")   ## problem!!


### (a)
install.packages('glmnet')
library(glmnet)
## glmnet can only work on matrics, not data frames
detach(train.df)
 
train.scale=as.matrix(train.df.scale)
test.scale=as.matrix(test.df.scale)
train.scale[is.na(train.scale)]<-0
test.scale[is.na(test.scale)]<-0
cv.fit.ridge<-cv.glmnet(train.scale, train.df$y, alpha=0, family="binomial")
 ##   plot(cv.fit.ridge)
pred.ridge<- predict(cv.fit.ridge, test.scale, type="class")
table(pred.ridge, test.df$y, useNA="always")

fit.ridge<-glmnet(train.scale, train.df$y, alpha=0)
cv.fit.ridge$lambda.min
n.ridge = which(cv.fit.ridge$lambda==cv.fit.ridge$lambda.min) ##  100 
beta=fit.ridge$beta[, n.ridge ]  ## too many beta, most near 0 

# find best ten words, biggest ten beta
beta[order(abs(beta), decreasing=TRUE)][1:10]


######### for lasso ###############
set.seed(103)
cv.fit.lasso<-cv.glmnet(train.scale, train.df$y, alpha=1, family="binomial")
###   plot(cv.fit.lasso)
pred.lasso <-predict(cv.fit.lasso, test.scale, type="class")
table(pred.lasso, test.df$y)

fit.lasso<- glmnet(train.scale, train.df$y, alpha=1)
cv.fit.lasso$lambda.min
n.lasso= which(cv.fit.lasso$lambda==cv.fit.lasso$lambda.min)
beta2=fit.lasso$beta[, n.lasso]
beta2[order(abs(beta2), decreasing=TRUE)][1:10]  







