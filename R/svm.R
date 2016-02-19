#############################
# < Yiqian Jin >
# STAT W4240 
# Homework 06 
# < Dec.8 >
# < Problem 4 >

############# use code from hw4, obtain 4 dtm ##############

#(a)
setwd("~/Documents/courses/data mining/hw6")


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

################### end reference from hw4 , then create two data frame#############
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

##standarlize training and testing, using method  2: total scale

num=nrow(mydictionary)
total.df=rbind(train.df, test.df)
total.df.scale=scale(total.df[, 1:num])
train.df.scale=total.df.scale[ 1:nrow(train.df),  ]
test.df.scale=total.df.scale[ -(1:nrow(train.df)),   ] ## only 4875, without y

train.scale=as.matrix(train.df.scale)
test.scale=as.matrix(test.df.scale)
train.scale[is.na(train.scale)]<-0
test.scale[is.na(test.scale)]<-0
train.df$y=as.factor(train.df$y)
test.df$y=as.factor(test.df$y)




####### question 4 
# (a)
library(e1071)
length=length(train.scale)
train.q4=train.scale[, 1:100]
test.q4=test.scale[ , 1:100]
train.df$y=as.factor(train.df$y)
test.df$y=as.factor(test.df$y)
model1=svm(train.df$y~. , data=train.q4, kernel='linear',  type='C')
print(model1)
summary(model1)
pred.test=predict(model1, test.q4)
table(pred.test, test.df$y)


#(b)
nword=seq(5,100,5)
correct_rate=vector( , length(nword))
for(i in 1:length(nword)){
	train.q4=train.scale[, 1:nword[i]]
    test.q4=test.scale[ , 1:nword[i]]
    model_b=svm(train.df$y~. , data=train.q4, kernel='linear',  type='C')
    pred.test=predict(model_b, test.q4)
    correct_rate[i]=mean(pred.test==test.df$y)

}

plot(nword, correct_rate, type='b')


#(c)

nword=seq(5,100,5)
correct_rate_c=vector( , length(nword))
for(i in 1:length(nword)){
	train.q4=train.scale[, 1:nword[i]]
    test.q4=test.scale[ , 1:nword[i]]
    model_b=svm(train.df$y~. , data=train.q4, kernel='radial',  type='C')
    pred.test=predict(model_b, test.q4)
    correct_rate_c[i]=mean(pred.test==test.df$y)

}

plot(nword, correct_rate_c, type='b')



# (d)

a=which(colnames(train.df)=='upon')
b=which(colnames(train.df)=='depart')
train.q4d=cbind(train.scale[, a], train.scale[ , b])
test.q4d=cbind(test.scale[, a], test.scale[, b])
model_d=svm(train.df$y~. ,  data=train.q4d, kernel='radial', type='C')
pred.test=predict(model_d, test.q4d)
correct_rate_d=mean(pred.test==test.df$y)
##   correct_rate_d=0.962963
 plot(model_d, dataframe)
 dataframe=as.data.frame(cbind(train.q4d, train.df$y))