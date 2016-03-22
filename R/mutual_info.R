#############################
# < Yiqian Jin >
# < Dec.8 >
# < 06 - 3 >

# setwd()
library(tm)
library('SnowballC')
source('prepare.R')  ## using modified code
preprocess.directory('fp_hamilton_train')
preprocess.directory('fp_hamilton_test')
preprocess.directory('fp_madison_train')
preprocess.directory('fp_madison_test')

hamilton.train=read.directory('fp_hamilton_train_clean')
hamilton.test=read.directory('fp_hamilton_test_clean')
madison.train=read.directory('fp_madison_train_clean')
madison.test=read.directory('fp_madison_test_clean')

full_list=c(hamilton.train, hamilton.test, madison.train, madison.test)
length(full_list)  ## output 77
mydictionary=make.sorted.dictionary.df(full_list)
dict.h.train=make.sorted.dictionary.df(hamilton.train)
dict.m.train=make.sorted.dictionary.df(madison.train)
dict.train=make.sorted.dictionary.df(c(hamilton.train, madison.train))

sum.h.train=sum(dict.h.train[ , 2 ])
sum.m.train=sum(dict.m.train[ , 2])
sum.train=sum(dict.train[ , 2])
########################################
prob.train=mydictionary  ## 4875
for(i in 1: nrow( mydictionary )){
	this=match(mydictionary[i , 1], dict.train[ , 1])
	if(! is.na(this)){
		prob.train[ i, 2]=dict.train[this, 2]/sum.train
		}
	else{
		prob.train[ i, 2]=0
		}
}

prob.h.train=mydictionary
for(i in 1: nrow( mydictionary )){
	this=match(mydictionary[i , 1], dict.h.train[ , 1])
	if(! is.na(this)){
		prob.h.train[ i, 2]=dict.h.train[this, 2]/sum.h.train
		}
	else{
		prob.h.train[ i, 2]=0
		}
}

prob.m.train=mydictionary
for(i in 1: nrow( mydictionary )){
	this=match(mydictionary[i , 1], dict.m.train[ , 1])
	if(! is.na(this)){
		prob.m.train[ i, 2]=dict.m.train[this, 2]/sum.m.train
		}
	else{
		prob.m.train[ i, 2]=0
		}
}

##############################################
prob.train=dict.train  ## 4131
for(i in 1: nrow( prob.train )){
	prob.train[ i, 2]=dict.train[i, 2]/sum.train
}

prob.h.train=dict.train
for(i in 1: nrow( prob.train )){
	this=match(prob.train[i , 1], dict.h.train[ , 1])
	if(! is.na(this)){
		prob.h.train[ i, 2]=dict.h.train[this, 2]/sum.h.train
		}
	else{
		prob.h.train[ i, 2]=0
		}
}

prob.m.train=dict.train
for(i in 1: nrow( prob.train )){
	this=match(prob.train[i , 1], dict.m.train[ , 1])
	if(! is.na(this)){
		prob.m.train[ i, 2]=dict.m.train[this, 2]/sum.m.train
		}
	else{
		prob.m.train[ i, 2]=0
		}
}
############################################################
p.h=35/50
p.m=15/50
mi1=vector(, nrow(mydictionary))
mi2=vector(, nrow(mydictionary))

mi1=rep(0, nrow(mydictionary))
mi1=rep(0, nrow(mydictionary))

for(i in 1: nrow(mydictionary)){
	mi1[i]=prob.h.train[i, 2]*p.h*log(prob.h.train[i, 2]/prob.train[i, 2])
	          +(1-prob.h.train[i, 2])*p.h*log( (1-prob.h.train[i, 2]) / (1-prob.train[i, 2]) )
	         	          
	mi2[i]=prob.m.train[i, 2]*p.m*log(prob.m.train[i, 2] / prob.train[i, 2])
	          +(1-prob.m.train[i, 2])*p.m*log((1-prob.m.train[i, 2]) / (1-prob.train[i, 2]) )

}


mi1[is.na(mi)]<-0
mi2[is.na(mi)]<-0
mi=mi1+mi2
sort(mi, decreasing=TRUE)[1:20]
numNA=sum(is.na(mi))

nlist=c(200,500,1000,2500)
index=c(1,2,3,4)
mi.df=data.frame(mi)
col_labels_words=as.vector(mydictionary$word)
colnames(mi.df)=col_labels_words
sort(mi.df)[1:nlist[i]]

labels_index=c(1:length(col_labels_words))
mi.df.index=data.frame(mi)
colnames(mi.df.index)=labels_index
selected_index=colnames( sort(mi.df.index[1:nlist[i]]) )

dtm.hamilton.train2=dtm.hamilton.train[ , selected_index]
dtm.madison.train2=dtm.madison.train[ , selected_index]
dtm.hamilton.test2=dtm.hamilton.test[ , selected_index]
dtm.madison.test2=dtm.madison.test[ , selected_index]

###  use Gini ipurity
#### first create two data frame
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

col_labels=as.vector(dict.train$word)
col_labels[length(col_labels)+1]='y'  ### the length is changed to 4131+1
colnames(train.df)=col_labels
colnames(test.df)=col_labels

library(rpart)
tree.Gini=rpart(train.df$y~., data=train.df, method='class')
pred.test.Gini=predict(tree.Gini, test.df, type='class')
table(pred.test.Gini, test.df$y)
correct_Gini=mean(pred.test.Gini==test.df$y)
#false negative 
#false positive

### using information gain split
tree.info=rpart(y~. , train.df, method='class', parms=list(split='information'))
pred.test.info=predict(tree.info, test.df, type='class')
table(pred.test.info, test.df$y)
correct_info=mean(pred.test.info==test.df$y)
#false negative 
#false positive
