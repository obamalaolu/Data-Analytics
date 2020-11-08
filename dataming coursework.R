getwd()# put csv file into current project folder

train<-read.csv("AdultData.csv",sep=",",
                header = F,na.strings = " ?",
                col.names=c("age", "workclass", "fnlwgt","education", "education_num","marital_status", 
                            "occupation","relationship","race", "sex","capital_gain", "capital_loss", 
                            "hours_per_week", "native_country", "income")
                
)   

test<-read.csv("test.csv",sep=",",
                header = F,na.strings = " ?",
                col.names=c("age", "workclass", "fnlwgt","education", "education_num","marital_status", 
                            "occupation","relationship","race", "sex","capital_gain", "capital_loss", 
                            "hours_per_week", "native_country", "income")
                
)   
###################################################  solve some bug problems and transform age (factor) into num variable
train$age[1]<-39
train$age<-as.numeric(as.character(train$age))
test$age[1]<-25
test$age<-as.numeric(as.character(test$age))


###################################################  delete missing value
combi<-rbind(train,test)  
combi<-na.omit(combi)
row.names(combi) <- 1:nrow(combi) # put this dataframe into correct order(since delete missing value, sequence changed) 

################################################## delete fnlwgt & education_num variables
combi<-combi[,-c(3,5)] 


################################################## transform int varible into factor variables and it has 5 levels
combi$hours_per_week[combi$hours_per_week < 40] <- " <40"
combi$hours_per_week[combi$hours_per_week >= 40 & combi$hours_per_week <= 45] <- " 40-45"
combi$hours_per_week[combi$hours_per_week > 45 &combi$hours_per_week <= 60  ] <- " 45-60"
combi$hours_per_week[combi$hours_per_week > 60 & combi$hours_per_week <= 80  ] <- " 60-80"
combi$hours_per_week[combi$hours_per_week > 80] <- " >80"
combi$hours_per_week <- factor(combi$hours_per_week,ordered = FALSE,levels = c(" <40", " 40-45", " 45-60", " 60-80",  " >80"))


combi$workclass <- droplevels(combi$workclass) ########### delete empty factor levels


############ transform this int variable into four levels' factor variable ("zero", "low", "medium", "high") based on its nonzero IQR data
c_gain<-ifelse(combi$capital_gain ==0, "zero", ifelse( combi$capital_gain < 3464 & combi$capital_gain > 0, "low",ifelse(combi$capital_gain >= 3464 & combi$capital_gain <14084, "medium",  "high" )))
combi$capital_gain<-c_gain
combi$capital_gain <- factor(combi$capital_gain,   ordered = FALSE, levels = c( "zero", "low", "medium", "high"))


######### same as capital gain variable, transform this int variable into four levels' factor variable ("zero", "low", "medium", "high") based on its nonzero IQR data
c_loss<-ifelse(combi$capital_loss ==0, "zero", ifelse( combi$capital_loss < 1672 & combi$capital_loss > 0, "low",ifelse(combi$capital_loss >= 1672 & combi$capital_loss <1977, "medium",  "high" ))) 
combi$capital_loss<-c_loss
combi$capital_loss <- factor(combi$capital_loss,ordered = FALSE, levels = c( "zero", "low", "medium", "high"))



######## reclassify this variable:  "1" denote united states, "0" denote other countries  
combi$native_country<-ifelse(combi$native_country== " United-States", 1, 0)
combi$native_country <- factor(combi$native_country,ordered = FALSE)
########

########## solve duplicate variable problems(<=50K & <=50K. are the total same)
combi$income<-ifelse(combi$income %in% c(" <=50K"," <=50K.")," <=50K", " >50K")
combi$income <- factor(combi$income,ordered = FALSE)
#########################################################################################

Train<-combi[1:31655,]    ######## re-allocate total data into train data and test data
Test<-combi[31656:45222,]
row.names(Test) <- 1:nrow(Test) 

#################################### decision tree model 1
install.packages("rpart.plot") 
library("rpart") 
library("rpart.plot")

tree_classifier<-rpart(income ~ .,data=Train) # build training classifier model
rpart.plot(tree_classifier,type=4,extra=2,clip.right.labs=FALSE, varlen=0, faclen=2, main="Decision Tree Model of Adult data set")

prediction1 <- predict(tree_classifier, newdata=Test,type='class') # predict test result 
acc<-sum(diag(table(prediction1,Test$income)))/sum(table(prediction1,Test$income)) # show accuracy pridiction result
acc

install.packages("gmodels")
library(gmodels)
CrossTable(x = Test$income, y = prediction1, prop.chisq=FALSE) ##### use confusion matrix table to show prediction result,report shows the result
###############

#################################  decision tree model 2 by using C50 package
install.packages("C50")
library(C50)
tree_classifier2 <- C5.0(income ~ ., data = Train) # build training classifier model
prediction2 <- predict(tree_classifier2, Test)     # predict test result
summary(tree_classifier2) #########report shows the result

CrossTable(x = Test$income, y = prediction2, prop.chisq=FALSE) ###### use confusion matrix table to show prediction result,report shows the result



###################################naive bayes model
install.packages("e1071")
library(e1071)
library(gmodels)
naive_classifier <- naiveBayes(income ~ ., data = Train)    # build training classifier model
prediction3 <- predict(naive_classifier, Test)              # predict test result

CrossTable(x = Test$income, y = prediction3,prop.chisq=FALSE) ######## report shows the result



################################# plot decesion tree 1 ROC curve
install.packages("ROCR")
library(ROCR)
predictions_prob1 <- predict(tree_classifier, Test)   
pred1<- prediction(predictions = predictions_prob1[,2] ,labels = Test$income)
perf1 <- performance(pred1,measure = "tpr", x.measure = "fpr")
plot(perf1, main = " rpart decision tree classifier ROC curve for income >50K ", col ="blue",lwd = 2) 
abline(a=0,b=1,lwd =2,lty =2)
auc1<-performance(pred1,measure = "auc") ####### get AUC value
auc1


########################### plot decesion tree 2 ROC curve
predictions_prob2 <- predict(tree_classifier2, Test,type='prob')
pred2<- prediction(predictions = predictions_prob2[,2] ,labels = Test$income)
perf2 <- performance(pred2,measure = "tpr", x.measure = "fpr")
plot(perf2, main = " C50 decision tree classifier ROC curve for income >50K ", col ="blue",lwd = 2)
abline(a=0,b=1,lwd =2,lty =2)

auc2<-performance(pred2,measure = "auc") ####### get AUC value
auc2


########################### plot naive bayes ROC curve
predictions_prob3 <- predict(naive_classifier, Test,type='raw')
pred3<- prediction(predictions = predictions_prob3[,2] ,labels = Test$income)
perf3 <- performance(pred3,measure = "tpr", x.measure = "fpr")
plot(perf3, main = " na??ve bayes classifier ROC curve for income >50K ", col ="blue",lwd = 2)
abline(a=0,b=1,lwd =2,lty =2)

auc3<-performance(pred3,measure = "auc") ####### get AUC value
auc3

####################################################################################################################
#########################################################################################################



################ plot age variables and analysis (histogram)
ggplot(Train,aes(age,fill=income))+geom_histogram(color="black")+facet_grid(income~ .)+scale_x_continuous(breaks=seq(0,100,10))+ theme(axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13))
ggplot(Train,aes(age,fill=income))+geom_density(alpha = .4, color=NA)+ scale_x_continuous(breaks = seq(0, 100, 5))+theme(axis.text.x = element_text(size = 15, face = "bold"),axis.text.y = element_text(size = 12))


#######################analize hours_per_week variable (box plot) 
ggplot(train,aes(income,hours_per_week))+geom_boxplot()+  stat_summary(fun.y = mean,geom = 'point',shape = 18, color = "blue",size=3)+scale_y_continuous(breaks = seq(10, 100, 5))+  ggtitle ("working hours per week by income")+theme( plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13,face="bold"))
######  histogram plot
ggplot(train,aes(hours_per_week,fill=income))+geom_histogram(color="black")+facet_grid(income~ .)+scale_x_continuous(breaks=seq(0,100,10))+ theme(axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13))
##### show bar plot after grouped into 5 levels:
ggplot(Train,aes(hours_per_week,fill=hours_per_week))+geom_bar(color="black")+facet_grid(income~ .)+scale_y_continuous(breaks=seq(0,15000,4000))+ ggtitle ("working hours per week by income")+theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13))





################## capital_gain and capital_loss variables


#summary(train$capital_gain)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0       0    1092       0   99999 
#summary(train$capital_loss)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00   88.37    0.00 4356.00 

combi_gain<-subset(train,train$capital_gain> 0)
combi_loss<-subset(train,train$capital_loss > 0)

#summary(combi_gain$capital_gain)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#114    3464    7298   12978   14084   99999 
#summary(combi_loss$capital_loss)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#155    1672    1887    1868    1977    4356

######################################################
##############nonzero capital gain histgram plot
ggplot(combi_gain,aes(capital_gain))+geom_histogram(fill="#3FE4F3",color="#000000") +scale_x_continuous(breaks=seq(0,100000,10000),labels =seq(0,100000,10000))+xlab("Nonzero capital gain")+scale_y_continuous(breaks = seq(0, 2000, 200))+ggtitle ("Nonzero capital gain ")+theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 12, face = "bold"),axis.text.y = element_text(size = 13))

ggplot(combi_gain,aes(capital_gain,fill=income))+geom_histogram('bins'=40,color="#000000") + facet_grid(income ~ .)+scale_x_continuous(breaks=seq(0,100000,10000),labels =seq(0,100000,10000))+xlab("Nonzero capital gain")+ggtitle ("Nonzero capital gain grouped by income")+theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 12, face = "bold"),axis.text.y = element_text(size = 13))
############ box plot
ggplot(combi_gain,aes(income,capital_gain))+geom_boxplot()+  stat_summary(fun.y = mean,geom = 'point',shape = 18, color = "blue",size=3)+ ggtitle ("nonzero capital gain by income")+theme( plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13,face="bold"))+scale_y_continuous(breaks = seq(0, 35000, 5000))+coord_cartesian(ylim = c(0, 35000))


####################captial loss

ggplot(combi_loss,aes(capital_loss))+geom_histogram(fill="#3FE4F3",color="#000000") +xlab("Nonzero capital loss")+scale_x_continuous(breaks=seq(0,5000,250))+ggtitle ("Nonzero capital loss ")+theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 11, face = "bold"),axis.text.y = element_text(size = 13))

ggplot(combi_loss,aes(income,capital_loss))+geom_boxplot()+  stat_summary(fun.y = mean,geom = 'point',shape = 18, color = "blue",size=3)+ ggtitle ("nonzero capital loss by income")+theme( plot.title = element_text(size=14, face="bold",hjust = 0.5),axis.text.x = element_text(size = 16, face = "bold"),axis.text.y = element_text(size = 13,face="bold"))+scale_y_continuous(breaks = seq(0, 5000, 500))

#summary(combi$capital_gain)
#zero    low medium   high 
#41432    941   1895    954 
#summary(combi$capital_loss)
#zero    low medium   high 
#43082    487    921    732

##########################################




























