loandf<-read.csv("loan_default.csv")
summary(loandf)
# taking a look at the summary for each variable , some things that dtand out are data types, 
#most are numeric but a few, gender marital_status and emp_status ar characters and many are binary variables.

(sum(is.na(loandf)))
# 0 missing values

#take a look at some of the non numeric columns try to get these into 1/0 if binary like i suspect they can be.

unique(loandf$Gender) # ok yes male and female are the options so going ot change to binary
unique(loandf$Marital_status) # again single/married can be expressed as 1/0
unique(loandf$Emp_status) # and same here


loandf$Gender <- ifelse(loandf$Gender == "Male", 1, 0)
loandf$Marital_status <- ifelse(loandf$Marital_status == "Single", 1, 0)
loandf$Emp_status <- ifelse(loandf$Emp_status == "unemployed", 1, 0)
# alphabetical order from 0-1 so f=0 m=1, marreid=0 single = 1 etc.

# now we have everything numeric we can run a correlation
cc<-cor(loandf)
View(cc)
# nothing too strongly correlated with default, Age and savings amount moderatley correlated. 
#the model
logmodel<- glm(Default~.,data = loandf,family=binomial(link=logit))
 summary(logmodel)

 # only a few of the items were found to be significant 
 #(intercept , Checking_amound, term, credit_score, savings_amount and Age)
 
 #Building new model just using significant variables
 
 logmodel2<-glm(Default~Checking_amount+Term+Credit_score+Saving_amount+Age, data = loandf,family =binomial(link=logit))

 summary(logmodel2) 
# everything is nice and significant now
 
 # interprut coefficients as log odds. for every 1 unit change in term, the log odds of default increase by .17
 
 # train test split
 
 train_obs<-floor(.7*nrow(loandf))
print(train_obs) 

set.seed(40)

train_ind<-sample(seq_len(nrow(loandf)),size = train_obs)
test<-train_ind
#splitting data
train_data<-loandf[train_ind,]
test_data<-loandf[-train_ind,]
# running model using training data and isgnificant values only
logmodel22<-glm(Default~Checking_amount+Term+Credit_score+Saving_amount+Age, data = train_data,family =binomial(link=logit))
summary(logmodel22)


library(car)
#checking multicolliniarity assumptions
vif(logmodel22)

prob<-predict(logmodel22,test_data,type='response')
prob1<-data.frame(prob)
pred<-prediction(prob,test_data$Default)

results<-ifelse(prob1>.7,1,0)
testing_high <- test_data$Default
table(testing_high,results)

mcerror<- mean(results != testing_high)
accuracy<- 1-mcerror
print(accuracy)

# accuracy rate of .9

install.packages('caret')
library(caret)
confusionMatrix(prob1,testing_high, positive = '1')
curve(AUC)
install.packages('ROCR')
library(ROCR)
install.packages("pROC")
library(pROC)


roc_curve <- roc(testing_high, results)
auc_value <- auc(roc_curve)

# Print the AUC value
print(auc_value)

# area under curve .8636

prob2<-predict()
#ROC curve
pmf<- performance(pred,measure='tpr',x.measure = 'fpr')
plot(pmf,col = 'blue')


