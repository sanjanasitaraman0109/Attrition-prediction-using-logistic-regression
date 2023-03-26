library (readxl)
setwd("C:/Users/sanja/OneDrive/Desktop/TERM III - LECTURES/Predictive analytics and forecasting")
attrition <- read_excel("LR.xlsx")
str(attrition)
attrition$Attrition=as.factor(attrition$Attrition)
attrition$Work_Challenging=as.factor(attrition$Work_Challenging)
attrition$Work_Envir=as.factor(attrition$Work_Envir)
attrition$Compensation=as.factor(attrition$Compensation)
attrition$Tech_Exper=as.factor(attrition$Tech_Exper)
str(attrition)
summary(attrition)

#lR model
attri_model=glm(Attrition~Yrs_Exp+Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=attrition,family=binomial(link="logit"))
summary(attri_model)

#model 2
#IDV which has a P value >0.05 will be eliminated from the model
attri_model2=glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=attrition,family=binomial(link="logit"))
summary(attri_model2)

#residual deviance reduces with the addition of coefficient
anova(attri_model2,"PChiSq")

#comparison of both the models
anova(attri_model2,attri_model, test="Chisq")

#Model Fit verification
(pseudo_R_sq=1-(attri_model2$deviance/attri_model2$null.deviance))

############## Not required ############


#model validation-DATA PARTITION
#set.seed(1234)
#cv = sort(sample(nrow(attrition), nrow(attrition)*.8))
#Training_Data<-attrition[cv,]
#Testing_Data<-attrition[-cv,]

##Method - 2
set.seed(1234)
ind <- sample(2, nrow(attrition),replace = TRUE,prob = c(0.8, 0.2))
Training_Data=attrition[ind==1,]
Testing_Data=attrition[ind==2,]
summary(Training_Data)
summary(Testing_Data)

#new Data
new_data=data.frame(Work_Challenging="No",Work_Envir="Low",Compensation="Excellent",Tech_Exper="Excellent")

##Model built using training data
model=glm(Attrition~Work_Challenging+Work_Envir+Compensation+Tech_Exper,data=Training_Data,family=binomial(link="logit"))
summary(model)

#predict values of training data using original Log reg model
p1=predict(attri_model2,Training_Data,  type="response")
p1
attributes(p1)
p1=ifelse(p1>.5,1,0)

(t1=table(Predicted = p1 , Actual = Training_Data$Attrition))

#classifications error -train data = (1 - accuracy rate)
1-sum(diag(t1))/sum(t1)

#predict values of test data using Log reg model built using training_data
p2=predict(model,Testing_Data,  type="response")
p2
p2=ifelse(p2>.5,1,0)
p2
(t2=table(Predicted = p2, Actual = Testing_Data$Attrition))

#miscalssification error -test data = (1 - accuracy rate)
1-sum(diag(t2))/sum(t2)

#Goodness of fit
with(attri_model2,pchisq(attri_model2$null.deviance-attri_model2$deviance,attri_model2$df.null-attri_model2$df.residual,lower.tail = F))

#predict
pn=predict(attri_model2,new_data,  type="response")
pn
attributes(pn)
pn1=ifelse(pn>.5,"attrition yes","attrition no")
pn1
?set.seed?
  

#ROC CURVE (choose any 1 code from below)
library(pROC)
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE)
par(pty = "s")
##Extra codes
#roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE)
#roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE,legacy.axes=TRUE)

roc.info <- roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE,legacy.axes=TRUE)
roc.info$thresholds
roc.info

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "attrition".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "no attrition". 
## Thus, TPP = 0% and FPP = 0%

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## We can calculate the area under the curve...
roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

#roc(attrition$Attrition, attri_model2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

