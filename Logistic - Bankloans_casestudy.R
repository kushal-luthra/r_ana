#Set Directory
setwd("F:\\BigData\\BIG DATA(R+HADOOP)\\Week-4\\Day-1")

#Read CSV file
mydata<-read.csv("bankloans.csv",header=T)
#View(mydata)
View(mydata)
str(mydata)

#convert categorical variable infto factor variable
mydata$ed <- factor(mydata$ed)

#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x) #table gives frequencies
    prop<-prop.table(table(x)) #prop table gives %ages - row and col wise
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#Vector of numerical variables:find posn of numericl and non-numeric variables
num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)



#Applying above defined function on numerical variables
my_num_data<-t(data.frame(apply(mydata[num_var], 2, var_Summ)))
my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)

# missing values
apply(is.na(mydata[,]),2,sum)

#split data into 2 parts -
#               1. mydata1=xisting customers(further divided into dafaulters and non-defaulters)
#               2. New_cust -  here dfault value is missing, representing customers are new and so will be PREDICTED to default or not.
mydata1 <- mydata[!is.na(mydata$default),]
New_cust <- mydata[is.na(mydata$default),]

View(mydata1)
View(New_cust)



#Missing Value Treatment in current data
mydata1[,num_var] <- apply(data.frame(mydata1[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
mydata1[,Other_var] <- apply(data.frame(mydata1[,Other_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})

#the below function does outlier treatment by capping all values less than p1 and above p99 
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
mydata1[,3] <- M1_fun( mydata1[,3] )
str(mydata1)

#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training<-mydata1[train_ind,]
testing<-mydata1[-train_ind,]
#nrow(training)
#nrow(testing)

#kushal
#Building Models for training dataset

fit<-glm(default~age+ed+employ+address+income+debtinc+creddebt+othdebt,data = training,
         family = binomial(logit))


#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

#Checking for concordance 
source("Concordance.R")
Concordance(fit)  #NOTE: To run these command, first run concordance function in Concordance.R 


#Stepwise regression
step1=step(fit)

#Final Model
fit2<-glm(default ~ employ + address + debtinc + creddebt,data = training,
         family = binomial(logit))
summary(fit2)
#source(choose.files())
Concordance(fit2)

################################VALIDATION ##############################
#Decile Scoring for 
##Training dataset
#about type= option: the type of prediction required. The default is on the scale of the linear predictors; 
# the alternative "response" is on the scale of the response variable. 
# Thus for a default binomial model the default predictions are of log-odds (probabilities on logit scale) 
# and type = "response" gives the predicted probabilities. 
# The "terms" option returns a matrix giving the fitted values of each term in the model formula on the linear predictor scale.
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.glm.html

train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)

##Creating Deciles for Decile Analysis
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)

train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), default_cnt=sum(default), 
           non_default_cnt=total_cnt -default_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)

write.csv(decile_summ_train,"fit_train_DA1.csv",row.names = F)

#Decile Analysis Reports
library(sqldf)
#it turns out sum on default variable is not working, 
# and so we needed to create a new variable new_default as int.
train1_temp<-train1
train1_temp$new_default<-as.integer(train1_temp$default)

fit_train_DA <- sqldf("select decile, 
                         min(Prob) as Min_prob,
                         max(Prob) as max_prob,
                         sum(new_default) as default_Count,
                         (count(decile)-sum(new_default)) as Non_default_Count 
                      from train1_temp
                      group by decile
                      order by decile desc")


write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)

##Testing dataset
test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)
#it turns out sum on default variable is not working, and so we needed to create a new variable new_default as int.
test_temp<-test1
test_temp$new_default<-as.integer(test_temp$default)

fit_test_DA <- sqldf("select decile, 
                        count(decile) as count, 
                        min(Prob) as Min_prob,
                        max(Prob) as max_prob, 
                        sum(new_default) as default_cnt
                     from test_temp
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)

New_cust1<-cbind(New_cust, Prob=predict(fit2, New_cust, type="response"))
View(New_cust1)
#belwo we set our default variable to 1 if Predicted prob of default is more than 0.23
New_cust1$default <- ifelse(New_cust1$Prob>0.23, 1,0)
sum(New_cust1$default)

# Confusion matrix
table(train1$Prob>0.23, train1$default)
table(test1$Prob>0.1, test1$default)

table(train1$default,ifelse(train1$Prob>0.23,1,0))
table(test1$default,ifelse(test1$Prob>0.23,1,0))

new_train1=train1
new_train1$new_pred<-ifelse(new_train1$Prob>0.5,1,0)
head(new_train1)
train1

# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(new_train1, row.vars="default",col.vars="new_pred",type='f')



#head(train1[,c("default","Prob")])

#Performance of the model
##Some Formulas
#### TPR =TP/TP+FN
#### TNR = TN/TN+FP
#### FPR = 1-TPR
#### PRECISION = TP/TP+FP
#### ACCURACY = TP+TN/P+N

train1<- cbind(training, Prob=predict(fit, type="response")) 
View(train1)
install.packages("ROCR")
require(ROCR)

#http://stats.stackexchange.com/questions/118633/finding-true-positive-negative-and-false-positive-negative-rates-using-r

pred_train_fit2 <- prediction(train1$Prob, train1$default)
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2,col='red')
abline(0, 1)
#auc = area under curve = performance
performance(pred_train_fit2, "auc")@y.values
