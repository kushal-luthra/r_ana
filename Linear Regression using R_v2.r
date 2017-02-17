#setwd("D:/Dropbox/Public/Temp/Analytics/BA/Regression modelling, CRR and Time series/Linear & Logistic In R")

mydata<-read.csv("car_sales.csv")
View(mydata)

#to view structure of data ~ to proc contents in SAS
str(mydata)

#now we perform Univariate analysis, i.e. analysis at each individual varaible level
#for numeric- mean, sd, missing, distrubtion etc. -- hels find outliers, missing etc
#For this we have a user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s  #upper limit is covered in 95% confidence interval = mean + 2*Sd
  LC <- m-2*s  #lower limit is covered in 95% confidence interval = mean - 2*Sd
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


vars <- c( "Sales_in_thousands" , "X__year_resale_value" ,  "Price_in_thousands",   
           "Engine_size" , "Horsepower", "Wheelbase" , "Width" ,"Power_perf_factor" , "Length" , "Curb_weight" , 
            "Fuel_capacity", "Fuel_efficiency" )

#function apply() helds us apply mystats() function on vars one by one; 
#we also did transpose for better display of output
diag_stats<-t(data.frame(apply(mydata[vars], 2, mystats)))
View(diag_stats)

diag_stats["Sales_in_thousands", "UC"]
#?data.frame()

# Writing Summary stats to external file

write.csv(diag_stats, file = "diag_stats.csv")

#names(diag_stats)
#View(t(diag_stats[,"UC"]))
#?names()
##We must first perform Missing value treatment
mydata<- mydata[!is.na(mydata$Sales_in_thousands),] # dropping obs where DV=missing


## Next we perform OUTLIERS treatment
#mydata$Sales_in_thousands[mydata$Sales_in_thousands>257.086342425636] <-257.086342425636
#mydata$X__year_resale_value[mydata$X__year_resale_value>52.4331275042866] <-52.4331275042866
#mydata$Price_in_thousands[mydata$Price_in_thousands>70.4457144064253] <-70.4457144064253

#################we perform outlier treatment using code below#############


x=t(data.frame(diag_stats[,"UC"]))
#View(x)

#code below tests for upper range of values
for (i in 1:ncol(x)) {
  z=colnames(x)[i]
  print(z)
  print(x[,z])
  
}


#lets take copy of original dataset mydata for testing below
#y=mydata                              
#View(y)                              

#let us capture summary stats of y in a dataset summary_y_before
library(dplyr)
summary_y_before<-summary(mydata)
summary_y_before

#lets perform the outlier treatment for upper level values
for (a in 1:ncol(x) ){
  z=colnames(x)[a]
  print(z)
  print(x[1,z])
  print(a)
  
  for (b in 1:nrow(mydata)){
    if (b==1){print("entering inner loop")}
    p=mydata[b,z]
    q=x[1,z]
    
    #since we are performing outlier treatment before missing value treatment, we must check for value is not NA.
    #else it will break down below IF code
    if(!is.na(p)){
      if(p>q)
      {
        print(b)
        print(mydata[b,z])
        print(x[1,z])
        mydata[b,z]=x[1,z]
      }
    }
    
    if (b==nrow(mydata)){print("exiting inner loop")} 
  }
  
}






#######################outlier treatment ends####################

################missing value treatment##################33
require(Hmisc)
#below we use function impute() which is in Hmisc library, and 
# in it for every variable passed, we replace missing value with Mean.
# similar to it we have proc standard function in SAS
mydata1<-data.frame(apply(mydata[vars],2, function(x) impute(x, mean))) #Imputing missings with mean for IV's

mydat2<-cbind(mydata1,Vehicle_type=mydata$Vehicle_type )

#R code for categorical variables(Converting as factor variable)
mydat2$Vehicle_type <- factor(mydat2$Vehicle_type)
levels(mydat2$Vehicle_type) <- c("Car","Passenger")
View(mydat2)
# at this point we have done outlier and missing value treatment.

###############################################################################
#here we test 2 assumtions- 
#           1. Y distribution is normal.
#           2. X-Y relationship is linear
#if not then we apply transformation.
#for assumption 1: plot histogram
#see the distribution of Y variable. we find it is not normal
hist(mydat2$Sales_in_thousands)
#so we apply logarithmic transformation 
hist(log(mydat2$Sales_in_thousands))

#combine this with original dataset.
mydat2$ln_sales<-log(mydat2$Sales_in_thousands)


#for assumption 2: we have 2 options
#             a. plot scatterplotmatrix - it will show kind of relationship bw y and x variable
#             b. print correlation matrix using corr() command. 
#                correlation function shows strength of linear relationships.
#                Higher the correlation value, higher the linear X-Y relationship.
#once we see kind of relationship, we apply transformation on X varaibles.
#possible options are - log(x), exp(x),1/x,sqrt(x), etc

#for this lets import car package: companion to applied regression
require(car)
scatterplotMatrix(mydat2)
View(t(cor(mydat2[,c(1:12,14)])[,"ln_sales"]))

#testing transformation with other variabeles
#mydat3<-mydat2
#mydat3$ln_Fuel_efficiency=(mydat3$Fuel_efficiency)^2
#View(t(cor(mydat3[,c(1:12,14,15)])[,"ln_sales"]))


#############################################################################
#Splitting data into Training, Validaton and Testing Dataset
#for this we set seed
set.seed(123)
#then we use sample() function. 
#this function gives us Indexes of values randomly selected and not the random values themselves
# size= option allows us to use 0.7 or 70% data
train_ind <- sample(1:nrow(mydat2), size = floor(0.70 * nrow(mydat2)))
#View(t(train_ind))

training<-mydat2[train_ind,]
testing<-mydat2[-train_ind,]


# Multiple Linear Regression Example 
# for this we use function lm() 
#here we do linear regression on untransformed Y variable: Sales_in_thousands

#one issue to notice: vehice_type is a categorical variable. When we use categorical variable in our
# regression model, we should ideally create dummy numeric variable in their place to for modelling.
# If we dont want to create dummy variable, then other option is to convert this categorical variable into factor variable.
# In this case, R will at the back end create its own dummy variable when using this factor variable.
#  Issue in use of factor variable is that once used, even if any particular value found insignificant later, it continues to remain in model.

fit <- lm(Sales_in_thousands ~ X__year_resale_value + Price_in_thousands+ Engine_size+Horsepower+Wheelbase+Width
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=training)
summary(fit)

#here we do linear regression on transformaed Y variable: ln_Sales
fit2 <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Wheelbase
          +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=training)

#summary(fit) # show results
#if you notice adjusted R-sq and r-sq gap is small and is above 0.5, which is good.
#also f-statistic is high and p-value is low, which is acceptable.
summary(fit2)

#above we created linear regression formula
#now we must do stepwise regression using MASS package so as to refine our model(i.e. formula) above.
#aim is to know which variables to keep in final model.
#in MASS package we usually use 2 options - stepAIC and stepBIC(bayesian)
require(MASS)
#stepAIC is a technique to do regression
#how to know whether model is good? if AIC value is low, then our model is good.
#one weakness of stepAIC is that it will include vairables whose AIC value is low, and 
# not look at whether a variable is significant or not.
#so even after running stepAIC you must look at final formula e and p-value to see all variables in it are significant or not.
#if not, then remove such insignificant variables and re-run stepAIC function.

#another alternative to stepAIC is step() function which looks at significance of variable.
step3<- stepAIC(fit2,direction="both")
#stepAIC also has options like sls and slentry in SAS; use help option to explore
?stepAIC()

ls(step3)
step3$anova



#fit2 <- lm(ln_sales ~ X__year_resale_value + Price_in_thousands+ Engine_size+Wheelbase
# +Length+Curb_weight+Fuel_capacity+Fuel_efficiency+Vehicle_type, data=training)
#if we run summary of fit2, we notice-
#         1. insignificant variables are- Length,Engine_size,X__year_resale_value(in decr order)
#         2. Fuel_capacity, Fuel_efficeincy are highly correlated, and hence one of them can be removed
#we did some of these steps in fit3
summary(fit2)


fit3<-lm(ln_sales~ Price_in_thousands + Engine_size + 
           Wheelbase +  Fuel_efficiency + 
           Vehicle_type, data = training)

summary(fit3)

step4<- stepAIC(fit3,direction="both")
#stepAIC also has options like sls and slentry in SAS; use help option to explore
step3$anova

step4$anova

#stepAIC() is not the final model. It is the base on which we build our model.


#Multicollinierity Check using VIF: accept values upto 4-5.
library(car)
vif(fit3)

#######################SCORING USING PREDICT FUNCTION
#?predict()
View(t(exp(predict(fit3))))


t1<-cbind(training, pred_sales = exp(predict(fit3)))
names(t1)
#typeof(t1)
#add APE(absolute %age error value)
t1<- transform(t1, APE = abs(pred_sales - Sales_in_thousands)/Sales_in_thousands)
#alternate to above code is :-
# t1<-cbind(t1, APE = abs(t1$pred_sales - t1$Sales_in_thousands)/t1$Sales_in_thousands)
#mean APE =MAPE; ideal value is 0.1 or 0.2
mean(t1$APE) #value = 1.51 = v high = so model is not good.
View(t1)


#we do simialr actions on validation/testing data
t2<-cbind(testing, pred_sales=exp(predict(fit3,testing)))
t2<- transform(t2, APE = abs(pred_sales - Sales_in_thousands)/Sales_in_thousands)
mean(t2$APE) #MAPE=1.67 = v high
View(t2)

#now 2 thing to notice-
#         1. model accuracy - high MAPE means our model is not accurate.
#         2. model validation - similar range of MAPE means our model(training Vs testing) is validated


##################################Decile Analysis Reports - t1(training)
#delving deeper, we can see where our model is failing using decile analysis.
#basic approach -[apply to training dataset]
#     1. divide data into deciles based on predicted sales value
#     2. compare avg predicted sales with actual sales for each decile, to find out which decile the key problem is.
#     3. 2 imp things to notice while doing decile analysis are-
#             a. our predicted sales are in an order, ascending or descending as the case may be.
#             b. diff bw actual and predicted sales. if high diff in a decile, 
#                look at induvidual values of that decile and see what really happened- have we done missing value imputation, outlier treatment correctly

# find the decile locations 
decLocations <- quantile(t1$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_sales,c(-Inf,decLocations, Inf))

require(sqldf)
t1_DA <- sqldf("select decile, 
                count(decile) as count, 
                avg(pred_sales) as avg_pre_sales,   
                avg(Sales_in_thousands) as avg_Actual_sales
                from t1
                group by decile
                order by decile desc")

View(t1_DA) #we see decile 7 and 4 are not followuing teh trend of gradual decrease
#we can view individual values below. 
View(t1[t1$decile==7,c("pred_sales","Sales_in_thousands","APE")])
View(t1[t1$decile==7,c("pred_sales","Sales_in_thousands","APE")])

write.csv(t1_DA,"mydata1_DA.csv")
View(diag_stats)

##################################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_sales,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(Sales_in_thousands) as avg_Actual_sales
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"t2_DA.csv")


# Other useful functions 

coefficients(fit3) # model coefficients
confint(fit3, level=0.95) # CIs for model parameters 
fitted(fit3) # predicted values
residuals(fit3) # residuals
anova(fit3) # anova table 
influence(fit3) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

#########SCORING USING MATHEMATICAL EQUATION############

#Creating dummy varibles

mydata1$VT[mydata1$Vehicle_type ==  "Passenger"] <- 1
mydata1$VT[mydata1$Vehicle_type ==  "Car"] <- 0

summary(fit)


############################### SCoring Data sets/Predicting the sales#####################################
mydata1$Ln_pre_sales<- (-2.321593  +
                         mydata1$Price_in_thousands* -0.054988 +
                         mydata1$Engine_size*0.254696  +
                         mydata1$Wheelbase*0.047546	+
                         mydata1$Fuel_efficiency*0.068975+
                         mydata1$VT*-0.573255)
mydata1$Pre_sales= exp(mydata1$Ln_pre_sales);


###################################END OF REGRESSION case study 




