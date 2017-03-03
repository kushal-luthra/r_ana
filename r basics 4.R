#r 4
stores <- read.csv("C:/Users/jai mata di/Desktop/ktemp/analabs/R/11-R-1/DataSets for R sessions/Stores.csv",stringsAsFactors = F)
str(stores)

#?read_csv
#?read.csv


#univariate analysis

#a. for numerical variable we can find-
#Mean, median, mode
#Sd, variance
#Min,max, range
#Skewness, kurtosis
#Correlation, covariance
#Sum,count
#Percentiles, quartiles,deciles
#Plot: histogram, box plot

#b. for categorical variable=
#  -frequency table
# -percentages or proportions


#------------------------------
#basic univariate abalysis of a Categorical variable
#Freq table
tb=table(stores$Location)
#proportions/percentage table
p_tb= prop.table(tb)

#combine the above 2: note now of variable should be same
rbind(tb,p_tb)

#for bivariate analysis on 2 categorical variables we can have -
#   a. chi-sq test
#   b. Bar chart
#   c. 2-Y axis plots

tb_bv=table(stores$Location,stores$StoreType)
p_tb_bv=prop.table(tb_bv)
rbind(tb_bv,p_tb_bv)
cbind(tb_bv,p_tb_bv)

#cross tabulation
#install.packages("gmodels")
library(gmodels)
CrossTable(stores$Location,stores$StoreType)

#-------------
#Univariate analysis on numeric variable
library(psych)
describe(stores)

min(stores$TotalSales)
max(stores$TotalSales)
range(stores$TotalSales)

sd(stores$TotalSales)
var(stores$TotalSales)

quantile(stores$TotalSales)
quantile(stores$TotalSales,probs=c(0.01,0.05,0.1,0.25,0.5,0.75,0.95,0.99,1))

#----------------------------
#function
summary_var=function(x){
  
  total_obs=length(x)
  avg=mean(x,na.rm=T)
  max=max(x,na.rm=T)
  min=min(x,na.rm=T)
  q1=quantile(x,probs=c(0.01),na.rm=T)
  return(c(tot=total_obs,avg=avg,max=max,min=min,q1=q1))
}

summary_var(my_vector)

#applying a function to entire dataset: skipping a loop
#apply(dataset name, margin, FUN=<function name>)
# margin = 1(row),2(column),c(1,2)(both row and col)
names(stores)
str(stores)
store_numerical=stores[,5:15]
apply(store_numerical,2,FUN=summary_var)

#for list- lapply()
