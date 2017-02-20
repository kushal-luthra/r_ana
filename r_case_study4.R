#case study 4 : R assighment

######################################################################################################
# Q1: dataset = dietstudy.
#reading in the dietstudy.csv
dietstudy <- read.csv("~/R/ba3/dietstudy.csv")
View(dietstudy)

#to measure impact of diet on weight and triglyceride levels we take difference in values in 6 months.
dietstudy$tg_change=dietstudy$tg4-dietstudy$tg0
dietstudy$wt_change=dietstudy$wgt4-dietstudy$wgt0

#columns below mention percentage change in weight and triglyceride levels for all observations
dietstudy$wt_change_pct=round((dietstudy$wt_change*100)/dietstudy$wgt0,digits=2)
dietstudy$tg_change_pct=round((dietstudy$tg_change*100)/dietstudy$tg0,digits=2)
#mean %age change is less than 5% in both cases- weight and triglyceride
summary(dietstudy)

View(dietstudy)
#names(dietstudy)
#plotting a scatterplot we find no relation between diet and weight change
plot(dietstudy$wt_change,dietstudy$tg_change)

#correlation test below gives p-value = 0.9611 which means not significant, implying no relationship between weight change and triglyceride level change
cor.test(dietstudy$wt_change,dietstudy$tg_change)

######################################################################################################
#Q2: credit promo study
credit_promo=read.csv("~/R/ba3/creditpromo.csv")
View(credit_promo)
names(credit_promo)

credit_promo$promo=credit_promo$insert

typeof(credit_promo)

y=data.frame(credit_promo)
credit_data=y[,c(1,3,4)]
View(credit_data)


#approach: finding sales trend for both categories. 
#method1: finding average sales. From values below, we see that New Promotion has worked.
library(sqldf)
sqldf("select promo , 
          count(id) as Number_of_cust,
        avg(dollars) as avg_purchases 
        from credit_data
          group by promo ")



#method2: lets partition data into 2 parts, and find how many values lie in 95% confidence interval
# that will tell us effectiveness of our new promotion 
promoted=sqldf("select * from credit_data where promo='New Promotion'")
library(Hmisc)
describe(promoted$dollars)
hist(promoted$dollars)


standard_sales=sqldf("select * from credit_data where promo!='New Promotion'")
describe(standard_sales$dollars)
hist(standard_sales$dollars)

#Both promoted and standard_sales data have near normal distribution.
#However, promoted data is skewed, even though we have higher average purchase in case of promotion as compared to standard purchase(w/o promotion)
#conclusion: campaign is not as effective as it ought to be



######################################################################################################
#Q3:pollination.csv 
pollination=read.csv("~/R/ba3/pollination.csv")
View(pollination)
names(pollination)

#Q= Is the overall population of Seed yield/plant (g) equals to 200?
#ans = NO. it is equal to 180.8
mean(pollination$Seed_Yield_Plant)

#Q= Test whether the natural pollination and hand pollination under open field
#   conditions are equally effective or are significantly different.
#approach: find mean values for both and conduct hypothesis test, where null hypothesis is that diff between the two is 0, i.e. equally effective.
#this is case of Independent Group T-test
# testing based on Seed_Yield_Plant. 
#In both cases, with and without Equal Variance, we find our Null Hypothesis getting rejected.
t.test(Seed_Yield_Plant~Group,data=pollination)
t.test(Seed_Yield_Plant~Group,data=pollination,var.equal=T)

#testing based on Seedling_length.
#If we take 95% confidence interval, then our Null Hypothesis gets rejected
t.test(Seedling_length~Group,data=pollination)
t.test(Seedling_length~Group,data=pollination,var.equal=T)

#thus effectiveness of pollination under different conditions -natural and hand- are significantly different.


#Q= Test whether hand pollination is better alternative in comparison to natural pollination
#A = Yes. based on above t.test mean of seed yield as well as seedling_length is higher in case of Hand Pollination

######################################################################################################
#Q4:dvdplayer.csv 
dvdplayer=read.csv("~/R/ba3/dvdplayer.csv")
View(dvdplayer)
names(dvdplayer)

#Q=Do you think that consumers of various ages rated the design differently?

#Approach:in dvd_data we have found avg scored and number of cust in each category.
# to find whether the scoring has been done significantly differently, we do anova test.

#Ans = P-value here is very low, implying we reject our null hypothesis.
#      Therefore, yes consumers of various ages rated design significantly
dvdplayer_frame=as.data.frame(dvdplayer)
anova_segment=aov(dvdscore ~ agegroup,data=dvdplayer_frame)
summary(anova_segment)


######################################################################################################
#Q5:tolerance.csv 
tolerance=read.csv("~/R/ba3/tolerance.csv")
View(tolerance)


#Need help: Unable to understand question.

######################################################################################################
#Q5:sample_survey.csv 
sample_survey=read.csv("~/R/ba3/sample_survey.csv")
View(sample_survey)

names(sample_survey)

#Q= Is there any relationship in between labour force status with marital status?
#Approach: case of comparing 2 categorical varaibles. So apply chi-sq test

# Load function
#source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#crosstab(sample_survey, row.vars="wrkstat",col.vars="marital",type='f')
#head(coffee_data)
#Ans = Yes there is relationship between the work status and marital status
mytable=table(sample_survey$wrkstat,sample_survey$marital)
x=as.data.frame(sample_survey[,c("wrkstat","marital")])

#omitting rows where either of wrkstat or marital status is missing
z=x[!is.na(x$wrkstat) & !is.na(x$marital),]
mytable=table(z$wrkstat,z$marital)
chisq.test(mytable,simulate.p.value = T)


#Q = Do you think educational qualification is somehow controlling the marital status?
# Ans = Yes. as seen below, P-value is very small [<.0001]
a=as.data.frame(sample_survey[,c("educ","marital")])
b=a[!is.na(a$educ) & !is.na(a$marital),]
mytable2=xtabs(~marital+ educ, data=b)
chisq.test(mytable2,simulate.p.value = T)


#Q= Is happiness is driven by earnings or marital status?
#

p=sample_survey[,c("happy","income","marital")]

#lets clean dataset p. we keep only those rows where all values are populated.
q=p[!is.na(p$happy) & !is.na(p$income) & !is.na(p$marital),]

#approach: we can find chi-sq value for happiness and martial status as well as happiness and income.
# The one with lower P-value will imply greater effect

table_h_m=xtabs(~happy+marital, data=q)
chisq.test(table_h_m,simulate.p.value = T)

table_h_i=xtabs(~happy+income, data=q)
chisq.test(table_h_i,simulate.p.value = T)

#as p value is same, we look at X-square value. for income, it is lower, indicating happiness is more dependent upon marital status
table_h_m
table_h_i

k=as.data.frame(sqldf("select distinct income from q"))
l=t(k)
c(gsub('\\"\\$',"#",l[1:4]))
x=t(l)[1:4]
print(x)


my_split_string=function(x){
  
  for (i in length(x)){
    if(i==1) 
    {
      print("hi")
    #  z=""
    }
    
    #z=c(z,x[i],",")
    
  }
  #print(z)
}


my_split_string(ts)


myfunc=function(x){
  
  print("hi")
  print(length(x))
  
  for (i in 1:length(x)){
    
    print(x[i])
    
    if(i==1){ 
      mystr1=''
      mystr2=''
      }
    
    mystr1=paste(mystr2,x[i],sep="','")
    #print(mystr1)
    mystr2=mystr1
  }

  print(mystr1)  
}

myfunc(ts)



names(l)
l[,1:4]
length(l)
ts=as.list(l[,1:4])
typeof(ts)

apply(l,1,FUN=split_string(l))

l=t(k)


typeof(l)

length(l)
t(l)
l

sqldf("select income from q where income in  ")

paste(c(l[1],l[4]),sep=",")
grepl('//"//$',l[1:4])

x=paste('\"',"$")
x
