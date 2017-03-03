library(readxl) 
Car_data_oth <- read_excel("C:/Users/jai mata di/Desktop/ktemp/analabs/R/11-R-1/DataSets for R sessions/Car_data_oth.xlsx")
View(Car_data_oth)
class(Car_data_oth)

#or library(XLConnect)
# a=readWorksheetFromFile(filename,sheet=1,...)
#why preferred - XLCOnnect will give more robust and well presented output

#we can otherwise use simpler library(xlsx) -- read.xlsx() and write.xlsx()


View(Car_data_oth)


#to get glimpse of data we have 2 options
#option 1: str() will glimpse of column, data type, and first few values.
str(Car_data_oth)

#it will extract value of entire column 
Car_data_oth$Manufacturer 

#store value to a VECTOR
x=Car_data_oth$Manufacturer
class(x) #data in 'x' is a character VECTOR

#option 2: summary() descriptive stats for all NUMERIC variable
summary(Car_data_oth)


#to find data type of Column
class(Car_data_oth$Manufacturer)
class(Car_data_oth$MPGcity)

#changing data type
#convert numeric into character
as.character(Car_data_oth$MPGcity)

#to make this conversion permanent
Car_data_oth$MPGcity=as.character(Car_data_oth$MPGcity)
str(Car_data_oth)

#renaming
#it returns a character vector containing names of Columns
names(Car_data_oth) 

col_list=names(Car_data_oth)

#will print TRUE where MPGcity is present
#a logical vector is returned containing boolean elements
col_list=="MPGcity" 

col_list[col_list=="MPGcity"] 

#now we renaming column in col_list
col_list[col_list=="MPGcity"] = 'City_Mileage'

#making rename to original dataframe
names(Car_data_oth) = col_list
names(Car_data_oth) #as reflected below, MPGcity has been renamed permanently in DataFrame


#rename using reshape
library(reshape)
Car_data_oth=rename(Car_data_oth,c(Manufacturer="New_Manufacturer"))

#rename using fix()
fix(Car_data_oth)

str(Car_data_oth)

#-------------------
#adding new column by pasisng it as a vector
my_vector=c(1:77)
Car_data_oth$new_col=my_vector
names(Car_data_oth)

#adding column by -deriving new column
Car_data_oth$sum_MPG=Car_data_oth$City_Mileage + Car_data_oth$MPGhighway
names(Car_data_oth)

#-------------------
#delete column by assigning it value NULL
Car_data_oth$new_col=NULL
Car_data_oth$sum_MPG=NULL
names(Car_data_oth)


#----------------------------
#subsetting rows and cols
Car_data_oth[1:8,1:3]

Car_data_oth[,1:4]
Car_data_oth[,c(1,2,3,4)]
Car_data_oth[,"New_Manufacturer"] 
Car_data_oth[,c("New_Manufacturer","Model")] 
names(Car_data_oth)

#-----------------------
#reordering columns ; CANNOT be done using fix()
#reversing column order
new_car_data=Car_data_oth[,c(24:1)]
#new_car_data=Car_data_oth[,c(give col NAMES in order u want )]

names(new_car_data)


#-----------------------
#subsetting
#select by column names
new_data=subset(Car_data_oth,select="New_Manufacturer")
new_data1=subset(Car_data_oth,select=c("New_Manufacturer","Model"))

#select by coumn numbers
new_data2=subset(Car_data_oth,select=c(1:3))

#subset by condition:City_Mileage>20: both give same output
new_data3=subset(Car_data_oth,select=c("New_Manufacturer","Model","City_Mileage"),new_data3$City_Mileage>20)
new_data4=subset(Car_data_oth,select=c("New_Manufacturer","Model","City_Mileage"), City_Mileage>20)


#------------------------------
#sorting: we use ORDER() function ---not SORT() which is applicable to VECTORS
#sort ascedning by column city_mileage
new_data5 =  Car_data_oth[order(Car_data_oth$City_Mileage) ,]

#sort descending
new_data6 =  Car_data_oth[order(Car_data_oth$City_Mileage,decreasing=TRUE) ,]
new_data7 =  Car_data_oth[order(-Car_data_oth$City_Mileage) ,] # use of "-" sign; it works only on Numeric column

#sort by multiple column
new_data8 =  Car_data_oth[order(Car_data_oth$City_Mileage,Car_data_oth$New_Manufacturer,-Car_data_oth$MPGhighway) ,]

#sort using dplyr package
#now we work on new dataset store()
library(dplyr)
new_data9=arrange(Car_data_oth,City_Mileage,desc(MPGhighway))
View(new_data9)

new_store=arrange(stores,StoreName,desc(Location))
View(new_store)

#-----------------------------------------
#removing Duplicates
#dataset = scores
Score <- read_csv("C:/Users/jai mata di/Desktop/ktemp/analabs/R/11-R-1/DataSets for R sessions/Score.csv")

duplicated(Score) #shows if there are any duplciates as boolean
unique(Score) #shows only unique values 

unique_Score= Score[ !duplicated(Score) ,]


#----------------
#merging datasets -Demographic_Data and Transaction_Summary
dd=read_csv("C:/Users/jai mata di/Desktop/ktemp/analabs/R/11-R-1/DataSets for R sessions/Demographic_Data.csv")
td=read_csv("C:/Users/jai mata di/Desktop/ktemp/analabs/R/11-R-1/DataSets for R sessions/Transaction_Summary.csv")

#inner join: all=FALSE
dd.td.inner=merge(x=dd,y=td,x.by=c("CustName"),y.by=c("CustomerName"),all=FALSE)
View(dd.td.inner)

#Outer join: all=TRUE
dd.td.outer=merge(x=dd,y=td,x.by=c("CustName"),y.by=c("CustomerName"),all=TRUE)
View(dd.td.outer)

#Left join: all=TRUE
dd.td.left=merge(x=dd,y=td,x.by=c("CustName"),y.by=c("CustomerName"),all.x=T)
View(dd.td.left)

#Right join: all=TRUE
dd.td.right=merge(x=dd,y=td,x.by=c("CustName"),y.by=c("CustomerName"),all.y=T)
View(dd.td.right)

#--------------------------------
#find misisng values
is.na(dd.td.outer$Mobile)

#for Column Mobile with any missing values, find them and replace them by 0
dd.td.outer$Mobile[is.na(dd.td.outer$Mobile)]  = 0

#to replace any missing value in entrie tabel by 0; will set to 0 expcet factor column as they are stored as levels.
dd.td.outer[is.na(dd.td.outer)] = 0
