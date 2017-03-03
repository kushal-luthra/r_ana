# GETTING SESSION INFORMATION ================================================

sessionInfo()

# GETTING HELP ================================================================

# View locally installed documentation:
help.start()

# help can also give you advise on syntax while 

help("function")                                  # developing your own functionalities  # remember quoting!
help(names)                                       # Use example() to run the example code.
example(names)
example(plot)

library(help = MASS)                              # Help for packages
help(package = MASS)                              # Help for packages

?data()                                            # Help for datasets
data(package = "MASS")                            # Help for datasets
data(package = .packages(all.available = TRUE))
??bacteri

# ... or on-line search of R manuals, help pages and discussion archives
RSiteSearch("bacteria")

?vignette()                                       # Another source of knowledge is vignette - PDF-based documentation included in package
vignette(package = "grid")
vignette("rotated", package = "grid")



#BASICS ABOUT R ===============================================================

x<-10+20                  # Simple Math 
10+20 -> x                # "<-" Assignment Operator
x                         # Print the x value
y = 1:10                  # Assign values
z = seq(1:10, by=2)       # Assign values

1:250                     # Prints range of numbers 
print("My First Program") # Prints data in console


#R objects ======================================================================

x <- 1:10                      # put the numbers 1-10 in the variable X - Assignment operator (= or <-)
1:10 -> x1                     # you can also assign the same
y <- c(11,12,13,14,15)         # concate nate function , puts the numbers into y
a<- b<- c <-3                  # multiple assignments
x+y                            # adds corresponding elements in x & y
x*2                            # Multiplication by each element with 2

# DATA TYPES ==================================================================

# Numeric ---------------------------------------------------------------------
1.7                        # Decimal mark is a dot
options(scipen=999)
7.8e3                      # One can use scientific notation
-1/0                       # Infinity is marked as inf or -inf 

4/6799                     # Formatting numbers
format(4/6799, sci = TRUE) # Formatting numbers
format(4/6799, digits = 2) # Formatting numbers

round(pi, 2)                # Rounding

# Complex ---------------------------------------------------------------------

complex(real = 2, imaginary = 1) # Definition of a complex number with real and imaginary part
sqrt(-1)                         # Square root of -1 only exists if number is complex
sqrt(as.complex(-1))             # Square root of -1 only exists if number is complex

# Logical ---------------------------------------------------------------------

2 == 5       #is 2 equal to 5?

(2 != 2)     # ! means negation
!(2 == 2)    # ! means negation

(2 == 2) & (5 == 5)     # & - logical conjunction  
(2 == 2) | (5 != 5)     # | - logical alternative 

# Character -------------------------------------------------------------------

"I love R"                                                  # strings should be enclosed with ' or "
paste("I love R", 'and promise to use it in the future')    # Concatenation of strings


# Special ---------------------------------------------------------------------

1/0                    # Inf, -Inf
-1/0                   # Inf, -Inf
log(-1)                # Not a number
NA                     # NA - Missing values
NULL                   # NULL


# Factor ----------------------------------------------------------------------

f <- factor(c("girl", "boy", "boy", "girl", "boy"))   # Categorical variables

# DATA STRUCTURES =============================================================

# Vectors # set of objects of the same type, one dimensional ---------------------------------------------------------------------

my_vector <- c(-1, 2, 6, 6.7, 2, 0.45, 2, 4)    # defined with a c() function (c for concatenate), vectors are vertical)  # Numeric vector

mode(my_vector)
typeof(my_vector)
class(my_vector)
is.numeric(my_vector)
is.character(my_vector)
is.data.frame(my_vector)
x5 <- c(5, 4, 2, 3, 1,6,8,9)   #concatenation
x7 <- c(3, 4, 2)
x8 <- c("HA", "DL","MH","AP")  # Character Vector

x7+x5                          # sum of vectors
data <- c(x5,x7)               # concatenation of vectors

(my_vector2 <- 2 * my_vector)  # Computations with numeric vectors
my_vector * c(2, 3)   #multiply with 2 then 3 then 2 then 3 and so on...        
my_vector + c(2, 3, 4) #add with 2 then 3 then 4 , then repeat till reach end.

# Character vector
(my_char_vector <- c(4, 2, "Hello, world!"))

mode(my_char_vector)
typeof(my_char_vector)
class(my_char_vector)
is.numeric(my_char_vector)

# Maths operations
sqrt(my_vector)        # NaN (not a number) produced as a resulting of squarting negative values
log(my_vector)         # etc.
sum(my_vector)         # sum
prod(my_vector)        # product
cumsum(my_vector)      # cumulates (sums up) all the values
cumprod(my_vector)     # multiplies up all the values
diff(my_vector)        # lagged differences of all the values

# Useful functions
length(my_vector)     # vector's length
unique(my_vector)     # distinct values of the vector
head(my_vector, 3)    # displays first 3 elements of my_vector
tail(my_vector, 2)    # displays last 2 elements of my_vector
sort(my_vector)       # sorts my_vector
rev(my_vector)        # reverts the order of elements in my_vector
order(my_vector)      # returns the order of my_vector
rank(my_vector)       # rank elements

#diff bw rank() and order()
#Order=if I have to sort the list. Where should each element have to be placed? 
#Rank- if I list was already sorted, what is the posn of current element as per that sorted list?

#both will display sorted list
my_vector[order(my_vector)] 
sort(my_vector)


which.max(my_vector)  # return position of the max value
which.min(my_vector)  # return position of the min value



# Indexing
#my_vector
my_vector[2]
my_vector[-2]
my_vector[1:4]
my_vector[-(1:7)] #notice () paranthesis
my_vector[-c(2,4,7,5)] 

my_vector[my_vector > 3] #displays values greater than 3 #notice sq brackets []
which(my_vector > 3) #displays Posn of elements greater than 3

#both statements display same output: values bw 0 and 4
my_vector[my_vector > 0 & my_vector < 4]
my_vector[my_vector %in% 0:4]


#match( myvector, <condition>)  - returns all elements in vector satisfying a particular condition
#It will display those values which are bw 1 to 3, and those not falling in that range will be shown as NA
match(my_vector, 1:3)

my_vector[c(rank(my_vector)) == 2]
my_vector[order(my_vector)]


# Combining two vectors
c(my_vector, my_vector2)
cbind(my_vector, my_char_vector)
rbind(my_vector, my_char_vector)
cbind(my_char_vector,my_vector)
rbind(my_char_vector,my_vector)

# Sequences
4:7
seq(0, 1, length.out = 16)
seq(1, 9, by = 2) 
seq(1, 8, by = 2) 
seq(9, 1, by = -2) 
seq(17) 

all(seq(17) == 1:17)

rep(1:4, 2)
rep(1:4, each = 2)      
rep(1:4, c(2, 1, 2, 3))
rep(1:4, each = 2, len = 10)  


rep(1:4, each = 2, times = 3)


# Matrix ----------------------------------------------------------------------

# Defined with a matrix() function with 3 arguments: vector of values, number of rows, number of columns

(my_matrix <- matrix(c(4, 2, 7, 9), nrow=2, ncol=2))
(my_matrix <- matrix(c(1, 2, 3, 4), 4, 1))
(my_matrix <- matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE))

#transpose
myMat_Transposed <- t(my_matrix)


my_matrix[1, 2]
my_matrix[, 1]
my_matrix[1, ]

dim(my_matrix)                # Dimesnion of matrix
det(my_matrix)                # Determinant
eigen(my_matrix)              # Eigenvalues/Eigenvectors
(t(my_matrix) %*% my_matrix)  # Transpose and multiply

rowSums(my_matrix)            # Sum of rows
colSums(my_matrix)            # Sum of colums
rowMeans(my_matrix)           # Avgs of rows
colMeans(my_matrix)           # Avgs of colums


# Array -----------------------------------------------------------------------

# Defined with a array() function with 3 arguments: vector of values, dimensions, dimension names

our_dim_names <- list(dimension1 = c("A", "B", "C", "D"), 
                      dimension2 = c("a", "b", "c"),
                      dimension3 = c("1", "2"))
(my_array <- array(data = 1:24, dim = c(4,3,2), dimnames = our_dim_names))

dim(my_array)

# Indexing
my_array[3, 2, 1]
my_array[3, 2, ]
my_array[3, , ]


# List ------------------------------------------------------------------------

# Set of objects that can have different types # Defined with the list() function
(my_list <- list(name = c("Analytics", "Labs"), age = 3, club = "Bulls", matrix = my_matrix))

(my_list2 <- list(c("Analytics", "Labs"), 3, "Bulls", my_matrix))



mode(my_list)

typeof(my_list)
class(my_list)

# Indexing
dim(list)
my_list$name
my_list[1]
my_list[[1]]
my_list[[1]][2]
names(my_list2)      #lists all names of the list

rm()
# Data frame ------------------------------------------------------------------

# table with the same type within a column and different types between columns # defined with a data.frame() function
my_df <- data.frame(id = c(1, 2, 3), 
                    name = c("Analytics", "Labs", "alabs"), 
                    Goals = c(50, 49, 25))
dim(my_df)
str(my_df)
summary(my_df)




# Workspace & Environment -----------------------------------------------------

getwd()                       # Get working directory
setwd("C:/")                  # set working directory
ls()                          # List elements of the environment

rm(x)                         # remove an object from workspace
rm(a,b)                       # remove multiple object from workspace
rm(list=ls())                 # clear workspace


# Installing & managing packages -------------------------------------------------------------------------------------------

browseURL("http://cran.r-project.org/web/views/")                              # categorical view of packages
browseURL("http://cran.stat.ucla.edu/web/packages/available_packages_by_name") #CRAN stands for comprehensive r archive network
browseURL("http://crantastic.org")

library()                     # see current packages #brings editor with list of installed packages
search()                      # shows packages that are currently loaded

install.packages("ggplot2")   # TO install packages # download packages from CRAN and install in R
library("ggplot2")            # make package available ; often used for loading in scripts
require("ggplot2")            # prefered for loading in functions; may be better
library(help="ggplot2")       # brings up documentation  in editor window
update.packages("ggplot2")    # check for pacakge updates; do it regularly
detach("package:ggplot2", unload=TRUE) # Unload/Remove  packages # by default, all loaded packges are unloaded when R quits.
remove.packages("ggplot2")    # TO permanantly remove it.   

#Datasets -----------------------------------------------------------------------------------------------------------------------
?datasets                     # Using R's built in data sets

library(help=datasets)
library(datasets)

data(mtcars)                 # Loading mtcars data set
cars <-mtcars                # Save the data into workspace
detach(package:datasets)     # To remove the datasets package

# Viewing data set
mtcars                       # Total data set in console
View(mtcars)                 # Viewing dataset in spreadsheet
head(mtcars,10)              # Viewing top-10 observations (default: top-6)
tail(mtcars)                 # Viewing bottom 10 observations
str(mtcars)                  # Viewing data dictionary
names(mtcars)                # Viewing column names

v1 = mtcars$disp
newvar <- mtcars$disp + mtcars$hp
v1 <- mtcars$mpg             # Assigning single variable from mtcars data to v1
v2 <- mtcars$cyl
v3 <- mtcars$disp
v4 <- mtcars$hp

mtcars1<-rbind(v1,v2,v3,v4) # Combined as rows #Horizontal joins
mtcars2<-cbind(v1,v2,v3,v4) # Combined as columns # Vertical joins

# DATA IMPORT =================================================================

# Import from flat file -------------------------------------------------------

path <- choose.dir()                            # Specify path to data set
file <- paste0(path, "\\", "Car_data_cf_prices.dat")  # Specify file
data_cf_prices <- read.table(file, header = TRUE, sep = " ", na.strings = "NA", stringsAsFactor = TRUE) # Import file

# Get overview of data
head(data_cf_prices)            # first 6 rows
tail(data_cf_prices)            # last 6 rows
summary(data_cf_prices)         # summary statistics
str(data_cf_prices)             # structure of data frame
nrow(data_cf_prices)            # number of rows
ncol(data_cf_prices)            # number of columns
dim(data_cf_prices)             # dimensions (rows, columns)
names(data_cf_prices)           # variable names

# Import from csv -------------------------------------------------------------

# Specify file
file <- paste0(path,"\\", "Car_data_cf.csv")
data_cf <- read.csv(file)

# SPSS files
require(foreign)
file <- paste0(path,"\\", "hsb2.sav")
dat.spss <- read.spss(file, to.data.frame=TRUE)

# Stata files
file <- paste0(path,"\\", "hsb2.dta")
dat.dta <- read.dta(file)
#Importing SAS File

install.packages("sas7bdat")
library(sas7bdat)
file <- paste0(path,"\\", "hsb2.sas7bdat")
data.Sas <- read.sas7bdat(file, debug=FALSE)

#Importing WEB File 

fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")




#---------------------------------------------------------
# 28 Feb, 2016
#---------------------------------------------------------






# DATA EXPORT =================================================================

# Export to flat file ---------------------------------------------------------

file <- paste0(path, "MyRClassDemo.csv")                     # Specify file
write.table(cars, file, row.names = TRUE, sep = " ", dec = ".", na = "NA", col.names = FALSE) # Export file
dir(path)

# Export to csv ---------------------------------------------------------------

file <- paste0(path, "MyRClassDemo.csv")        # Specify file
write.csv(cars, file)                              # Export file
dir(path)

# Export to RData file --------------------------------------------------------

path <- getwd()                                       # Save entire environment
save.image(file = paste0(path, "environment.RData"))  
save(fpe, file = paste0(path, "data.RData"))         # Save single R objects
load(file = paste0(path, "data.RData"))               # Load RData file(s)

