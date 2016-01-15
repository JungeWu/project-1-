#lab#
#Assignment 1#
#problem 0#
firstName<- "Junge"
lastName <- "Wu"
print(paste(firstName,lastName))
StudentID<-1505086
print(StudentID)

#problem 1#
library(foreign)
df.dta <-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <-read.csv("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.table("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.rdata<-load("/Users/sashazhu/Downloads/NHIS_2007_RData-2.RData")
##Load Data

#problem 2#
#a#
#df.dta is 188kb
#df.csv is 139kb
#df.td is 139kb
#df.rdata is 45.3kb
#b#
#the smallest one is NHIS_2007_RData.RData
#c#
#Their variability is due to their differert way of being coded.

##problem 3
typeof(NHIS_2007_RData)
#list
class(NHIS_2007_RData)
#data.frame
length(NHIS_2007_RData)
#9
dim(NHIS_2007_RData)
#4785    9
nrow(NHIS_2007_RData)
#4785
ncol(NHIS_2007_RData)
#9
summary(NHIS_2007_RData)
#HHX             FMX             FPX             SEX       
#Min.   :   16   Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:13404   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
#Median :27527   Median :1.000   Median :1.000   Median :2.000  
#Mean   :27009   Mean   :1.019   Mean   :1.359   Mean   :1.549  
#3rd Qu.:40192   3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.:2.000  
#Max.   :53955   Max.   :6.000   Max.   :8.000   Max.   :2.000  
#BMI            SLEEP             educ           height     
#Min.   :12.91   Min.   : 3.000   Min.   : 0.00   Min.   :59.00  
#1st Qu.:23.63   1st Qu.: 6.000   1st Qu.:12.00   1st Qu.:64.00  
#Median :26.97   Median : 7.000   Median :13.00   Median :67.00  
#Mean   :31.73   Mean   : 9.507   Mean   :14.25   Mean   :69.58  
#3rd Qu.:31.51   3rd Qu.: 8.000   3rd Qu.:16.00   3rd Qu.:71.00  
#Max.   :99.99   Max.   :99.000   Max.   :99.00   Max.   :99.00  
#weight     
#Min.   :100.0  
#1st Qu.:149.0  
#Median :175.0  
#Mean   :266.2  
#3rd Qu.:215.0  
#Max.   :999.0  

#problem 4#
d.dta<-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(d.dta)
#1119754 obs. of  30 variables
summary(d.dta$rw)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.8    10.7    15.9    19.8    24.4   354.8  521279 

#problem 5#
v<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
length(v)
#9
summary(v)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00    2.75    4.00    4.00    5.25    7.00       1 

#problem 6#
x <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow=3, ncol=3) 
t(x)
#we can use the function t(x) to help us get the result of matrix transpose
eigen(x)
#we can use the function eigen(x) Run this command in order to get the eigenvalues and eigenvactors

y <- matrix(   c(1, 3, 2, 2, 2, 3, 3, 1, 0),   nrow=3,  ncol=3) 
z<-solve(y)
#we can use the function solve(y) to help us to get the inverse matrix of y
y%*%z
#we can use the command solve(y) to help us get the inverse of matrix

#problem 7#
carat = c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA" )
price = c(850, 450, 450, 0, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
print(diamonds)
#We use the functions above to get the following form:
#carat       cut clarity price
#1   5.0      fair     SI1   850
#2   2.0      good      I1   450
#3   0.5 very good     VI1   450
#4   1.5      good     VS1     0
#5   5.0      fair      IF   750
#6    NA     Ideal    VVS2   980
#7   3.0      fair      NA   420
#a
summary(diamonds)
# carat                  cut    clarity      price      
#Min.   :0.500   fair     :3   I1  :1   Min.   :  0.0  
#1st Qu.:1.625   good     :2   IF  :1   1st Qu.:435.0  
#Median :2.500   Ideal    :1   NA  :1   Median :450.0  
#Mean   :2.833   very good:1   SI1 :1   Mean   :557.1  
#3rd Qu.:4.500                 VI1 :1   3rd Qu.:800.0  
#Max.   :5.000                 VS1 :1   Max.   :980.0  
#NA's   :1                     VVS2:1       
#b
meanprice1<-subset(diamonds,(cut=="fair"))
summary(meanprice1)
#
#carat                   cut   clarity      price      
#Min.   :3.000   fair     :3   I1  :0   Min.   :420.0  
#1st Qu.:4.000   good     :0   IF  :1   1st Qu.:585.0  
#Median :5.000   Ideal    :0   NA  :1   Median :750.0  
#Mean   :4.333   very good:0   SI1 :1   Mean   :673.3  
#3rd Qu.:5.000                 VI1 :0   3rd Qu.:800.0  
#Max.   :5.000                 VS1 :0   Max.   :850.0  
#                              VVS2:0              
#c
meanprice2<-subset(diamonds,(cut=="good"|cut=="very good"|cut=="Ideal"))
summary(meanprice2)
# carat              cut    clarity      price      
#Min.   :0.500   fair     :0   I1  :1   Min.   :  0.0  
#1st Qu.:1.000   good     :2   IF  :0   1st Qu.:337.5  
#Median :1.500   Ideal    :1   NA  :0   Median :450.0  
#Mean   :1.333   very good:1   SI1 :0   Mean   :470.0  
#3rd Qu.:1.750                 VI1 :1   3rd Qu.:582.5  
#Max.   :2.000                 VS1 :1   Max.   :980.0  
#NA's   :1                     VVS2:1       
#d
meanprice3<-subset(diamonds,(cut=="very good"|cut=="Ideal"|carat>"2"))
summary(meanprice3)
#carat                    cut    clarity      price    
#Min.   :0.500   fair     :3   I1  :0   Min.   :420  
#1st Qu.:2.375   good     :0   IF  :1   1st Qu.:450  
#Median :4.000   Ideal    :1   NA  :1   Median :750  
#Mean   :3.375   very good:1   SI1 :1   Mean   :690  
#3rd Qu.:5.000                 VI1 :1   3rd Qu.:850  
#Max.   :5.000                 VS1 :0   Max.   :980  
#NA's   :1                     VVS2:1                
