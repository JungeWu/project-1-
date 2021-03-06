#lab#
#Assignment 2#
#problem 0#
JungeWuAssignment2 <- list(
  firstName = "Junge",
  lastName  = "Wu",
  email     = "jwu118@ucsc.edu",
  studentID = 1505086
)
#problem 1#
install.packages("repmis")
library("repmis")
install.packages("source_data")
diamonds<-source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
View(diamonds)
JungeWuAssignment2$s1a <- nrow(diamonds) #to see how many observations
#there are 7 observations
JungeWuAssignment2$s1b <- ncol(diamonds)#to see how many columns
#there are 4 colums
JungeWuAssignment2$s1c <- names(diamonds)#to see the header names
#the name are"carat""cut""clarity""price"
JungeWuAssignment2$s1d <- summary(diamonds$price)#to summary the price
# Min. 1st Qu.  Median    Mean 3rd   Qu.    Max.    NA's 
# 420      450     600     650       825     980       1 
#problem 2#
NHIS_2007_TSV <- read.delim("~/Desktop/NHIS_2007_TSV.txt")
View(NHIS_2007_TSV)
JungeWuAssignment2$s2a <- nrow(NHIS_2007_TSV)
#There are 4785 observations
JungeWuAssignment2$s2b <- ncol(NHIS_2007_TSV)
#There are 9 column 
JungeWuAssignment2$s2c <- names(NHIS_2007_TSV)
#"HHX"    "FMX"    "FPX"    "SEX"    "BMI"    "SLEEP"  "educ"  
#"height" "weight"
JungeWuAssignment2$s2d <- mean(NHIS_2007_TSV[["weight"]])
#the mean is 266
JungeWuAssignment2$s2e <-median(NHIS_2007_TSV[["weight"]])
#the median is 175
weights <- NHIS_2007_TSV$weight
hist(weights, right=FALSE)
table(weights) #draw a histogram with the weights
NHIS_2007_TSV$weightnew<- ifelse(test = NHIS_2007_TSV$weight<996 | NHIS_2007_TSV$weight>999,
                                 yes = NHIS_2007_TSV$weight,
                                 no = NA)#create a new column setting these weight observations to NA.
View(NHIS_2007_TSV)
JungeWuAssignment2$s2f <- mean(NHIS_2007_TSV[["weightnew"]], na.rm=T)
#the mean is 174
JungeWuAssignment2$s2g <- median(NHIS_2007_TSV[["weightnew"]],na.rm=T)
#the median is 170
sub.m<- subset(NHIS_2007_TSV,(SEX==1))#men with 1
sub.w<- subset(NHIS_2007_TSV,(SEX==2))#woman with 2
JungeWuAssignment2$s2h <- summary(sub.m[["weightnew"]])
#Min.   1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#128.0   169.0   187.0   192.8   212.0   298.0     207 
JungeWuAssignment2$s2i <- summary(sub.w[["weightnew"]])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#100.0   130.0   150.0   158.2   178.0   274.0     329 
#problem 3#
vec <- c(letters,LETTERS)
JungeWuAssignment2$s3a <- paste(vec[c(1:26*2)])#Extract even index values
#"b" "d" "f" "h" "j" "l" "n" "p" "r" "t" "v" "x" "z" "B" "D" "F" "H" "J" "L" "N" "P" "R" "T" "V" "X" "Z"
JungeWuAssignment2$s3b <- paste(vec[c(36,21,14)],collapse="") #Use vec to extract to extract the first three letters of your name
# "J" "u" "n"
arr <- array(c(letters,LETTERS), dim=c(3,3,3))
View(arr)
arr1 <- arr[,,1]
arr2 <- arr[,,2]
arr3 <- arr[,,3]
JungeWuAssignment2$s3c <- arr2[,1]#to extract the first column from the second matrix of arr
#"j" "k" "l"
JungeWuAssignment2$s3d <- c(arr1[2,2],arr2[2,2],arr3[2,2])#to extract the middle values from each of the three matrices in arr 
#"d" "e" "f" "m" "n" "o" "v" "w" "x"
JungeWuAssignment2$s3e <- paste(arr[1,1,2],arr[3,1,3],arr[2,2,2],sep = "")#spell the first three letters of your first name 
#"jun"
Status API Training Shop Blog About Pricing
save(JungeWuAssignment2, file = "~/Documents/JungeWuAssignment2.RData")