#2#
#a#
NR <-function(x) {
  counter<-1
  while( abs((-2+exp(x))/(3*exp(x)))>0.0000001 & counter<1000000)
  {
    x<-x-(-2+exp(x))/(3*exp(x))
    counter<-counter+1
  }
  return(x)
}
NR(0.2)

NR <-function(x) {
  counter<-1
  while( abs((-2+exp(x))/(3*exp(x)))>0.0000001 & counter<1000000)
  {
    x<-x-(-2+exp(x))/(3*exp(x))
    counter<-counter+1
    }
  return(counter)
}
NR(0.2)
#b#
NR <-function(x,n) {
  counter<-1
  while(abs((-2+exp(x))/(n*exp(x)))>0.0000001 & counter < 1000000)
  {
   x<-x-(-2+exp(x))/(n*exp(x))
   counter<-counter+1
  }
  return(counter)
}
NR(0.2,2)
NR(0.2,3)
NR(0.2,4)
NR(0.2,5)
NR(0.2,6)
NR(0.2,7)
NR(0.2,8)

#3#
#a#
library(foreign)
d<-read.dta("~/Downloads/org_example.dta")
summary(d)
subd<-subset(d, year==2013&state=="CA")
reg<-glm(rw~female+educ+age+wbho,subd,family=poisson(link="log"))
summary(reg)


