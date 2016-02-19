##Assignment 5
print("Junge Wu")
print("jwu118@ucsc.edu")
print("StudentID:1505086")

##Question 1
#a#
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
d1 <- ggplot(diamonds, aes(x=x*y*z,y=price,color=clarity))
d1 + geom_point(aes(color+clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()

#b#
d2<-ggplot(diamonds,aes(carat,fill=clarity,..density..))
d2+geom_histogram()+facet_grid(cut~.)

#c#
d3<-ggplot(diamonds,aes(x=cut,price))
d3+geom_jitter(alpha=0.1)+geom_violin()

##Question 3
#a#
library(foreign)
require(dplyr)
example<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

gd <- example %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    q1 = quantile(rw, .1, na.rm = T),
    q2= quantile(rw, .9, na.rm = T),
    q3 = quantile(rw, .25, na.rm = T),
    q4 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

gd<-gd %>% mutate(date=paste(year,month,"01", sep="-"),date=as.Date(date,format="%Y-%m-%d"))

d4 <- ggplot(gd, aes(x=date, y=Median.RW))
d4 + geom_ribbon(aes(ymin=q1 , ymax=q2),alpha=0.6) + geom_ribbon(aes(ymin=q3 , ymax=q4),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))

#b#
gd2 <- example %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(Median.RW = median(rw, na.rm = T),count = n())

gd2<-gd2 %>% mutate(date=paste(year,month,"01", sep="-"),date=as.Date(date,format="%Y-%m-%d"))

d5 <- ggplot(gd2, aes(x=date, y=Median.RW,group=educ))
d5 + geom_line(aes(color=educ))
