#Name:Junge Wu#
#ID:1505086#
#Final Exam#
#a) weather
install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library(ggplot2)
use_data <- tbl(nycflights13_sqlite(), "flights") %>% left_join(weather, by = "origin", copy = TRUE) %>%  mutate(canceled = is.na(arr_time)) %>% select(dep_delay,canceled,visib,temp,wind_speed) 
use_data <- as.data.frame(use_data)
qq <- ggplot(use_data, aes(dep_delay,temp)) + geom_point(alpha=0.3)
qq
qq <- ggplot(use_data, aes(dep_delay,wind_speed)) + geom_point(alpha=0.3)
qq
qq <- ggplot(use_data, aes(dep_delay,visib)) + geom_point(alpha=0.3)
qq
with(use_data,
     cor(dep_delay,visib))
cor(na.omit(use_data[,c("dep_delay","temp")]))[1,2]
cor(na.omit(use_data[,c("dep_delay","wind_speed")]))[1,2]

#b) time of day, day of month, and time of day of week
Month <- tbl(nycflights13_sqlite(), "flights") %>%  mutate(canceled = is.na(arr_time)) %>% select(dep_delay,canceled,month,canceled)  %>% group_by(month) %>% summarise(meandep_delay=mean(dep_delay),counts =n(), cancels = sum(canceled))
Month <- as.data.frame(Month)
Month$probcancel <- Month[ ,4]/ Month[ ,3]
qq <- ggplot(Month, aes(factor(month), meandep_delay)) + geom_boxplot() + xlab("month")
qq
Day <- tbl(nycflights13_sqlite(), "flights") %>%  mutate(canceled = is.na(arr_time)) %>% select(dep_delay,canceled,day)  %>% group_by(day) %>% summarise(meandep_delay=mean(dep_delay),counts =n(), cancels = sum(canceled))
Day <- as.data.frame(Day)
Day$probcancel <- Day[ ,4]/ Day[ ,3]
qq <- ggplot(Day, aes(factor(day), meandep_delay)) + geom_boxplot() + xlab("day")
qq
qq <- ggplot(Month, aes(factor(month), probcancel)) + geom_boxplot() + xlab("month")
qq
qq <- ggplot(Day, aes(factor(day), probcancel)) + geom_boxplot() + xlab("day")
qq
#c) airport destination
DEST <- tbl(nycflights13_sqlite(), "flights") %>% mutate(canceled = is.na(arr_time)) %>% select(dep_delay,canceled,dest)  %>% group_by(dest)  %>% summarise(meandep_delay=mean(dep_delay),counts =n(), cancels = sum(canceled))
DEST <- as.data.frame(DEST)
DEST $probcancel <-DEST [ ,4]/DEST[ ,3]
qq <- ggplot(DEST, aes(x= dest,y=meandep_delay)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
qq <- ggplot(DEST, aes(x=dest,y=probcancel)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
#d) characteristics of the plane
Plane <- tbl(nycflights13_sqlite(), "flights")  %>% left_join(planes, by = "tailnum", copy = TRUE)%>% mutate(canceled = is.na(arr_time))
Plane  <- Plane %>% group_by(manufacturer) %>% summarise(meandep_delay=mean(dep_delay),counts =n(), cancels = sum(canceled))
Plane<- as.data.frame(Plane)
Plane$probcancel <-Plane[ ,4]/Plane[ ,3]
qq <- ggplot(Plane, aes(x= manufacturer,y=meandep_delay)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
qq <- ggplot(Plane, aes(x= manufacturer,y=probcancel)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
Plane  <- tbl(nycflights13_sqlite(), "flights")  %>% left_join(planes, by = "tailnum", copy = TRUE)%>% mutate(canceled = is.na(arr_time)) %>% group_by(engine) %>% summarise(meandep_delay=mean(dep_delay),counts =n(), cancels = sum(canceled))
Plane<- as.data.frame(Plane)
Plane$probcancel <-Plane[ ,4]/Plane[ ,3]
qq <- ggplot(Plane, aes(x= engine,y=meandep_delay)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
qq <- ggplot(Plane, aes(x= engine,y=probcancel)) + geom_boxplot() + theme(axis.text.x=element_text(angle = 90))
qq
