#Junge Wu#
#Assignment 3#
#Q0 
print("Junge Wu")
print(1505086)
print("jwu118@ucsc.edu")
#Q1
install.packages("dplyr")#install package pdlyr
library(dplyr)
library(foreign)#loading data
df.ex <- read.dta(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
) 
#Q2
filter(df.ex,year==2013)#subset df.ex to just the last month of 2013
print(sum(with(df.ex,year==2013)))
#the number of observations in Summer 2013
df.ex.2a <- dplyr::filter(df.ex,year == 2013 & month == 12)
dim(df.ex.2a)
df.ex.2b <- dplyr::filter(df.ex,year == 2013 & month == 7 |month == 8 |month== 9)
dim(df.ex.2b)
#Q3
#a new data frame called df.ex.3a that is sorted with year and month ascending 
df.ex.3a <-arrange(df.ex, year, month)
#Q4
df.ex.4a <- select(df.ex.3a,year:age)#a new data frame called df.ex.4a with only columns year through age.
df.ex.4b <- select(df.ex.3a,year,month,starts_with("i"))#a new data frame called df.ex.4b with only columns year, month, and columns that start with i
print(unique(df.ex$state))
#Q5
stndz <-function(x){
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)
}
nrmlz <-function(x) (x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm=T))
df.ex.5a<- 
df.ex %>% mutate(
rw.stndz= stndz(rw),
rw.nrmlz=nrmlz(rw))
df.ex.5b <- df.ex %>% 
group_by(year, month) %>%
mutate(
    rw_stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw),
    count=n()
  )
#Q6
df.ex.6<-
df.ex %>% 
group_by(year,month,state)%>%
summarise(
rw_min=min(rw,na.rm = T),
rw_1stQnt=quantile(rw,na.rm = T,0.25),
rw_mean.art =mean(rw,na.rm = T),
rw_3rdQnt=quantile(rw,na.rm = T,0.75),
rw_max=max(rw,na.rm = T),
rw_median=median(rw,na.rm=T),
count=n()
            )%>%
select(state,starts_with("rw_"),count)
print(df.ex.6 %>% ungroup() %>% arrange(desc(rw_mean.art)) %>%
        select(year,month,state) %>% head(1))
#Q7
df.ex$state.char <-as.character(df.ex$state)
df.ex.7a <-
df.ex %>% arrange(year,month,desc(state.char))
