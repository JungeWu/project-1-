#294 assignment 4
#problem 0
print("Junge Wu")
print(1505086)
print("jwu118@ucsc.edu")
#problem 1
library(foreign)
flights <-read.csv("~/Downloads/econ294_2015-master 2/data/flights.csv", stringsAsFactors = FALSE)
airports <-read.csv("~/Downloads/econ294_2015-master 2/data/airports.csv", stringsAsFactors = FALSE)
weather <-read.csv("~/Downloads/econ294_2015-master 2/data/weather.csv", stringsAsFactors = FALSE) 
planes <-read.csv("~/Downloads/econ294_2015-master 2/data/planes.csv", stringsAsFactors = FALSE) 
#problem 2
flights$date<-as.Date(flights$date)
weathers$date<-as.Date(weathers$date)
airports$date<-as.Date(airports$date)
planes$date<-as.Date(planes$date)
#the first one has succeed to convert type char to type data, the others have error reports.##
#problem 3
flights.2a<-subset(flights,dest=="SFO"|dest=="OAK")
nrow(flights.2a)
#3508
flights.2b<-subset(flights,dep_delay>=60)
nrow(flights.2b)
#10474
flights.2c<-subset(flights,arr_delay>=2*dep_delay)
nrow(flights.2c)
#77830
#problem 4
library(dplyr)
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))
#problem 5
flights.5a<-flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)
#  dep_delay
#1       981
#2       970
#3       931
#4       869
#5       814
flights.5b<-flights%>%mutate(
  catchuptime=(dep_delay-arr_delay)
)%>%arrange(desc(catchuptime))%>%head(5)
#cancelled time dist catchuptime
#1         0   66  468          69
#2         0  201 1635          57
#3         0  234 1874          54
#4         0  200 1635          53
#5         0  147 1195          53
#problem 6
flights.6a<-flights %>%mutate(speed = dist / (time/60))%>%arrange(desc(speed)) %>%head(5)
delta<-flights %>% mutate(delta=dep_delay-arr_delay)
View(delta)
flights.6b<-delta%>%arrange(desc(delta))%>%head(5)
flights.6c<-delta%>%arrange(delta)%>%head(1)
#problem 7
flights.7a <- flights %>% group_by(carrier) %>%
  summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    cancelled_percent = cancelled/total_flights,
    min = min(delta, na.rm = T),
    quantile_1st = quantile(delta, 0.25, na.rm = T),
    mean = mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile_3rd = quantile(delta, 0.75, na.rm = T),
    quantile_90th = quantile(delta, 0.90, na.rm = T),
    max = max(delta, na.rm = T)
  )

print(flights.7a %>% arrange(desc(cancelled_percent)))
#fliter#
day_delay <- dplyr::filter(summarize(group_by(dplyr::filter(flights,!is.na(dep_delay)),date),delay = mean(dep_delay),n = n()),n > 10)
day_delay1<- dplyr::filter(flights,!is.na(dep_delay))%>% group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n = n(),
    n>10)

#problem 8
flights.8<- mutate(day_delay, difference = delay-lag(delay)) %>%arrange(desc(difference))
#problem 9
dest_delay<-flights %>% group_by(dest) %>%
  summarise (
    mean = mean(arr_delay, na.rm = T),
    number_flights=n()
  )
airports <- airports %>% 
  select(
    dest = iata, 
    name = airport, 
    city,
    state, 
    lat, 
    long
  )
dest_delay %>% tbl_df
df.9a<- airports%>% 
  left_join(dest_delay,by="dest")
df.9b<- airports%>% 
  inner_join(dest_delay,by="dest")
df.9c<- airports%>% 
  right_join(dest_delay,by="dest")
df.9d<- airports%>% 
  full_join(dest_delay,by="dest")
#problem 10
hourly_delay <- flights %>% filter(!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())
hourly_delay %>% full_join(weather) %>% group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% arrange(desc(max_delay))
#problem 11
#a
require(tidyr)
library(dplyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)

#b

df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

#c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df %>% separate(demo, into = c('sex','age','state') , sep = '_')

#d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
df <- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df