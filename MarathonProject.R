#1. Set working directory and read in the files
setwd("~/Documents/FA2020/IS470/FinalProject")
Marathon <- read.csv("Marathon.csv")
MarathonNames <- read.csv("marathon_names.csv")

#2. Clean up the data by removing columns and rows and creating calculated columns
library(dplyr)
Marathon2 <- Marathon
Marathon2$canada <-NULL
Marathon2$europe <-NULL
Marathon2$other <-NULL
Marathon2$female <-NULL
Marathon2$split_10k <-NULL
Marathon2$split_30k <-NULL
Marathon2$split_40k <-NULL
Marathon2$split_half <-NULL
Marathon2$age_gender <-NULL
Marathon2$us <- NULL

Marathon2 <- subset(Marathon2, !(age %in% c(6,7,8,9,10,11,12,13,14,15)))

Marathon2 <- subset(Marathon2, ! marathon %in% c("Boston Marathon", "White River 50 Mile", "2004 USA Olympic Team Trials - Men's Marathon", "2004 USA Olympic Team Trials - Women's Marathon",
                                                 "2008 U.S. Olympic Team Trials - Men's Marathon","2008 U.S. Olympic Team Trials - Women's Marathon","2012 U.S. Olympic Team Trials - Men's Marathon",
                                                 "2012 U.S. Olympic Team Trials - Women's Marathon", "World Championships Men's Marathon 2005", "World Championships Men's Marathon 2009",
                                                 "World Championships Men's Marathon 2011", "World Championships Men's Marathon 2013", "World Championships Women's Marathon 2005",
                                                 "World Championships Women's Marathon 2009", "World Championships Women's Marathon 2011", "World Championships Women's Marathon 2013", 
                                                 "World Track & Field Championships Men's Marathon", "World Track & Field Championships Women's Marathon", "Yukon Arctic Ultra", "Olympics 2004",
                                                 "Olympics 2008 (Men)", "Olympics 2008 (Women)","Olympics 2012 (Men)","Olympics 2012 (Women)", "US Men Olympics Trials",
                                                 "US Womens Olympic Marathon Trial", "Run4rkids Toronto Indoor Ultra Marathon", "Yukon Arctic Ultra"))

MarName <- subset(MarathonNames, ! marathon %in% c("Boston Marathon", "White River 50 Mile", "2004 USA Olympic Team Trials - Men's Marathon", "2004 USA Olympic Team Trials - Women's Marathon",
                                                 "2008 U.S. Olympic Team Trials - Men's Marathon","2008 U.S. Olympic Team Trials - Women's Marathon","2012 U.S. Olympic Team Trials - Men's Marathon",
                                                 "2012 U.S. Olympic Team Trials - Women's Marathon", "World Championships Men's Marathon 2005", "World Championships Men's Marathon 2009",
                                                 "World Championships Men's Marathon 2011", "World Championships Men's Marathon 2013", "World Championships Women's Marathon 2005",
                                                 "World Championships Women's Marathon 2009", "World Championships Women's Marathon 2011", "World Championships Women's Marathon 2013", 
                                                 "World Track & Field Championships Men's Marathon", "World Track & Field Championships Women's Marathon", "Yukon Arctic Ultra", "Olympics 2004",
                                                 "Olympics 2008 (Men)", "Olympics 2008 (Women)","Olympics 2012 (Men)","Olympics 2012 (Women)", "US Men Olympics Trials",
                                                 "US Womens Olympic Marathon Trial", "Run4rkids Toronto Indoor Ultra Marathon", "Yukon Arctic Ultra"))

Marathon2 <- Marathon2 %>% mutate(location = ifelse(Marathon2$country == "US", "United States", "International"))

Marathon2 <- Marathon2 %>% mutate(decade = ifelse(Marathon2$year < 1980 & Marathon2$year >= 1970, "1970s",
                                           ifelse(Marathon2$year < 1990 & Marathon2$year >= 1980, "1980s",
                                           ifelse(Marathon2$year < 2000 & Marathon2$year >= 1990, "1990s",
                                           ifelse(Marathon2$year < 2010 & Marathon2$year >= 2000, "2000s",
                                           ifelse(Marathon2$year < 2020 & Marathon2$year >= 2010, "2010s", "Oops"))))))

MarName2 <- MarName %>% mutate(decade = ifelse(MarName$year < 1980 & MarName$year >= 1970, "1970s",
                                        ifelse(MarName$year < 1990 & MarName$year >= 1980, "1980s",
                                        ifelse(MarName$year < 2000 & MarName$year >= 1990, "1990s",
                                        ifelse(MarName$year < 2010 & MarName$year >= 2000, "2000s",
                                        ifelse(MarName$year < 2020 & MarName$year >= 2010, "2010s", "Oops"))))))

Marathon2 <- Marathon2 %>% mutate(agegroup = ifelse(Marathon2$age < 20 & Marathon2$age >= 16, "16-19",
                                             ifelse(Marathon2$age < 30 & Marathon2$age >= 20, "20-29",
                                             ifelse(Marathon2$age < 40 & Marathon2$age >= 30, "30-39",  
                                             ifelse(Marathon2$age < 50 & Marathon2$age >= 40, "40-49",
                                             ifelse(Marathon2$age < 60 & Marathon2$age >= 50, "50-59",
                                             ifelse(Marathon2$age < 70 & Marathon2$age >= 60, "60-69",
                                             ifelse(Marathon2$age < 80 & Marathon2$age >= 70, "70-79",
                                             ifelse(Marathon2$age >= 80, "80+","N/A")))))))))

Marathon3 <- Marathon2 %>% mutate(category = paste(agegroup,gender))

Marathon3$agegroup <-NULL 
Marathon3$country <-NULL

Mar70 <- subset(Marathon3, year<1980 & year>=1970)
Mar80 <- subset(Marathon3, year<1990 & year>=1980)
Mar90 <- subset(Marathon3, year<2000 & year>=1990)
Mar00 <- subset(Marathon3, year<2010 & year>=2000)
Mar10 <- subset(Marathon3, year<2020 & year>=2010)

MarNam70 <- subset(MarName2, year<1980 & year>=1970)
MarNam80 <- subset(MarName2, year<1990 & year>=1980)
MarNam90 <- subset(MarName2, year<2000 & year>=1990)
MarNam00 <- subset(MarName2, year<2010 & year>=2000)
MarNam10 <- subset(MarName2, year<2020 & year>=2010)

MarName2 <- MarName2 %>% mutate(location = ifelse(MarName2$country == "US", "United States", "International"))

#3. Some general statistics
summary(Marathon3$age)
summary(MarNam70$meantime)
summary(MarNam80$meantime)
summary(MarNam90$meantime)
summary(MarNam00$meantime)
summary(MarNam10$meantime)

summary(Mar70$chiptime)
summary(Mar80$chiptime)
summary(Mar90$chiptime)
summary(Mar00$chiptime)
summary(Mar10$chiptime)

#4. Let's create some visualizations
library(ggplot2)
library(ggthemes)
library(reshape2)

##4.1. Density plot of average times by decade
ggplot(data = MarName2) + geom_density(aes(x=meantime)) + facet_wrap(~decade, scales = "free") + 
  coord_cartesian(xlim=c(100,500),ylim=c(0,.04)) + theme_economist() + 
  labs(title ="Average Time by Decade")
 
#4.2. Density plot of average times gender in each decade
ggplot(data = Marathon3) + geom_density(aes(x=meantime)) +facet_wrap(~decade + gender, scales = "free") + 
  coord_cartesian(xlim=c(100,500),ylim=c(0,.15)) + theme_economist()

#4.3. Box plot of chiptimes per decade
ggplot(data = Marathon3) + geom_boxplot(aes(x=decade, y=chiptime))

#4.4. Box plot of meantimes per decade
ggplot(data = MarName2) + geom_boxplot(aes(x=decade, y=meantime))

#4.5 Bar graph of average number of finishers per decade
MarsCount <- aggregate(MarName2$finishers, by=list(MarName2$decade), sum)
MarsCount2 <- aggregate(MarName2$marathon, by=list(MarName2$decade), length)
MarsCount <- MarsCount %>% mutate(races = paste(MarsCount2$x))
MarsCount$new <- MarsCount$x / as.numeric(MarsCount$races)
ggplot(data = MarsCount) + geom_bar(aes(x=Group.1, y=new), stat = "identity") 

ggplot(data = MarsCount) + geom_bar(aes(x=Group.1, y=x), stat = "identity")

ggplot(data = MarsCount2) + geom_bar(aes(x=Group.1, y=x), stat = "identity")
#4.6. Bar plot of number of races by location by decade
ggplot(data = MarName2) + geom_bar(aes(x=location)) + facet_wrap(~decade, scales = "free") + 
  theme_economist() + coord_cartesian(ylim=c(0,3000))

#4.7. Density plot of how meantimes compare to chip times by decade
frame <- data.frame(CHIP=Marathon3$chiptime, MEAN=Marathon3$meantime, DECADE=Marathon3$decade)
data <- melt(frame)
ggplot(data, aes(x=value, fill= variable)) + geom_density(alpha=.5) + facet_wrap(~DECADE, scales = "free") +
  coord_cartesian(xlim=c(100,1250),ylim=c(0,.12))

