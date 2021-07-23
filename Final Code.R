library(lubridate)
library(dplyr)
library(sqldf)
library(Hmisc)
library(reshape)
library(fastDummies)
library(dummies)
library(readxl)
library(openxlsx)
library(stringr)
library(tidyr)
library(tidyselect)
library(zoo)
library('RODBC')
library(tcltk)
library(ggpubr)

#----- Importing data-----------

setwd("C:\\Users\\Sathish\\OneDrive\\Personal\\Case study - PP")
data <- read.csv("data.csv" , header = TRUE , sep = "," , stringsAsFactors = FALSE)

table(data$name, data$hand)

#------ Summary ---------------
Allsumm <- sqldf("select name , count(name) Match_count, sum(attempts) total_attempts, sum(score) total_Score , avg(score) avg_Score, median(score) med , min(score) min, max(score) max, stdev(score)*stdev(score) va , stdev(score) std
                from data group by name ")
summary(data)
summ <- sqldf("select name , hand , count(name) Match_count, sum(attempts) total_attempts, sum(score) total_Score , avg(score) avg_Score, median(score) med , min(score) min, max(score) max, stdev(score)*stdev(score) va , stdev(score) std
                from data group by name , hand ")

names(data)
summ$prop <-  prop.table(summ$total_attempts)*count(distinct(summ,summ$name))[[1]] 
names(data)
data1 <- data[,-c(5)]

Arielle <- data[data$name == 'Arielle',]
Boris <- data[data$name == 'Boris',]
#--- Adding fields----
Arielle1 <- sqldf("Select a.*, b.prop from Arielle a left join summ b on a.name = b.name and a.hand = b.hand ")
#1st preferred hand - right
Arielle1$preference_hand <- ifelse(Arielle1$prop > 0.5 , "High" , "Low")

Boris1 <- sqldf("Select a.*, b.prop from Boris a left join summ b on a.name = b.name and a.hand = b.hand ")
#1st preferred hand - left
Boris1$preference_hand <- ifelse(Boris1$prop > 0.5 , "High" , "Low")
#----- Data Manipulation----

match2 <- cast(data1,match_day~name+hand, sum)
match2$Arielle <- match2$Arielle_L+match2$Arielle_R
match2$Boris <- match2$Boris_L + match2$Boris_R
match2$win <- ifelse(match2$Arielle==match2$Boris , 'Draw',
                     ifelse(match2$Arielle>match2$Boris,'Arielle',  'Boris' ))
table(match2$win)
#Both are equal with 23 wins each

#---------- Comparing Overall ------------------------- 

group_by(data, name) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )


#--- Plot-----
boxplot(match2$Arielle,match2$Boris, col = c("Red","Pink"), horizontal = TRUE)
#----
ggboxplot(data, x = "name", y = "score", 
          color = "name", palette = c("#00AFBB", "#E7B800"),
          ylab = "score", xlab = "Players")


#--- to find normal distributed--
shapiro.test(match2$Arielle) # p-value > 0.05 , normally distributed
shapiro.test(match2$Boris) # p-value > 0.05 , normally distributed

#------ Variance check F-test-------------

ftest <- var.test(match2$Arielle,match2$Boris)
ftest # p-value >0.05, not significant -  variance are equal

# ----- t-test ------
tsat <- t.test(match2$Arielle,match2$Boris, var.equal = TRUE , alternative = "less") # identify arielle mean is less than boris mean
tsat
tsat <- t.test(match2$Arielle,match2$Boris, var.equal = TRUE ) # identify arielle mean is less than boris mean
tsat
#P-value
tsat$p.value
#estimate
tsat$estimate
#confidence level
tsat$conf.int

print(ifelse(tsat$p.value<= 0.05, "Arielle mean is less than Boris", "Not significant"))


#-----Win based on preferred hand------
#summary explains Arielle strong is right hand and Boris strong is left hand
match3 <- sqldf("Select a.*, b.preference_hand Airelle_hand , c.preference_hand Boris_hand from match2 a left join Arielle1 b on a.match_day = b.match_day 
                 left join Boris1 c on a.match_day = c.match_day")

match3$win_hand <- ifelse(match3$win == 'Arielle' , match3$Airelle_hand , 
                          ifelse(match3$win == 'Boris', match3$Boris_hand , match3$win ))

table(match3$win , match3$win_hand)

match3$hand_flag <- ifelse(match3$Airelle_hand == match3$Boris_hand , 0 , 1)

match3$Arielle_L = ifelse(match3$Arielle_L == 0 , NA , match3$Arielle_L)
match3$Arielle_R = ifelse(match3$Arielle_R == 0 , NA , match3$Arielle_R)
match3$Boris_L = ifelse(match3$Boris_L == 0 , NA , match3$Boris_L)
match3$Boris_R = ifelse(match3$Boris_R == 0 , NA , match3$Boris_R)

table(match2$win ,match3$hand_flag )

data$combine <- paste(data$name , data$hand, sep ="-")
#----

group_by(data, combine) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )


#---Box Plot-----
boxplot(match3$Arielle_R,match3$Boris_L, col = c("Red","Pink"), horizontal = TRUE)
#----
ggboxplot(data, x = "combine", y = "score", 
          color = "combine", palette = c("#00AFBB", "#E7B800" , "#333333" ,"#DD1212" ),
          ylab = "score", xlab = "Players by hand")


#--- to find normal distributed--
shapiro.test(match3$Arielle_R) # p-value > 0.05 , normally distributed
shapiro.test(match3$Boris_L) # p-value > 0.05 , normally distributed
shapiro.test(match3$Arielle_L) # p-value > 0.05 , normally distributed
shapiro.test(match3$Boris_R) # p-value > 0.05 , normally distributed

#------ Variance check F-test-------------
# 1st Preferred hand

ftest1 <- var.test(match3$Arielle_R,match3$Boris_L)
ftest1 # p-value >0.05, not significant -  variance are equal

# 2nd Preferred hand

ftest2 <- var.test(match3$Arielle_L,match3$Boris_R)
ftest2 # p-value >0.05, not significant -  variance are equal


# ----- t-test 1st hand ------
tsat1 <- t.test(match3$Arielle_R,match3$Boris_L, var.equal = TRUE, alternative = "greater" ) # identify arielle mean is less than boris mean
tsat1
#P-value
tsat1$p.value
#estimate
tsat1$estimate
#confidence level
tsat1$conf.int

print(ifelse(tsat1$p.value<= 0.05, "Arielle is better than Boris when he is using 1st preferred hand", "Not significant"))

# ----- t-test 2nd hand ------
tsat2 <- t.test(match3$Arielle_L,match3$Boris_R, var.equal = TRUE , alternative = "less") # identify arielle mean is less than boris mean
tsat2
#P-value
tsat2$p.value
#estimate
tsat2$estimate
#confidence level
tsat2$conf.int

print(ifelse(tsat2$p.value<= 0.05, "Boris is better than Arielle when he is using 2nd preferred hand", "Not significant"))


#----Exporting files-----

write.xlsx(match3,'summary.xlsx')
write.xlsx(summ,'Hand summary.xlsx')
