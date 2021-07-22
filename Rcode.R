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

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("sathishbairavsb\Paypal-Case-study")

setwd("C:\\Users\\S-K.Baskaran\\OneDrive - Shell\\Personal\\Case study - PP")
data <- read.csv("data.csv" , header = TRUE , sep = "," , stringsAsFactors = FALSE)

table(data$name, data$hand)
summary(data)
summ <- sqldf("select name , hand , count(name) t_c, sum(attempts) total_attempts, sum(score) total_Score , avg(score) avg_Score, median(score) med , min(score) min, max(score) max, stdev(score)*stdev(score) va , stdev(score) std
                from data group by name , hand ")
#10*count(distinct(summ,summ$name))[[1]] 
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
# <- cast(data1,match_day~name, sum)
#ar <- match1[,c(2)]
#br <- match1[,c(3)]
match2 <- cast(data1,match_day~name+hand, sum)
match2$Arielle <- match2$Arielle_L+match2$Arielle_R
match2$Boris <- match2$Boris_L + match2$Boris_R
match2$win <- ifelse(match2$Arielle==match2$Boris , 'Draw',
                     ifelse(match2$Arielle>match2$Boris,'Arielle',  'Boris' ))
#ar1 <- rnorm(ar, mean(ar),sd(ar))
#br1 <- rnorm(br, mean(br),sd(br))
#---------- Comparing Overall ------------------------- 

group_by(data, name) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )


#---Box Plot-----
boxplot(match2$Arielle,match2$Boris, col = c("Red","Pink"), horizontal = TRUE)
#----
ggboxplot(data, x = "name", y = "score", 
          color = "name", palette = c("#00AFBB", "#E7B800"),
          ylab = "score", xlab = "Groups")


#--- to find normal distributed--
shapiro.test(match2$Arielle) # p-value > 0.05 , normally distributed
shapiro.test(match2$Boris) # p-value > 0.05 , normally distributed

#------ Variance check F-test-------------

ftest <- var.test(match2$Arielle,match2$Boris)
ftest # p-value >0.05, not significant -  variance are equal

# ----- t-test ------
tsat <- t.test(match2$Arielle,match2$Boris, var.equal = TRUE , alternative = "less") # identify arielle mean is less than boris mean
tsat
#P-value
tsat$p.value
#estimate
tsat$estimate
#confidence level
tsat$conf.int

print(ifelse(tsat$p.value<= 0.05, "Arielle mean is less than Boris", "Not significant"))

# #-- Wilcox text----- 1 to 1 comparison
# wilcox.test(score ~ name, data = data , paired = TRUE, alternative ="g")
# #
# #--- to find normal distributed--
# shapiro.test(ar) # p-value > 0.05 , normally distributed
# shapiro.test(br) # p-value > 0.05 , normally distributed
# 
# #-Kolmogorov-Smirnov test is used to check whether 2 samples follow the same distribution.
# ks.test(ar,br) 
# #P-value > 0.05 hence follow same distribution
# 
# #--- Independent test-------
# chisq.test(table(data$name, data$hand))
# # Pvalye <0.05, both are not independent
# tsat$estimate
# #---ar is less than br---
# t.test(score ~ name, data = data,
#        var.equal = TRUE, alternative = "less")
# #----ar is greater than br------
# t.test(score ~ name, data = data,
#        var.equal = TRUE, alternative = "greater")
# 
# t.test(score ~ name, data = data,
#          paired = FALSE)
# 
# var.test(data$score ~ data$name, alternative = c("two.sided", "less", "greater") )
# summary(match1)

#-- Compare with preferred hands-------
#match1 <- cast(data1,match_day~name, sum)

#Both are equal with 23 wins each

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
          ylab = "score", xlab = "Groups")


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


tsat3 <- t.test(match3$Arielle_L,match3$Boris_L, var.equal = TRUE, alternative = "less" ) # identify arielle mean is less than boris mean
tsat3

tsat4 <- t.test(match3$Arielle_R,match3$Boris_R, var.equal = TRUE, alternative = "less" ) # identify arielle mean is less than boris mean
tsat4
write.csv()
