###4/9/2020
#New R code to create dataset for Women who gave birth 5 years before the interview
###Comments/Findings: 

#import packages
library(haven)
library(dplyr)
library(car)
install.packages("ROCR")
install.packages("randomForest")
library(randomForest)
library(pROC) 
library(Metrics)
library(ggplot2)
install.packages('Deducer')
install.packages("caret")
install.packages("lattice")
library(caret)
library(ROCR)

#import dataset
hh <- read_sav("D:/Downloads/EDYS Capstone/MICS data set/Viet Nam_MICS5_Datasets/Viet Nam MICS 2013-14 SPSS Datasets/hh.sav")
bh <- read_sav("D:/Downloads/EDYS Capstone/MICS data set/Viet Nam_MICS5_Datasets/Viet Nam MICS 2013-14 SPSS Datasets/bh.sav")
wm <- read_sav("D:/Downloads/EDYS Capstone/MICS data set/Viet Nam_MICS5_Datasets/Viet Nam MICS 2013-14 SPSS Datasets/wm.sav")

#Create ID for all observation from HH1, HH2, LN: women's ID : ID, household ID: hhID
wm$ID <- paste(wm$HH1, wm$HH2, wm$LN)
bh$ID <- paste(bh$HH1,bh$HH2,bh$LN)
hh$hhID <- paste(hh$HH1, hh$HH2)

#filter out 15-49 yr women who gave birth
wm <- subset(wm, wm$CM1 == 1)

#create a subset with women ID, and their years of interview
wm_interviewYear <- subset(wm, select = c(ID, WM6Y))

#filter out all women who gave birth to child in 2008 and 2009 
bh0809 <- subset(bh, bh$BH4Y ==2008|bh$BH4Y == 2009|bh$BH4Y == 2010|bh$BH4Y == 2011|bh$BH4Y == 2012|bh$BH4Y == 2013|bh$BH4Y == 2014)

#merge the year of interview WM6Y to the data set 2008 2009
bh0809 <- left_join(bh0809, wm_interviewYear, by="ID")

#create a new variable to check whether the birth-interview gap is within 5 year
bh0809$gap <- 0
as.numeric(bh0809$WM6Y)
summary(bh0809$WM6Y)
bh0809$gap <- bh0809$WM6Y-bh0809$BH4Y #466 people gave birth within 6 years will be excluded
table(bh0809$gap)

#filter out gap = 6, keep gap within 5
bh0809 <- subset(bh0809,bh0809$gap == 5)
table(bh0809$BH5) #16 death/628 live birth = 2.547%

#find duplicates by tabling the frequency of each ID
bh0809_n_occur <- data.frame(table(bh0809$ID)) #2 duplicates for twins
table(bh0809_n_occur$Freq)
bh0809_duplicated <- subset(bh0809, bh0809$ID == "172 14 2" |bh0809$ID == "22 12 2"|bh0809$ID == "253 16 1" |bh0809$ID =="260 15 2"|bh0809$ID ==	"286 19 5"|bh0809$ID =="319 12 4"|bh0809$ID ==	"8 9 3")

#create data set to analyze from bh0809
#Choose variables from bh0809
#ID, BH2 (twins), BH3 (sex of the child), BH5 Still alive, BH6, BH7, BH8, BH9U (age at death unite), BH9N (age at death number)
#BH10 (any one between birth), HH6 Area, HH7, brthord, birthint (previous birth interval)
#wmweight, wscore (wealth score), windex5

data <- subset(bh0809, select = c(ID,BH5,BH2, BH3,BH6, BH7, BH8, BH9U, BH9N, BH10, HH6,HH7,brthord,birthint,
                                  wmweight,wscore, windex5))

#Clean bh to prepare for merging ((use dpylr))
#BH5: 1 alive 2 died ->> d_alive: 0 alive 1 died
data <- rename(data, d_death = BH5)
data$d_death[data$d_death == 1] <- 0
data$d_death[data$d_death == 2] <- 1


#BH2: 1single 2 multiple->> d_twin: 0 single 1 twin
data <-rename(data, d_twin = BH2)
data$d_twin[data$d_twin == 1] <- 0
data$d_twin[data$d_twin == 2] <- 1

#BH3: 1 boy 2 girl->> d_girl: 0 boy 1 girl
data <-rename(data, d_girl = BH3)
data$d_girl[data$d_girl == 1] <- 0
data$d_girl[data$d_girl == 2] <- 1

#BH6: child_age (age at the interview time)
data <-rename(data, child_age = BH6)

#BH7: live_w_mom
data<- rename(data, live_w_mom = BH7)

#BH9U & BH9N: age_at_death_unit & age_at_death_numb
data <-rename(data, age_at_death_unit = BH9U)
data <-rename(data, age_at_death_numb = BH9N)

#HH6: urban 1 ruran 2 -->  d_rural urban 0 rural 1
data <-rename(data, d_rural = HH6)
data$d_rural[data$d_rural == 1] <- 0
data$d_rural[data$d_rural == 2] <- 1

#HH7: region Red River Delta: 1
#Northern Midlands and Mountain area: 2
#North Central and Central Coastal area: 3
#Central Highlands: 4
#South East: 5
#Mekong River Delta: 6 
data <- rename(data, region = HH7)

data <- subset(data, select = c(ID, d_death, d_twin, d_girl, child_age, live_w_mom, BH8, age_at_death_unit,
                                age_at_death_numb,d_rural, region, brthord,birthint, wmweight, wscore, windex5))
data_bh <- data
##############
#Clean data for women
wm$hhID <- paste(wm$HH1, wm$HH2)
data_wm<-subset(wm, select = c(hhID, ID,WB2,WB3,WB4,WB5,WB7,MT0,MT2,MT3,MT4,MT6,MT7,MT9,MT10,MT11,
                               CM10,CP1,MA2,MA3,MA1,MA6,MA7,MA8Y,HA1,HA2,HA3,HA4,HA5,
                               HA6,HA7,HA8A,HA8B,HA8C, u5death))

wmCP2CP2A <- subset(wm, select = c(ID,CP2, CP2A))

data_wm <- left_join(data_wm,wmCP2CP2A,by="ID")
#rename 
#rename WB2 -> age_interview
data_wm <- rename(data_wm, age_interview = WB2)

#rename WB3 -> d_scl_attend 1 2 -> 1 0 yes no
data_wm <- rename(data_wm, d_scl_attend = WB3)
data_wm$d_scl_attend[data_wm$d_scl_attend == 2] <- 0

#rename WB4 highest_edlevel_cmpl
data_wm <- rename(data_wm, highest_edlevel_cmpl = WB4)

#create d_lit variable: 1 literate 0 illiterate (can't read or can only read a part of the sentence)
data_wm$d_lit <- 1
data_wm$d_lit[data_wm$WB7 == 9] <- NA
data_wm$d_lit[data_wm$WB7 == 1|data_wm$WB7 == 2] <- 0 #10% illiterate/barely literate

cor(data_wm$MT2, data_wm$MT4, use="pairwise.complete.obs")

#create d_news, d_radio, d_TV: 1: good access to information, 0: poor access to information
data_wm$d_news <- 0
data_wm$d_news[data_wm$MT2 == 1 | data_wm$MT2 == 2] <- 1

data_wm$d_radio <- 0
data_wm$d_radio[data_wm$MT3 == 1 | data_wm$MT3 == 2] <- 1

data_wm$d_TV <- 0
data_wm$d_TV[data_wm$MT4 == 1 | data_wm$MT4 == 2] <- 1

#rename CM10 --> tot_brth
data_wm <- rename(data_wm, tot_brth = CM10)
summary(data_wm$tot_brth)

#rename CP1 --> cur_preg 1 2 to 1 0 8 yes no unknown
data_wm <- rename(data_wm, cur_preg = CP1)
data_wm$cur_preg[data_wm$cur_preg == 2] <- 0

#create variable for contraception d_CP when either CP2 = 1 or CP2A=1
data_wm$d_CP <- 0
data_wm$d_CP[data_wm$CP2 == 1 | data_wm$CP2A == 1] <- 1
table(data_wm$d_CP)
data_wm$d_CP[data_wm$CP2A == 9] <- NA

#d_phone: own a phone MT0 1 2 -> 1 0 yes no
data_wm <- rename(data_wm, d_phone = MT0)

#age of husband: age_hsb: MA2
data_wm <- rename(data_wm, age_hsb = MA2)

#d_husb_wife: MA3 husband has other wife 1 2 -> 0 1 no yes
data_wm <- rename(data_wm, d_husb_wife = MA3)

#d_married: MA1 1 married 2 living w a man 3 not in union
data_wm <- rename(data_wm, d_married = MA1)
table(data_wm$d_married)
table(wm$MA1)
#d_divorced: MA6 1 divorced 2 widowed 3 separated 3
data_wm <- rename(data_wm, d_divorced = MA6)

#more than 1 marriage d_more_married 1 2 -> 0 No 1 Yes
data_wm$MA7[data_wm$MA7 == 1] <- 0
data_wm$MA7[data_wm$MA7 == 2] <- 1
data_wm <- rename(data_wm, d_more_married = MA7)

#year of first marriage/union fst_mar MA8Y
data_wm <- rename(data_wm, fst_mar = MA8Y)

#knowledge about HIV virus:
#HA1 1 #HA2 1 #HA3 2 #HA4 1 #HA5 2 #HA6 2 #HA7 1 #HA8A 1 #HA8B 1 #HA8C 1
data_wm$HA1[data_wm$HA1 == 1] <-1
data_HA1_9 <- subset(data_wm, data_wm$HA1 == 9)


#d_never__heard_HIV: HA1 = 2 or 9
data_wm$d_never__heard_HIV <- 0
View(data_wm)
data_wm$d_never__heard_HIV[data_wm$HA1 != 1] <- 1
summary(data_wm$d_never__heard_HIV)
table(data_wm$d_never__heard_HIV) #468 out of 7101 people heard of HIV

summary(data_wm$HA2)
table(data_wm$HA2) 

#check correct answer d_HA2 1 correct 0 incorrect
data_wm$d_HA2 <- 0
data_wm$d_HA2[data_wm$HA2 == 1] <- 1 
table(data_wm$d_HA2)
table(data_wm$HA2) #1 answer is 9!

#check correct answer d_HA3 1 correct 0 incorrect
data_wm$d_HA3 <- 0
data_wm$d_HA3[data_wm$HA3 == 2] <- 1 
table(data_wm$d_HA3)
table(data_wm$HA3)

#check correct answer d_HA4 1 correct 0 incorrect
data_wm$d_HA4 <- 0
data_wm$d_HA4[data_wm$HA4 == 1] <- 1 
table(data_wm$d_HA4)
table(data_wm$HA4)

#check correct answer d_HA5 1 correct 0 incorrect
data_wm$d_HA5 <- 0
data_wm$d_HA5[data_wm$HA5 == 2] <- 1 
table(data_wm$d_HA5)
table(data_wm$HA5)

#check correct answer d_HA6 1 correct 0 incorrect
data_wm$d_HA6 <- 0
data_wm$d_HA6[data_wm$HA6 == 2] <- 1 
table(data_wm$d_HA6)
table(data_wm$HA6)

#check correct answer d_HA7 1 correct 0 incorrect
data_wm$d_HA7 <- 0
data_wm$d_HA7[data_wm$HA7 == 1] <- 1 
table(data_wm$d_HA7)
table(data_wm$HA7)

#check correct answer d_HA8A 1 correct 0 incorrect
data_wm$d_HA8A <- 0
data_wm$d_HA8A[data_wm$HA8A == 1] <- 1 
table(data_wm$d_HA8A)
table(data_wm$HA8A)

#check correct answer d_HA8B 1 correct 0 incorrect
data_wm$d_HA8B <- 0
data_wm$d_HA8B[data_wm$HA8B == 1] <- 1 
table(data_wm$d_HA8B)
table(data_wm$HA8B)

#check correct answer d_HA8C 1 correct 0 incorrect
data_wm$d_HA8C <- 0
data_wm$d_HA8C[data_wm$HA8C == 1] <- 1 
table(data_wm$d_HA8C)
table(data_wm$HA8C)

#total score: HIV_score
data_wm$HIV_score <- apply(data_wm[,c(44, 45, 46, 47, 48, 49, 50, 51,52)], 1, function(x) sum(x))
table(data_wm$HIV_score)

table(data_wm$HA1)


#choose variables from data_wm and clear no more needed vars
data_wm <- subset(data_wm,select = c(hhID, ID, age_interview, d_scl_attend, highest_edlevel_cmpl, WB5, d_phone, tot_brth, cur_preg, age_hsb,d_husb_wife, d_married, d_divorced, d_more_married, fst_mar, d_lit, d_news, d_radio, d_TV, d_CP, d_never__heard_HIV, HIV_score,u5death))

######## hh data set
#summary hh
summary(hh$HH9)
table(hh$HH9)

#rename tot_hhmem: total household member HH11
hh <- rename(hh, tot_hhmem = HH11)
summary(hh$WS1)
summary(hh$WS2)

#d_piped_water: 1 yes, 0 no WS1: 11,12,13,14, 
hh$d_piped_water <- 0
hh$d_piped_water[hh$WS1 == 11 |hh$WS1 == 12|hh$WS1 == 13 |hh$WS1 == 14] <- 1
hh$d_piped_water[is.na(hh$WS1) == TRUE] <- NA
table(hh$d_piped_water)
table(hh$WS1)

#d_flush_toilet: 1 yes, 0 no WS1: 11,12,13,14,15 
hh$d_flush_toilet <- 0
hh$d_flush_toilet[hh$WS8 == 11 |hh$WS8 == 12|hh$WS8 == 13 |hh$WS8 == 14|hh$WS8 == 15 ] <- 1
hh$d_flush_toilet[is.na(hh$WS8) == TRUE] <- NA

#ethnicity HC1A d_Kinh 1 0 Kinh, no Kinh
hh$d_Kinh <- 0
hh$d_Kinh[hh$HC1C == 1] <- 1 

#religion d_religion 1 yes 0 no
hh$d_religion <- 1
hh$d_religion[hh$HC1A == 97] <- 0

table(hh$HC1A)

#rename head_sex: HHSEX 1 2 -> 0 1 male female
table(hh$HHSEX)
hh$HHSEX[hh$HHSEX == 1] <- 0
hh$HHSEX[hh$HHSEX == 2] <- 1
hh <- rename(hh, head_sex = HHSEX)

#create new dataset for hh with desired variables:
#Var names: hhID, tot_hhmem,d_piped_water, d_flush_toilet, head_sex, helevel, hhweight
data_hh <- subset(hh, select= c(hhID, d_Kinh, d_religion, tot_hhmem, d_piped_water, d_flush_toilet, head_sex, helevel, hhweight))
View(data_hh)

#create dummy variable for rural d_rural
rural <- subset(hh, select = c(hhID, HH6))
table(rural$HH6) #1 urban 2 rural ==> 0 1
rural$HH6[rural$HH6 == 1] <- 0
rural$HH6[rural$HH6 == 2] <- 1
rural <- rename(rural, d_rural = HH6)
data_wm_hh <- left_join(data_wm_hh, rural, by="hhID")

####### MERGING 3 DATA SET
### merge data_wm with data_hh by hhID
data_wm_hh <- left_join(data_wm, data_hh, by="hhID")
View(data_wm_hh)

### merge data_wm_hh to birth history of 628 women
data <- left_join(data_bh, data_wm_hh, by ="ID")

###
wm_year_born <- subset(wm, select=c(ID,WB1Y)) #year they born
#merge to dataset: data and data_wm_hh
data <- left_join(data,wm_year_born,by="ID")
data$age_1st_married <- data$fst_mar-data$WB1Y 

summary(data$age_1st_married)

table(data$d_scl_attend)

data_wm_hh <- left_join(data_wm_hh,wm_year_born,by="ID")

### married or live w a man more than once
marriage <- subset(wm, select=c(ID,MA7))#1: once 2: more than once
marriage$MA7[marriage$MA7 == 2] <- 0
marriage$MA7[marriage$MA7 == 9] <- NA
table(marriage$MA7)

data <- left_join(data,marriage,by="ID")
data_wm_hh <- left_join(data_wm_hh,marriage, by="ID")

data$age_1st_married <- data$fst_mar-data$WB1Y 

summary(data$age_1st_married)

table(data$d_scl_attend)


data_wm_hh <- left_join(data_wm_hh,wm_year_born,by="ID")


###REGRESSION ANALYSIS: LOGISTIC MODEL
########
data_wm_hh$region <- factor(data_wm_hh$region) 
data$d_married <- factor(data$d_married) 
data$region <- defactor(data$helevel) 
data$ever_married <- 1
data$ever_married[data$fst_mar == 9998] <- 0
table(data$ever_married)

m1 <- glm(d_death ~ d_tert+d_Kinh+d_religion+d_rural, data = data, family = "binomial")
summary(m1)
m2
coef(m1)

m2 <- glm(d_death ~ d_kgarten+d_primary+d_midschl+d_upper_midschl+d_tert+d_vocational+head_sex+d_news+d_TV+d_CP +age_interview + d_flush_toilet +HIV_score +d_piped_water+birthint+d_girl+d_radio+region+d_twin + d_Kinh +d_religion+windex5, data = data, family = "binomial")
summary(m2)
m3 <- glm(d_death ~ d_primary+d_Kinh+d_religion+d_CP+region+birthint+brthord+wscore+age_1st_married+MA7,data=data,family = "binomial"(link="logit"))
summary(m3)

cor(data$d_scl_attend, data$wscore)

summary(data$highest_edlevel_cmpl)

cor(data)
#create variable for upper secondary attendance d_highscl
data$d_noschool <- 0
data$d_noschool[is.na(data$highest_edlevel_cmpl) == TRUE] <- 1
data$d_kgarten <- 0
data$d_kgarten[data$highest_edlevel_cmpl == 0|data$highest_edlevel_cmpl == 1|data$highest_edlevel_cmpl == 2|data$highest_edlevel_cmpl == 3|data$highest_edlevel_cmpl == 5|data$highest_edlevel_cmpl == 4] <-1
data$d_primary <- 0
data$d_primary[data$highest_edlevel_cmpl == 1|data$highest_edlevel_cmpl == 2|data$highest_edlevel_cmpl == 3|data$highest_edlevel_cmpl == 5|data$highest_edlevel_cmpl == 4] <-1
data$d_midschl <- 0
data$d_midschl[data$highest_edlevel_cmpl == 2|data$highest_edlevel_cmpl == 3|data$highest_edlevel_cmpl == 5|data$highest_edlevel_cmpl == 4] <-1 
data$d_upper_midschl <- 0
data$d_upper_midschl[data$highest_edlevel_cmpl == 3|data$highest_edlevel_cmpl == 5|data$highest_edlevel_cmpl == 4] <-1 
data$d_tert <- 0
data$d_tert[data$highest_edlevel_cmpl == 5] <- 1
data$d_vocational <- 0
data$d_vocational[data$highest_edlevel_cmpl == 4]  <- 1

table(data$d_piped_water)

table(data$d_CP)
table(wm$ethnicity)

table(hh$HC1C)
summary(hh$HC1A)


########## try with whole women population
#u5death:0 never experience child death, 1 experienced at least 1 death
wm$u5death <- 0
wm$u5death[wm$ID =="7 18 4"|wm$ID =="12 7 2"|wm$ID =="15 7 4"|wm$ID =="15 7 4"|wm$ID =="16 5 2"|wm$ID =="16 5 2"|wm$ID =="16 13 2"|wm$ID =="17 4 2"|wm$ID =="17 19 2"|wm$ID =="19 3 2"|wm$ID =="19 3 2"|wm$ID =="19 3 2"|wm$ID =="19 4 2"|wm$ID =="19 10 2"|wm$ID =="19 11 2"|wm$ID =="19 12 2"|wm$ID =="19 20 2"|wm$ID =="21 3 2"|wm$ID =="21 4 2"|wm$ID =="21 4 2"|wm$ID =="22 2 1"|wm$ID =="22 8 4"|wm$ID =="22 12 2"|wm$ID =="22 12 2"|wm$ID =="23 8 2"|wm$ID =="24 3 2"|wm$ID =="26 3 2"|wm$ID =="28 13 2"|wm$ID =="29 2 2"|wm$ID =="29 8 2"|wm$ID =="31 14 2"|wm$ID =="31 14 2"|wm$ID =="33 9 1"|wm$ID =="33 17 2"|wm$ID =="33 17 2"|wm$ID =="34 10 2"|wm$ID =="36 1 3"|wm$ID =="36 7 2"|wm$ID =="36 10 2"|wm$ID =="37 15 2"|wm$ID =="38 18 2"|wm$ID =="41 2 2"|wm$ID =="41 8 6"|wm$ID =="41 10 2"|wm$ID =="41 10 2"|wm$ID =="41 15 2"|wm$ID =="41 15 3"|wm$ID =="41 16 2"|wm$ID =="41 16 2"|wm$ID =="41 18 2"|wm$ID =="41 18 2"|wm$ID =="41 19 2"|wm$ID =="42 1 2"|wm$ID =="42 1 2"|wm$ID =="42 8 2"|wm$ID =="42 9 4"|wm$ID =="42 9 4"|wm$ID =="42 15 2"|wm$ID =="43 2 5"|wm$ID =="43 9 3"|wm$ID =="43 9 3"|wm$ID =="43 10 2"|wm$ID =="43 10 2"|wm$ID =="43 10 2"|wm$ID =="43 13 2"|wm$ID =="43 13 2"|wm$ID =="43 14 2"|wm$ID =="43 17 2"|wm$ID =="43 18 2"|wm$ID =="44 18 2"|wm$ID =="45 4 2"|wm$ID =="45 4 2"|wm$ID =="45 6 2"|wm$ID =="45 12 2"|wm$ID =="45 20 2"|wm$ID =="45 20 2"|wm$ID =="46 1 2"|wm$ID =="46 8 1"|wm$ID =="46 8 1"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 10 4"|wm$ID =="46 12 2"|wm$ID =="46 19 1"|wm$ID =="47 9 2"|wm$ID =="48 9 1"|wm$ID =="48 12 2"|wm$ID =="51 4 2"|wm$ID =="51 13 3"|wm$ID =="52 1 3"|wm$ID =="52 1 3"|wm$ID =="52 1 3"|wm$ID =="52 2 2"|wm$ID =="52 4 2"|wm$ID =="52 7 2"|wm$ID =="54 13 2"|wm$ID =="55 1 2"|wm$ID =="55 1 2"|wm$ID =="55 1 4"|wm$ID =="55 2 2"|wm$ID =="55 8 2"|wm$ID =="55 8 9"|wm$ID =="55 9 2"|wm$ID =="55 13 4"|wm$ID =="55 13 4"|wm$ID =="55 13 4"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="56 20 2"|wm$ID =="56 20 2"|wm$ID =="57 9 1"|wm$ID =="58 5 2"|wm$ID =="58 5 2"|wm$ID =="58 6 2"|wm$ID =="58 16 2"] <- 1
wm$u5death[wm$ID =="58 20 4"|wm$ID =="59 14 2"|wm$ID =="59 15 2"|wm$ID =="60 11 2"|wm$ID =="60 11 2"|wm$ID =="60 16 2"|wm$ID =="61 5 3"|wm$ID =="62 5 2"|wm$ID =="62 5 2"|wm$ID =="63 3 2"|wm$ID =="67 9 3"|wm$ID =="68 12 2"|wm$ID =="69 8 2"|wm$ID =="75 1 4"|wm$ID =="77 14 2"|wm$ID =="79 13 2"|wm$ID =="84 1 4"|wm$ID =="85 6 2"|wm$ID =="85 12 1"|wm$ID =="85 12 1"|wm$ID =="87 20 2"|wm$ID =="88 2 2"|wm$ID =="88 17 1"|wm$ID =="89 3 2"|wm$ID =="89 9 2"|wm$ID =="89 16 1"|wm$ID =="89 20 2"|wm$ID =="93 18 4"|wm$ID =="96 9 2"|wm$ID =="96 18 2"|wm$ID =="96 19 2"|wm$ID =="96 20 2"|wm$ID =="100 4 2"|wm$ID =="102 2 3"|wm$ID =="103 13 1"|wm$ID =="104 15 2"|wm$ID =="105 1 3"|wm$ID =="105 6 3"|wm$ID =="105 10 2"|wm$ID =="105 10 2"|wm$ID =="106 9 2"|wm$ID =="106 9 2"|wm$ID =="106 12 2"|wm$ID =="106 14 2"|wm$ID =="106 18 2"|wm$ID =="107 13 4"|wm$ID =="109 19 2"|wm$ID =="114 12 2"|wm$ID =="115 6 2"|wm$ID =="116 9 2"|wm$ID =="120 6 3"|wm$ID =="122 20 3"|wm$ID =="122 20 3"|wm$ID =="123 18 3"|wm$ID =="125 13 2"|wm$ID =="127 3 2"|wm$ID =="127 9 2"|wm$ID =="128 3 2"|wm$ID =="130 8 2"|wm$ID =="132 17 1"|wm$ID =="132 20 2"|wm$ID =="133 7 2"|wm$ID =="135 15 2"|wm$ID =="135 17 2"|wm$ID =="136 1 1"|wm$ID =="136 3 1"|wm$ID =="141 8 2"|wm$ID =="143 14 2"|wm$ID =="143 19 2"|wm$ID =="143 20 2"|wm$ID =="144 16 2"|wm$ID =="149 8 4"|wm$ID =="149 8 4"|wm$ID =="150 16 2"|wm$ID =="152 1 1"|wm$ID =="152 2 2"|wm$ID =="153 16 3"|wm$ID =="154 2 2"|wm$ID =="154 2 2"|wm$ID =="158 2 2"|wm$ID =="161 17 2"|wm$ID =="163 17 4"|wm$ID =="165 5 2"|wm$ID =="166 2 2"|wm$ID =="170 13 2"|wm$ID =="170 20 1"|wm$ID =="171 6 2"|wm$ID =="171 20 1"|wm$ID =="172 14 2"|wm$ID =="173 11 2"|wm$ID =="173 13 2"|wm$ID =="176 20 2"|wm$ID =="179 5 2"|wm$ID =="182 20 2"|wm$ID =="184 6 2"|wm$ID =="188 4 2"|wm$ID =="188 8 2"|wm$ID =="189 7 2"|wm$ID =="189 15 2"|wm$ID =="190 19 2"|wm$ID =="192 4 1"|wm$ID =="193 18 3"|wm$ID =="195 9 2"|wm$ID =="195 17 2"|wm$ID =="195 18 2"|wm$ID =="196 8 2"|wm$ID =="196 19 2"|wm$ID =="197 4 2"|wm$ID =="197 13 2"|wm$ID =="199 9 2"|wm$ID =="200 7 2"|wm$ID =="202 16 2"|wm$ID =="204 10 2"|wm$ID =="207 4 2"] <- 1
wm$u5death[wm$ID =="207 10 2"|wm$ID =="207 10 2"|wm$ID =="208 6 2"|wm$ID =="208 14 4"|wm$ID =="209 12 1"|wm$ID =="212 7 2"|wm$ID =="213 5 4"|wm$ID =="218 13 5"|wm$ID =="221 4 2"|wm$ID =="222 13 2"|wm$ID =="225 18 2"|wm$ID =="229 3 2"|wm$ID =="230 5 1"|wm$ID =="231 11 2"|wm$ID =="231 20 2"|wm$ID =="234 1 3"|wm$ID =="234 8 2"|wm$ID =="235 9 2"|wm$ID =="235 19 2"|wm$ID =="237 12 2"|wm$ID =="237 14 2"|wm$ID =="238 8 2"|wm$ID =="239 17 2"|wm$ID =="241 16 2"|wm$ID =="241 19 1"|wm$ID =="242 12 2"|wm$ID =="242 14 3"|wm$ID =="243 18 1"|wm$ID =="244 17 2"|wm$ID =="248 13 2"|wm$ID =="248 13 2"|wm$ID =="248 13 2"|wm$ID =="249 3 1"|wm$ID =="249 3 1"|wm$ID =="252 12 2"|wm$ID =="253 3 2"|wm$ID =="253 8 2"|wm$ID =="253 16 1"|wm$ID =="253 18 2"|wm$ID =="253 18 2"|wm$ID =="255 4 2"|wm$ID =="255 8 2"|wm$ID =="255 19 2"|wm$ID =="257 14 1"|wm$ID =="257 14 1"|wm$ID =="257 19 2"|wm$ID =="259 5 2"|wm$ID =="260 1 2"|wm$ID =="260 2 2"|wm$ID =="260 2 2"|wm$ID =="260 17 2"|wm$ID =="261 3 2"|wm$ID =="261 7 2"|wm$ID =="261 20 2"|wm$ID =="262 6 2"|wm$ID =="262 6 2"|wm$ID =="262 10 2"|wm$ID =="262 11 2"|wm$ID =="263 17 2"|wm$ID =="264 7 2"|wm$ID =="264 7 2"|wm$ID =="266 5 1"|wm$ID =="267 13 2"|wm$ID =="270 18 2"|wm$ID =="274 4 2"|wm$ID =="274 5 2"|wm$ID =="275 7 2"|wm$ID =="275 17 2"|wm$ID =="275 18 2"|wm$ID =="276 12 2"|wm$ID =="276 12 2"|wm$ID =="280 12 2"|wm$ID =="281 4 4"|wm$ID =="281 4 4"|wm$ID =="281 5 2"|wm$ID =="281 8 2"|wm$ID =="281 12 3"|wm$ID =="282 10 3"|wm$ID =="283 14 2"|wm$ID =="283 14 2"|wm$ID =="290 17 1"|wm$ID =="292 8 2"|wm$ID =="292 8 2"|wm$ID =="292 12 2"|wm$ID =="292 14 2"|wm$ID =="293 9 2"|wm$ID =="293 9 2"|wm$ID =="293 13 2"|wm$ID =="293 13 2"|wm$ID =="295 11 2"|wm$ID =="296 2 3"|wm$ID =="299 9 1"|wm$ID =="299 9 1"|wm$ID =="299 10 2"|wm$ID =="299 10 2"|wm$ID =="299 14 2"|wm$ID =="299 15 2"|wm$ID =="300 1 1"|wm$ID =="300 10 2"|wm$ID =="300 11 1"|wm$ID =="302 6 2"|wm$ID =="302 17 2"|wm$ID =="303 10 2"|wm$ID =="303 12 2"|wm$ID =="303 19 2"|wm$ID =="304 9 2"|wm$ID =="304 16 2"|wm$ID =="305 9 1"|wm$ID =="305 15 3"|wm$ID =="307 12 2"|wm$ID =="307 12 2"|wm$ID =="307 19 1"] <- 1
wm$u5death[wm$ID =="310 4 1"|wm$ID =="310 13 2"|wm$ID =="312 2 2"|wm$ID =="312 7 2"|wm$ID =="312 15 2"|wm$ID =="313 11 2"|wm$ID =="313 11 2"|wm$ID =="315 9 2"|wm$ID =="316 7 2"|wm$ID =="317 9 2"|wm$ID =="317 15 2"|wm$ID =="324 2 2"|wm$ID =="324 2 2"|wm$ID =="327 2 2"|wm$ID =="327 2 2"|wm$ID =="328 1 2"|wm$ID =="328 8 2"|wm$ID =="328 13 7"|wm$ID =="329 7 2"|wm$ID =="330 1 2"|wm$ID =="330 1 2"|wm$ID =="331 11 1"|wm$ID =="331 16 1"|wm$ID =="334 11 1"|wm$ID =="334 11 4"|wm$ID =="338 4 2"|wm$ID =="340 2 2"|wm$ID =="340 17 2"|wm$ID =="340 17 2"|wm$ID =="341 13 1"|wm$ID =="342 10 3"|wm$ID =="344 19 2"|wm$ID =="345 5 3"|wm$ID =="345 14 2"|wm$ID =="346 16 2"|wm$ID =="349 1 1"|wm$ID =="349 15 2"|wm$ID =="356 3 1"|wm$ID =="357 6 2"|wm$ID =="357 8 1"|wm$ID =="357 20 1"|wm$ID =="358 2 2"|wm$ID =="358 13 3"|wm$ID =="359 1 2"|wm$ID =="359 1 2"|wm$ID =="359 20 2"|wm$ID =="364 20 2"|wm$ID =="368 8 2"|wm$ID =="369 19 2"|wm$ID =="369 20 2"|wm$ID =="370 15 2"|wm$ID =="372 7 2"|wm$ID =="375 1 2"|wm$ID =="376 12 1"|wm$ID =="376 19 1"|wm$ID =="381 16 2"|wm$ID =="381 19 2"|wm$ID =="382 12 2"|wm$ID =="382 20 1"|wm$ID =="391 16 4"|wm$ID =="400 11 1"|wm$ID =="404 18 2"|wm$ID =="405 19 2"|wm$ID =="407 17 2"|wm$ID =="407 18 1"|wm$ID =="410 6 4"|wm$ID =="417 17 5"|wm$ID =="420 16 2"|wm$ID =="421 6 2"|wm$ID =="422 18 2"|wm$ID =="425 1 2"|wm$ID =="430 8 2"|wm$ID =="431 12 1"|wm$ID =="434 14 2"|wm$ID =="434 14 2"|wm$ID =="436 15 6"|wm$ID =="437 5 2"|wm$ID =="438 3 2"|wm$ID =="442 7 2"|wm$ID =="442 12 2"|wm$ID =="442 12 2"|wm$ID =="442 18 1"|wm$ID =="446 19 1"|wm$ID =="447 13 1"|wm$ID =="447 13 1"|wm$ID =="449 12 1"|wm$ID =="449 12 1"|wm$ID =="450 1 1"|wm$ID =="452 8 3"|wm$ID =="452 8 3"|wm$ID =="456 17 2"|wm$ID =="457 1 1"|wm$ID =="460 10 1"|wm$ID =="461 10 1"|wm$ID =="463 19 1"|wm$ID =="465 1 2"|wm$ID =="467 3 1"|wm$ID =="467 14 2"|wm$ID =="467 17 3"|wm$ID =="468 12 2"|wm$ID =="468 12 2"|wm$ID =="469 12 2"|wm$ID =="472 12 2"|wm$ID =="472 14 2"|wm$ID =="473 11 2"|wm$ID =="476 1 4"|wm$ID =="476 11 1"|wm$ID =="477 18 2"|wm$ID =="478 4 2"|wm$ID =="479 12 2"|wm$ID =="481 5 4"|wm$ID =="482 8 2"] <- 1
wm$u5death[wm$ID =="484 3 2"|wm$ID =="484 11 1"|wm$ID =="487 2 1"|wm$ID =="500 3 2"|wm$ID =="501 19 2"|wm$ID =="504 10 2"|wm$ID =="506 4 2"|wm$ID =="508 10 4"|wm$ID =="510 1 2"|wm$ID =="510 8 2"] <- 1


#regression w all women : data = data_wm_hh
data_wm_hh$d_noschool <- 0
data_wm_hh$d_noschool[is.na(data_wm_hh$highest_edlevel_cmpl) == TRUE] <- 1
data_wm_hh$d_kgarten <- 0
data_wm_hh$d_kgarten[data_wm_hh$highest_edlevel_cmpl == 0|data_wm_hh$highest_edlevel_cmpl == 1|data_wm_hh$highest_edlevel_cmpl == 2|data_wm_hh$highest_edlevel_cmpl == 3|data_wm_hh$highest_edlevel_cmpl == 5|data_wm_hh$highest_edlevel_cmpl == 4] <-1
data_wm_hh$d_primary <- 0
data_wm_hh$d_primary[data_wm_hh$highest_edlevel_cmpl == 1|data_wm_hh$highest_edlevel_cmpl == 2|data_wm_hh$highest_edlevel_cmpl == 3|data_wm_hh$highest_edlevel_cmpl == 5|data_wm_hh$highest_edlevel_cmpl == 4] <-1
data_wm_hh$d_midschl <- 0
data_wm_hh$d_midschl[data_wm_hh$highest_edlevel_cmpl == 2|data_wm_hh$highest_edlevel_cmpl == 3|data_wm_hh$highest_edlevel_cmpl == 5|data_wm_hh$highest_edlevel_cmpl == 4] <-1 
data_wm_hh$d_upper_midschl <- 0
data_wm_hh$d_upper_midschl[data_wm_hh$highest_edlevel_cmpl == 3||data_wm_hh$highest_edlevel_cmpl == 5||data_wm_hh$highest_edlevel_cmpl == 4] <-1

data_wm_hh$d_tert <- 0
data_wm_hh$d_tert[data_wm_hh$highest_edlevel_cmpl == 5] <- 1
data_wm_hh$d_vocational <- 0
data_wm_hh$d_vocational[data_wm_hh$highest_edlevel_cmpl == 4]  <- 1


#add region
hh <- rename(hh, region = HH7)
region <- subset(hh, select=c(hhID,region))
data_wm_hh <- left_join(data_wm_hh,region, by ="hhID")

#add wealth index/score
wealth <- subset(hh, select=c(hhID,wscore,windex5))
data_wm_hh <- left_join(data_wm_hh,wealth, by ="hhID")


#ever married
data_wm_hh$ever_married <-1
data_wm_hh$ever_married[data_wm_hh$fst_mar == 9998] <- 0
data_wm_hh$ever_married[is.na(data_wm_hh$fst_mar)== TRUE] <- NA
table(data_wm_hh$ever_married)
summary(data_wm_hh$ever_married)


#adding weight variable
weight <- subset(wm,select=c(ID,wmweight))
data_wm_hh <- left_join(data_wm_hh,weight,by="ID")

#add marrital status
data_wm_hh <- subset(data_wm_hh, select = -c(d_married))
wm_marriage <- subset(wm, select=c(ID,MA1,MA5))
wm_marriage$mar <- 1 #married
wm_marriage$mar[wm_marriage$MA1 == 2] <- 2 #living with a man
wm_marriage$mar[wm_marriage$MA5 == 1] <- 3 #formally married
wm_marriage$mar[wm_marriage$MA5 == 2] <- 4 #formally live with a man
wm_marriage$mar[wm_marriage$MA5 == 3] <- 5 #formally 
wm_marriage <- subset(wm_marriage, select = c(ID, mar))
data_wm_hh <- left_join(data_wm_hh,wm_marriage, by = "ID")
