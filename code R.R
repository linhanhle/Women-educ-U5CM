### 4/7/2020
#Data Cleaning for Vietnam MICS5 wm and bh
#Merge two data set

#remove women in wm dataset without birth history/never gave birth
wm <- subset(wm, wm$CM1 == 1)
table(wm$WM6Y)
summary(bh$BH5)
table(bh$BH5) #3.2% death ==> relatively low

#subset: birth in 2009-2014, sample size=3285 
bh0914 <- subset(bh, bh$BH4Y == 2009 |bh$BH4Y == 2010 |bh$BH4Y == 2011|bh$BH4Y == 2012|bh$BH4Y == 2013| bh$BH4Y == 2014 )
#subset: birth in 2009, sample size=615
bh1114 <- subset(bh, bh$BH4Y == 2011|bh$BH4Y == 2012|bh$BH4Y == 2013|bh$BH4Y == 2014)
bh14 <-subset(bh, bh$BH4Y == 2014)
bh0809 <- subset(bh, bh$BH4Y ==2008|bh$BH4Y == 2009)

table(bh0809$BH5) #601 live, 14 death
table(bh0914$BH5) #3228 live, 57 birth

rm(bh0809, bh09, bh0914, bh1112, bh1113, bh1114, bh14)

death09 <- subset(bh09, bh09$BH5 == 2)

#replace BH5 1 2 to 0:live 1:death
bh09$BH5[bh09$BH5 == 1] <- 0
bh09$BH5[bh09$BH5 == 2] <- 1
bh$BH5[bh$BH5 == 1] <- 0 #total data set
bh$BH5[bh$BH5 == 2] <- 1


summary(bh09$HH7)
m1 <- glm(BH5 ~ BH3 + HH6 + HH7 + ethnicity + welevel +brthord +magebrt +wscore,data=bh09 , family = "binomial")
summary(m1)
table(bh09$ethnicity)

### 4/8/2020
#Data cleaning and merging
#Create ID for women in bh data from HH1, HH2, LN
bh$ID <- paste(bh$HH1,bh$HH2,bh$LN)
wm$ID <- paste(wm$HH1, wm$HH2, wm$LN)

#Create a dataset only with women who experienced child mortality 
bh_childmor <- subset(bh,bh$BH5 == 1)
summary(bh_childmor$BH9N)

#filter out death not child mortality
bh_childmor$u5death <- 1 
bh_childmor$u5death[(bh_childmor$BH9U == 3) & (bh_childmor$BH9N > 5)] <- 0  
table(bh_childmor$u5death)  
bh_childmor <- subset(bh_childmor, bh_childmor$u5death == 1)
summary(bh_childmor)

#merge dataset
#u5death:0 never experience child death, 1 experienced at least 1 death
wm$u5death <- 0
wm$u5death[wm$ID =="7 18 4"|wm$ID =="12 7 2"|wm$ID =="15 7 4"|wm$ID =="15 7 4"|wm$ID =="16 5 2"|wm$ID =="16 5 2"|wm$ID =="16 13 2"|wm$ID =="17 4 2"|wm$ID =="17 19 2"|wm$ID =="19 3 2"|wm$ID =="19 3 2"|wm$ID =="19 3 2"|wm$ID =="19 4 2"|wm$ID =="19 10 2"|wm$ID =="19 11 2"|wm$ID =="19 12 2"|wm$ID =="19 20 2"|wm$ID =="21 3 2"|wm$ID =="21 4 2"|wm$ID =="21 4 2"|wm$ID =="22 2 1"|wm$ID =="22 8 4"|wm$ID =="22 12 2"|wm$ID =="22 12 2"|wm$ID =="23 8 2"|wm$ID =="24 3 2"|wm$ID =="26 3 2"|wm$ID =="28 13 2"|wm$ID =="29 2 2"|wm$ID =="29 8 2"|wm$ID =="31 14 2"|wm$ID =="31 14 2"|wm$ID =="33 9 1"|wm$ID =="33 17 2"|wm$ID =="33 17 2"|wm$ID =="34 10 2"|wm$ID =="36 1 3"|wm$ID =="36 7 2"|wm$ID =="36 10 2"|wm$ID =="37 15 2"|wm$ID =="38 18 2"|wm$ID =="41 2 2"|wm$ID =="41 8 6"|wm$ID =="41 10 2"|wm$ID =="41 10 2"|wm$ID =="41 15 2"|wm$ID =="41 15 3"|wm$ID =="41 16 2"|wm$ID =="41 16 2"|wm$ID =="41 18 2"|wm$ID =="41 18 2"|wm$ID =="41 19 2"|wm$ID =="42 1 2"|wm$ID =="42 1 2"|wm$ID =="42 8 2"|wm$ID =="42 9 4"|wm$ID =="42 9 4"|wm$ID =="42 15 2"|wm$ID =="43 2 5"|wm$ID =="43 9 3"|wm$ID =="43 9 3"|wm$ID =="43 10 2"|wm$ID =="43 10 2"|wm$ID =="43 10 2"|wm$ID =="43 13 2"|wm$ID =="43 13 2"|wm$ID =="43 14 2"|wm$ID =="43 17 2"|wm$ID =="43 18 2"|wm$ID =="44 18 2"|wm$ID =="45 4 2"|wm$ID =="45 4 2"|wm$ID =="45 6 2"|wm$ID =="45 12 2"|wm$ID =="45 20 2"|wm$ID =="45 20 2"|wm$ID =="46 1 2"|wm$ID =="46 8 1"|wm$ID =="46 8 1"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 9 2"|wm$ID =="46 10 4"|wm$ID =="46 12 2"|wm$ID =="46 19 1"|wm$ID =="47 9 2"|wm$ID =="48 9 1"|wm$ID =="48 12 2"|wm$ID =="51 4 2"|wm$ID =="51 13 3"|wm$ID =="52 1 3"|wm$ID =="52 1 3"|wm$ID =="52 1 3"|wm$ID =="52 2 2"|wm$ID =="52 4 2"|wm$ID =="52 7 2"|wm$ID =="54 13 2"|wm$ID =="55 1 2"|wm$ID =="55 1 2"|wm$ID =="55 1 4"|wm$ID =="55 2 2"|wm$ID =="55 8 2"|wm$ID =="55 8 9"|wm$ID =="55 9 2"|wm$ID =="55 13 4"|wm$ID =="55 13 4"|wm$ID =="55 13 4"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="55 17 2"|wm$ID =="56 20 2"|wm$ID =="56 20 2"|wm$ID =="57 9 1"|wm$ID =="58 5 2"|wm$ID =="58 5 2"|wm$ID =="58 6 2"|wm$ID =="58 16 2"] <- 1
wm$u5death[wm$ID =="58 20 4"|wm$ID =="59 14 2"|wm$ID =="59 15 2"|wm$ID =="60 11 2"|wm$ID =="60 11 2"|wm$ID =="60 16 2"|wm$ID =="61 5 3"|wm$ID =="62 5 2"|wm$ID =="62 5 2"|wm$ID =="63 3 2"|wm$ID =="67 9 3"|wm$ID =="68 12 2"|wm$ID =="69 8 2"|wm$ID =="75 1 4"|wm$ID =="77 14 2"|wm$ID =="79 13 2"|wm$ID =="84 1 4"|wm$ID =="85 6 2"|wm$ID =="85 12 1"|wm$ID =="85 12 1"|wm$ID =="87 20 2"|wm$ID =="88 2 2"|wm$ID =="88 17 1"|wm$ID =="89 3 2"|wm$ID =="89 9 2"|wm$ID =="89 16 1"|wm$ID =="89 20 2"|wm$ID =="93 18 4"|wm$ID =="96 9 2"|wm$ID =="96 18 2"|wm$ID =="96 19 2"|wm$ID =="96 20 2"|wm$ID =="100 4 2"|wm$ID =="102 2 3"|wm$ID =="103 13 1"|wm$ID =="104 15 2"|wm$ID =="105 1 3"|wm$ID =="105 6 3"|wm$ID =="105 10 2"|wm$ID =="105 10 2"|wm$ID =="106 9 2"|wm$ID =="106 9 2"|wm$ID =="106 12 2"|wm$ID =="106 14 2"|wm$ID =="106 18 2"|wm$ID =="107 13 4"|wm$ID =="109 19 2"|wm$ID =="114 12 2"|wm$ID =="115 6 2"|wm$ID =="116 9 2"|wm$ID =="120 6 3"|wm$ID =="122 20 3"|wm$ID =="122 20 3"|wm$ID =="123 18 3"|wm$ID =="125 13 2"|wm$ID =="127 3 2"|wm$ID =="127 9 2"|wm$ID =="128 3 2"|wm$ID =="130 8 2"|wm$ID =="132 17 1"|wm$ID =="132 20 2"|wm$ID =="133 7 2"|wm$ID =="135 15 2"|wm$ID =="135 17 2"|wm$ID =="136 1 1"|wm$ID =="136 3 1"|wm$ID =="141 8 2"|wm$ID =="143 14 2"|wm$ID =="143 19 2"|wm$ID =="143 20 2"|wm$ID =="144 16 2"|wm$ID =="149 8 4"|wm$ID =="149 8 4"|wm$ID =="150 16 2"|wm$ID =="152 1 1"|wm$ID =="152 2 2"|wm$ID =="153 16 3"|wm$ID =="154 2 2"|wm$ID =="154 2 2"|wm$ID =="158 2 2"|wm$ID =="161 17 2"|wm$ID =="163 17 4"|wm$ID =="165 5 2"|wm$ID =="166 2 2"|wm$ID =="170 13 2"|wm$ID =="170 20 1"|wm$ID =="171 6 2"|wm$ID =="171 20 1"|wm$ID =="172 14 2"|wm$ID =="173 11 2"|wm$ID =="173 13 2"|wm$ID =="176 20 2"|wm$ID =="179 5 2"|wm$ID =="182 20 2"|wm$ID =="184 6 2"|wm$ID =="188 4 2"|wm$ID =="188 8 2"|wm$ID =="189 7 2"|wm$ID =="189 15 2"|wm$ID =="190 19 2"|wm$ID =="192 4 1"|wm$ID =="193 18 3"|wm$ID =="195 9 2"|wm$ID =="195 17 2"|wm$ID =="195 18 2"|wm$ID =="196 8 2"|wm$ID =="196 19 2"|wm$ID =="197 4 2"|wm$ID =="197 13 2"|wm$ID =="199 9 2"|wm$ID =="200 7 2"|wm$ID =="202 16 2"|wm$ID =="204 10 2"|wm$ID =="207 4 2"] <- 1
wm$u5death[wm$ID =="207 10 2"|wm$ID =="207 10 2"|wm$ID =="208 6 2"|wm$ID =="208 14 4"|wm$ID =="209 12 1"|wm$ID =="212 7 2"|wm$ID =="213 5 4"|wm$ID =="218 13 5"|wm$ID =="221 4 2"|wm$ID =="222 13 2"|wm$ID =="225 18 2"|wm$ID =="229 3 2"|wm$ID =="230 5 1"|wm$ID =="231 11 2"|wm$ID =="231 20 2"|wm$ID =="234 1 3"|wm$ID =="234 8 2"|wm$ID =="235 9 2"|wm$ID =="235 19 2"|wm$ID =="237 12 2"|wm$ID =="237 14 2"|wm$ID =="238 8 2"|wm$ID =="239 17 2"|wm$ID =="241 16 2"|wm$ID =="241 19 1"|wm$ID =="242 12 2"|wm$ID =="242 14 3"|wm$ID =="243 18 1"|wm$ID =="244 17 2"|wm$ID =="248 13 2"|wm$ID =="248 13 2"|wm$ID =="248 13 2"|wm$ID =="249 3 1"|wm$ID =="249 3 1"|wm$ID =="252 12 2"|wm$ID =="253 3 2"|wm$ID =="253 8 2"|wm$ID =="253 16 1"|wm$ID =="253 18 2"|wm$ID =="253 18 2"|wm$ID =="255 4 2"|wm$ID =="255 8 2"|wm$ID =="255 19 2"|wm$ID =="257 14 1"|wm$ID =="257 14 1"|wm$ID =="257 19 2"|wm$ID =="259 5 2"|wm$ID =="260 1 2"|wm$ID =="260 2 2"|wm$ID =="260 2 2"|wm$ID =="260 17 2"|wm$ID =="261 3 2"|wm$ID =="261 7 2"|wm$ID =="261 20 2"|wm$ID =="262 6 2"|wm$ID =="262 6 2"|wm$ID =="262 10 2"|wm$ID =="262 11 2"|wm$ID =="263 17 2"|wm$ID =="264 7 2"|wm$ID =="264 7 2"|wm$ID =="266 5 1"|wm$ID =="267 13 2"|wm$ID =="270 18 2"|wm$ID =="274 4 2"|wm$ID =="274 5 2"|wm$ID =="275 7 2"|wm$ID =="275 17 2"|wm$ID =="275 18 2"|wm$ID =="276 12 2"|wm$ID =="276 12 2"|wm$ID =="280 12 2"|wm$ID =="281 4 4"|wm$ID =="281 4 4"|wm$ID =="281 5 2"|wm$ID =="281 8 2"|wm$ID =="281 12 3"|wm$ID =="282 10 3"|wm$ID =="283 14 2"|wm$ID =="283 14 2"|wm$ID =="290 17 1"|wm$ID =="292 8 2"|wm$ID =="292 8 2"|wm$ID =="292 12 2"|wm$ID =="292 14 2"|wm$ID =="293 9 2"|wm$ID =="293 9 2"|wm$ID =="293 13 2"|wm$ID =="293 13 2"|wm$ID =="295 11 2"|wm$ID =="296 2 3"|wm$ID =="299 9 1"|wm$ID =="299 9 1"|wm$ID =="299 10 2"|wm$ID =="299 10 2"|wm$ID =="299 14 2"|wm$ID =="299 15 2"|wm$ID =="300 1 1"|wm$ID =="300 10 2"|wm$ID =="300 11 1"|wm$ID =="302 6 2"|wm$ID =="302 17 2"|wm$ID =="303 10 2"|wm$ID =="303 12 2"|wm$ID =="303 19 2"|wm$ID =="304 9 2"|wm$ID =="304 16 2"|wm$ID =="305 9 1"|wm$ID =="305 15 3"|wm$ID =="307 12 2"|wm$ID =="307 12 2"|wm$ID =="307 19 1"] <- 1
wm$u5death[wm$ID =="310 4 1"|wm$ID =="310 13 2"|wm$ID =="312 2 2"|wm$ID =="312 7 2"|wm$ID =="312 15 2"|wm$ID =="313 11 2"|wm$ID =="313 11 2"|wm$ID =="315 9 2"|wm$ID =="316 7 2"|wm$ID =="317 9 2"|wm$ID =="317 15 2"|wm$ID =="324 2 2"|wm$ID =="324 2 2"|wm$ID =="327 2 2"|wm$ID =="327 2 2"|wm$ID =="328 1 2"|wm$ID =="328 8 2"|wm$ID =="328 13 7"|wm$ID =="329 7 2"|wm$ID =="330 1 2"|wm$ID =="330 1 2"|wm$ID =="331 11 1"|wm$ID =="331 16 1"|wm$ID =="334 11 1"|wm$ID =="334 11 4"|wm$ID =="338 4 2"|wm$ID =="340 2 2"|wm$ID =="340 17 2"|wm$ID =="340 17 2"|wm$ID =="341 13 1"|wm$ID =="342 10 3"|wm$ID =="344 19 2"|wm$ID =="345 5 3"|wm$ID =="345 14 2"|wm$ID =="346 16 2"|wm$ID =="349 1 1"|wm$ID =="349 15 2"|wm$ID =="356 3 1"|wm$ID =="357 6 2"|wm$ID =="357 8 1"|wm$ID =="357 20 1"|wm$ID =="358 2 2"|wm$ID =="358 13 3"|wm$ID =="359 1 2"|wm$ID =="359 1 2"|wm$ID =="359 20 2"|wm$ID =="364 20 2"|wm$ID =="368 8 2"|wm$ID =="369 19 2"|wm$ID =="369 20 2"|wm$ID =="370 15 2"|wm$ID =="372 7 2"|wm$ID =="375 1 2"|wm$ID =="376 12 1"|wm$ID =="376 19 1"|wm$ID =="381 16 2"|wm$ID =="381 19 2"|wm$ID =="382 12 2"|wm$ID =="382 20 1"|wm$ID =="391 16 4"|wm$ID =="400 11 1"|wm$ID =="404 18 2"|wm$ID =="405 19 2"|wm$ID =="407 17 2"|wm$ID =="407 18 1"|wm$ID =="410 6 4"|wm$ID =="417 17 5"|wm$ID =="420 16 2"|wm$ID =="421 6 2"|wm$ID =="422 18 2"|wm$ID =="425 1 2"|wm$ID =="430 8 2"|wm$ID =="431 12 1"|wm$ID =="434 14 2"|wm$ID =="434 14 2"|wm$ID =="436 15 6"|wm$ID =="437 5 2"|wm$ID =="438 3 2"|wm$ID =="442 7 2"|wm$ID =="442 12 2"|wm$ID =="442 12 2"|wm$ID =="442 18 1"|wm$ID =="446 19 1"|wm$ID =="447 13 1"|wm$ID =="447 13 1"|wm$ID =="449 12 1"|wm$ID =="449 12 1"|wm$ID =="450 1 1"|wm$ID =="452 8 3"|wm$ID =="452 8 3"|wm$ID =="456 17 2"|wm$ID =="457 1 1"|wm$ID =="460 10 1"|wm$ID =="461 10 1"|wm$ID =="463 19 1"|wm$ID =="465 1 2"|wm$ID =="467 3 1"|wm$ID =="467 14 2"|wm$ID =="467 17 3"|wm$ID =="468 12 2"|wm$ID =="468 12 2"|wm$ID =="469 12 2"|wm$ID =="472 12 2"|wm$ID =="472 14 2"|wm$ID =="473 11 2"|wm$ID =="476 1 4"|wm$ID =="476 11 1"|wm$ID =="477 18 2"|wm$ID =="478 4 2"|wm$ID =="479 12 2"|wm$ID =="481 5 4"|wm$ID =="482 8 2"] <- 1
wm$u5death[wm$ID =="484 3 2"|wm$ID =="484 11 1"|wm$ID =="487 2 1"|wm$ID =="500 3 2"|wm$ID =="501 19 2"|wm$ID =="504 10 2"|wm$ID =="506 4 2"|wm$ID =="508 10 4"|wm$ID =="510 1 2"|wm$ID =="510 8 2"] <- 1

table(wm$u5death)

#this is only to write code in excel
summary(wm$HH1)
library('xlsx')
write.xlsx(wm, "D:/Downloads/EDYS Capstone/MICS data set/Viet Nam_MICS5_Datasets/U5Childmor.xlsx")
write.xlsx(bh_childmor, "d:/Downloads/EDYS Capstone/MICS data set/Viet Nam_MICS5_Datasets/U5Childmor.xlsx")

#investigating available variables to choose in wm
#WB2 women age
#WB3 ever attended schools 1 yes 2 no => recode
#WB4 highest level of school attended 0 presch, 1 primary, 2 lower sec, 3 upper sec, 4 professional sch, 5 college/uni
#WB5 MAYBE USE highest grade at either 4 or 5
#WB7 literacy: needs recode 
#MT0, MT2, MT3, MT4: recode to create new variables: access to basic news
#MT6 MT7 MT9 MT10 MT11 a lot of missing data tho Access to the internet
#CM10 total number of birth
#######BH1 try to use this to create a variable of AGE AT FIRST BIRTH
#CP1 CP2 knowledge about contraception and actually using
#MA2 age of partner (maybe not really important) 98 is Don't know
#MA3 MA4 any other wives and how many that the husbands have
#MA1 and MA6 married/divored/separated => create new variable
#MA7 married more than once time?
#MA8 or MA9 first Married year to compare with year of first birth
#HA1 

#####################################
#4/9/2020
#work on hh1 dataset for household data
wm$hhID <- paste(wm$HH1, wm$HH2)
data_wm<-subset(wm, select = c(hhID, ID,u5death,WB2,WB3,WB4,WB5,WB7,MT0,MT2,MT3,MT4,MT6,MT7,MT9,MT10,MT11,
            CM10,CP1,MA2,MA3,MA1,MA6,MA7,MA8Y,HA1,HA2,HA3,HA4,HA5,
            HA6,HA7,HA8A,HA8B,HA8C))
trial1 <- subset(data, select=c(hhID,u5death))

hh$hhID <- paste(hh$HH1,hh$HH2)
trial2 <- subset(hh,select=c(hhID,HC1A))


library(dplyr)
trial3 <- left_join(trial1, trial2, by="hhID")

sum(is.na(trial3$hhID))
sum(duplicated(trial3$hhID))

rm(trial1, trial2, trial3)

#variables to choose from hh dataset
#HC1A religion
#HC1C ethnics
#HC2 number of room to sleep in
#HC3 HC4 HC5 material for house
#HC6 type of fuel
#HC8A electricty
#HC8J computer
#WS11 drinking water source 
#WS6 treat water to use
#WS7A WS7B WS7C WS7D WS7E WS7F WS7X WS7Z how they treat water 
#WS8 type of toilet
#WS9 toilet shared
#HW1 handwash place(can you see it)
#HW2 is there water there
#Hw3A Hw4 use any kind of soap
#HHSEX : Sex of the household heads
#ethnicity: 
#windex wscore : about wealth
#hhweight sample


#subset women interview year ID and WM6Y
wm_interview_date <- subset(wm, select = c(wm$ID,wm$WM6Y))
