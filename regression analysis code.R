### 4/24/2020
# Regression: Logistic model

#import packages
install.packages("DescTools") #for Pseudo R2
library(DescTools)
library(reshape2)
#include wmweight
weight <- subset(wm,select=c(ID,wmweight))
data_wm_hh <- left_join(data_wm_hh,weight,by = "ID")
table(wm$DV1D)
#regression
data_wm_hh$d_married <- factor(data_wm_hh$d_married)
data_wm_hh$mar <- factor(data_wm_hh$mar)

###this is between no school and some school level
m1 <- glm(u5death ~ d_noschool, data = data_wm_hh, family = "binomial")
summary(m1)
m1.1 <- glm(u5death ~ d_noschool+d_Kinh+d_rural, data = data_wm_hh, family = "binomial")
summary(m1.1)
m1.2 <- glm(u5death ~ d_noschool+d_Kinh+d_rural+region, data = data_wm_hh, family = "binomial")
summary(m1.2)
m1.3 <- glm(u5death ~ d_noschool+d_Kinh+d_rural+region+mar, data = data_wm_hh, family = "binomial")
summary(m1.3)
m1.4 <- glm(u5death ~ d_noschool+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score, data = data_wm_hh, family = "binomial")
summary(m1.4)
m1.5 <- glm(u5death ~ d_noschool+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+d_TV+d_radio, data = data_wm_hh, family = "binomial")
summary(m1.5)
m1.6 <- glm(u5death ~ d_noschool+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+d_TV+d_radio+age_birth1, data = data_wm_hh, family = "binomial")
summary(m1.6)


m3 <- glm(u5death ~ d_primary, data = data_wm_hh, family = "binomial")
summary(m3)
m3.1 <- glm(u5death ~ d_primary+d_Kinh+d_rural, data = data_wm_hh, family = "binomial")
summary(m3.1)
m3.2 <- glm(u5death ~ d_primary+d_Kinh+d_rural+region, data = data_wm_hh, family = "binomial")
summary(m3.2)
m3.3 <- glm(u5death ~ d_primary+d_Kinh+d_rural+region+mar, data = data_wm_hh, family = "binomial")
summary(m3.3)
m3.4 <- glm(u5death ~ d_primary+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score, data = data_wm_hh, family = "binomial")
summary(m3.4)
m3.5 <- glm(u5death ~ d_primary+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+d_TV+d_radio, data = data_wm_hh, family = "binomial")
summary(m3.5)
m3.6 <- glm(u5death ~ d_primary+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+d_TV+d_radio+age_birth1, data = data_wm_hh, family = "binomial")
summary(m3.6)


m4 <- glm(u5death ~ d_midschl, data = data_wm_hh, family = "binomial")
summary(m4)

data_wm_hh$new_weight <- data_wm_hh$tot_brth^(1/2)

###this is between attending high school and never attending high school
m5 <- glm(u5death ~ d_upper_midschl, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m5)
m5.1 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar, data = data_wm_hh, family = "binomial")
summary(m5.1)
m5.2 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m5.2)
m5.3 <- glm(u5death ~  d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m5.3)
m5.4 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m5.4)
m5.5 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+tot_brth, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m5.5)
m5.6 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial"(link="probit"))
summary(m5.6)
#m5.6 <- glm(u5death ~ d_upper_midschl+d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+d_TV+d_radio+age_birth1, data = data_wm_hh,weight=wmweight, family = "binomial")
#summary(m5.6)
#m5.7 <- glm(u5death ~ d_upper_midschl +d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+age_birth1+age_interview+d_flush_toilet+d_piped_water+wscore+d_TV+d_radio, data = data_wm_hh,weight=wmweight, family = binomial)
#summary(m5.7)
#m5.8 <- glm(u5death ~ d_upper_midschl +d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+age_birth1+age_interview+d_flush_toilet+d_piped_water+wscore+d_TV+d_radio, data = data_wm_hh, family = binomial)
#summary(m5.8)
coef5.6 <- margins(m5.6,type="link")
summary(coef5.6)
data_wm_hh$region <- as.factor(data_wm_hh$region)

install.packages("margins")
library(margins)
coef5.6 <- margins(m5.6, type="link")
summary(coef5.8)

PseudoR2(m5.6, which="all")
library(stargazer)
stargazer(m5, m5.1, m5.2, m5.3, m5.4, m5.5, m5.6, title="Table: Results for under five child mortality", align=TRUE,single.row= TRUE,column.sep.width = "20pt", type = "html",out="U5CM.html",
          column.labels = c("model 1","model 2","model 3","model 4","model 5","model 6","model 7","model 8"),
          dep.var.labels = c("Probability of Experiencing Under-5 Child mortality"),
          covariate.labels = c("Attended High school","Northern Midlands and Mountain area","North Central and Central Coastal area","Central Highlands","South East","Mekong River Delta","Rural", "Living with a man", "Used to married", "Used to live with a man", "Never married/lived with a man",
                               "Kinh", "Religious", "Wealth score",
                               "Piped water", "Flushed toilet",
                               "Read newspaper","Watch TV", "Listen to Radio", "Used/Have used Contraception", "HIV knowledge",
                               "Age at first birth","Total birth"))

cor(data_wm_hh$tot_brth,data_wm_hh$age_interview, use = "pairwise.complete.obs")


###This is to find the ROC and AUC ##failed
u5D <- data_wm_hh[data_wm_hh$u5death==1,]
nrow(u5D)
u5A <- data_wm_hh[data_wm_hh$u5death==0,]
deathTrain.yes <- u5D[sample(nrow(u5D),198),]
deathTrain.no <- u5A[sample(nrow(u5A),198),]
deathTrain <- rbind(deathTrain.yes,deathTrain.no)
deathVali.yes <- u5D[sample(nrow(u5D),198),]
deathVali.no <- u5A[sample(nrow(u5A),3353),]
deathVali <- rbind(deathVali.yes,deathVali.no)
install.packages("rpart")
library(rpart)
m5.8_Tree <- rpart(u5death ~ d_upper_midschl +d_Kinh+d_religion+d_rural+region+mar+d_CP+HIV_score+d_news+age_birth1+age_interview+d_flush_toilet+d_piped_water+wscore+d_TV+d_radio, data = data_wm_hh,weight=wmweight)
summary(m5.8_Tree)
install.packages("ROSE")
library(ROSE)
Log.predict <- predict(m5.8, newdata=deathVali)
Tree.predict <- predict(m5.8_Tree, newdata = deathVali)
roc.curve(deathVali$u5death,Log.predict)
roc.curve(deathVali$u5death,Tree.predict)
roc()
library(pROC)
library(randomForest)
roc(obese, glm.fit$fitted.values, plot=TRUE)

install.packages('sandwich')
library(sandwich)
bptest(m5.8)
cor(data_wm_hh$tot_brth,data_wm_hh$age_birth1, use="pairwise.complete.obs")
cor(data_wm_hh$age_interview,data_wm_hh$age_birth1, use="pairwise.complete.obs")

summary(data_wm_hh$tot_brth)
summary(data_wm_hh$age_birth1)

#by regions: Central Highlands
data_cen_hi <- subset(data_wm_hh, data_wm_hh$region == 4)
m5.6_cen_hi <- glm(u5death ~ d_upper_midschl+d_religion+d_rural+mar+d_Kinh+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_cen_hi,weight=wmweight, family = "binomial")
summary(m5.6_cen_hi)
data_southeast <- subset(data_wm_hh, data_wm_hh$region == 6)
m5.6_SE <- glm(u5death ~ d_upper_midschl+d_religion+d_rural+mar+d_Kinh+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_southeast,weight=wmweight, family = "binomial")
summary(m5.6_SE)
data_rural <- subset(data_wm_hh, data_wm_hh$d_rural == 1)
m5.6_rural <- glm(u5death ~ d_upper_midschl+region+d_religion+mar+d_Kinh+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_rural,weight=wmweight, family = "binomial")
summary(m5.6_rural)
data_urban <- subset(data_wm_hh, data_wm_hh$d_rural == 0)
m5.6_urban <- glm(u5death ~ d_upper_midschl+region+d_religion+mar+d_Kinh+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_urban,weight=wmweight, family = "binomial")
summary(m5.6_urban)

plot(m5.6,2)
library(DescTools)
PseudoR2(m5,which="McFaddenAdj")
PseudoR2(m5.1,which="McFaddenAdj")
PseudoR2(m5.2,which="McFaddenAdj")
PseudoR2(m5.3,which="McFaddenAdj")
PseudoR2(m5.4,which="McFaddenAdj")
PseudoR2(m5.5,which="McFaddenAdj")
PseudoR2(m5.6,which="McFaddenAdj")

data_lwm <- subset(data_wm_hh)
data_lwm$mar2 <- 0
data_lwm$mar2[data_lwm$mar == 2] <- 1




##control for endogeneity
# naive probit: biased regression 
r1 <- glm(y2~x1+y1,binomial(link="probit"))
dat1 <- data.frame(cbind(mean(x1),seq(ceiling(min(y1)),floor(max(y1)),0.2)))
names(dat1) <- c("x1","y1")
asf1 <- cbind(dat1$y1,pnorm(predict(r1,dat1)))



# 2 step control function approach
v1 <- (residuals(lm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_2SCF,weight=wmweight)))/ sd(residuals(lm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data = data_2SCF,weight=wmweight)))
describe(data_wm_hh)                                                                                                                                                                               
r2 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth+v1, data =data_2SCF,weight=wmweight,family="binomial"(link="probit"))
summary(r2)
data_2SCF$v1 <- v1
# proceedure to get asf
asf2 <- cbind(seq(ceiling(min(y1)),floor(max(y1)),0.2),NA)
for(i in 1:dim(asf2)[1]){
  dat2 <- data.frame(cbind(mean(x1),asf2[i,1],v1))
  names(dat2) <- c("x1","y1","v1")
  asf2[i,2] <- mean(pnorm(predict(r2,dat2)))
}
library('psych')

data_2SCF <- subset(data_wm_hh,is.na(data_wm_hh$d_CP) == FALSE)

b <- biprobit(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth, data=data_wm_hh)
### ### ###
install.packages('GJRM')
install.packages('polycor')
install.packages('Rtools')
library(GJRM)
library(polycor)
install.packages("jsonlite", type = "source")
install.packages("installr")
library(installr)
updateR()

help.search("export")
install.packages("xlsx")
library(xlsx)
library(foreign)
write.xlsx(data_wm_hh, "D:/Downloads/EDYS Capstone/MICS data set/data.xlsx")

m5.6_lwm <- glm(mar2 ~ d_upper_midschl+region+d_rural+d_religion+d_Kinh+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+tot_brth+u5death, data = data_lwm,weight=wmweight, family = "binomial")
summary(m5.6_lwm )
PseudoR2(m5.6_lwm)

bpm <- gjrm(list(u5death ~ d_upper_midschl+region+d_rural+d_religion+d_Kinh+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+mar2,
                 mar2 ~ d_upper_midschl+region+d_rural+d_religion+d_Kinh+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+u5death),
            data =data_lwm, 
            margins=c("probit","probit"), 
            Model ="B")
summary(bpm)
AIC(bpm)

cor(data_wm_hh$tot_brth,data_wm_hh$wscore)
install.packages("dplyr")
library(dplyr)

grade_lv <- subset(wm, select = c(ID,WB4))
data_wm_hh <- left_join(data_wm_hh,grade_lv,by = "ID")
data_wm_hh$grade_level_attended
table(wm$WB3)

grade_NA <- subset(wm, is.na(wm$WB5) == TRUE)
summary(grade_NA$WB4)
grade_lv_NA <- subset(wm, is.na(wm$WB4) == TRUE)

data_wm_hh$completed_gr_lv <- 0
data_wm_hh$completed_gr_lv[data_wm_hh$WB4 == 4|data_wm_hh$WB4 == 5] <- 12
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 12] <- 12  
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 11] <- 11 
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 10] <- 10
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 9] <- 9
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 8] <- 8
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 7] <- 7
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 6] <- 6
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 5] <- 5
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 4] <- 4
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 3] <- 3
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 2] <- 2
data_wm_hh$completed_gr_lv[data_wm_hh$WB5 == 1] <- 1

table(data_wm_hh$completed_gr_lv)

m6.6 <- glm(u5death ~ completed_gr_lv+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial"(link="probit"))
summary(m6.6)
m5.6 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh, family = "binomial"(link="probit"))
summary(m5.6)
PseudoR2(m5.6)
traceback()
options(error = recover)
options(error = NULL, wpR2(object, ...)arn = 0)

debug(glm)
install.packages("tryCatchLog")

#### calculate Pseudo Rsquare
blr_rsq_mcfadden_adj(m5.6)
blr_rsq_mcfadden_adj(m5.6)
blr_rsq_mcfadden_adj(m5.6)

m5 <- glm(u5death ~ d_primary, data = data_wm_hh,weight=new_weight, family = "binomial")
summary(m5)
m5.1 <- glm(u5death ~ d_midschl+region+d_rural+mar,weight=new_weight, data = data_wm_hh, family = "binomial")
summary(m5.1)
m5.2 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore, data = data_wm_hh,weight=new_weight, family = "binomial")
summary(m5.2)
m5.3 <- glm(u5death ~  d_upper_no_school+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water, data = data_wm_hh,weight=new_weight, family = "binomial")
summary(m5.3)
m5.4 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score, data = data_wm_hh,weight=new_weight, family = "binomial")
summary(m5.4)
m5.5 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=new_weight, family = "binomial")
summary(m5.5)

m6 <- glm(u5death ~ completed_gr_lv, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m6)
m6.1 <- glm(u5death ~ completed_gr_lv+region+d_rural+mar,weight=wmweight, data = data_wm_hh, family = "binomial")
summary(m6.1)
m6.2 <- glm(u5death ~ completed_gr_lv+region+d_rural+mar+d_Kinh+d_religion+wscore, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m6.2)
m6.3 <- glm(u5death ~  completed_gr_lv+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m6.3)
m6.4 <- glm(u5death ~ completed_gr_lv+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m6.4)
m6.5 <- glm(u5death ~ completed_gr_lv+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview+high_birth, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m6.5)
data_wm_hh$high_birth <- 0
data_wm_hh$high_birth[data_wm_hh$tot_brth > 2] <- 1

m7.5 <- glm(u5death ~ d_kgarten+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m7.5)
m8.5 <- glm(u5death ~ d_primary+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m8.5)
m9.5 <- glm(u5death ~ d_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m9.5)
m10.5 <- glm(u5death ~ d_upper_midschl+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m10.5)
m11.5 <- glm(u5death ~ d_noschool+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m11.5)
data_wm_hh$d_higher_ed <- 0
data_wm_hh$d_higher_ed[data_wm_hh$d_tert == 1|data_wm_hh$d_vocational == 1] <- 1
m12.5 <- glm(u5death ~ d_higher_ed+region+d_rural+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_wm_hh,weight=wmweight, family = "binomial")
summary(m12.5)
stargazer(m6, m6.1, m6.2, m6.3, m6.4, m6.5, title="Table: Results for under five child mortality", align=TRUE,single.row= TRUE,column.sep.width = "20pt", type = "html",out="U5CM_updated.doc",
          column.labels = c("model 1","model 2","model 3","model 4","model 5","model 6","model 7","model 8"),
          dep.var.labels = c("Probability of Experiencing Under-5 Child mortality"),
          covariate.labels = c("Highest general education grades completed","Northern Midlands and Mountain area","North Central and Central Coastal area","Central Highlands","South East","Mekong River Delta","Rural", "Living with a man", "Used to married", "Used to live with a man", "Never married/lived with a man",
                               "Kinh", "Religious", "Wealth score",
                               "Piped water", "Flushed toilet",
                               "Read newspaper","Watch TV", "Listen to Radio", "Used/Have used Contraception", "HIV knowledge",
                               "Age at first birth","Age at interview time"))

stargazer(m11.5, m7.5, m8.5, m9.5, 10.5, m12.5, title="Table: Results for under five child mortality", align=TRUE,single.row= TRUE, type = "html",out="U5CM_updated02.html"),
          column.labels = c("model 1","model 2","model 3","model 4","model 5","model 6","model 7","model 8"),
          #dep.var.labels = c("Probability of Experiencing Under-5 Child mortality"),
          #covariate.labels = c("No school","Attended Preschools","Attended Primary school","Attended Middle school","Attended High school","Attended Higher education",
					"Northern Midlands and Mountain area","North Central and Central Coastal area","Central Highlands","South East","Mekong River Delta","Rural", "Living with a man", "Used to married", "Used to live with a man", "Never married/lived with a man",
                               "Kinh", "Religious", "Wealth score",
                               "Piped water", "Flushed toilet",
                               "Read newspaper","Watch TV", "Listen to Radio", "Used/Have used Contraception", "HIV knowledge",
                               "Age at first birth","Age at interview time"))

install.packages("margins")
library(margins)
summary(margins(m6.5_rural))


data_rural <- subset(data_wm_hh, data_wm_hh$d_rural== 1)
data_urban <- subset(data_wm_hh, data_wm_hh$d_rural == 0)
m6.5_rural <- glm(u5death ~ completed_gr_lv+region+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_rural,weight=wmweight, family = "binomial")
summary(m6.5_rural)
urban <- glm(u5death ~ completed_gr_lv+region+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview,,weight=wmweight, data = data_urban, family = "binomial")
summary(urban)
stargazer(m6.5_rural,m6.5_urban, title="Table: Results for under five child mortality", align=TRUE,single.row= TRUE,column.sep.width = "20pt", type = "html",out="U5CM_updated03.doc",
          column.labels = c("Rural","Urban"),
          dep.var.labels = c("Probability of Experiencing Under-5 Child mortality"),
          covariate.labels = c("Highest general education grades completed","Northern Midlands and Mountain area","North Central and Central Coastal area","Central Highlands","South East","Mekong River Delta", "Living with a man", "Used to married", "Used to live with a man", "Never married/lived with a man",
                               "Kinh", "Religious", "Wealth score",
                               "Piped water", "Flushed toilet",
                               "Read newspaper","Watch TV", "Listen to Radio", "Used/Have used Contraception", "HIV knowledge",
                               "Age at first birth","Age at interview time"))

urban <- glm(u5death ~ completed_gr_lv+region+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview,,weight=wmweight, data = data_urban, family = "binomial")
summary(urban)

data_urban$mar5 <- 0
data_urban$mar5[data_urban$mar == 5] <- 1
data_urban$mar4 <- 0
data_urban$mar4[data_urban$mar == 4] <- 1

table(data_urban$mar5,data_urban$u5death)
table(data_urban$mar4,data_urban$u5death)

urb_test <- subset(data_urban, data_urban$mar == 1|data_urban$mar == 2|data_urban$mar == 3)
urban <- glm(u5death ~ completed_gr_lv+region+mar+d_Kinh+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview,,weight=wmweight, data = urb_test, family = "binomial")
summary(urban)
margins(m6.5_rural)
margins(urban)


data_Kinh <- subset(data_wm_hh, data_wm_hh$d_Kinh== 1)
data_nonKinh <- subset(data_wm_hh, data_wm_hh$d_Kinh == 0)
m6.5_Kinh <- glm(u5death ~ complesummaryted_gr_lv+region+d_rural+mar+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview, data = data_Kinh,weight=wmweight, family = "binomial")
summary(m6.5_Kinh)
m6.5non_Kinh <- glm(u5death ~ completed_gr_lv+region+d_rural+mar+d_religion+wscore+d_flush_toilet+d_piped_water+d_news+d_TV+d_radio+d_CP+HIV_score+age_birth1+age_interview,weight=wmweight, data = data_nonKinh, family = "binomial")
summary(m6.5_nonKinh)

data_nonKinh$re1 <- 0
data_nonKinh$re1[data_nonKinh$region == 1] <- 1
data_nonKinh$re2 <- 0
data_nonKinh$re2[data_nonKinh$region == 2] <- 1
data_nonKinh$re3 <- 0
data_nonKinh$re3[data_nonKinh$region == 3] <- 1
data_nonKinh$re4 <- 0
data_nonKinh$re4[data_nonKinh$region == 4] <- 1
data_nonKinh$re5 <- 0
data_nonKinh$re5[data_nonKinh$region == 5] <- 1
data_nonKinh$re6 <- 0
data_nonKinh$re6[data_nonKinh$region == 1] <- 1
table(data_nonKinh$re1,data_nonKinh$u5death)
table(data_nonKinh$re2,data_nonKinh$u5death)
table(data_nonKinh$re3,data_nonKinh$u5death)
table(data_nonKinh$re4,data_nonKinh$u5death)
table(data_nonKinh$re5,data_nonKinh$u5death)
table(data_nonKinh$re6,data_nonKinh$u5death)


###STATA
gen region1 = 0
replace region1 = 1 if region == 1
gen region2 = 0
replace region2 = 1 if region == 2
gen region3= 0
replace region3 = 1 if region == 3
gen region4 = 0
replace region5 = 1 if region == 4
gen region5 = 0
replace region5 = 1 if region == 5
gen region6 = 0
replace region6 = 1 if region == 6

gen mar1 = 0
replace mar1 = 1 if mar == 1
gen mar2 = 0
replace mar2 = 1 if mar == 2
gen mar3= 0
replace mar3 = 1 if mar == 3
gen mar4 = 0
replace mar4= 1 if mar == 4
gen mar5 = 0
replace mar5 = 1 if mar == 5
gen mar6 = 0
replace mar6 = 1 if mar == 6

table(data_wm_hh$mar)


summary(margins(m11.5))
summary(margins(m7.5))
summary(margins(m8.5))
summary(margins(m9.5))
summary(margins(m10.5))
summary(margins(m12.5))