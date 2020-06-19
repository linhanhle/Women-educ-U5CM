###Descriptive stats

install.packages("RColorBrewer")
install.packages("scales")
install.packages("ggpubr")
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(ggpubr)
library(plyr)
theme_set(theme_pubr())
display.brewer.all() #this will show every color pallette
#this research uses Pastel2
#pie chart for women who experienced child death: Graph 01
table(data_wm_hh$u5death)
u5deathrate <- data.frame(
  group = c("never experience child death", "experienced child death"),
  value = round(c(6705/7101*100,396/7101*100),2)
)
head(u5deathrate)
bp<- ggplot(u5deathrate, aes(x="",y = value, fill=group))+geom_bar(width = 1,stat = "identity")+
  coord_polar("y") + scale_fill_brewer(palette="Pastel2")+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),label = percent(value/100)), size=4, color = "black") +
  theme_minimal()+ggtitle("Graph: Proportion of women ever experienced child mortality")
bp

### 5/2: ballon plot
#graph2: 
data_U5M_1 <- subset(data_wm_hh, u5death == 1)
test <- data.frame(table(data_U5M_1$region))
test0 <- data.frame(table(subset(data_wm_hh, age_birth1 <= 18)$region))
test1 <- data.frame(table(subset(data_wm_hh, d_lit == 0)$region))
test2 <- data.frame(table(subset(data_wm_hh,d_primary == 1)$region))
test3 <- data.frame(table(subset(data_wm_hh,d_upper_midschl == 1)$region))
test4 <- data.frame(table(subset(data_wm_hh,d_tert == 1 | d_vocational == 1)$region))
test5 <- data.frame(table(data_wm_hh$region))

test <- rename(test, U5_Child_mortality = Freq)
test0 <- rename(test0, First_birth_under_18 = Freq)
test1 <- rename(test1, Illiterate  = Freq)
test2 <- rename(test2, Ever_attended_Primary_School = Freq)
test3 <- rename(test3, Ever_attended_High_School = Freq)
test4 <- rename(test4, Ever_attended_university_or_vocational = Freq)
test5 <- rename(test5, Number_of_women = Freq)

test <- left_join(test5, test, by= "Var1")
test <- left_join(test, test0, by="Var1")
test <- left_join(test, test1, by="Var1")
test <- left_join(test, test2, by="Var1")
test <- left_join(test, test3, by="Var1")
test <- left_join(test, test4, by="Var1")
test <- rename(test, region = Var1)
test$region <- as.character(test$region)
test$region[test$region == "1"] <- "Red River Delta"
test$region[test$region == "2"] <- "Northern Midlands and Mountain area"
test$region[test$region == "3"] <- "North Central and Central Coastal area"
test$region[test$region == "4"] <- "Central Highlands"
test$region[test$region == "5"] <- "South East"
test$region[test$region == "6"] <- "Mekong River Delta"
test$region <- as.factor(test$region)

# keep the first column
n <-  test[,1]
# Transpose everything other than the first column
ctest <- as.data.frame(t(test[,-1]))
# Assign first column as the column names of the transposed dataframe
colnames(ctest) <- n

ggballoonplot(ctest, fill = "value")+
  scale_fill_viridis_c(option = "C")+
  ggtitle("Graph: Frequency of women experiencing child mortality and 
          their levels of education attainment by regions")

#ballon graph: rural vs urban, Kinh, religious. U5death, total number of wm

#piechart for ethnicity
table(wm_ethnic$HC1C)
ethnic <- subset(hh, select = c(hhID, HC1C))
wm_ethnic <- left_join(data_wm_hh,ethnic, by="hhID")
wm_ethnic <- data.frame(table(wm_ethnic$HC1C))
names(wm_ethnic)[names(wm_ethnic) == "Var1"] <- "Ethnicity"
names(wm_ethnic)[names(wm_ethnic) == "Freq"] <- "Counts"
wm_ethnic$Ethnicity <- as.character(wm_ethnic$Ethnicity)
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="1"] <- "Kinh"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="2"] <- "Tay"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="3"] <- "Thai"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="4"] <- "Muong"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="5"] <- "Khmer"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="6"] <- "Chinese"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="7"] <- "Nung"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="8"] <- "Mong"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="9"] <- "Gia Rai"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="10"] <- "Ê dê"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="11"] <- "Ba na"
wm_ethnic$Ethnicity[wm_ethnic$Ethnicity=="96"] <- "Other ethnic group"
table(wm_ethnic$Ethnicity)


wm_ethnic <- subset(wm_ethnic,select = c(Freq))
wm_ethnic <- wm_ethnic[order(-wm_ethnic$Counts),]
wm_ethnic$proportion <- wm_ethnic$Counts/7101*100

bp <- ggplot(wm_ethnic, aes(x="",y = Counts, fill=Ethnicity))+geom_bar(width = 1,stat = "identity")+theme_minimal()+coord_polar("y", start = 0)+
  ggtitle("Graph: Distribution of Women by ethnic groups") + scale_fill_brewer(palette="Set3")
bp
display.brewer.all()

#histogram: total number of child death: Graph 04
ggplot(data_wm_hh, aes(x=tot_brth)) + geom_histogram(binwidth = 1, color = "darkblue", fill = "lightblue") +
  geom_vline(aes(xintercept=mean(tot_brth)),
               color="black", linetype="dashed", size=0.8) +
  ggtitle("Graph: Distribution of total birth")

wm_survey <- data.frame(table(wm_original$WM7))
bp <- ggplot(wm_survey, aes(x="",y = Freq, fill=Var1))+geom_bar(width = 1,stat = "identity")+theme_minimal()+coord_polar("y", start = 0)+
  ggtitle("Graph: Distribution of Women by ethnic groups") + scale_fill_brewer(palette="Set3")

table(data_wm_hh$tot_brth)

ethnic_orig <- data.frame(ethnic_group: )