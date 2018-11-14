library(dplyr)
library(stringr)
library(tidyverse)
library(DMwR)

data <- read.csv("domestic.csv")

#-------------------------------------------------------------------#
### Fix the  missing USStateregion variable in 2009 dataset
ref1 <- data %>% select(HomeState,USStateRegion)
ref1 <- unique(ref1)
ref1 <- na.omit(ref1)
ref1 <- ref1 %>% distinct(HomeState,.keep_all = TRUE)
data2009 <- data[which(data$DateFrom==2009),]
homest2009 <- data2009 %>% select(HomeState)
homest2009 <- left_join(homest2009,ref1,by="HomeState")
check <- homest2009[which(is.na(homest2009$USStateRegion)==TRUE),]
check <- c(as.character(check$HomeState[which(!is.na(check$HomeState))]))
homest2009$USStateRegion[which(homest2009$HomeState %in% check)] <- "International"
data2009$USStateRegion <-homest2009$USStateRegion
# NA is international in USSTATEREGION

#-------------------------------------------------------------------#
### Fix Race
## Fix Race in 2009
data_else <- subset(data,data$DateFrom>=2012)
#Finding 1: For any student with a hispanic origin (HS/PR/MA), his race is definded as "Hispanic"; Vice
#Versa, for student that has more than one race but has no hispanic origin, his race is defined as Multi Race.
disc1 <- data_else %>% select(FullRace,Race) %>% unique() 
index <- grep("HS|PR|MA",disc1$FullRace)
disc1 <- disc1[index,]

data_else$Race <- recode_factor(data_else$Race,"1"="American Indian or Alaska Native","2"="Asian","3"="Black or African American",
                           "5"="White","6"="Hispanic/Latino","7"="Multi Racial","8"="Unknown","9"="Non-Resident Alien",
                           "WH"="White","UK"="Unknown","HS"="Hispanic/Latino","AS"="Asian","BL"="Black or African American",
                           "AI"="American Indian or Alaska Native","PI"="Native Hawaiian or Other Pacific Islander",
                           "MR"="Multi Racial", "NC"="Non-Resident Alien","Hispanic/Latino, White"="Multi Racial",
                           "Asian, White"="Multi Racial","Black or African American, White"="Multi Racial",
                           "Native Hawaiian or Other Pacific Islander, White"="Multi Racial",
                           "American Indian or Alaska Native, Hispanic/Latino, White"="Hispanic/Latino",
                           "American Indian or Alaska Native, Black or African American"="Multi Racial",
                           "American Indian or Alaska Native, White"="Multi Racial",
                           "Asian, Native Hawaiian or Other Pacific Islander, White"="Multi Racial",
                           "American Indian or Alaska Native, Asian, White"="Multi Racial",
                           "Asian, Black or African American"="Multi Racial",
                           "American Indian or Alaska Native, Black or African American, White"="Multi Racial",
                           "Asian, Hispanic/Latino"="Hispanic/Latino",
                           "Asian, Hispanic/Latino, White"="Hispanic/Latino",
                           "Black or African American, Hispanic/Latino, White"="Hispanic/Latino",
                           "Black or African American, Hispanic/Latino"="Hispanic/Latino",
                           "American Indian or Alaska Native, Hispanic/Latino"="Hispanic/Latino",
                           "American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, White"="Multi Racial"
                           )
data_else$Race[is.na(data_else$Race)]<-"Unknown"
ref2 <- data_else %>% select(OneRace,Race)
ref2 <- unique(ref2)
ref2$Race[which(ref2$OneRace=="Non-Resident Alien")] <- "Non-Resident Alien"
ref2 <- ref2[-12,]
ref2 <- unique(ref2)
race2009 <- data2009 %>% select(OneRace)
race2009 <- left_join(race2009,ref2,by="OneRace")
unique(race2009)
race2009$Race[which(race2009$OneRace=="PI")]<-"Native Hawaiian or Other Pacific Islander"
data2009$Race <- race2009$Race

## Fix Race of 2011
data2011 <- data[which(data$DateFrom==2011),]
race2011 <- data2011 %>% select(OneRace)
race2011 <- left_join(race2011,ref2,by="OneRace")
unique(race2011)
race2011$Race[which(race2011$OneRace=="PI")]<-"Native Hawaiian or Other Pacific Islander"
data2011$Race <- race2011$Race

## Fix Race of 2010.
data2010 <- data[which(data$DateFrom==2010),]
#Finding 2: If a student is International and VisaType is NA, then he is a Non-Resident Alien
disc2 <- data_else %>% select(Race,InternationalFlag,Visa.Type) %>% 
  filter(Race=="Non-Resident Alien") %>% unique()
ref4 <- data_else %>% select(FullRace,Race) %>% unique()
ref4 <- ref4[!duplicated(ref4$FullRace),]
ref4$Race[which(ref4$FullRace=="AS")] <- "Asian"
ref4$Race[which(ref4$FullRace=="Non-Resident Alien")] <-"Non-Resident Alien"
ref4 <- unique(ref4)
race2010 <- data2010 %>% select(FullRace)
race2010 <- left_join(race2010,ref4,by="FullRace")
unique(race2010)
race2010$Race[which(race2010$FullRace=="PI")] <- "Native Hawaiian or Other Pacific Islander"
race2010$Race[which(race2010$FullRace %in% c("AIBLPR","BLMA","PR"))] <- "Hispanic/Latino"
data2010$Race <- race2010$Race
data2010$Race[which(data2010$InternationalFlag=="international" & is.na(data2010$Visa.Type))] <- "Non-Resident Alien"

## Combining the data
data2 <- rbind(data2009,data2010,data2011,data_else)

## Fixing USStateRegion NAs --> International
#Finding3: All USstateregions that are coded as NAs are infact international.
disc3 <- data2 %>% select(USStateRegion,NationDesc) %>% filter(is.na(USStateRegion)) %>% unique()
data2$USStateRegion[is.na(data2$USStateRegion)] <- "International"
unique(data2$USStateRegion)

#-------------------------------------------------------------------#
### Fix Division
UDidx <- grep("^00",data2$Major)
data2$Division[UDidx] <- "00"

# Fix GPA
data2$GPA <- round(((data2$OriginalGPA/data2$GPAScale)*4),2)
rm(data,data_else,data2009,data2010,data2011,data2_0910,data2_11,data2_1215,homest2009,race2009,race2010,race2011,ref1,ref2,ref4)

#-------------------------------------------------------------------#
### Cleaning the data
data2 <- data2 %>% select(retained, 
                          DateFrom,
                        USStateRegion,
                        Race,
                        Gender,
                        FirstGen,
                        AlumniConnection,
                        Question,
                        Division,
                        ApplicationType	,
                        Housing	,
                        SpecialConsideration	,
                        DecisionType	,
                        ConfirmDate 	,
                        AcadRS	,
                        RankPercent,
                        ClassSize	,
                        GPA,
                        OriginalGPA	,
                        GPAScale,
                        GPAOrig,
                        ACTChoice	,
                        ACTBest	,
                        ACTComposite	,
                        ACTEng	,
                        ACTMath	,
                        ACTRdng	,
                        ACTSci	,
                        ACTWRSC	,
                        SATVerbal	,
                        SATMath	,
                        SATWRSC	,
                        ON	,
                        EER,
                        DisciplinaryQuestion1
                        )

#We would want to recreate RankPercent for the 2016 data
data2$ACTChoice <- as.character(data2$ACTChoice)
data2$ACTChoice <- factor(data2$ACTChoice,levels=c("1","S","4","3","2","6","5"))

### Transforming EER into scores
data_else <- data2[which(!data2$DateFrom==2015),]
for (i in 1:5) {
  assign("EERSCORE",data.frame(toupper(substr(data_else$EER,i,i))))
  colnames(EERSCORE) <- paste("EERScore",i,sep="_")
  data_else <- cbind(data_else,EERSCORE)
}
data_else$EERCount <- 5-str_count(data_else$EER,"-")
data_else$EERScore_1[is.na(data_else$EERScore_1)] <-"-"
data_else$EERScore_2[is.na(data_else$EERScore_2)] <-"-"
data_else$EERScore_3[is.na(data_else$EERScore_3)] <-"-"
data_else$EERScore_4[is.na(data_else$EERScore_4)] <-"-"
data_else$EERScore_5[is.na(data_else$EERScore_5)] <-"-"

data2015 <- data2[which(data2$DateFrom==2015),]
data2015$EER <- gsub(" ","",data2015$EER)
data2015$EER <- gsub(",","",data2015$EER)
for (i in 1:5) {
  assign("EERSCORE",data.frame(toupper(substr(data2015$EER,i,i))))
  colnames(EERSCORE) <- paste("EERScore",i,sep="_")
  data2015<- cbind(data2015,EERSCORE)
}
data2015$EERCount <- nchar(as.character(data2015$EER))
data2015$EERScore_1 <- as.character(data2015$EERScore_1)
data2015$EERScore_2 <- as.character(data2015$EERScore_2)
data2015$EERScore_3 <- as.character(data2015$EERScore_3)
data2015$EERScore_4 <- as.character(data2015$EERScore_4)
data2015$EERScore_5 <- as.character(data2015$EERScore_5)
data2015$EERScore_1[is.na(data2015$EERScore_1)] <-"-"
data2015$EERScore_2[is.na(data2015$EERScore_2)] <-"-"
data2015$EERScore_2[which(data2015$EERScore_2=="")] <-"-"
data2015$EERScore_3[is.na(data2015$EERScore_3)] <-"-"
data2015$EERScore_3[which(data2015$EERScore_3=="")] <-"-"
data2015$EERScore_4[is.na(data2015$EERScore_4)] <-"-"
data2015$EERScore_4[which(data2015$EERScore_4=="")] <-"-"
data2015$EERScore_5[is.na(data2015$EERScore_5)] <-"-"
data2015$EERScore_5[which(data2015$EERScore_5=="")] <-"-"

# Fixing Question
data2015$Question <- data2015$DisciplinaryQuestion1
# Fix GPA
data2015$GPA <- round(((data2015$GPAOrig/data2015$GPAScale)*4),2)

# Bring them together
data2 <- rbind(data_else,data2015)
data2$EERScore_3[which(data2$EERScore_3=="_")] <-"-"
data2$EERScore_5[which(data2$EERScore_5=="_")] <-"-"
data2$EERScore_1 <- as.factor(data2$EERScore_1)
data2$EERScore_2 <- as.factor(data2$EERScore_2)
data2$EERScore_3 <- as.factor(data2$EERScore_3)
data2$EERScore_4 <- as.factor(data2$EERScore_4)
data2$EERScore_5 <- as.factor(data2$EERScore_5)
data2$EERCount[is.na(data2$EERCount)] <- 0
#Collaspe Factors EERScore
data2$EERScore_1 <- recode_factor(data2$EERScore_1,"0"="Oth","6"="Oth","8"="Oth",
                                  "9"="Oth","B"="Oth","A"="Oth","C"="Oth","D"="Oth",
                                  "E"="Oth","F"="Oth","I"="Oth","S"="Oth","S"="Oth",
                                  "T"="Oth","U"="Oth","7"="Oth"
                                  )
data2$EERScore_2 <- recode_factor(data2$EERScore_2,"2"="Oth","3"="Oth","4"="Oth",
                                  "5"="Oth","G"="Oth","K"="Oth","M"="Oth","O"="Oth",
                                  "X"="Oth"
                                  )
data2$EERScore_3 <- recode_factor(data2$EERScore_3,"0"="Oth","1"="Oth","2"="Oth",
                                  "4"="Oth","8"="Oth","A"="Oth","B"="Oth","R"="Oth",
                                  "X"="Oth","N"="Oth","P"="Oth"
                                   )
data2$EERScore_4 <- recode_factor(data2$EERScore_4,"0"="Oth","1"="Oth","8"="Oth",
                                  "B"="Oth","A"="Oth","C"="Oth","D"="Oth",
                                  "P"="Oth","N"="Oth"
                                   )
data2$EERScore_5 <- recode_factor(data2$EERScore_5,"0"="R","A"="R","D"="R",
                                  "F"="R","H"="R","P"="R"
                                   )

## Fixing the levels of EERScores
data2$EERScore_3 <- as.factor(as.character(data2$EERScore_3))
data2$EERScore_5 <- as.factor(as.character(data2$EERScore_5))

### Recode the dates--->Months
data2$Month <-as.integer(substr(as.character(data2$ConfirmDate),6,6))
data2 <- data2 %>% select(-c(ConfirmDate,EER,DisciplinaryQuestion1,GPAOrig,OriginalGPA,GPAScale))
data2$retained <- as.factor(data2$retained)

#-------------------------------------------------------------------------------------#
### Imputation
### Imputation----Categorical
source("data summary.r")
data.summary(data2)

data2 <- data2[-which(is.na(data2$retained)),]
data2$FirstGen <- as.character(data2$FirstGen)
data2$FirstGen[is.na(data2$FirstGen)] <- "N"
data2$FirstGen <- as.factor(data2$FirstGen)

data2$AlumniConnection <- as.character(data2$AlumniConnection)
data2$AlumniConnection[is.na(data2$AlumniConnection)] <- "NP"
data2$AlumniConnection<- as.factor(data2$AlumniConnection)

data2$Question <- as.character(data2$Question)
data2$Question[is.na(data2$Question)] <- "NP"
data2$Question<- as.factor(data2$Question)

data2$Housing <- as.character(data2$Housing )
data2$Housing [is.na(data2$Housing )] <- "NO"
data2$Housing <- as.factor(data2$Housing)

data2$SpecialConsideration <- as.character(data2$SpecialConsideration)
data2$SpecialConsideration [is.na(data2$SpecialConsideration)] <- "NP"
data2$SpecialConsideration <- as.factor(data2$SpecialConsideration)

data2$DecisionType<- as.character(data2$DecisionType)
data2$DecisionType[is.na(data2$DecisionType)] <- "RA" #Regular Admission
data2$DecisionType<- as.factor(data2$DecisionType)

data2$ACTChoice<- as.character(data2$ACTChoice)
data2$ACTChoice[is.na(data2$ACTChoice)] <- "NP"
data2$ACTChoice<- as.factor(data2$ACTChoice)

## Imputations Numeric----Misising Value Flags
Flags <- data.frame(index=seq(1,24618,1))
Flags$M_AcadRS <- as.factor(ifelse(is.na(data2$AcadRS),1,0))
Flags$M_RankPercent<- as.factor(ifelse(is.na(data2$RankPercent),1,0))
Flags$M_ClassSize <- as.factor(ifelse(is.na(data2$ClassSize),1,0))
Flags$M_GPA <- as.factor(ifelse(is.na(data2$GPA),1,0))
Flags$M_ACTComposite <- as.factor(ifelse(is.na(data2$ACTComposite),1,0))
Flags$M_ACTEng <- as.factor(ifelse(is.na(data2$ACTEng),1,0))
Flags$M_ACTMath <- as.factor(ifelse(is.na(data2$ACTMath),1,0))
Flags$M_ACTRdng <- as.factor(ifelse(is.na(data2$ACTRdng),1,0))
Flags$M_ACTSci <- as.factor(ifelse(is.na(data2$ACTSci),1,0))
Flags$M_ACTWRSC <- as.factor(ifelse(is.na(data2$ACTWRSC ),1,0))
Flags$M_SATVerbal <- as.factor(ifelse(is.na(data2$SATVerbal ),1,0))
Flags$M_SATMath <- as.factor(ifelse(is.na(data2$SATMath),1,0))
Flags$M_SATWRSC <- as.factor(ifelse(is.na(data2$SATWRSC ),1,0))
Flags$M_ON <- as.factor(ifelse(is.na(data2$ON),1,0))
Flags$M_Month <- as.factor(ifelse(is.na(data2$Month),1,0))
Flags <- Flags[,-1]

#Variables that has the same pattern of missing data is considered redundant
identical(Flags$M_ACTEng,Flags$M_ACTMath) #Redundant
identical(Flags$M_ACTEng,Flags$M_ACTRdng) #Redundant
identical(Flags$M_ACTEng,Flags$M_ACTSci)  # Redundant
identical(Flags$M_ACTEng,Flags$M_ACTWRSC) #Not Redundant
identical(Flags$M_ACTComposite,Flags$M_ACTEng) #Redundant
identical(Flags$M_SATVerbal,Flags$M_SATMATH) #Not Redundant
identical(Flags$M_SATVerbal,Flags$M_SATWRSC) #Not Redundant
identical(Flags$M_SATMath,Flags$M_SATWRSC)   # Not Redundant
#Should Eliminate ACTComposite/ActMath/ACTRdng/ACTSci,keep ACTEng
data2 <- data2 %>% select(-c(ACTComposite,ACTRdng,ACTMath,ACTSci))
Flags <- Flags %>% select(-c(M_ACTComposite,M_ACTRdng,M_ACTMath,M_ACTSci))

data2.nums <- select_if(data2,is.numeric)
data2.cats <- select_if(data2,negate(is.numeric))
data.nums.na <- select_if(data2.nums,anyNA)
data.nums.com <- select_if(data2.nums,negate(anyNA))
data.nums.na <- knnImputation(data.nums.na,meth="median") 
data.nums <- cbind(data.nums.na, data.nums.com)

#-------------------------------------------------------------------------------------#
### Dummy Variables
dummies <- model.matrix(retained ~.,data=data2.cats)
dummies <- as.data.frame(dummies)
dummies <- dummies[,-1]
dummies <- lapply(dummies,factor)
dummies <- as.data.frame(dummies)
data2 <- cbind(data2.cats,data.nums,Flags,dummies)

#-------------------------------------------------------------------------------------#
### Last Step: Checking colinearity
data2.nums <- select_if(data2,is.numeric)
M<-cor(data2.nums)
library(corrplot)
corrplot(M,method="number") #No Big Colinearity Problem. 

#------------------------------------------------------------------------------------------#
### Class Profile
ClassProf <- data2 %>% group_by(DateFrom) %>% summarise(Size=n(),MinGPA=min(GPA),AvgGPA=mean(GPA),MedGPA=median(GPA),MaxGPA=max(GPA),
                                                        MinACT=min(ACTBest), MeanACT=mean(ACTBest),MedACT=median(ACTBest),MaxACT=max(ACTBest))

PlotData <- ClassProf %>% select(DateFrom,Size,AvgGPA,MeanACT) %>% gather(Score,Values,-c(1))
Size <- PlotData[which(PlotData$Score=="Size"),]
GPA  <- PlotData[which(PlotData$Score=="AvgGPA"),]
ACT <- PlotData[which(PlotData$Score=="MeanACT"),]
Sp <- ggplot() + geom_line(aes(x=DateFrom,y=Values),data=Size,color="blue",size=3)+
  theme_bw()+labs(x="Years",y="Class Size",title="Class Size By Years")+scale_x_continuous(breaks=seq(2009, 2015, 1), limits=c(2009,2015))+
  theme(plot.title = element_text(size=15,face="bold",hjust = 0.5),panel.grid.major = element_blank())
Gp <-ggplot() + geom_line(aes(x=DateFrom,y=Values),data=GPA,color="darkgreen",size=3)+theme_bw()+
  labs(x="Years",y="Average Highschool GPA",title="Average GPA by Years")+scale_x_continuous(breaks=seq(2009, 2015, 1), limits=c(2009,2015))+
  theme(plot.title = element_text(size=15,face="bold",hjust = 0.5),panel.grid.major = element_blank())
Ap <- ggplot() + geom_line(aes(x=DateFrom,y=Values),data=ACT,color="orange",size=3)+theme_bw()+
  labs(x="Years",y="Average ACT Score",title="Average ACT score by Years")+scale_x_continuous(breaks=seq(2009, 2015, 1), limits=c(2009,2015))+
  theme(plot.title = element_text(size=15,face="bold",hjust = 0.5),panel.grid.major = element_blank())
library(ggpubr)
theme_set(theme_pubr())
figure <- ggarrange(Sp, Gp, Ap,
                    ncol = 1, nrow = 3)

# We will not be using the year, unless we want to do a time series regression.
#But we will keep it for now----to filter.

###Summary Statisics
data.summary(data2)
str(data2)
FD <- cbind(Flags,dummies)
dumF <- data.summary(FD)
dumF <- dumF[,-3]
dumF$class <- "Factors"
dumF <- dumF[c(3,1,2)]
write.csv(dumF,file="dum_Flg.csv")


## End of DATA CLEANING---Rename Retained
data2$retained <- recode_factor(data2$retained,"1"="Retained","0"="Not Retained")
write.csv(data2,file="BannerData.csv", row.names = FALSE)
rm(data_else,data.nums,data.nums.com,data.nums.na,data2.cats,data2.nums,data2015,dummies,EERSCORE,Flags)
