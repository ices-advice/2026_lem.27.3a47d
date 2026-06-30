### ------------------------------------------------------------------------ ###
### Preprocess data, write data tables ####
### ------------------------------------------------------------------------ ###

## Before: bootstrap/data/ICES_1950_2010_zero.csv
##         bootstrap/data/ICESCatchDataset2006-2023.csv
##         bootstrap/data/PreliminaryCatchesFor2024.csv
##         bootstrap/data/PreliminaryCatchesFor2025.csv
##         bootstrap/data/CatchAndSampleDataTables.txt
##         bootstrap/data/lem_lenFreq.csv
##         bootstrap/data/Age Length Key by Sex and Maturity _2026-04-14 10_31_34_IBTSQ1.csv
##         bootstrap/data/Age Length Key by Sex and Maturity _2026-04-14 10_34_34_IBTSQ3.csv
##
## After:  data/Official_country_division.csv
##         data/Official_division.csv
##         data/Official_total.csv
##         data/Prel_division_bms_v2.csv
##         data/lem_lenFreq.csv
##         data/catch.csv
##         data/IC_length


 
#setwd("N:/git_/2024_lem.27.3a47d_assessment")
setwd("Y:\\NS lemon sole\\Lemon sole 2026_Elisa\\TAF 2026\\2026_lem.27.3a47d_assessment")



library(icesTAF)
library(icesAdvice)
library(TAF)
library(FLCore)
library(tidyr)
library(dplyr)
library(reshape2)
library(data.table)
library(gplots)
library(lattice)


year<-2026   # assessment, last survey year
datayear<-2025  # catch data


#taf.bootstrap(clean = FALSE, data = TRUE, software = FALSE) ##

### create folder to store data

mkdir("data")
mkdir("model")
mkdir("report")
mkdir("output")


#source("utilities_data.R")


### ------------------------------------------------------------------------ ###
### official landings ####
### ------------------------------------------------------------------------ ###


##  read official landings 1950-2005


his1<-read.csv("bootstrap/data/ICES_1950_2010_zero.csv", header=T)
his1$Division<-as.character(his1$Division)
his1$Country<-as.character(his1$Country)

his1[his1$Division=="III a","Division"]<-"3a"
his1[his1$Division%in%c("IV a+b (not specified)", "IV (not specified)","IV", "IV a", "IV b", "IV c", "IIIa  and  IVa+b (not specified)" ),"Division" ]<-"4"
his1[his1$Division%in%c("VII d", "VII d+e (not specified)"),"Division"]<-"7d"

his1[his1$Country%in%c("Germany, Fed0 Rep0 of", "Germany","Germany, New Länder"),"Country"]<-"DE"
his1[his1$Country%in%c("Belgium"),"Country"]<-"BE"
his1[his1$Country%in%c("Denmark"),"Country"]<-"DK"
his1[his1$Country%in%c("Norway"),"Country"]<-"NO"
his1[his1$Country%in%c("Sweden"),"Country"]<-"SE"
his1[his1$Country%in%c("Netherlands"),"Country"]<-"NL"
his1[his1$Country%in%c("France"),"Country"]<-"FR"
his1[his1$Country%in%c("UK 0 Eng+Wales+N0Irl0","UK 0 England & Wales","UK 0 England & Wales","UK 0 Scotland"),"Country"]<-"UK"

his1[his1$Division=="4" & his1$Country%in%c("Faeroe Islands", "SE","Ireland"),"Country"]<-"other"
his1[his1$Division=="7d" & his1$Country%in%c("DE","Faeroe Islands", "NO", "SE","Ireland"),"Country"]<-"other"
his1[his1$Division=="3a" & his1$Country%in%c("UK","France","Faeroe Islands", "NO","Ireland"),"Country"]<-"other"



history<-his1%>% filter(Division %in% c("4","3a","7d") &
                              Species %in% c("Lemon sole")) %>%
  reshape2::melt(id.vars = c("Country", "Species", "Division"), variable.name = "Year", value.name = "AMS.Catch") %>%
  mutate(AMS.Catch = as.numeric(gsub("-|<0.5|[.]", "", AMS.Catch)), 
         Year = as.numeric(gsub("X", "", Year)),
         Area = as.character(Division),
         Species="LEM",
         Country = as.character(Country),
         Division = NULL, BMS.Catch = NA) %>% filter(Year<2006)



# read ICES official landings 2006_2022

catch<-read.csv("bootstrap/data/ICESCatchDataset2006-2023.csv", header=T)
names (catch)

####For "ICESCatchDataset2006_2022.csv" there is one extra column named "X". I will remove it. 
#Fix EB 14/01/2025
#catch <- catch[, !names(catch)%in% "X"]
#names (catch)

#There are characters ("0 c") within this files in columns 2018-2020. Replace "0 c" by zero and convert all into numeric. 
#Fix EB 09/04/2024
idx<- grep("X2", names(catch))
for(i in idx)
{
  catch[,i]<- gsub("0 c", 0, catch[,i])
  catch[,i]<- as.numeric(catch[,i])
}
#######
catch1<-catch[catch$Species=="LEM",]
catch1$Area<-as.character(catch1$Area)
catch1$Country<-as.character(catch1$Country)

catch1[catch1$Area%in%c("27.3.a" ), "Area"]<-"3a"
catch1[catch1$Area%in%c("27.7.d"), "Area"]<-"7d"
catch1[catch1$Area%in%c("27.4"),"Area"]<-"4"
catch1<-catch1[catch1$Area%in%c("4","3a","7d"),]


catch1[catch1$Area=="3a" & catch1$Country%in%c("GB","ES","FO", "IE", "IM","IS", "JE", "NO", "PL", "PT","GG"),"Country"]<-"other"
catch1[catch1$Area=="7d" & catch1$Country%in%c("DE", "ES","FO", "IE", "IM","IS", "JE", "NO", "PL", "PT", "SE","GG"),"Country"]<-"other"
catch1[catch1$Area=="4" & catch1$Country%in%c("ES","FO", "IE", "IM","IS", "JE", "PL", "PT", "SE","GG"),"Country"]<-"other"


#For the 2024 WG, it was cols 5-19 (year 2006 is not included)
#catch11<-cbind(catch1[,1:4],rev(catch1[,5:19]))

#For the 2025 WG, it it will be cols 5-20 (year 2006 is not included)
catch11<-cbind(catch1[,1:4],rev(catch1[,5:20]))

#For the 2026 WG, it it will be cols 5-21 (year 2006 is not included)
catch11<-cbind(catch1[,1:4],rev(catch1[,5:21]))

catch2<-aggregate(.~Country+Species+ Area, data=catch11[,-3], FUN=sum)
catch3<-aggregate(.~Species+ Area, data=catch2[,-1], FUN=sum)
catch4<-aggregate(.~Species, data=catch3[,-2], FUN=sum)

official <- catch1 %>% filter(Area %in% c("3a", "4", "7d") &
                                Species == "LEM") %>%
  reshape2::melt(id.vars = c("Country", "Species", "Area", "Units"), variable.name = "Year", value.name = "AMS.Catch") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Area = as.character(Area), Units = NULL, BMS.Catch = NA)




#Preliminary stats
#Load data
#prel1<-read.csv("Prelimcatch2021v2.csv", header=T, sep=",")
#For 2024
prel1<-read.csv("bootstrap/data/PreliminaryCatchesFor2024.csv", header=T, sep=",")
names (prel1)

#For 2025
prel2<-read.csv("bootstrap/data/PreliminaryCatchesFor2025.csv", header=T, sep=",")
names (prel2)

#For 2026 WG, "PreliminaryCatchesFor2024.csv" has 2 columns with the same name "Species.Latin.Name". I will remove one. 
#Fix 10/04/2026EB
prel1 <- prel1[, !names(prel1)%in% "Species_Latin_Name"]
names (prel1)

#check if names of the variables are the same.For the 2023 prelim data names changes from 2022prelim data. Also in 2024 prelim data names differ
#Fix EB 10 /04/2024#
names (prel1)
names (prel2)
names(prel2)[names(prel2)=="AMS.Catch.TLW."] <- "AMS_Catch"
names(prel2)[names(prel2)=="BMS.Catch.TLW."] <- "BMS_Catch"

names(prel1)[names(prel1)=="AMS.Catch.TLW."] <- "AMS_Catch"
names(prel1)[names(prel1)=="BMS.Catch.TLW."] <- "BMS_Catch"

#colnames(prel2)<-colnames(prel1)
colnames(prel1)[1]<-"Year"
colnames(prel2)[1]<-"Year"

prel<-rbind(prel1[,c("Species.Latin.Name","Year","Area","Country","AMS_Catch","BMS_Catch")], prel2[,c("Species.Latin.Name","Year","Area","Country","AMS_Catch","BMS_Catch")])
prel<-prel[prel$Species.Latin.Name=="Microstomus kitt",]
prel$Area<-as.character(prel$Area)
prel$Country<-as.character(prel$Country)

prel[is.na(prel$BMS.Catch),"BMS_Catch"]<-0
prel$BMS.Catch<-as.numeric(prel$BMS_Catch)
prel$AMS.Catch<-prel$AMS_Catch

#Original from Tanja
#prel[prel$Area%in%c("27_3_A","27_3_A_20","27_3_A_21", "27_3_C_22", "27_3_B_23", "27.3.a.20", #"27.3.c.22", "27.3.a.21","27.3.a", "27.3.b.23" ), "Area"]<-"3a"

#I removed "27_3_C_22", "27_3_B_23", "27.3.c.22", "27.3.b.23"# May 2026 EB

prel[prel$Area%in%c("27_3_A","27_3_A_20","27_3_A_21", "27.3.a.20", "27.3.a.21","27.3.a" ), "Area"]<-"3a"
prel[prel$Area%in%c("27_7_D", "27.7.d"), "Area"]<-"7d"
prel[prel$Area%in%c("27_4","27_4_A", "27_4_B", "27_4_C", "27.4.b", "27.4.c", "27.4.a"),"Area"]<-"4"
prel<-prel[prel$Area%in%c("4","3a","7d"),]

prel[prel$Area=="7d" & prel$Country%in%c("DE", "ES","FO", "IE", "IM","IS", "JE", "NO", "PL", "PT", "SE","GG"),"Country"]<-"other"
prel[prel$Area=="3a" & prel$Country%in%c("UK","FR", "ES","FO", "IE", "IM","IS", "JE", "NO", "PL", "PT","GG"),"Country"]<-"other"
prel[prel$Area=="4" & prel$Country%in%c( "ES","FO", "IE", "IM","IS", "JE", "PL", "PT", "SE","GG"),"Country"]<-"other"


prelim <- prel %>% filter(Area %in% c("3a", "4", "7d") & Species.Latin.Name == "Microstomus kitt" ) %>%
  mutate(Species="LEM",
         Year = as.numeric(Year),
         Area = as.character(Area),
         Species.Latin.Name = NULL, AphiaID = NULL, AMS_Catch=NULL, BMS_Catch=NULL) 




#combine and save

off <- rbind(history, official, prelim)



off3<-aggregate(AMS.Catch~Species +Year +Area +Country , data=off, FUN=sum, na.rm=T)
off4<-aggregate(AMS.Catch~ Species + Year + Area, data=off3, FUN=sum)
off5<-aggregate(AMS.Catch~ Species+Year , data=off3, FUN=sum)

prelbms<-aggregate(BMS.Catch~Species+ Year+ Area, data=prelim, FUN=sum)


write.taf(off3, file="Official_country_division.csv",dir="data")
write.taf(off4, file="Official_division.csv" ,dir="data")
write.taf(off5, file="Official_total.csv",dir="data")
write.taf(prelbms,file="Prel_division_bms.csv",dir="data")



### ------------------------------------------------------------------------ ###
### Intercatch data ####
### ------------------------------------------------------------------------ 

catch<-read.csv("bootstrap\\data\\catch.csv", header=TRUE)
write.taf(catch,file="catch.csv",dir="data")


### ------------------------------------------------------------------------ ###
### recent Intercatch data ####
### ------------------------------------------------------------------------ 

test <- scan("bootstrap/data/CatchAndSampleDataTables.txt",what='character',sep='\t')
table2 <- test[(which(test=="TABLE 2.")+3):length(test)]
tmp<-table2[-c(1:56)]			  

table2_bis<-data.frame(matrix(tmp,ncol=27,byrow=T), stringsAsFactors =FALSE)
colnames(table2_bis) <- table2[1:27]
table2_bis <- data.table(table2_bis)
table2_bis <- table2_bis[,CATON:=as.numeric(as.character(CATON))]
table2_bis <- table2_bis[,CANUM:=as.numeric(as.character(CANUM))]
table2_bis <- table2_bis[,WECA:=as.numeric(as.character(WECA))]
table2_bis <- table2_bis[,AgeOrLength:=as.numeric(as.character(AgeOrLength))]

table2_bis <- table2_bis[,Area:=as.factor(Area)]
table2_bis <- table2_bis[,Fleet:=as.factor(Fleet)]
table2_bis <- table2_bis[,Season:=as.factor(Season)]
table2_bis <- table2_bis[,Country:=as.factor(Country)]

table2_tot <- table2_bis

colnames(table2_tot)[colnames(table2_tot)=='AgeOrLength'] <- 'MeanLength'
DF <- table2_tot[,list(CANUM=sum(CANUM), MeanWeight_in_g=weighted.mean((WECA))), by=c('Year','Stock','CatchCategory','Sex','MeanLength')]

#test<-test[Gender=="Male", Gender:="M"]
#test<-test[Gender=="Female", Gender:="F"]

Number <- DF[,list(CANUM=sum(CANUM)), by=c('Year','Stock','Sex','MeanLength')]
Number <- reshape(Number, idvar=c('Stock','Sex','MeanLength'), timevar="Year", direction='wide')
colnames(Number)[substr(colnames(Number),1,3)=="CAN"] <- substr(colnames(Number)[substr(colnames(Number),1,3)=="CAN"],7,10)
Number[is.na(Number)] <- 0
Number <- Number[order(MeanLength),]


Wgt <- DF[,list(MeanWeight_in_g=weighted.mean((MeanWeight_in_g))), by=c('Year','Stock','Sex','MeanLength')]
wgt <- reshape(Wgt, idvar=c('Stock','Sex','MeanLength'), timevar="Year", direction='wide')
colnames(wgt)[substr(colnames(wgt),1,10)=="MeanWeight"] <- substr(colnames(wgt)[substr(colnames(wgt),1,10)=="MeanWeight"],17,20)
wgt[is.na(wgt)] <- 0
wgt <- wgt[order(MeanLength),]


IC <- list()
IC$male_length <- as.data.frame(Number[Sex=='M',c('MeanLength',as.character(datayear)), with=FALSE])
IC$female_length <- as.data.frame(Number[Sex=='F',c('MeanLength',datayear), with=FALSE])
IC$male_weight <- as.data.frame(wgt[Sex=='M',c('MeanLength',datayear), with=FALSE])
IC$female_weight <- as.data.frame(wgt[Sex=='F',c('MeanLength',datayear), with=FALSE])

IC$Undetermined_length <- as.data.frame(Number[Sex=='Undetermined',c('MeanLength',datayear), with=FALSE])
IC$Undetermined_weight <- as.data.frame(wgt[Sex=='Undetermined',c('MeanLength',datayear), with=FALSE])


write.csv(Number[Number$Sex == "Undetermined",], file = "data\\IC_length.csv")

# merge with historical length distributions

ic<-IC$Undetermined_length
colnames(ic)<-c("MeanLength",paste0("X", datayear))
mL<-read.csv("bootstrap/data/lem_lenFreq.csv", header=T, sep=",")

all<-as.data.frame(merge(mL, ic, by="MeanLength", all=TRUE))
all[is.na(all)]<-0

write.taf(all, file = "lem_lenFreq.csv", dir="data")


### ------------------------------------------------------------------------ ###
### estimate stock weights at age ####
### ------------------------------------------------------------------------ 
set.seed(1234)

# Read in IBTS Age Length Key by Sex and Maturity data
ibtsq1 <- read.csv("bootstrap/data/Age Length Key by Sex and Maturity _2026-04-14 10_31_34_IBTSQ1.csv")
ibtsq3 <- read.csv("bootstrap/data/Age Length Key by Sex and Maturity _2026-04-14 10_34_34_IBTSQ3.csv")

# Expand using NoAtLngt
ibtsq1 <- ibtsq1[rep(row.names(ibtsq1), times = ibtsq1$CANoAtLngt),]
ibtsq1 <- subset(ibtsq1, select = -c(CANoAtLngt))
ibtsq3 <- ibtsq3[rep(row.names(ibtsq3), times = ibtsq3$CANoAtLngt),]
ibtsq3 <- subset(ibtsq3, select = -c(CANoAtLngt))

# Limit to year, age and weight only
ibtsq1 <- subset(ibtsq1, select = c(Year, Age, IndWgt))
ibtsq3 <- subset(ibtsq3, select = c(Year, Age, IndWgt))

# Years with observations
ibtsq1<-ibtsq1[ibtsq1$Year>2006,]
ibtsq3<-ibtsq3[ibtsq3$Year>=2006,]

# missing ages
a1<-setdiff(1:18,unique(ibtsq1$Age))
a3<-setdiff(0:20,unique(ibtsq3$Age))

ibtsq1[ibtsq1$Age==-9,"IndWgt"]<-NA
ibtsq3[ibtsq3$Age==-9,"IndWgt"]<-NA
ibtsq1[ibtsq1$Age==-9,"Age"]<-a1
ibtsq3[ibtsq3$Age==-9,"Age"]<-a3

# Calculate tabulated averages
ibtsq1.means <- round(tapply(ibtsq1$IndWgt, list(ibtsq1$Year, ibtsq1$Age), FUN = mean) / 1000, 3)
ibtsq3.means <- round(tapply(ibtsq3$IndWgt, list(ibtsq3$Year, ibtsq3$Age), FUN = mean) / 1000, 3)


write.taf(ibtsq1.means,file="wt_ibtsq1.means.csv", dir="data")
write.taf(ibtsq3.means,file="wt_ibtsq3.means.csv", dir="data")

# produce table for collation sheet 

iq1<-data.frame("q1",sort(unique(ibtsq1$Year)),NA,NA)
colnames(iq1)<-c("survey", "year","age","wt")
ibts.q1<-NULL
for(i in 1:18){
  for(j in c(sort(unique(ibtsq1$Year)))){
    iq1$age<-i  
    iq1[iq1$year==j & iq1$age==i,"wt"]<-ibtsq1.means[rownames(ibtsq1.means)==as.character(j) , colnames(ibtsq1.means)==as.character(i)]
  }
  ibts.q1<-rbind(ibts.q1,iq1)}

iq3<-data.frame("q3",sort(unique(ibtsq3$Year)),NA,NA)
colnames(iq3)<-c("survey", "year","age","wt")
ibts.q3<-NULL
for(i in c(0:20)){
  for(j in c(sort(unique(ibtsq3$Year)))){
    iq3$age<-i  
    iq3[iq3$year==j & iq3$age==i,"wt"]<-ibtsq3.means[rownames(ibtsq3.means)==as.character(j) , colnames(ibtsq3.means)==as.character(i)]
  }
  ibts.q3<-rbind(ibts.q3,iq3)}


ibtsall<-rbind(ibts.q1,ibts.q3)


write.taf(ibts.q1,file="wt_ibtsq1.csv", dir="data")
write.taf(ibts.q3,file="wt_ibtsq3.csv", dir="data")
write.taf(ibtsall,file="wt_ibtsall.csv", dir="data")
write.taf(ibtsall,file="Weight Estimation.csv", dir="data")


