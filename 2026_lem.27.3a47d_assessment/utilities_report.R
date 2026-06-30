
################################## readStockOverview ###############################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){
  
  Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
  names(Wdata)[7] <- "Fleet"
  names(Wdata)[10] <- "CatchWt"
  names(Wdata)[11] <- "CatchCat"
  names(Wdata)[12] <- "ReportCat"
  levels(Wdata$CatchCat)<-c(levels(Wdata$CatchCat),"R")
  Wdata$CatchCat[Wdata$CatchCat %in% "Logbook Registered Discard"]<-"R"
  Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
  Wdata <- Wdata[,-ncol(Wdata)]
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[9] <- "Fleet"
  
  Wdata <- merge(Wdata,Ndata[,c(3,4,5,7,9,10,11)],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)
  
  return(Wdata)
}


################################## readNumbersAtAgeLength ###############################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[8] <- "ReportCat"
  names(Ndata)[9] <- "Fleet"
  Ndata <- Ndata[,-ncol(Ndata)]
  ageNames <- names(Ndata)[16:ncol(Ndata)]
  if(nchar(ageNames)[1]!=4){
    ages <- as.numeric(substr(ageNames,16,nchar(ageNames)))
  } else {
    ages <- as.numeric(substr(ageNames,4,nchar(ageNames)))
  }
  allAges <- min(ages):max(ages)
  missingAges <- allAges[allAges %in% ages]
  colnames(Ndata)[16:ncol(Ndata)] <- ages
  return(Ndata)
}

################################## aggregateStockOverview ###############################

aggregateStockOverview <- function(dat,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE) {
  
  if (byFleet=="FALSE" & bySampled=="TRUE")
    stop("Sorry this function has a bug with byFleet=FALSE and bySampled=TRUE.")
  
  impLand <- dat[dat$CatchCat=="L",]
  impDis <- dat[dat$CatchCat=="D",]
  impBms <- dat[dat$CatchCat=="B",]
  impLrd <- dat[dat$CatchCat=="R",]
  
  nArea <- nSeason <- nCountry <- nFleet <- 1
  
  SeasonNames <- sort(unique(impLand$Season))
  AreaNames <- sort(unique(impLand$Area))
  
  if (byFleet) nFleet <- length(unique(impLand$Fleet))
  if (byCountry) { 
    nCountry <- length(unique(impLand$Country))
    countryLegend <- TRUE
  }    
  if (byArea) nArea <- length(AreaNames)
  if (bySeason) nSeason <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE
  
  LsummaryList <- list()
  DsummaryList <- list()
  BsummaryList <- list()
  RsummaryList <- list()
  
  summaryNames <- NULL
  i <- 1
  if (byFleet) {
    LsummaryList[[i]] <- impLand$Fleet
    DsummaryList[[i]] <- impDis$Fleet
    BsummaryList[[i]] <- impBms$Fleet
    RsummaryList[[i]] <- impLrd$Fleet
    
    summaryNames <- c(summaryNames,"Fleet")
    i <- i+1
  }
  if(byCountry) {
    LsummaryList[[i]] <- impLand$Country
    DsummaryList[[i]] <- impDis$Country
    BsummaryList[[i]] <- impBms$Country
    RsummaryList[[i]] <- impLrd$Country
    
    summaryNames <- c(summaryNames,"Country")
    i <- i+1
  }
  if(bySeason) {
    LsummaryList[[i]] <- impLand$Season
    DsummaryList[[i]] <- impDis$Season
    BsummaryList[[i]] <- impBms$Season
    RsummaryList[[i]] <- impLrd$Season
    
    summaryNames <- c(summaryNames,"Season")
    i <- i+1
  }
  if (byArea) {
    LsummaryList[[i]] <- impLand$Area
    DsummaryList[[i]] <- impDis$Area
    BsummaryList[[i]] <- impBms$Area
    RsummaryList[[i]] <- impLrd$Area
    
    summaryNames <- c(summaryNames,"Area")
    i <- i+1
  }
  if (bySampled) {
    LsummaryList[[i]] <- impLand$Sampled
    DsummaryList[[i]] <- impDis$Sampled
    BsummaryList[[i]] <- impBms$Sampled
    RsummaryList[[i]] <- impLrd$Sampled
    
    summaryNames <- c(summaryNames,"Sampled")
    i <- i+1
  }
  byNames <- summaryNames[summaryNames!="Sampled"]
  summaryNames <- c(summaryNames,"CatchWt")
  
  landSummary <- aggregate(impLand$CatchWt,LsummaryList,sum)
  disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum)
  bmsSummary <- aggregate(impBms$CatchWt,BsummaryList,sum)
  lrdSummary <- aggregate(impLrd$CatchWt,RsummaryList,sum)
  
  names(landSummary) <- summaryNames
  names(disSummary) <- summaryNames
  names(bmsSummary) <- summaryNames
  names(lrdSummary) <- summaryNames
  
  
  if (bySampled) {
    names(landSummary)[names(landSummary)=="Sampled"] <- "LandSampled"
    names(disSummary)[names(disSummary)=="Sampled"] <- "DisSampled"
    names(bmsSummary)[names(bmsSummary)=="Sampled"] <- "BmsSampled"
    names(lrdSummary)[names(lrdSummary)=="Sampled"] <- "LrdSampled"
    
  }
  
  disSummary <- disSummary[!(disSummary$CatchWt==0 & disSummary$DisSampled==FALSE),]
  
  stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE)
  names(stratumSummary)[names(stratumSummary)=="CatchWt.x"] <- "LandWt"
  names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"
  stratumSummary <- merge(stratumSummary,bmsSummary,by=byNames,all=TRUE)
  names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "BmsWt"
  stratumSummary <- merge(stratumSummary,lrdSummary,by=byNames,all=TRUE)
  names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LrdWt"
  
  stratumSummary$LandWt[is.na(stratumSummary$LandWt)] <- 0
  
  if (bySampled ) {
    stratumSummary$LandSampled[is.na(stratumSummary$LandSampled)] <- FALSE
    stratumSummary$DisSampled[is.na(stratumSummary$DisSampled)] <- FALSE
    stratumSummary$BmsSampled[is.na(stratumSummary$BmsSampled)] <- FALSE
    stratumSummary$LrdSampled[is.na(stratumSummary$LrdSampled)] <- FALSE
    stratumSummary <- stratumSummary[rev(order(stratumSummary$LandSampled,stratumSummary$LandWt)),]
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
  }
  return(stratumSummary)                                                        
}



################################## plotStockOverview ###############################

plotStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL){
  
  plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided","BmsWt","LrdWt","BMSProvided")
  if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))
  
  stock <- dat$Stock[1]
  
  impLand <- dat[dat$CatchCat=="L",]
  impDis <- dat[dat$CatchCat=="D",]
  impBms <- dat[dat$CatchCat=="B",]
  impLrd <- dat[dat$CatchCat=="R",]
  
  nArea <- nSeason <- nCountry <- nFleet <- 1
  
  SeasonNames <- sort(unique(impLand$Season))
  AreaNames <- sort(unique(impLand$Area))
  
  countryLegend <- FALSE
  
  if (byFleet) nFleet <- length(unique(c(impLand$Fleet, impDis$Fleet))) ## YV
  if (byCountry) { 
    nCountry <- length(unique(c(impLand$Country, impDis$Country))) # YV
    countryLegend <- TRUE
  }    
  if (byArea) nArea <- length(AreaNames)
  if (bySeason) nSeason <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE
  
  
  if (length(countryColours)==1 &&countryColours){
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
    #"Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
    #, stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 "Colour"=rainbow(length(unique(dat$Country)))
                                 , stringsAsFactors=FALSE)
  }
  if (length(countryColours)==1 && countryColours==FALSE){
    countryLegend <- FALSE
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Norway","Netherlands","Poland","Sweden","UK (England)","UK(Scotland)")
    # , stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 stringsAsFactors=FALSE)
    countryColours$Colour <- rep("grey",length(countryColours$Country))
  }
  
  
  LsummaryList <- list()
  DsummaryList <- list()
  BsummaryList <- list()
  RsummaryList <- list()
  summaryNames <- NULL
  i <- 1
  if (byFleet) {
    LsummaryList[[i]] <- impLand$Fleet
    DsummaryList[[i]] <- impDis$Fleet
    BsummaryList[[i]] <- impBms$Fleet
    RsummaryList[[i]] <- impLrd$Fleet
    summaryNames <- c(summaryNames,"Fleet")
    i <- i+1
  }
  if(byCountry) {
    LsummaryList[[i]] <- impLand$Country
    DsummaryList[[i]] <- impDis$Country
    BsummaryList[[i]] <- impBms$Country
    RsummaryList[[i]] <- impLrd$Country
    summaryNames <- c(summaryNames,"Country")
    i <- i+1
  }
  if(bySeason) {
    LsummaryList[[i]] <- impLand$Season
    DsummaryList[[i]] <- impDis$Season
    BsummaryList[[i]] <- impBms$Season
    RsummaryList[[i]] <- impLrd$Season
    summaryNames <- c(summaryNames,"Season")
    i <- i+1
  }
  if (byArea) {
    LsummaryList[[i]] <- impLand$Area
    DsummaryList[[i]] <- impDis$Area
    BsummaryList[[i]] <- impBms$Area
    RsummaryList[[i]] <- impLrd$Area
    summaryNames <- c(summaryNames,"Area")
    i <- i+1
  }
  if (bySampled) {
    LsummaryList[[i]] <- impLand$Sampled
    DsummaryList[[i]] <- impDis$Sampled
    BsummaryList[[i]] <- impBms$Sampled
    RsummaryList[[i]] <- impLrd$Sampled
    summaryNames <- c(summaryNames,"Sampled")
    i <- i+1
  }
  byNames <- summaryNames
  summaryNames <- c(summaryNames,"CatchWt")
  
  ### YV changed
  if(plotType%in%c("LandWt","LandPercent")){
    Summary <- aggregate(impLand$CatchWt,LsummaryList,sum)
  } else if (plotType=="DisWt"){
    Summary <- aggregate(impDis$CatchWt,DsummaryList,sum)
  } else if (plotType=="BmsWt"){
    Summary <- aggregate(impBms$CatchWt,BsummaryList,sum)
  } else if (plotType=="LrdWt"){
    Summary <- aggregate(impLrd$CatchWt,RsummaryList,sum)
  } else if (plotType=="DisRatio"){
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[!is.na(Summary$DRatio),c(testN,paste('Group.',length(testN)+1,'.x', sep=''),paste('Group.',length(testN)+1,'.y', sep=''),'DRatio')]
  } else if (plotType=="DiscProvided"){
    if(bySampled){
      DsummaryList <- DsummaryList[1: (length(DsummaryList)- 1)]
      LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
    } else {
      DsummaryList <- DsummaryList[ length(DsummaryList)]
      LsummaryList <- LsummaryList[ length(LsummaryList)]
    }
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[,c(testN,'DRatio','x.y')]
    Summary$DRatio[!is.na(Summary$DRatio)] <- TRUE
    Summary$DRatio[is.na(Summary$DRatio)] <- FALSE
    Summary <- unique(Summary) 
    ProvidedDiscards <<- Summary
    
  }else if (plotType=="BMSProvided"){
    if(bySampled){
      BsummaryList <- BsummaryList[1: (length(BsummaryList)- 1)]
      LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
    } else {
      BsummaryList <- BsummaryList[ length(BsummaryList)]
      LsummaryList <- LsummaryList[ length(LsummaryList)]
    }
    SummaryB <- aggregate(impBms$CatchWt,BsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryB)[grep('Group', colnames(SummaryB))]
    } else {
      testN <- colnames(SummaryB)[grep('Group', colnames(SummaryB))]	
    }
    Summary <- merge(SummaryB, SummaryL, all=T, by=testN)
    Summary$BRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[,c(testN,'BRatio','x.y')]
    Summary$BRatio[!is.na(Summary$BRatio)] <- TRUE
    Summary$BRatio[is.na(Summary$BRatio)] <- FALSE
    Summary <- unique(Summary) 
    ProvidedBMS <<- Summary
    
  }
  ### end YV changed
  
  
  #disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum) YV
  if (plotType!="DisRatio") {
    names(Summary) <- summaryNames #YV
  } else {
    names(Summary) <- c(summaryNames[-c(grep(c('Sampled'),summaryNames), grep(c('CatchWt'),summaryNames))],
                        "SampledD", "SampledL", "DRatio")
  }
  #names(disSummary) <- summaryNames # yv
  #names(disSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledD" ,"DisWt") # yv
  #names(landSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledL" ,"LandWt") # yv
  
  
  #stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE) #YV
  stratumSummary <- Summary #YV
  if(plotType%in%c("LandWt","LandPercent","DiscProvided","BMSProvided")){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt" # YV
  } else if (plotType=="DisWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt" # YV
  } else if (plotType=="BmsWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "BmsWt" # HC
  } else if (plotType=="LrdWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LrdWt" # HC
  } 
  
  #names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"  # YV
  
  #if (bySampled ) {
  ##  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$DisWt)),]
  #	}
  #} else {
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$DisWt)),]
  #	}
  #}
  if (bySampled ) {
    if(plotType!="DisRatio"){
      stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary[,dim(stratumSummary)[2]])),]
    } else {
      stratumSummary <- stratumSummary[rev(order(stratumSummary$SampledL,stratumSummary$SampledD,stratumSummary[,dim(stratumSummary)[2]])),]	
    }
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary[,dim(stratumSummary)[2]])),]
  }
  
  #catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2) YV
  catchData <- stratumSummary[,dim(stratumSummary)[2]] #YV
  
  
  if (set.mar) par(mar=c(10,4,1,1)+0.1) 
  
  for (a in 1:nArea) {
    #windows()
    if (bySeason & !(byCountry | byFleet)) nSeason <- 1
    for (s in 1:nSeason) {
      area <- AreaNames[a]
      season <- SeasonNames[s]
      
      indx <- 1:nrow(stratumSummary)
      if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season 
      if (!bySeason & byArea) indx <- stratumSummary$Area==area 
      if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season
      
      if (individualTotals) {
        sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
      } else {
        sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
      }  
      if(byCountry) {
        colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
      } else {
        colVec <- "grey"
      }  
      
      if (plotType%in%c("LandWt","DiscProvided","BMSProvided")) yvals <- stratumSummary$LandWt[indx]
      if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
      if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx] 
      if (plotType=="BmsWt") yvals <- stratumSummary$BmsWt[indx] 
      if (plotType=="LrdWt") yvals <- stratumSummary$LrdWt[indx] 
      if (plotType=="CatchWt") yvals <- catchData[,indx] 
      if (plotType=="DisRatio") yvals <- stratumSummary$DRatio
      
      if(plotType!="DisRatio"){
        if (!is.null(ymax)) newYmax <- ymax
        if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
        if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
        #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
        #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
        #if (plotType=="CatchWt") countryLegend <- FALSE #YV
        if (markSampled) newYmax <- 1.06*newYmax
        if (byFleet) namesVec=stratumSummary$Fleet[indx]
        if (!byFleet) {
          if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx]) 
          if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx]) 
          if (byCountry) namesVec=paste(stratumSummary$Country[indx]) 
          if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx]) 
        }
        
        
        cumulativeY <- cumsum(yvals)
        yvals[yvals>newYmax] <- newYmax
        
        if ((newYmax==-Inf)) {
          plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
          box()
        } else {
          b <- barplot(yvals,names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
          
          if (bySampled & markSampled) {
            nSampled <- sum(stratumSummary$Sampled[indx])
            if(nSampled>0){
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(!(plotType %in% c("DiscProvided","BMSProvided"))){
                text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                if(plotType == "DiscProvided"){
                  text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with Discards",cex=0.8)
                  text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no Discards",cex=0.8)
                }
                if(plotType == "BMSProvided"){
                  text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with BMS",cex=0.8)
                  text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no BMS",cex=0.8)
                }
              }
            } else {
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(!(plotType %in% c("DiscProvided","BMSProvided"))){
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                if(plotType == "DiscProvided"){
                  text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no Discards",cex=0.8)
                }
                if(plotType == "BMSProvided"){
                  text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no BMS",cex=0.8)
                }
              }
            }
          }
          if (countryLegend) legend("center", cex = 0.75, inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
          box()
          if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
          if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=100)
        }
        if (!bySeason & !byArea) title.txt <- paste(stock)
        if (!bySeason & byArea) title.txt <- paste(stock,area)
        if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
        if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
        if (bySeason & byArea) title.txt <- paste(stock,area,season)
        title.txt <- paste(title.txt,plotType)
        title(title.txt)
      } else {
        par(mfrow=c(2,2))
        listSample <- unique(paste(stratumSummary$SampledD,stratumSummary$SampledL))
        for(i in 1:length(listSample)){
          idx <- which(stratumSummary$SampledD[indx]==strsplit(listSample[i],' ')[[1]][1] & stratumSummary$SampledL[indx]==strsplit(listSample[i],' ')[[1]][2])
          if(length(idx)>0){
            if(byCountry) {
              colVec <- countryColours$Colour[match(stratumSummary$Country[indx][idx],countryColours$Country)]
            } else {
              colVec <- "grey"
            }  
            
            if (!is.null(ymax)) newYmax <- ymax
            if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
            if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
            #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
            #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
            #if (plotType=="CatchWt") countryLegend <- FALSE #YV
            if (markSampled) newYmax <- 1.06*newYmax
            if (byFleet) namesVec=stratumSummary$Fleet[indx][idx]
            if (!byFleet) {
              if (byArea & bySeason) namesVec=paste(stratumSummary$Area[idx],stratumSummary$Season[indx][idx]) 
              if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx][idx]) 
              if (byCountry) namesVec=paste(stratumSummary$Country[indx][idx]) 
              if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx][idx]) 
            }
            
            
            cumulativeY <- cumsum(yvals[indx][idx])
            yvals[yvals>newYmax] <- newYmax
            
            if ((newYmax==-Inf)) {
              plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
              box()
            } else {
              b <- barplot(yvals[indx][idx],names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
              
              #if (bySampled & markSampled) {
              #  nSampledD <- sum(stratumSummary$SampledD[idx]==T)
              #  nSampledL <- sum(stratumSummary$SampledL[idx]==T)
              #  if (nSampledD>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              #  if (nSampledD>0) arrows(b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              
              #  if (nSampledL>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*92/106,b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              #  if (nSampledL>0) arrows(b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,b[length(b)]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              
              #  } 
              if (countryLegend) legend("center", cex = 0.75, inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
              box()
              if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
              if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=100)
            }
            if (!bySeason & !byArea) title.txt <- paste(stock)
            if (!bySeason & byArea) title.txt <- paste(stock,area)
            if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
            if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
            if (bySeason & byArea) title.txt <- paste(stock,area,season)
            title.txt <- paste(title.txt,plotType, "D/L", listSample[i])
            title(title.txt)
          }
        }
      }
    }
  }                                                                                                                
}


############################ plotAgeDistribution1 ###############################

plotAgeDistribution1 <- function(dat,plotType="perCent", DiscardProvided=FALSE,BmsProvided=FALSE,LrdProvided=FALSE) {
  
  plotTypes <- c("perCent","frequency")
  
  if (!(plotType %in% plotTypes)) 
    stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))
  
  sampAge <- dat
  ageCols <- 16:ncol(dat)
  
  
  if (TRUE) {
    par(mar=c(4,2,1,1)+0.1)
    par(mfcol=c(5,2))
    
    year <- sampAge$Year[1]
    
    sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
    AFClist <- sort(unique(sampAge$AFC))
    nAFC <- length(AFClist)
    
    overallAgeDist <- list()
    overallAgeDist[["L"]] <- colSums(sampAge[sampAge$CatchCat=="L" ,ageCols])/sum(sampAge[sampAge$CatchCat=="L" ,ageCols])
    catchCat <- c("L")
    if(DiscardProvided & dim(sampAge[sampAge$CatchCat=="D" ,ageCols])[1]>0 ){	
      overallAgeDist[["D"]] <- colSums(sampAge[sampAge$CatchCat=="D" ,ageCols])/sum(sampAge[sampAge$CatchCat=="D" ,ageCols])
      catchCat <- c(catchCat,"D")
    }
    if(BmsProvided & dim(sampAge[sampAge$CatchCat=="B" ,ageCols])[1]>0 ){	
      overallAgeDist[["B"]] <- colSums(sampAge[sampAge$CatchCat=="B" ,ageCols])/sum(sampAge[sampAge$CatchCat=="B" ,ageCols])
      catchCat <- c(catchCat,"B")
    }
    if(LrdProvided & dim(sampAge[sampAge$CatchCat=="R" ,ageCols])[1]>0 ){	
      overallAgeDist[["R"]] <- colSums(sampAge[sampAge$CatchCat=="R" ,ageCols])/sum(sampAge[sampAge$CatchCat=="R" ,ageCols])
      catchCat <- c(catchCat,"R")
    }
    
    listSeason <- unique(sampAge$Season)
    #ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
    for (i  in 1:nAFC) {
      for (catch in catchCat) {
        for (season in listSeason) {
          #if (sampAge[sampAge$AFC==AFClist[i],"Country"][1]=="UK(Scotland)" & catch=="D" & season==1) {
          #  season <- year
          #}
          indx <- sampAge$AFC==AFClist[i] & sampAge$CatchCat==catch & sampAge$Season==season
          dat <- sampAge[indx,]
          if (nrow(dat)==0) {
            plot(0,0,type="n",xlab="",ylab="")
          } else {
            ageDist <- as.matrix(dat[,ageCols],nrow=nrow(dat),ncol=length(ageCols))
            if (plotType=="perCent") ageDist <- ageDist/rowSums(ageDist)
            newYmax <- max(sampAge[sampAge$AFC==AFClist[i],ageCols],na.rm=TRUE)
            if (plotType=="perCent") newYmax <- max(ageDist)
            newYmax <- 1.05*newYmax
            #newYmax <- max(ageDist,na.rm=TRUE)
            barplot(ageDist,ylim=c(0,newYmax),las=2)
            box()
          }
          title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
        }
      }
    }
  }
  
}


