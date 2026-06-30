### ------------------------------------------------------------------------ ###
### LBIs and advice ####
### ------------------------------------------------------------------------ ###

## Before: bootstrap/data/lem_lifehist.csv
##         data/lem_lenFreq.csv
##         data/catch.csv
##         model/SURBARsummary.csv
##        
## After:  output/lem_length_dist.png
##         output/lem_length_dist_newbins.png
##         output/lem_length_dist_truncated.png
##         output/lem_length_dist_newbins_99percLinf.png
##         output/lem_IndicatorRatios_table.csv
##         output/lem_timeseries.png
##         output/lem_timeseries_ratios.png
##         output/length.rds
##         output/advice.Rdata



# Libraries
library(reshape2)
library(lattice)


# Data preparation LBIs

# Years of available data
startyear <- 2002
endyear<- datayear
Year <- c(startyear:endyear)

# Define life history parameters (males first), name available sexes

L <- read.csv("bootstrap/data/lem_lifehist.csv", header=TRUE)
Linf <- L[1,3]
Lmat <- L[1,2]
S <- c("N")  # sexes for which analyses have to be performed on, M males, F females, N unsexed

######################################################################################################


ns <- read.csv("data/lem_lenFreq.csv")

colnames(ns)<-c("MeanLength",Year)
ns[is.na(ns)]<-0


if(TRUE){
  ns$MeanLength <-ns$MeanLength+5  #midpoint her 2 cm classes
}
half_int<-5
######################################################################################################
# step 1 check length distribution plots to decide whether regrouping is necessary to determine Lc 
# (Length at first catch= 50% of mode)

df0<-ns                # Insert f for Females, m for Males or ns for Unsexed
sexplot<-"N"           # M male data set for length frequ. plot

# Plot the original InterCatch lengths
df0.long <- reshape2::melt(df0, id.vars='MeanLength')
 
png("output/lem_length_dist.png", bg="white", pointsize=5,units="cm", width=30, height=25, res = 600)
#
#windows(width = 18, height = 30)
barchart(value~MeanLength|variable, data=df0.long, horizontal=F,layout=c(3,7), as.table=T, ylim=c(0,NA), xlab='Length (mm)', ylab='Number',
         scales=list(x=list(at=seq(1,length(unique(df0.long$MeanLength)),7),
                            labels=seq(min(df0.long$MeanLength),max(df0.long$MeanLength),70))), main="Lemon sole", between = list(x = 0.5) )
dev.off()

# Remove fish < 100 mm (to avoid mis-estimation of length at first capture)
# and downscale abundances in 2013
df0 <- df0[df0$MeanLength > 100,]
# df0[df0$MeanLength < 210,"2013"] <- df0[df0$MeanLength < 210,"2013"] / 20
# Remove the 2013 year as the length data are problemmatic
df0 <- df0[,names(df0) != "2013"]
# save the original plot
df0.long <- reshape2::melt(df0, id.vars='MeanLength')
png("output/lem_length_dist_truncated.png", bg="white", pointsize=5,units="cm", width=30, height=25, res = 600)
#windows(width = 35, height = 18)
barchart(value~MeanLength|variable, data=df0.long, horizontal=F, as.table=T, ylim=c(0,NA), xlab='Length (mm)', ylab='Number',
         scales=list(x=list(at=seq(1,length(unique(df0.long$MeanLength)),7),
                            labels=seq(min(df0.long$MeanLength),max(df0.long$MeanLength),70))), main="Lemon sole",layout=c(3,7),between = list(x = 0.5)  )
dev.off()

#regrouping - optional
ClassInt <- 20 # 1cm length class - it can be changed!
minCL <- floor((min(df0$MeanLength)-half_int)/ClassInt)*ClassInt  # original data 1mm length class
maxCL <- ceiling((max(df0$MeanLength)+half_int)/ClassInt)*ClassInt
df0$LC <- cut(df0$MeanLength, breaks=seq(minCL,maxCL,ClassInt), include.lowest=T)
df0.gr <- aggregate(df0[,2:ncol(df0)-1], by=list(df0$LC), sum)
names(df0.gr)[1] <- 'lclass'
df0.gr <- cbind(lclass=df0.gr$lclass, lmidp=seq(minCL,maxCL-ClassInt,ClassInt)+ClassInt/2, df0.gr[,3:ncol(df0.gr)])
df0.gr.long <-reshape2::melt(df0.gr[,-1], id.var='lmidp')
names(df0.gr.long)[2:3] <- c('year', 'Number')
df0.gr.long$year <- as.numeric(as.character(df0.gr.long$year))

#regrouped plot
png("output/lem_length_dist_newbins.png", pointsize=5,units="cm", width=30, height=25, res = 600)
#windows(width = 35, height = 18)
barchart(Number~lmidp|as.factor(year), data=df0.gr.long, horizontal=F, as.table=T, ylim=c(0,NA), xlab='Length', ylab='Number',
         scales=list(x=list(at=seq(1,length(unique(df0.gr.long$lmidp)),4),
                            labels=seq(min(df0.gr.long$lmidp),max(df0.gr.long$lmidp),4*ClassInt))), main="Lemon sole", cex.main=1.2,layout=c(3,7),between = list(x = 0.5))
dev.off()

# step 6 final decision on regrouping fill in! 
ClassInt<-20

##############################################

# Plot full length distribution (df0 - after removing fish < 100 mm and fixing 2013 problem)
# Lines give suggestion for L(max)

full.len <- data.frame(cbind(df0$MeanLength, apply(df0[,2:(dim(df0)[2]-1)], 1, sum)))
names(full.len) <- c("meanlength","allyears")
png("output/lem_length_dist_newbins_99percLinf.png", pointsize=8,units="cm", width=20, height=15, res = 300)
#windows(width = 25, height = 18)
plot(full.len$meanlength,full.len$allyears,type="l", lwd=2,xlab = "Length (mm)", ylab = "Catch abundance",xlim=c(0,750))
abline(v = 695, lty = 8, col = "red")
text(740,10000000,"695 mm", cex=2)
text(440,10000000,"385 mm",cex=2)

x <- with(full.len, rep(meanlength, times = allyears))
xmax <- round(quantile(x, probs = c(0.99)),0)

abline(v = xmax, lty = 8, col = "red")
dev.off()
rm(x)

######################################################################################################################
# step 2 Calculate indicators per sex

#for(s in 1:length(S)){
s <- 1

sex<-S[s] 
if(sex=="M") final<-m  #numbers
if(sex=="F") final<-f
if(sex=="N") final<-ns

if(sex=="M") weight<-mw   #mean weights
if(sex=="F") weight<-fw
#if(sex=="N") weight<-nsw

Ind <- data.frame(matrix(ncol=24, nrow=endyear-startyear+1)) 
names(Ind) <- c('Year','L75','L25','Lmed', 'L90', 'L95', 'Lmean','Lc','LFeM','Lmaxy' ,'Lmat', 'Lopt','Linf', 'Lmax5',  'Lmean_LFeM','Lc_Lmat','L25_Lmat','Lmean_Lmat','Lmean_Lopt', 'L95_Linf', 'Lmaxy_Lopt','Lmax5_Linf','Pmega','Pmegaref')
Ind$Year <- startyear:endyear

#  regrouping with selected length class width
df0<-final
df0 <- df0[df0$MeanLength > 100,]
minCL <- floor((min(df0$MeanLength)-.5)/ClassInt)*ClassInt  #originaldat 1cm length class
maxCL <- ceiling((max(df0$MeanLength)+.5)/ClassInt)*ClassInt
df0$LC <- cut(df0$MeanLength, breaks=seq(minCL,maxCL,ClassInt), include.lowest=T)
df0.gr <- aggregate(df0[,3:ncol(df0)-1], by=list(df0$LC), sum)
names(df0.gr)[1] <- 'lclass'
df0.gr <- cbind(lclass=df0.gr$lclass, lmidp=seq(minCL,maxCL-ClassInt,ClassInt)+ClassInt/2, df0.gr[,2:ncol(df0.gr)])
df0.gr.long <- reshape::melt(df0.gr[,-1], id.var='lmidp')
names(df0.gr.long)[2:3] <- c('year', 'Number')
df0.gr.long$year <- as.numeric(as.character(df0.gr.long$year))

res <- data.frame(year=min(as.numeric(df0.gr.long$year)):max(as.numeric(df0.gr.long$year)), lmidp=NA, nmax=NA, lc=NA)

for (j in 3:ncol(df0.gr))
{
  for (i in 2:nrow(df0.gr))
  {
    if(df0.gr[i+1,j]-df0.gr[i,j]>=0)                # to include in the final script
      next
    else 
    {
      res$lmidp[j-2] = df0.gr$lmidp[i]
      res$nmax[j-2] = df0.gr[i,j]
      a = res$nmax[j-2]/2
      df1 = df0.gr[,c(2,j)]
      for (k in 1:nrow(df1))
      {
        if (df1[k,2] < a)
          next
        else 
        {  
          res$lc[j-2] = df1[k,1]
        }
        break
      }
    }
    break
  }
  # print (res[j-2,])
}

Ind$Lc <- res$lc

Ind$Lmat <- Lmat[s]
Ind$Lopt <- 2/3*Linf[s]
Ind$Linf <- Linf[s]


for(jj in (1:length(Year))+1){
  j<-jj-1 
  
  final2<-final[,c(1,jj)]
  colnames(final2)<-c("lngth","number")
  
  final2$cumsum<-cumsum(final2[,2])
  final2$cumsum_perc<-final2$cumsum/sum(final2$number)
  
  # find mean top 5%
  numb<- as.data.frame(final2[rev(order(final2$lngth)),"number"])    # from largest starting
  colnames(numb)<-"number"
  numb$cum<-cumsum(numb$number) 
  numb$lngth<-final2[rev(order(final2$lngth)),"lngth"] 
  numb$cumperc<-round(numb$cum/sum(numb$number),5)  
  numb$num5<-0
  numb[numb$cumperc<=0.05,"num5"]<-numb[numb$cumperc<=0.05,"number"]
  numb[max(which(numb$cumperc<=0.05))+1,"num5"]<-(0.05-numb[max(which(numb$cumperc<=0.05)),"cumperc"])*sum(numb$number)
  Ind[j,"Lmax5"]<-sum(numb$num5*numb$lngth)/sum(numb$num5)
  
  # indicators
  
  Ind[j,"L75"]<-min(final2[which(final2$cumsum_perc>=0.75),"lngth"])
  Ind[j,"L25"]<-min(final2[which(final2$cumsum_perc>=0.25),"lngth"])
  Ind[j,"Lmed"]<-min(final2[which(final2$cumsum_perc>=0.5),"lngth"])
  Ind[j,"L95"]<-min(final2[which(final2$cumsum_perc>=0.95),"lngth"])
  Ind[j,"L90"]<-min(final2[which(final2$cumsum_perc>=0.90),"lngth"])
  
  final3<-final2[final2$lngth>=Ind[j,"Lc"],]    # calculate mean of individuals above Lc
  Ind[j,"Lmean"]<-sum(as.numeric(final3$lngth*final3$number))/sum(final3$number)
  
  #final2$biomass<-final2$number*weight[,jj]
  #Ind[j,"Lmaxy"]<-final2[final2$biomass==max(final2$biomass), "lngth"]  # length class with max yield
  
  Lopt <- (2/3)*Linf[s]
  
  Ind[j,"Pmega"] <- sum(final2[which(final2$lngth>=(Lopt+0.1*Lopt)),"number"])/sum(final2$number)   # proportion larger Lopt+10%
  Ind[j,"Year"] <- Year[j]
  Ind[j,"Pmegaref"] <- 0.3   # proxy reference point of 30% in catch
  Ind[j,"LFeM"] <- 0.75*Ind[j,"Lc"]+0.25*Ind[j,"Linf"]
}

#calculate various ratios

Ind$Lmaxy_Lopt <- Ind$Lmaxy/Ind$Lopt
Ind$L95_Linf <- Ind$L95/Ind$Linf
Ind$Lmean_LFeM <- Ind$Lmean/Ind$LFeM
Ind$Lmean_Lmat <- Ind$Lmean/Ind$Lmat
Ind$Lmean_Lopt <- Ind$Lmean/Ind$Lopt
Ind$Lmax5_Linf <- Ind$Lmax5/Ind$Linf
Ind$Lc_Lmat <- Ind$Lc/Ind$Lmat
Ind$L25_Lmat <- Ind$L25/Ind$Lmat

# Set 2013 values to NA
Ind[Ind$Year == "2013",] <- c(2013, rep(NA, length = dim(Ind)[2]-1))

if(sex=="M") Males <- Ind
if(sex=="F") Females <- Ind
if(sex=="N") Unsexed <- Ind

write.csv(Ind, file="output/lem_IndicatorRatios_table.csv", row.names=F)

#}


###################################################################################################
## step 3 plot indicator time series per sex

#for(s in 1:length(S)){
s <- 1
sex<-S[s] 
if(sex=="M") Ind<-Males  
if(sex=="F") Ind<-Females
if(sex=="N") Ind<-Unsexed

png("output/lem_timeseries.png", bg="white", pointsize=5,units="cm", width=10, height=18, res = 600)
par(mar = c(5, 4, 3, 4), mfrow=c(3,1), family="serif", cex=1.5)

plot(Linf~Year, data=Ind, ylab="Length", col="transparent", main="(a) Conservation", xlab="Year", 
     xlim=c(Year[1],tail(Year,1)+3), 
     ylim=c(min(Ind$Lc, na.rm = TRUE)*.9, as.numeric(na.omit(unique(Ind$Linf)))*1.1), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
lines(L95~Year, data=Ind, lwd=2,col="purple")
text(tail(Year,1)+1, tail(Ind$L95,1), expression(L["95%"]),col="purple",cex=1.1)
lines(Lmax5~Year, data=Ind, lwd=2,col="black")
text(tail(Year,1)+1, tail(Ind$Lmax5,1), expression(L["max5%"]),col="black",cex=0.9)
lines(Lmat~Year, data=Ind, lwd=1, col="black" ,lty="dashed")
text(tail(Year,1)+3, tail(Ind$Lmat,1), expression(L["mat"]), col="black", cex=1.1)
lines(Lc~Year, data=Ind, lwd=2, col="blue")
text(tail(Year,1)+3, tail(Ind$Lc,1), expression(L["c"]), col="blue", cex=1.1)
lines(Linf~Year, data=Ind, lwd=1, col="black", lty="dashed")
text(tail(Year,1)+3, tail(Ind$Linf,1), expression(L["inf"]), col="black", cex=1.1)
lines(L25~Year, data=Ind, lwd=1, col="red")
text(tail(Year,1)+1, tail(Ind$L25,1), expression(L["25%"]), col="red", cex=1.1)

plot(Linf~Year, data=Ind, ylab="Length", main="(b) Optimal Yield", col="transparent", xlab="Year", 
     xlim=c(Year[1],tail(Year,1)+3), 
     ylim=c(min(Ind$Lc, na.rm = TRUE)*.9, as.numeric(na.omit(unique(Ind$Linf)))*1.1), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
lines(L75~Year, data=Ind, lwd=1, col="red")
text(tail(Year,1)+1, tail(Ind$L75,1), expression(L["75%"]), col="red", cex=1.1)
lines(Lmean~Year, data=Ind, lwd=2, col="darkred")
text(tail(Year,1)+1, tail(Ind$Lmean,1), expression(L["mean"]), col="darkred", cex=1.1)
lines(Lopt~Year, data=Ind, lwd=1, col="black", lty="dashed")
text(tail(Year,1)+2, tail(Ind$Lopt,1), expression(L["opt"]), col="black", cex=1.2)
#lines(Lmaxy~Year, data=Ind, lwd=2, col="green")
#text(tail(Year,1)+1, tail(Ind$Lmaxy,1), expression(L["maxy"]), col="green", cex=1.2)
lines(Lmat~Year, data=Ind, lwd=1, col="black", lty="dashed")
text(tail(Year,1)+2, tail(Ind$Lmat,1), expression(L["mat"]), col="black", cex=1.1)
lines(L25~Year, data=Ind, lwd=1, col="red")
text(tail(Year,1)+1, tail(Ind$L25,1), expression(L["25%"]), col="red", cex=1.1)

plot(Lmat~Year, data=Ind, type="l", ylab="Length", main="(c) Maximum Sustainable Yield", 
     col="black", lty="dashed", xlab="Year", 
     xlim=c(Year[1],tail(Year,1)+2), 
     ylim=c(min(Ind$Lc, na.rm = TRUE)*.9, as.numeric(na.omit(unique(Ind$Linf)))*1.1), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
text(tail(Year,1)+2, tail(Ind$Lmat,1), expression(L["mat"]), col="black", cex=1.2)
lines(Lmean~Year, data=Ind, lwd=2, col="darkred")
text(tail(Year,1)+1, tail(Ind$Lmean,1), expression(L["mean"]), col="darkred", cex=1.1)
lines(LFeM~Year, data=Ind, lwd=2, col="blue", lty="dashed")
text(tail(Year,1)+2, tail(Ind$LFeM,1), expression(L["F=M"]), col="blue", cex=1.2, lty="dashed")

dev.off()

png("output/lem_timeseries_ratios.png", bg="white", pointsize=5, units="cm", width=10, height=20, res = 600)

par( mar = c(5, 4, 3, 4), mfrow=c(3,1), family="serif", cex=1.5)

plot(c(Year[1], tail(Year,1)+5), c(0, 1.5), ylab="Indicator Ratio", col="transparent", main="(a) Conservation", 
     xlab="Year", xlim=c(Year[1], tail(Year,1)+4), ylim=c(0,2.0), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
lines(Lmax5_Linf~Year, data=Ind, lwd=2, col="black")
text(tail(Year,1)+2, tail(Ind$Lmax5_Linf,1), expression(L["max5%"]/L["inf"]), col="black", cex=1.0)
lines(L95_Linf~Year, data=Ind, lwd=2, col="purple")
text(tail(Year,1)+3, tail(Ind$L95_Linf,1)-0.1, expression(L["95%"]/L["inf"]), col="purple", cex=1.1)
lines(Pmega~Year, data=Ind, lwd=2, col="blue")
text(tail(Year,1)+2, tail(Ind$Pmega,1), expression(P["mega"]), col="blue", cex=1.2)
#lines(Pmegaref~Year, data=Ind, lwd=1, col="black", lty="dashed")
# text(tail(Year,1)+2, tail(Ind$Pmegaref,1), expression("30%"), col="black", cex=1.0)
lines(Lc_Lmat~Year, data=Ind, lwd=2, col="red")
text(tail(Year,1)+2, tail(Ind$Lc_Lmat,1), expression(L["c"]/L["mat"]), col="red", cex=1.1)
lines(L25_Lmat~Year, data=Ind, lwd=2, col="darkred")
text(tail(Year,1)+3, tail(Ind$L25_Lmat,1)+0.1, expression(L["25"]/L["mat"]), col="darkred", cex=1.1)

plot(c(Year[1], tail(Year,1)+3), c(0, 1.6), ylab="Indicator Ratio", col="transparent", 
     main="(b) Optimal yield", xlab="Year" ,xlim=c(Year[1], tail(Year,1)+5), ylim=c(0,2.0), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
lines(Lmean_Lopt~Year, data=Ind, lwd=2, col="darkred")
text(tail(Year,1)+3, tail(Ind$Lmean_Lopt,1), expression(L["mean"]/L["opt"]), col="darkred", cex=1.1)
#lines(Lmaxy_Lopt~Year, data=Ind, lwd=2, col="green")
#text(tail(Year,1)+2, tail(Ind$Lmaxy_Lopt,1), expression(L["maxy"]/L["opt"]), col="green", cex=1.1)

plot(c(Year[1], tail(Year,1)+3), c(0, 1.6), ylab="Indicator Ratio", col="transparent", 
     main="(c) Maximum sustainable yield", xlab="Year", xlim=c(Year[1], tail(Year,1)+5), ylim=c(0,2.0), bty="l")
axis(1, at=Ind$Year, labels=FALSE, cex.axis=0.1, tick=TRUE)
lines(Lmean_LFeM~Year, data=Ind, lwd=2, col="blue")
text(tail(Year,1)+3, tail(Ind$Lmean_LFeM,1), expression(L["mean"]/L["F=M"]), col="blue",cex=1.1)

dev.off()



### ------------------------------------------------------------------------ ###
### Advice: Apply chr rule ####
### ------------------------------------------------------------------------ ###


library(cat3advice)

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###


c<-read.csv("data\\catch.csv", header=TRUE)
i<-read.csv("bootstrap\\data\\SURBARsummary_2025.csv", header=TRUE)  # used the actual run slight change due to lack of seed
l<-read.csv("data\\lem_lenfreq.csv", header=TRUE)
ll<-read.csv("output\\lem_IndicatorRatios_table.csv", header=TRUE)


index<-i[i$X>2006, c("X","SSB")]
colnames(index)<-c("year","index")
lc<-ll$Lc

length<-NULL
for(j in 2007:2025){
  ii<-paste0("X",j)
  ll<-cbind(j,"catch",l[,1], l[,ii])
  length<-rbind(length, ll)
}                        
length<-as.data.frame(length)

colnames(length)<-c("year", "catch_category", "length", "numbers")
length$length<-as.numeric(as.character(length$length))
length$numbers<-as.numeric(as.character(length$numbers))

saveRDS(length, file = "output\\length.rds")



ll<-length[length$length>100 & !length$year==2013,]

### ------------------------------------------------------------------------ ###
### reference catch ####
### ------------------------------------------------------------------------ ###
### use last catch advice value

A <- A(c, basis="advice", units = "tonnes", advice_metric="catch")
A
### ------------------------------------------------------------------------ ###
### r - recent biomass index  ####
### ------------------------------------------------------------------------ ###

I <- I(index)
plot(I)
I
### ------------------------------------------------------------------------ ###
### b - biomass safeguard ####
### ------------------------------------------------------------------------ ###

B <- b(index, units = "",w=1, Itrigger=min(index)/1.4)  # Itrigger=Iloss
advice(B)
plot(B)
B

### ------------------------------------------------------------------------ ###
### multiplier ####
### ------------------------------------------------------------------------ ###
### generic multiplier based on life history (von Bertalanffy k)

M<-m(hcr="chr") 


### ------------------------------------------------------------------------ ###
### f - length-based indicator/fishing pressure proxy ####
### ------------------------------------------------------------------------ ###
ll$year<- as.numeric(ll$year)
lc <- Lc(ll ,lstep=20) # length at first capture
plot(lc)
lc


### mean annual catch length above Lc
ll$length<-ll$length+5  # take midpoint of length classes
#ll$year<- as.numeric(ll$year)
lmean <- Lmean(ll, Lc = lc,  units = "mm") # mean catch length
plot(lmean)
lmean


### calculate new reference length LF=M (only in first year of application, then constant)

lref <- Lref(Lc = lc, Linf = 375) # reference length
fi <- f(Lmean = lmean, Lref = lref, units = "mm") # f indicator
resf<-data.frame(as.numeric(as.character(fi@indicator$year)), fi@indicator$indicator)
graphics::plot(resf[,1], resf[,2], type="l", ylim=c(0,1.5), xlab="Year", ylab="F indicator")
abline(h=1, col="red")


### Harvest rate

df <- merge(c, index, by="year", all = TRUE) # combine catch & index data
df<- df[df$year%in%c(2007:2012,2014:2022),]    # choose year range

hr <- HR(df, units_catch = "tonnes", units_index = "kg/hr") # harvest rate

plot(hr)


FF <- F(hr, fi) # calculate (relative) target harvest rate
#plot(FF)
FF

#difference in Fproxy from excel sheet calculation
# 4119.557947 due to slight difference in SURBAR run


# apply chr rule
advice <- cat3advice::chr(A = A, I = I, F = FF, b = B, m = M)
advice

### ------------------------------------------------------------------------ ###
### save output ####
### ------------------------------------------------------------------------ ###

save(A,I,FF,lmean,lref, FF,hr,fi,B,M,advice, file = "output/advice.Rdata")

