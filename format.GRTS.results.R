Results.Formatted=NULL
Results.Formatted
library(gridExtra)
header = read.csv("header.csv", header=T)


results.by.year = NULL
resultsTrend = NULL
resultsAllYears = read.csv("c:results files\\CHaMP_All_Years.csv")
resultsAllYears$Year = rep("Average of All Years", nrow(resultsAllYears))

files = dir("c:results files")
if ("CHaMPY_Y_Trend.csv" %in% files){
resultsTrend  = read.csv("c:results files\\CHaMPY_Y_Trend.csv")
resultsTrend$Year = rep("Linear Trend Across Years", nrow(resultsTrend))
}

minyear=min(header$Years,na.rm=T)
fn = paste("c:results files\\CHaMP_", minyear,".csv", sep="")
fn

results.by.year = read.csv(fn, header=T)

#results.by.year$Year = rep(minyear, length(results.by.year))
# changed 3/17/17
results.by.year$Year = rep(minyear, nrow(results.by.year))

files
year
  for (year in header$Years[2:length(na.omit(header$Years))]){
    if (paste("CHaMP_",year,".csv",sep="") %in% files)
{
  	  temp = read.csv(paste("c:results files\\CHaMP_",year,".csv",sep=""))
 	  temp$Year = rep(year, nrow(temp))
        results.by.year = rbind(results.by.year, temp)
	  rm(temp)
   }
}


All.Results = rbind(results.by.year, resultsAllYears, resultsTrend)
All.Results
All.Results
All.Results = All.Results[
              ((All.Results$Statistic  == "Mean") |  
                 (All.Results$Statistic=="50Pct") |
                    (All.Results$Statistic=="Std. Deviation")),]


Trend.Results =All.Results[All.Results$Year == "Linear Trend Across Years",]
Median.Results = All.Results[All.Results$Statistic == "50Pct",]
Std.Dev.Results = All.Results[All.Results$Statistic == "Std. Deviation",]
Std.Dev.Results

All.Results =All.Results[All.Results$Statistic  == "Mean",]
All.Results = All.Results[All.Results$Year != "Linear Trend Across Years",]



All.Results$sd = rep(NA, nrow(All.Results))
All.Results$Median = rep(NA, nrow(All.Results))
All.Results$Trend = rep(NA, nrow(All.Results))
All.Results$Trend.sd = rep(NA, nrow(All.Results))
All.Results$Trend.se = rep(NA, nrow(All.Results))
All.Results$Trend.lcb = rep(NA, nrow(All.Results))
All.Results$Trend.ucb = rep(NA, nrow(All.Results))

median.idx = match(
  paste(All.Results$Type, All.Results$Subpopulation, All.Results$Indicator, All.Results$Year),
    paste(Median.Results$Type, Median.Results$Subpopulation, Median.Results$Indicator, Median.Results$Year))

sd.idx = match(
  paste(All.Results$Type, All.Results$Subpopulation, All.Results$Indicator,All.Results$Year),
    paste(Std.Dev.Results$Type, Std.Dev.Results$Subpopulation, Std.Dev.Results$Indicator, Std.Dev.Results$Year))


Trend.Results
trend.mean.idx = match(
  paste(All.Results$Type, All.Results$Subpopulation, All.Results$Indicator),
    paste(Trend.Results[Trend.Results$Statistic=="Mean",]$Type, Trend.Results[Trend.Results$Statistic=="Mean",]$Subpopulation, Trend.Results[Trend.Results$Statistic=="Mean",]$Indicator))


trend.sd.idx = match(
  paste(All.Results$Type, All.Results$Subpopulation, All.Results$Indicator),
    paste(Trend.Results[Trend.Results$Statistic=="Std. Deviation",]$Type, Trend.Results[Trend.Results$Statistic=="Std. Deviation",]$Subpopulation, Trend.Results[Trend.Results$Statistic=="Std. Deviation",]$Indicator))


sd.idx
median.idx
trend.mean.idx
trend.sd.idx


All.Results$sd= Std.Dev.Results$Estimate[sd.idx]
All.Results$median= Median.Results$Estimate[median.idx]

All.Results$Trend = Trend.Results[Trend.Results$Statistic=="Mean",]$Estimate[trend.mean.idx]
All.Results$Trend.sd = Trend.Results[Trend.Results$Statistic=="Std. Deviation",]$Estimate[trend.sd.idx]


All.Results$Trend.se = Trend.Results[Trend.Results$Statistic=="Mean",]$StdError[trend.mean.idx]
All.Results$Trend.lcb = Trend.Results[Trend.Results$Statistic=="Mean",]$LCB95Pct[trend.mean.idx]
All.Results$Trend.ucb = Trend.Results[Trend.Results$Statistic=="Mean",]$UCB95Pct[trend.mean.idx]


All.Results$Trend[(All.Results$Year %in% (2011:9999))] = NA
All.Results$Trend.sd[(All.Results$Year %in% (2011:9999))] = NA
All.Results$Trend.se[(All.Results$Year %in% (2011:9999))] = NA
All.Results$Trend.lcb[(All.Results$Year %in% (2011:9999))] = NA
All.Results$Trend.ucb[(All.Results$Year %in% (2011:9999))] = NA

All.Results

for (j in 1:ncol(All.Results)) {
 if (is.numeric(All.Results[,j])) {All.Results[,j] = round(All.Results[,j], digits=3)
}}


All.Results[is.na(All.Results)] = ""


###################################################################################################################
###### Big mess just to match original "available" N by subgroups with actual used N by subgroup in spsurvey'


M_and_C = read.csv("Metrics_and_Covariates.csv", header=T)
nrow(M_and_C)
head(M_and_C)
M_and_C$VisitNumber
M_and_C$Watershed


names(M_and_C)
M_and_C$SiteID=M_and_C$SiteName

yearly.sites.all.sites = tapply(M_and_C$SiteID, M_and_C$VisitYear,length)
yearly.sites.by.watershed =tapply(M_and_C$SiteID, paste(M_and_C$VisitYear, M_and_C$Watershed), length)
yearly.sites.by.watershed
yearly.sites.by.ValleyClass = tapply(M_and_C$SiteID, paste(M_and_C$VisitYear,M_and_C$ValleyClass), length)
yearly.sites.by.Stratum = tapply(M_and_C$SiteID, factor(paste(M_and_C$VisitYear,M_and_C$Watershed,".",M_and_C$Stratum, sep="")), length)


Total.sites.all.sites.mean = length(unique(M_and_C$SiteID))
Total.sites.by.watershed.mean = 
   tapply(M_and_C$SiteID[match(levels(M_and_C$SiteID),M_and_C$SiteID)], 
       paste("Average of All Years",M_and_C$Watershed)[match(levels(M_and_C$SiteID),M_and_C$SiteID)], length)


Total.sites.by.ValleyClass.mean = 
   tapply(M_and_C$SiteID[match(levels(M_and_C$SiteID),M_and_C$SiteID)], 
       paste("Average of All Years",M_and_C$ValleyClass)[match(levels(M_and_C$SiteID),M_and_C$SiteID)], length)


Total.sites.by.Stratum.mean = 
   tapply(M_and_C$SiteID[match(levels(M_and_C$SiteID),M_and_C$SiteID)], 
       paste("Average of All Years",M_and_C$Watershed,".", M_and_C$Stratum, sep="")[match(levels(M_and_C$SiteID),M_and_C$SiteID)], length)
Total.sites.by.Stratum.mean


yearly.sites.all.sites
Total.sites.all.sites.mean
all.sites = c(yearly.sites.all.sites, "Average of All Years"=Total.sites.all.sites.mean)

all.watershed = c(yearly.sites.by.watershed, Total.sites.by.watershed.mean)
all.watershed
all.valleyclass = c(yearly.sites.by.ValleyClass, Total.sites.by.ValleyClass.mean)
all.stratum = c(yearly.sites.by.Stratum, Total.sites.by.Stratum.mean)
all.stratum


all.sites
all.watershed

All.Results$N_CHaMP.Sites = rep(NA, nrow(All.Results))

All.Results$N_CHaMP.Sites[All.Results$Type == "All.Sites"]= 
   all.sites[match(All.Results$Year[All.Results$Type=="All.Sites"], names(all.sites))]

All.Results$N_CHaMP.Sites[All.Results$Type == "Watershed"]= 
   all.watershed[match(paste(All.Results$Year,All.Results$Subpopulation)[All.Results$Type=="Watershed"]
,names(all.watershed))]


names(all.stratum)
paste(All.Results$Year,All.Results$Subpopulation, sep="")[All.Results$Type=="Stratum"]
all.stratum

All.Results$N_CHaMP.Sites[All.Results$Type == "Stratum"]= all.stratum [match(
paste(All.Results$Year,All.Results$Subpopulation,sep="")[All.Results$Type=="Stratum"],names(all.stratum))]



All.Results$N_CHaMP.Sites[All.Results$Type == "ValleyClass"]
All.Results$N_CHaMP.Sites[All.Results$Type == "Stratum"]

All.Results$N_CHaMP.Sites[All.Results$Type == "ValleyClass"]= all.valleyclass[match(paste(All.Results$Year,All.Results$Subpopulation)[All.Results$Type=="ValleyClass"], names(all.valleyclass))]



All.Results$N_CHaMP.Sites[All.Results$Type == "ValleyClass"]
All.Results$N_CHaMP.Sites[All.Results$Type == "Stratum"]
All.Results$N_CHaMP.Sites[All.Results$Type == "All.Sites"]
All.Results$N_CHaMP.Sites[All.Results$Type == "Watershed"]


ord = order(All.Results$Subpopulation,All.Results$Indicator, All.Results$Year)



All.Results$Mean

names(All.Results)

#All.Results$Display.Name = header$Display.Name[match(All.Results$Indicator, header$Metric.List)]
All.Results$Display.Name = All.Results$Indicator

Results.Formatted = data.frame(
"Metric"=All.Results$Display.Name,
"Visit.Year" = All.Results$Year,
"Sub.Population" = All.Results$Subpopulation,
"Number.of.CHaMP Sites" = All.Results$N_CHaMP.Sites,
"N" = All.Results$NResp,
"Mean" = All.Results$Estimate,
"Std.Error of Mean Estimate" = All.Results$StdError,
"Standard.Deviation" = All.Results$sd,
"Median" = All.Results$median,
"CV" = All.Results$sd/All.Results$Estimate,
"95.PCT.LCB" = All.Results$LCB95Pct,
"95.PCT.UCB" = All.Results$UCB95Pct,
"Trend" = All.Results$Trend,
"Std.Error.of.Trend.Estimate" = All.Results$Trend.se, 
"Trend.95.PCT5.LCB"  = All.Results$Trend.lcb,
"Trend.95.PCT.UCB"  = All.Results$Trend.ucb
)
Results.Formatted


cols=c(1,3,2,5,6,8)
names(Results.Formatted)
write.csv(Results.Formatted[ord,][Results.Formatted[ord,]$Sub.Population !="",],"Formatted.Results.csv",row.names=F)
write.csv(Results.Formatted[ord,cols][Results.Formatted[ord,]$Sub.Population !="",],"EP_Formatted.Results.csv",row.names=F)


#############################################
# Redone again for Pat Zimmer Format Request
# 8 individual plots, laid out in a panel

wsheds =c("Entiat","John Day","Lemhi","Methow", "South Fork Salmon", 
"Tucannon", "Upper Grande Ronde", "Wenatchee","Yankee Fork")


mets = levels(Results.Formatted$Metric)
met=mets[1]
for (met in mets){
met
bar.data = Results.Formatted[Results.Formatted$Metric == met,]
bar.data

VY.SP = paste(bar.data$Visit.Year, bar.data$Sub.Population)
VY.SP
VY.SP.toMatch = paste(rep(c("2011","2012","2013","2014","2015","2016","2017"),9),
c(rep("Entiat",7),rep("John Day",7),rep("Lemhi",7),rep("Methow",7),
rep("South Fork Salmon",7),rep("Tucannon",7),
rep("Upper Grande Ronde",7),rep("Wenatchee",7),rep("Yankee Fork",7)))
VY.SP.toMatch


idx = match(VY.SP.toMatch, VY.SP)
idx
#col = c("dark gray","dark gray","dark gray","dark gray",2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,
#   6,6,6,6,7,7,7,7,"brown","brown","brown","brown","purple","purple","purple","purple")


names = rep(c("2011", "2012", "2013","2014","2015","2016","2017"),9)

filename=met
filename=gsub("<", "", filename)
filename=gsub(":","",filename)

jpeg(paste("c:Boxplots/Barplot",filename,".jpg",sep=""), 12,10, units='in', res=600)

#dev.new(width=8, height=10)
#layout(matrix(c(1,2,3,4,5,6,7,8,9,10),1,1,4,2), heights=c(1,))
layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10), 4, 3, byrow = TRUE), heights=c(.5,2,2,2,2.5))

par(mar = c(.5,1,2,1))
plot.new()
title(met,cex.main=2)

ws=wsheds[1]
ws
for (ws in wsheds){

par(mar = c(5,5,2,2))

bar.data
bar.data[bar.data$Sub.Population == ws,]

bar.data.ws = bar.data[idx,][bar.data[idx,]$Sub.Population==ws,]
bar.data.ws = bar.data.ws[is.na(bar.data.ws$Mean)==F,]
bar.data.ws
names=bar.data.ws$Visit.Year
names

bar.data.ws
#bar.title=paste(strwrap(met,20),collapse="\n")
bar.title=met
col=match(ws, wsheds)
if (col==1){col="brown"}
if (col==9){col="purple"}
names
bar.data.ws

if (nrow(bar.data.ws) > 0){
a=barplot(bar.data.ws$Mean, pch=19,names=names,
main=ws,
cex.names=1,cex.lab=.8,
cex.axis=1, cex.main=1.5, las=2,col=col, 
ylab =met,
ylim=c(0, max(bar.data$Mean[idx]+2*bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)))



for (i in (1:length(a))){
lines(c(a[i],a[i]),
c(bar.data.ws$Mean[i]+1.96*bar.data.ws$Std.Error.of.Mean.Estimate[i],
  bar.data.ws$Mean[i]-1.96*bar.data.ws$Std.Error.of.Mean.Estimate[i]),
lw=2)}
} # end of "if nrow(bar.data.ws)  > 1

} # end of cycle through watersheds (for ws in wsheds)

dev.off()
}
#####################################



##############################################################
if (1==2) {
# Barplots by Assessment Unit 8/14/2015
dev.off()
mets
windows(record=T)
for (met in mets){

print(met)

names(bar.data)
bar.data = Results.Formatted[Results.Formatted$Metric == met,]
data.frame(bar.data$Metric, bar.data$Sub.Population)
is.na(bar.data$Metric)

bar.data = bar.data[is.na(bar.data$Metric) == "FALSE",]
nrow(bar.data)

names(bar.data)
AUs=levels(bar.data$Sub.Population)
AUs = AUs[AUs != " "]
AUs = AUs[AUs != "#N/A"]
AUs = AUs[AUs != "All.Sites"]
AUs = AUs[AUs %in%wsheds == "FALSE"]


AU=AUs[10]
AU
for (AU in AUs) {
AU.bar.data = bar.data[bar.data$Sub.Population == AU,]
AU.bar.data = AU.bar.data[is.na(AU.bar.data$Metric)=="FALSE",]
AU.bar.data

nrow(AU.bar.data)
AU.bar.data

if (nrow(AU.bar.data) > 0) {

idx = 
match(max(AU.bar.data$Mean+2*AU.bar.data$Std.Error.of.Mean.Estimate)
,( AU.bar.data$Mean+2*AU.bar.data$Std.Error.of.Mean.Estimate)
)

names = as.character(AU.bar.data$Visit.Year)
names
names[names=="Average of All Years"] = "Average"
met
a=barplot(AU.bar.data$Mean, names=names,
main=paste(met,"
for Assessment Unit", AU),
ylab=met,
cex.names=1,cex.lab=1,
cex.axis=1, cex.main=1, las=2, 
ylim=c(0, max(AU.bar.data$Mean[idx]+2*AU.bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)))




for (i in (1:length(a))){
lines(c(a[i],a[i]),
c(AU.bar.data$Mean[i]+1.96*AU.bar.data$Std.Error.of.Mean.Estimate[i],
  max(0,AU.bar.data$Mean[i]-1.96*AU.bar.data$Std.Error.of.Mean.Estimate[i])),
lw=2)}


} # if nrow AU.bar.data > 1
} # cycle through assessment units
} # cycle through metrics
######################



##############################################################
# Barplots by Assessment Unit within Watershed Average of all Years, 8/14/2015
dir.create("AU_Barplots")



met = mets[1]
for (met in mets){
for (shed in sheds) {
 print(met)

 names(bar.data)
 bar.data = Results.Formatted[Results.Formatted$Metric == met,]
 bar.data = bar.data[bar.data$Visit.Year=="Average of All Years",]

 sheds=levels(factor(data$WatershedName))


   	print(shed)

	idx=match(header$Subgroup.s.[1],colnames(data))
	codes = data[,idx][data$WatershedName == shed]
      AU_Type = colnames(data)[idx]
      AU_Type
	
	bar.data = bar.data[bar.data$Sub.Population %in% codes,]
	bar.data = bar.data[is.na(bar.data$Metric) == "FALSE",]
	names = as.character(bar.data$Sub.Population)
      print(nrow(bar.data))

if (nrow(bar.data) > 0) {


		idx = match(max(bar.data$Mean+2*bar.data$Std.Error.of.Mean.Estimate)
		,(bar.data$Mean+2*bar.data$Std.Error.of.Mean.Estimate))

filename = paste(AU_Type,"_", shed,"_", met, sep="")
filename = gsub("<", "LT", filename)
filename=gsub(":","",filename)


print(filename)
jpeg(paste("c:AU_Barplots/",filename,".jpg",sep=""), 10,8, units='in', res=600)

		a= barplot(bar.data$Mean,
		names=names,
		ylab=met,
		cex.names=1,cex.lab=1,
		cex.axis=1, cex.main=1, las=2, 
            col="cyan",
		main = paste(met, "in",shed,"by Assessment Unit"),
		ylim=c(0,max(bar.data$Mean[idx]+2*bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)))




for (i in (1:length(a))){
lines(c(a[i],a[i]),
c(bar.data$Mean[i]+1.96*bar.data$Std.Error.of.Mean.Estimate[i],
  max(0,bar.data$Mean[i]-1.96*bar.data$Std.Error.of.Mean.Estimate[i])),
lw=2)}

dev.off()
	} # end  of "if nrow(bar.data) > 0"
 } #end of "for (shed in sheds)"
} # end of "for met in mets"

######################

} # end of "if 1==2" to not run by assessment unit....










#############################################
# Write to a .pdf file
##############################################
mets = levels(Results.Formatted$Metric)
mets
pdf(file="barplots.pdf", width=11)
plot.new()

text(0,.5, adj=c(0,0),
"This document contains a set of plots and a large table summarizing the 
status and trend results for selected CHaMP metrics from 2011-2017, as 
discussed in the Status and Trend Summary report within the annual CHaMP-
ISEMP report.  Specific questions or requests for additional information 
or results summarized at different spatial or temporal scales may be 
addressed to: 

Matt Nahorniak, South Fork Research
(541)740-5487
matt@southforkresearch.org

"
)

met

for (met in mets){

bar.data = Results.Formatted[Results.Formatted$Metric == met,]


VY.SP = paste(bar.data$Visit.Year, bar.data$Sub.Population)
VY.SP
VY.SP.toMatch = paste(rep(c("2011","2012","2013","2014","2015","2016","2017"),9),
c(rep("Entiat",7),rep("John Day",7),rep("Lemhi",7),rep("Methow",7),
rep("South Fork Salmon",7),rep("Tucannon",7),
rep("Upper Grande Ronde",7),rep("Wenatchee",7),rep("Yankee Fork",7)))


idx = match(VY.SP.toMatch, VY.SP)

#col = c("dark gray","dark gray","dark gray","dark gray",2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,
#   6,6,6,6,7,7,7,7,"brown","brown","brown","brown","purple","purple","purple","purple")
names = rep(c("2011", "2012", "2013","2014","2015","2016","2017"),9)

filename=met
filename=gsub("<", "", filename)
filename=gsub(":","",filename)

#jpeg(paste("c:Boxplots/Barplot",filename,".jpg",sep=""), 8,10, units='in', res=600)

#dev.new(width=8, height=10)

# for 9 plots
layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10), 4, 3, byrow = TRUE), heights=c(.5,2,2,2,2.5))

# for 2 [plots
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights=c(.5,2,2,2,2.5))


par(mar = c(.5,2,2,2))
plot.new()
title(met,cex.main=2)

wsheds
ws=wsheds[1]
for (ws in wsheds){

par(mar = c(5,5,2,2))

bar.data.ws = bar.data[idx,][bar.data[idx,]$Sub.Population==ws,]
bar.data.ws = bar.data.ws[is.na(bar.data.ws$Mean)==F,]
bar.data.ws
names=bar.data.ws$Visit.Year
names

bar.data.ws
#bar.title=paste(strwrap(met,20),collapse="\n")
bar.title=met
col=match(ws, wsheds)
if (col==1){col="brown"}
if (col==9){col="purple"}

names
bar.data.ws
if (nrow(bar.data.ws) > 0){

a=barplot(bar.data.ws$Mean, pch=19,names=names,
main=ws,
cex.names=1,cex.lab=.8,
cex.axis=1, cex.main=1.5, las=2,col=col, 
ylab =met,
ylim=c(0, max(bar.data$Mean[idx]+2*bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)))


for (i in (1:length(a))){
lines(c(a[i],a[i]),
c(bar.data.ws$Mean[i]+1.96*bar.data.ws$Std.Error.of.Mean.Estimate[i],
  bar.data.ws$Mean[i]-1.96*bar.data.ws$Std.Error.of.Mean.Estimate[i]),
lw=2)}

} # end of "if (nrow(bar.data.ws) > 1)"
} # end of cycle through watersheds (for ws in wsheds)
}


#dev.off()


####################################################################
# Added this 3/5/2017
#grid.newpage()


tab.results = Results.Formatted

colnames(tab.results)[7]= "Std Err"
colnames(tab.results)[4]= "N CHaMP Sites"
colnames(tab.results)[8]= "Std Dev"
colnames(tab.results)[11]= "95% LCB"
colnames(tab.results)[12]= "95% UCB"
colnames(tab.results)[14]= "Trend Std Err"
colnames(tab.results)[15]= "Trend 95% LCB"
colnames(tab.results)[16]= "Trend 95% LCB"



#dev.off()

#if (1==2) {
nrow(tab.results)
#pdf("results_table.pdf",width=11)
i=1
for (i in 1:(nrow(tab.results)/25+1)){

grid.newpage()
print(paste("i=",i))
print(c(1+(i-1)*25):(i*25))
temp = tab.results[(1+(i-1)*25):min((i*25),nrow(tab.results)),]

grid.table(temp,gpar.coretext = gpar(fontsize=6), gpar.coltext = gpar(fontsize=6))
}
#}
dev.off()
#####################################
