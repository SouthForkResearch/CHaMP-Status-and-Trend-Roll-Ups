

library(spsurvey)
library(shapefiles)
library(survey)
dir.create("pdf plots")
dir.create("results files")



# Code to Define and Adjust GRTS weights for CHaMP sites and metrics
# January 2014
# Matt Nahorniak (matt@southforkresearch.org)





# Header file is used to specify file names containing data files,
# site eval files, sample frame files, and master sample frame file
header = read.csv("header.csv", header=T)

Years= na.omit(header$Years)
n.years = length(Years)
minyear= min(Years)

# Calling this "pop.frame", but it's reallyt the sample frame. This is used
# to link SiteName from the data file(s) to the stratum, given in the pop frame file.
pop.frame = read.csv(as.character(header$Frame.File.Names[1]), header=T)


# Pull list of watersheds to analyze from header file
watersheds = as.character(header$watersheds[header$watersheds !=""])


## initialize for later
#Strata.Extents.All.Watersheds2011 = data.frame(watershed="na", "Strata"="na", "Extents"=0)
#Strata.Extents.All.Watersheds2012 = Strata.Extents.All.Watersheds2011
#Strata.Extents.All.Watersheds2013 = Strata.Extents.All.Watersheds2011



#######################################################################
# Read all metric files and combine into one file covering all years
# Right now each file has different columns, so this is a pain....

#######################################################################
##### read data files, and make one data file that combines all years

## Build an additional Metric.File.Name and Eval.File.Name that
## combines all years.  Write these to .csv files and use them
## just as other files are used.

#names(header)
#datafiles = header$Data.File.Names.by.Year
#datafiles = datafiles[datafiles !=""]
#datafiles = as.character(datafiles)
#datafiles

## read the data files and build a single data frame "data" from all years' data
#data = read.csv(datafiles[1], header=T)
#data$VisitYear = minyear
#for (k in 2:length(datafiles)){
#
#datafiles
#datanew = read.csv(datafiles[k], header=T)
#datanew$VisitYear = rep((minyear-1+k), nrow(datanew))
#data = rbind(data, datanew)
#}

########################################################

mets = read.csv("Metrics Database/MetricVisitInformation.csv", header=T)
covars = read.csv("Metrics Database/MetricAndCovariates.csv", header=T)

names(mets)
names(covars)
nrow(mets)
nrow(covars)
ncol(mets)
ncol(covars)
# merge metrics and covariates  

data = merge(covars, mets, by="VisitID",suffixes = c("",".y"))
data$WatershedName
data$VisitYear


#Temp - add Minam to this list for Seth
data = data[data$WatershedName %in% c(
"Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon",
"Yankee Fork", "Minam"),]}

# Order teh watershed name levels
factor(data$Watershed, levels=c("Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon","Yankee Fork"))
(data$WatershedName)


#data.frame(data$SiteName, data$VisitStatus, data$AnlSub_TrtStat_2013, data$Grad)

# filter by QA status
# TEMP!!! Removed for Yankee Fork Analysis
#data = subset(data, VisitStatus=="Released to Public")
data = subset(data, Primary.Visit=="Yes")

Primary Visit


names(data)
nrow(data)



#jdsites =levels(factor(data$SiteName[data$WatershedName == "John Day" & data$VisitYear == 2013]))
#jdsites
#names(pop.frame)
#pop.frame$Site.ID

#jdstrat = levels(factor(pop.frame$Stratum[match(jdsites, pop.frame$Site_ID)]))
#jdstrat
#data.frame(jdlevs)
#names(data)


#############################################################3
names(data)

# Filter and reorder watershed names for boxplots
data = data[data$WatershedName %in% c(
"Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon","Yankee Fork"),]
# Order teh watershed name levels
factor(data$Watershed, levels=c("Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon","Yankee Fork"))
(data$WatershedName)

####################################################

# Some junk to account for constantly changing column names.
data$Watershed = as.character(data$WatershedName)
#data$Watershed[data$Watershed=="Methow "] = "Methow"
data$Watershed = factor(data$Watershed)
levels(data$Watershed)

data$VisitYear
write.csv(data,"Metrics_and_Covariates_All_Data.csv")
###########################################################3

nrow(data)
names(data)


# Do some clean up of the data.  Only first visit number used.
data = data[data$Primary.Visit =="Yes",]

# To Do:  Update this (below doesn't work cause column names change on cm.org
#data = data[data$Primary.Visit  =="Yes",]
#data = data[data$CHaMP.Core == "Yes",]
#data = data[data$Target=="Target",]

# Only carry data from watersheds of interest
data=data[data$Watershed %in% watersheds,]
nrow(data)
#data$VisitYear

###########################
# initialize



All.Site.Evals = read.csv(as.character(header$Evaluation.File.Names[n.years]), header=T)

for (y in (n.years-1):1) {
  temp = read.csv(as.character(header$Evaluation.File.Names[y]), header=T)
  All.Site.Evals = rbind(All.Site.Evals, temp)
}
All.Site.Evals$Site.ID
sites = levels(All.Site.Evals$Site.ID)
n.sites = length(sites)


# initialize evals for one-row-per-site
All.Evals = All.Site.Evals[1:n.sites,]

n.sites
# Specify priority level for picking a single evaluation from [possible] multiple
# evaluations (year after year)

sites

names(All.Site.Evals)

All.Site.Evals$Site.ID
k=1
for (k in 1: n.sites){
print(k)
	temp =(All.Site.Evals[All.Site.Evals$Site.ID == sites[k],])


priority = 
    1* (temp$GRTS.Eval.Status == "Target - Sampled") +
    2* (temp$GRTS.Eval.Status == "Target - Not Sampled") +
    3* (temp$GRTS.Eval.Status == "Non-Target") +
    4* (temp$GRTS.Eval.Status == "Not Evaluated") 

	index = 1:(nrow(temp))
      pointer = index[priority == min(priority)][1]

	All.Evals[k,] = temp[pointer,]
     }

write.csv(All.Evals, "SITEEVALUATION_AllYears.csv")

############################





# Initialize wgt
data$wgt = rep(NA, nrow(data))

names(pop.frame)
# Assign Stratum based on file CV provided.
idx = match(paste(data$SiteName, data$VisitYear), paste(pop.frame$Site_ID, pop.frame$Year))
data$Stratum = pop.frame$Stratum[idx]
pop.frame$Stratum[pop.frame$WatershedName == "Lemhi" & pop.frame$Year==2013]

#data.frame(data$SiteName, data$VisitYear,data$Stratum)

names(data)
data$SiteName
nrow(data)
data$Stratum
data = data[(is.na(data$Stratum) == F),]
data = data[data$Stratum !="",]
# Remove data if I don't have a stratum.  Ouch!

data$VisitYear

###############################################################
# Now I need to take the mean and find trend of all quantitative variables


sites = levels(factor(data$SiteName))
length(sites)
Data_Mean = data[1:length(sites),]
Data_Mean$wgt = rep(0, nrow(Data_Mean))
Data_YY_Trend = Data_Mean
Data.adj.wgt = data # used to store adj.wgts later on a per-metric basis

#Get first row for every site, from which to assign site Name and other
#Categorical info
index = match(sites, data$SiteName)
index
sites

length(sites)

i=1
index



for (i in 1:length(index)){

Data_Mean[i,] = data[index[i],]
Data_YY_Trend[i,] = data[index[i],]

#Set all numeric values to NA

for (j in 1:ncol(data)){
if (is.numeric(data[i,j])){
 Data_Mean[i,j] = NA
 Data_YY_Trend[i,j] = NA
}}
}

for (j in 1:ncol(data)){
 if (is.numeric(data[,j])) {
 Data.adj.wgt[,j] = rep(NA, nrow(data))}
}
Data.adj.wgt$VisitYear = data$VisitYear
#Set adj.wgt dataframes for later
 Data_Mean.adj.wgt = Data_Mean
 Data_YY_Trend.adj.wgt = Data_Mean

#Calculate site level means and trends for analysis
for (i in 1:nrow(Data_Mean)) {
print(paste("row",i,"of", nrow(Data_Mean)))

for (j in na.omit(match(levels(factor(header$Metric.List)), colnames(data)))){
site.data = data[data$SiteName == Data_Mean$SiteName[i],]
site.data
site.data[j]
 Data_Mean[i,j] = mean(site.data[,j], na.rm=T)
if (length(na.omit(site.data[,j])) > 1) {
 Data_YY_Trend[i,j]=coefficients(lm(site.data[,j] ~ site.data$VisitYear))[2]
} # end of "if" everything is numeric and more than one site from which to calc trend
Data_Mean$x_albers[i] = mean(site.data$x_albers, na.rm=T)
Data_Mean$y_albers[i] = mean(site.data$y_albers, na.rm=T)
Data_YY_Trend$x_albers[i] = mean(site.data$x_albers, na.rm=T)
Data_YY_Trend$y_albers[i] = mean(site.data$y_albers, na.rm=T)
} # end of loop through metrics

} # end of loop through sites



#Assign Stratum for Data_Mean and Data_YY_Trend
# Assign Stratum

idx = match(paste(Data_Mean$SiteName, 2013), paste(pop.frame$Site_ID, pop.frame$Year))

Data_Mean$Stratum = pop.frame$Stratum[idx]
# Remove data if I don't have a stratum.  Ouch!

#Assign Stratum for Data_Mean and Data_YY_Trend
# Assign Stratum
idx = match(paste(Data_YY_Trend$SiteName, 2013), paste(pop.frame$Site_ID, pop.frame$Year))
idx
Data_YY_Trend$Stratum = pop.frame$Stratum[idx]
#Data_YY_Trend = Data_YY_Trend[(is.na(Data_YY_Trend$Stratum) == F),]
#Data_YY_Trend = Data_YY_Trend[Data_YY_Trend$Stratum !="",]
# Remove data if I don't have a stratum.  Ouch!







Data_Mean$Stratum

write.csv(Data_YY_Trend, "METRICS_Trends.csv")
write.csv(Data_Mean, "METRICS_Ave_of_All_Years.csv")

# all.years is the name of the file with all years data, average
# over all years.  Now I've got to combine site-eval file for all years
#















################################################################
################################################################

##########
############
############
############

#### HERE!!!! #####
# Loop through watersheds.  


# Figure out Extents for each Strata, for weights



#####################################################
# Find weights for data (by year) and mean and trend 
#Need to uniquely find Weights for each set
#########################################################
#############################################################
# Read Shape File from which strata extents are obtained

shape = read.dbf(paste("Frame Files/",header$Frame.File.Names[2],".dbf",sep="" ))
#shape$dbf= shape$dbf[shape$dbf$Target2013 == "Target",]

#####################################################################
####### Checks - frame length by watershed x stratum ################
# Note - not yet sorted by "target".  That happens below, by year ###
tapply(shape$dbf$LengthKM, shape$dbf$CHaMPshed, sum)

Lem=shape$dbf[shape$dbf$CHaMPshed=="Lemhi",]
Wen=shape$dbf[shape$dbf$CHaMPshed=="Wenatchee",]
Tuc=shape$dbf[shape$dbf$CHaMPshed=="Tucannon",]
SFS=shape$dbf[shape$dbf$CHaMPshed=="South Fork Salmon",]
UGR=shape$dbf[shape$dbf$CHaMPshed=="Upper Grande Ronde",]
JD=shape$dbf[shape$dbf$CHaMPshed=="John Day",]
Ent=shape$dbf[shape$dbf$CHaMPshed=="Entiat",]
Met=shape$dbf[shape$dbf$CHaMPshed=="Methow",]
YF=shape$dbf[shape$dbf$CHaMPshed=="Yankee Fork",]

tapply(Wen$LengthKM, factor(Wen$AStrat2011),sum)
tapply(Wen$LengthKM, factor(Wen$AStrat2012),sum)
tapply(Wen$LengthKM, factor(Wen$AStrat2013),sum)
tapply(Wen$LengthKM, factor(Wen$AStrat2014),sum)

tapply(Lem$LengthKM, factor(Lem$AStrat2011),sum)
tapply(Lem$LengthKM, factor(Lem$AStrat2012),sum)
tapply(Lem$LengthKM, factor(Lem$AStrat2013),sum)
tapply(Lem$LengthKM, factor(Lem$AStrat2014),sum)

tapply(Tuc$LengthKM, factor(Tuc$AStrat2011),sum)
tapply(Tuc$LengthKM, factor(Tuc$AStrat2012),sum)
tapply(Tuc$LengthKM, factor(Tuc$AStrat2013),sum)
tapply(Tuc$LengthKM, factor(Tuc$AStrat2014),sum)

tapply(SFS$LengthKM, factor(SFS$AStrat2011),sum)
tapply(SFS$LengthKM, factor(SFS$AStrat2012),sum)
tapply(SFS$LengthKM, factor(SFS$AStrat2013),sum)
tapply(SFS$LengthKM, factor(SFS$AStrat2014),sum)

tapply(UGR$LengthKM, factor(UGR$AStrat2011),sum)
tapply(UGR$LengthKM, factor(UGR$AStrat2012),sum)
tapply(UGR$LengthKM, factor(UGR$AStrat2013),sum)
tapply(UGR$LengthKM, factor(UGR$AStrat2014),sum)

tapply(YF$LengthKM, factor(YF$AStrat2011),sum)
tapply(YF$LengthKM, factor(YF$AStrat2012),sum)
tapply(YF$LengthKM, factor(YF$AStrat2013),sum)
tapply(YF$LengthKM, factor(YF$AStrat2014),sum)

tapply(Ent$LengthKM, factor(Ent$AStrat2011),sum)
tapply(Ent$LengthKM, factor(Ent$AStrat2012),sum)
tapply(Ent$LengthKM, factor(Ent$AStrat2013),sum)
tapply(Ent$LengthKM, factor(Ent$AStrat2014),sum)

tapply(Met$LengthKM, factor(Met$AStrat2011),sum)
tapply(Met$LengthKM, factor(Met$AStrat2012),sum)
tapply(Met$LengthKM, factor(Met$AStrat2013),sum)
tapply(Met$LengthKM, factor(Met$AStrat2014),sum)

tapply(JD$LengthKM, factor(JD$AStrat2011),sum)
tapply(JD$LengthKM, factor(JD$AStrat2012),sum)
tapply(JD$LengthKM, factor(JD$AStrat2013),sum)
tapply(JD$LengthKM, factor(JD$AStrat2014),sum)

######################### end of checks ################################

shape$dbf$AStrat2014

# Assign Strata to combine watershedname and stratum.
#shape$dbf$WS_Strata2011= paste(shape$dbf$CHaMPshed, shape$dbf$Strata2011)
#shape$dbf$WS_Strata2012= paste(shape$dbf$CHaMPshed, shape$dbf$Strata2012)
#shape$dbf$WS_Strata2013= paste(shape$dbf$CHaMPshed, shape$dbf$Strata2013)
#shape$dbf$WS_Strata2014= paste(shape$dbf$CHaMPshed, shape$dbf$Strata2014)
shape$dbf$WS_Strata2011= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2011)
shape$dbf$WS_Strata2012= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2012)
shape$dbf$WS_Strata2013= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2013)
shape$dbf$WS_Strata2014= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2014)

names(shape$dbf)

dbf2011 = shape$dbf
dbf2012 = shape$dbf
dbf2013 = shape$dbf
dbf2014 = shape$dbf
dbf2011 = shape$dbf[shape$dbf$Target2011 == "Target",]
dbf2012 = shape$dbf[shape$dbf$Target2012 == "Target",]
dbf2013 = shape$dbf[shape$dbf$Target2013 == "Target",]
dbf2014 = shape$dbf[shape$dbf$Target2014 == "Target",]

#frame lengths by stratum
tapply(dbf2014$LengthKM, dbf2014$CHaMPshed, sum)


k=1
for (k in 1:3) {
if (k==1) {temp.data = data}
if (k==2) {temp.data = Data_Mean
           temp.data$VisitYear = rep(2013, nrow(temp.data))
} #Use 2013 Frame Info for means
if (k==3) {temp.data = Data_YY_Trend
           temp.data$VisitYear = rep(2013, nrow(temp.data))
} #Use 2013 Frame Info for trends


i=1
for (i in 1:nrow(temp.data)){
print(paste(i, "of", nrow(temp.data)))
strat = paste(temp.data$Watershed[i], temp.data$Stratum[i])
# Find strata extents and n, repeat for each year since it's
# a different metric name for each year.  A bit of brute-force
# programming here that could be streamlined quite a bit.

temp.data$VisitYear
strat

levels(factor(dbf2011$WS_Strata2011))

if (temp.data$VisitYear[i] == 2011) {
   strat.dbf = dbf2011[dbf2011$WS_Strata2011 == strat,]}

if (temp.data$VisitYear[i] == 2012) {
   strat.dbf = dbf2012[dbf2012$WS_Strata2012 == strat,]}

if (temp.data$VisitYear[i] == 2013) {
   strat.dbf = dbf2013[dbf2013$WS_Strata2013 == strat,]}


if (temp.data$VisitYear[i] == 2014) {
   strat.dbf = dbf2014[dbf2014$WS_Strata2014 == strat,]}

dbf2013$WS_Strata2013
dbf2014$WS_Strata2014

   temp.data$Strata.extent[i] = sum(strat.dbf$LengthKM, na.rm=T)

   temp.data$Strata.n[i] =  sum(1* 
     (paste(temp.data$Watershed, temp.data$Stratum, temp.data$VisitYear)==
      paste(strat, temp.data$VisitYear[i])))
}

data.frame(temp.data$wgt, temp.data$VisitYear)

temp.data$wgt = temp.data$Strata.extent / temp.data$Strata.n



if (k==1) {data$wgt= temp.data$wgt}
if (k==2) {Data_Mean$wgt = temp.data$wgt}
if (k==3) {Data_YY_Trend$wgt = temp.data$wgt}
}
#########################################################333




tapply(dbf2011$LengthKM[dbf2011$CHaMPshed=="John Day"], 
 factor( dbf2011$Strata2011[dbf2011$CHaMPshed=="John Day"]), sum)



## Checks....

########################


#Now set weights to zero for any non-target sites.  This WILL REDUCE the
#total frame length, as it should


year=1
############################################################
# cycle through each year, plus one extra time where we'll combine all years

# looping through all the years to get
# each year, plus an extra each
# for site mean across years, and site level trend

# the "+2" includes 1) average across years, and 2) trend across years
# Note: if data is missiung in any years, program will crash.
# if not 2011 data, run from year 2 to n.years +2


year=1
year=2
year=4
year
n.years
#for (year in 1:n.years) {
for (year in 1:(n.years+2)){

print(paste("year=", year))

if (year <= n.years) {
names(header)
Eval.File.Name = as.character(header$Evaluation.File.Names[year])
Eval = read.csv(Eval.File.Name, header=T)
idx = match(paste(data$SiteName, data$VisitYear), paste(Eval$Site.ID, (year+minyear-1)))
idx
for (k in 1:length(idx)){
 if (is.na(idx[k]) ==F) {data$GRTS.Eval.Status[k] = Eval$GRTS.Eval.Status[idx[k]]} 
}
}

if (year > n.years) {
Eval.File.Name = "SITEEVALUATION_AllYears.csv"
Eval = read.csv(Eval.File.Name, header=T)
idx = match(Data_Mean$SiteName, Eval$Site.ID)
idx
Data_Mean$GRTS.Eval.Status = Eval$GRTS.Eval.Status[idx]
Data_YY_Trend$GRTS.Eval.Status = Eval$GRTS.Eval.Status[idx]
}
}


#data[data$Watershed=="John Day",]

data$wgt[data$GRTS.Eval.Status == "Non-Target"]=0
Data_Mean$wgt[Data_Mean$GRTS.Eval.Status == "Non-Target"]=0
Data_YY_Trend$wgt[Data_YY_Trend$GRTS.Eval.Status == "Non-Target"]=0











# Now I have weights, and "non-target" sites have been removed and
# total stratum extents similarly reduced.
# Now time to go metric by metric, adjusting weights as needed for missing data
# on a per-metric basis.  Will make files corresponding to data files with
# just adjusted weights for each metric.


################# HERE ####################
################################################3

year
year=1
for (year in 1:(n.years + 2)) {
#for (year in 4:(n.years + 2)) {



if (year <= n.years) {
outputfile = paste("CHaMP","_",as.character(minyear-1+year),".csv", sep="")
metrics = data[data$VisitYear == (minyear-1+year),]
} else {
# +1 is for site mean across years
if (year == (n.years+1)) {
  outputfile = paste("CHaMP","_All_Years.csv", sep="")
  metrics = Data_Mean} else {
# +2 is for site level year-year trends
  outputfile = paste("CHaMP","Y_Y_Trend.csv", sep="")
  metrics = Data_YY_Trend}
}

metrics$WatershedName

###############################################################
# Go through metrics one by one, and account for any missing values
# By adjusting adjusted weigths on a per-stratum basis

metric.list = as.character(header$Metric.List)
metric.list = metric.list[metric.list != ""]

metric.list
##################################################################
# Loop through metrics one by one (as we'll have to check for
# needed wgt adjustments for each metric, in case of NA values

############### HERE ###################
sum(metrics$wgt)
metric.list
i=1
i=5
metrics.saved = metrics
i


for (i in 1: length(metric.list)) {
metrics = metrics.saved
metric.name = metric.list[i]
print(metric.name)

metric.name
values = metrics[,colnames(metrics) == metric.name]
values

sum(metrics$wgt)
(metrics$Stratum)
metrics$WS_Stratum = paste(metrics$Watershed, metrics$Stratum)
metrics$Watershed
metrics$WS_Stratum
names(metrics)

Strat.totals = tapply(metrics$wgt, as.factor(as.character(metrics$WS_Stratum)), sum)
Strat.totals
sum(Strat.totals)
values
is.na(values)
use = 1*(is.na(values) == F)
data.frame(use, metrics$WatershedName)



nrow(Data_Mean)
nrow(Data_Mean.adj.wgt)

metrics$WS_Stratum


n.by.strat = tapply(use, as.factor(as.character(metrics$WS_Stratum)), sum)
n.by.strat

Strat.totals
new.adjwgt.by.strat = Strat.totals / n.by.strat
new.adjwgt.by.strat[n.by.strat ==0] = 0

n.by.strat
new.adjwgt.by.strat
sum(new.adjwgt.by.strat*n.by.strat)

metric.adjwgt = rep(0, length(metrics$WS_Stratum))


for (j in 1:length(metrics$Stratum)){
metric.adjwgt[j] = 
new.adjwgt.by.strat[names(n.by.strat) == metrics$WS_Stratum[j]]
}

metric.adjwgt

metric.adjwgt = metric.adjwgt * use
sum(metric.adjwgt)

metrics$wgt = metric.adjwgt

# Record all the adjusted weights, on a per-metric basis, to a data.frame that
# matches, exactly, the size (rows, columns) and format of the orginal data frame.
# Will write these files for later use.


if (year < (n.years+1))  {
col=match(metric.name,colnames(Data.adj.wgt))
col
rows = match(paste(metrics$SiteName, year+minyear-1), paste(Data.adj.wgt$SiteName, Data.adj.wgt$VisitYear))
Data.adj.wgt[rows, col] = metric.adjwgt
}
if (year == (n.years+1)){
col=match(metric.name,colnames(Data_Mean.adj.wgt))
Data_Mean.adj.wgt[, col] = metric.adjwgt
}


if (year == (n.years+2)){
col=match(metric.name,colnames(Data_YY_Trend.adj.wgt))
Data_YY_Trend.adj.wgt[, col] = metric.adjwgt
}



metric.name
watershed.metrics = metrics
watershed.metrics$wgt
use = use * (watershed.metrics$wgt != 0)
use
data.frame(watershed.metrics$wgt, use)
# Now build spsurvey data.frames and run spsurvey analysis tools with calculated weights

my.sites <- data.frame(siteID=watershed.metrics$SiteName, Use=(use==1))
my.sites

names(watershed.metrics)
watershed.metrics$SiteName
watershed.metrics$wgt
watershed.metrics$x_albers
watershed.metrics$y_albers


my.design <- data.frame(siteID=watershed.metrics$SiteName,
                        wgt=watershed.metrics$wgt,
                         xcoord=watershed.metrics$x_albers,
                         ycoord=watershed.metrics$y_albers)

my.design


subgroups = (header$Subgroup.s.)
subgroups = subgroups[subgroups != ""]
subgroups
names(watershed.metrics)
subpop.index = match(subgroups, names(watershed.metrics))
subpop.index

# Build dataframe with all subgroups specified, along with "all sites" and "watershed"
Subgroups = data.frame( 
  "All.Sites" =rep("All.Sites",length(watershed.metrics$SiteName)),
  "Watershed"= metrics$Watershed)

subpop.index[is.na(subpop.index)==F]
if (length(subpop.index[is.na(subpop.index)==F]) > 0) {

for (l in 1:length(subpop.index)) {
Subgroups[,l+2]= watershed.metrics[,subpop.index[l]]
names(Subgroups)[l+2] = as.character(names(watershed.metrics)[subpop.index[l]])

# If stratum, combine watershed and stratum, since stratum are watershed specific but share
# common names across watersheds.
if (names(watershed.metrics[subpop.index[l]])=="Stratum") {
  Subgroups$Stratum = paste(metrics$Watershed, ".",watershed.metrics[,subpop.index[l]],sep="")
}
}

} # end of (if (length(subpop.index) > 1)...

names(header)
Subgroups

header$Subgroup.s.

my.subpop <- data.frame(siteID=watershed.metrics$SiteName, Subgroups)
#				  All.Sites= rep("All.Sites",length(watershed.metrics$SiteName)),
#                          Watershed = metrics$Watershed,
#				  Stratum = paste(metrics$Watershed, ".",watershed.metrics[,subpop.index],sep=""))
my.subpop

data.frame(my.subpop, my.design,values)


# Note - do NOT log metric for sp.survey, in most cases.  spsurvey does not require
# assumption of normality.  log metric is specified in header for variance decomposition,
# which is not part of this script.

cont.data.values = values


my.cont.data <- data.frame(siteID=watershed.metrics$SiteName,cont.data.values)   
colnames(my.cont.data)[2] = metric.name


my.cont.data

Results = 0
# Don't run if data is empty
length(is.na(my.cont.data[,2])==F)

if (nrow(my.cont.data[is.na(my.cont.data[,2])==F,]) < 2) {
print(paste("i=", i," empty my.cont.data")) }

if (nrow(my.cont.data[is.na(my.cont.data[,2])==F,]) > 1) {

################################################################################
################### Make a weighted boxplot of metric by watershed ######################
 if (1==2) {
if ((year == (n.years+1))|(year==(n.years+2))) {
n.years

names(my.design)
names(my.cont.data)
names(my.subpop)
my.design$wgt
my.design$siteID

nrow(my.design)
nrow(my.cont.data)
nrow(my.subpop)
use
bp.data = data.frame("SiteID" = my.design$siteID[use==T]
, weights = my.design$wgt[use==T],              "watershed" = my.subpop$Watershed[use==T], value= my.cont.data[,2][use==T])
bp.data

bp.data$watershed = as.character(bp.data$watershed)
bp.data$watershed[bp.data$watershed=="South Fork Salmon"] = "Secesh"
bp.data$watershed[bp.data$watershed=="Upper Grande Ronde"] = "UGR"

#bp.data$idx=rep(1, nrow(bp.data))
#bp.data$idx

#bp.data$idx[bp.data$watershed=="Wenatchee"] = 1
#bp.data$idx[bp.data$watershed=="Entiat"] = 2
#bp.data$idx[bp.data$watershed=="Methow"] = 3
#bp.data$idx[bp.data$watershed=="John Day"] = 4
#bp.data$idx[bp.data$watershed=="UGR"] = 5
#bp.data$idx[bp.data$watershed=="Tucannon"] = 6
#bp.data$idx[bp.data$watershed=="Lemhi"] = 7
#bp.data$idx[bp.data$watershed=="Secesh"] = 8
#
#bpidx = order(bp.data$idx)
#bp.data = bp.data[bpidx,]



bp.data
#bp.data = bp.data[is.na(bp.data$value) == F,]
bp.data$watershed=factor(bp.data$watershed, c("Wenatchee", "Entiat", "Methow", "John Day", "UGR", "Tucannon", "Lemhi", "Secesh","Yankee Fork"))

bp.design = svydesign(id = my.design$siteID[use==T], weights = my.design$wgt[use==T], data= bp.data)

dir.create("c:Boxplots")
bp.name = paste("c:Boxplots/", metric.name,".jpg",sep="")
if (year==5) {bp.name=paste("c:Boxplots/Trend_", metric.name,".jpg",sep="")}

if (bp.name ==  "c:Boxplots/Substrate <6mm.jpg") {bp.name = "c:Boxplots/Substrate LT 6mm.jpg"}
jpeg(bp.name, 10,6, units='in', res=600)


d.name=header$Display.Name[match(metric.name, header$Metric.List)]
if (year==4) {d.name=paste("Status:",d.name)}

if (year==5) {d.name=paste("Trend:",d.name)}
bp.data
#bp.data = bp.data[is.na(bp.data$value == F),]
levels(factor(bp.data$watershed))
dev.off()
boxplot(bp.data$value ~ bp.data$watershed)
bp.data$value
svyboxplot(value ~ watershed, design  = bp.design, data=bp.data,
main=paste(d.name,": 2011-2014", sep=""),col="cyan")
d.name

#boxplot(bp.data$value ~ bp.data$watershed)

dev.off()
}
} #if (1==2) to not run boxplots.  Can't unless we run all CHaMP Sheds.
###########################################################################




Results <- cont.analysis(sites=my.sites,
 subpop = my.subpop,
design = my.design, data.cont = my.cont.data, total=FALSE)
head(Results$Pct)

Results$Pct
###############################################
# Change names in "Results" to "Display Names"

Results$CDF$Indicator=header$Display.Name[match(Results$CDF$Indicator, header$Metric.List)]

###############################################

names(my.subpop)
# Get subpop extents to include with output summary
subpops=levels(as.factor(paste(my.subpop$Watershed,as.character(my.subpop$subpop))))
subpop.extents =tapply(my.design$wgt,paste(my.subpop$Watershed,as.factor(as.character(my.subpop$subpop))), sum)
# add another row for "total"
subpop.extents = c(subpop.extents, sum(subpop.extents))
names(subpop.extents)[length(subpop.extents)] = "All.Sites"
subpop.extents
subpop.extents[is.na(subpop.extents)] = 0
subpop.extents[names(subpop.extents)=="All.Sites"] = 
  sum(subpop.extents[names(subpop.extents)!= "All.Sites"])
sum(subpop.extents)



#Make pdf plots
outputfile
pdf.filename = paste("pdf plots/",gsub(".csv", "",outputfile),"_",metric.name,"_CDF.pdf",sep="")
pdf.filename



#
if (year == 4) {
cont.cdfplot(pdf.filename, Results$CDF, cdf.page=1)
}
Results_to_write = Results$Pct[Results$Pct$Statistic == "Mean",]
Results_to_write$Var = Results$Pct$Estimate[Results$Pct$Statistic == "Variance"]
Results_to_write$Var_Std_Error = Results$Pct$StdError[Results$Pct$Statistic == "Variance"]
#Results_to_write$Watershed = rep(watershed, nrow(Results_to_write))
Results_to_write$Year = rep(year, nrow(Results_to_write))
Results_to_write$Metric.Name = rep(metric.name, nrow(Results_to_write))
index =match(Results_to_write$Subpopulation, names(subpop.extents))
Results_to_write$Subpop.Extent = subpop.extents[index]

print(paste("year=",year))



i
if(i==1) {
Watershed.Results = Results$Pct
names(Watershed.Results)
Watershed.Results = Watershed.Results[NULL,]}

if (nrow(Watershed.Results)==0) {
	 Watershed.Results = Results$Pct

    if(year==1)
{
   Summary_Results = Results_to_write } else {
     Summary_Results = data.frame(rbind(Summary_Results, Results_to_write))
}
} else {
      Summary_Results = data.frame(rbind(Summary_Results, Results_to_write))
Watershed.Results$Indicator
 	Watershed.Results$Indicator <-  factor(Watershed.Results$Indicator,levels = levels(as.factor(metric.list)) )

      Watershed.Results[(nrow(Watershed.Results)+1):(nrow(Watershed.Results)+nrow(Results$Pct)),1:9]=Results$Pct
}

} # end of "don't run cont.analysis if no data or only 1 row of data"

} # end of loop through metrics


head(Watershed.Results)
Watershed.Results
Watershed.Results$Indicator = as.character(Watershed.Results$Indicator)
Watershed.Results$Indicator =header$Display.Name[match(Watershed.Results$Indicator, header$Metric.List)]


#header$Display.Name[match(Results$Pct$Indicator, header$Metric.List)]

write.csv(Watershed.Results, paste("results files/",outputfile,sep=""))
outputfile

} # end of cycle through years





#write.csv(Strata.Extents.All.Watersheds2013,"Strata.Extents.All.Watersheds2013.csv")



# To DO:
# Write file of adjusted weights corresponding to data, Data_Mean, and Data_YY_Trend

write.csv(data, "Metrics_and_Covariates.csv")
write.csv(Data.adj.wgt, "CHaMP_Data_All_AdjWgt_by_Metric.csv")
write.csv(Data_Mean.adj.wgt, "CHaMP_Data_Mean_AdjWgt_by_Metric.csv")
write.csv(Data_YY_Trend.adj.wgt, "CHaMP_Data_Trend_AdjWgt_by_Metric.csv")





  