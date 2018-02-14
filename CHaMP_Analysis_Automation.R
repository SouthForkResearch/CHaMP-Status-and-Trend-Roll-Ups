# Code to Define and Adjust GRTS weights for CHaMP sites and metrics
# January 2014, last modified Dec 2017
# Matt Nahorniak (matt@southforkresearch.org)

# Note: After running this code, run "Format GRTS Results" to convert output into the nice format
# used for the annual writeup.  This code generates a full set of results, but the latter code trims it down and
# cleans up the output for the report, makes plots, etc.


# Instructions for adding a year
# Add additional year to header.csv file
# Download "SITEEVALUATION_20xx.csv" from cm.org for new year and add it to folder
# Download results database and export new version of MetricVisitINformation and MetricsandCovariates" from database on cm.org, convert to .csv files
# Get file w/ updated strata by site_ID: something like the file "CHaMPSites_AnalysisStrata_20150415.csv", and add that file to the header file.
# Note several points in the code where the year needs to be updated.  See in-line comments.

# Load required libraries
library(spsurvey)
library(shapefiles)
library(survey)

# Create required directories
dir.create("pdf plots")
dir.create("results files")




# Header file is used to specify file names containing data files,
# site eval files, sample frame files, and master sample frame file
header = read.csv("header.csv", header=T)
sg=header$Subgroup.s.[1]
Years= na.omit(header$Years)
# Figure out how many years we're rolling up
n.years = length(Years)
minyear= min(Years)


# Calling this "pop.frame" for outdated reasons.  This is a file with strata names by site_ID, used
# to link SiteName from the data file(s) to the stratum, given in the pop frame file.
pop.frame = read.csv(as.character(header$Frame.File.Names[1]), header=T)

# Pull list of watersheds to analyze from header file
watersheds = as.character(header$watersheds[header$watersheds !=""])
watersheds


#######################################################################
# Read all metric file(s)
# In years past these have come from multiple files.  By 2017 this should all be in one file
# or a single file is created externally.  "MetricVisitInformation.csv" contains the CHaMP metrics.
# MetricsAndCovariates.csv" contains x_albers, y_albers, primary.visit, linkable by VisitID.  
# This file used to be available on cm.org.  As of 2017, it needed to be manually constructed from
# data provided by Carol.  I'm not confident the format and filename provided by Carol will be used
# in the future (if indeed this analysis is done at all in the future), so I'm simply re-creating
# MetricsAndCovariates.csv (as well as MetricVisitInformation.csv") manually from Carol's data and
# using the code as-is rather than modifying the code to use the new files from Carol.



########################################################

# Make sure this points to the most up to date data file!!!
mets = read.csv("Metrics Database/GRTSData_20180212.csv", header=T)
nrow(mets)
#mets = read.csv("Metrics Database/MetricVisitInformation.csv", header=T)
covars = read.csv("Metrics Database/MetricAndCovariates.csv", header=T)

# merge metrics and covariates  
data = merge(covars, mets, by="VisitID",suffixes = c("",".y"))
data = data[is.na(data$x_albers)=="FALSE",]
nrow(data)

##Temp - add Minam to this list for Seth
#data = data[data$WatershedName %in% c(
#"Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon",
#"Yankee Fork", "Minam"),]


#############################################################3

# Don't use this, but use code above, if running Minam
# Filter and reorder watershed names for boxplots
data = data[data$Watershed %in% c(
"Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon","Yankee Fork"),]
# Order the watershed name levels
#factor(data$Watershed, levels=c("Wenatchee", "Entiat", "Methow", "John Day", "Upper Grande Ronde", "Tucannon", "Lemhi", "South Fork Salmon","Yankee Fork"))

nrow(data)
####################################################

#data$Watershed
#factor(data$Watershed)
# Some junk to account for constantly changing column names.
#data$Watershed = as.character(data$WatershedName)
data$Watershed = factor(data$Watershed)
nrow(data)
#data$Watershed
# Write the combined data to a file
write.csv(data,"Metrics_and_Covariates_All_Data.csv")
###########################################################3


#
# Do some clean up of the data.  
# Only primary visit is used.

# Carol's new data file uses "VisitObjective" instead of "Primary Visit"
data = data[data$VisitObjective == "Primary Visit",]
#data = data[data$Primary.Visit =="Yes",]

# Only carry data from watersheds of interest
data=data[data$Watershed %in% watersheds,]
nrow(data)
#data$Alk

###########################
# initialize
All.Site.Evals = read.csv(as.character(header$Evaluation.File.Names[n.years]), header=T)

# Get all the site evals into a single dataframe
for (y in (n.years-1):1) {
  temp = read.csv(as.character(header$Evaluation.File.Names[y]), header=T)
# 2/3/2017 fix
  colnames(temp) = colnames(All.Site.Evals)
  All.Site.Evals = rbind(All.Site.Evals, temp)
}

sites = levels(All.Site.Evals$Site.ID)
n.sites = length(sites)

# initialize evals for one-row-per-site
All.Evals = All.Site.Evals[1:n.sites,]


# Specify priority level for picking a single evaluation from [possible] multiple
# evaluations (year after year)
for (k in 1: n.sites){
print(paste("Setting eval status site",k,"of", n.sites))
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

# Write file of all site evals
write.csv(All.Evals, "SITEEVALUATION_AllYears.csv")
#nrow(data)
############################


# Initialize wgt
data$wgt = rep(NA, nrow(data))
data$SiteName
as.character(data$SiteName)
data$VisitYear
# Assign Stratum based on file CV provided.

pop.frame$SiteName
idx = match(paste(data$SiteName, data$VisitYear), paste(pop.frame$SiteName, pop.frame$Year))
idx
data$Stratum = pop.frame$Stratum[idx]
nrow(data)
data$Stratum
# remove any sites w/0 stratum
# Remove data if I don't have a stratum.  Ouch!
data = data[(is.na(data$Stratum) == F),]
data = data[data$Stratum !="",]
# Good to here!

###############################################################
# Now I need to take the mean and find trend of all quantitative variables for
# long term status and trend estimates.  We'll end up doing GRTS rollups for
# each individual year AND the average over all years AND the trend over all years.


sites = levels(factor(data$SiteName))
# initialize variagbles
Data_Mean = data[1:length(sites),]
Data_Mean$wgt = rep(0, nrow(Data_Mean))
Data_YY_Trend = Data_Mean
Data.adj.wgt = data # used to store adj.wgts later on a per-metric basis

#Get first row for every site, from which to assign site Name and other
#Categorical info
index = match(sites, data$SiteName)

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
		print(paste("Calculating Site level means and trends, row",i,"of", nrow(Data_Mean)))

	for (j in na.omit(match(levels(factor(header$Metric.List)), colnames(data)))){
	site.data = data[data$SiteName == Data_Mean$SiteName[i],]
	 Data_Mean[i,j] = mean(site.data[,j], na.rm=T)

# Calculating Trends
# Force use to have at least three data points for trend
# I might want to make the year range > 5 or something, rather than specify
# actual point counts.
if (length(na.omit(site.data[,j])) > 2) {
	 Data_YY_Trend[i,j]=coefficients(lm(site.data[,j] ~ site.data$VisitYear))[2]
	} # end of "if" everything is numeric and more than one site from which to calc trend

# Assign x_albers and y_albers to the mean and trend data frame
Data_Mean$x_albers[i] = mean(site.data$x_albers, na.rm=T)
Data_Mean$y_albers[i] = mean(site.data$y_albers, na.rm=T)
Data_YY_Trend$x_albers[i] = mean(site.data$x_albers, na.rm=T)
Data_YY_Trend$y_albers[i] = mean(site.data$y_albers, na.rm=T)
} # end of loop through metrics
} # end of loop through sites



#Assign Stratum for Data_Mean and Data_YY_Trend
# Assign Stratum
# NOTE: Need to updates year every time w do this for a new year.  Means and Trends should always
# use the most recent year, such that (later in the code) they use the stratum extents from the most recent year

idx = match(paste(Data_Mean$SiteName, 2017), paste(pop.frame$SiteName, pop.frame$Year))
idx
Data_Mean$Stratum = pop.frame$Stratum[idx]

# Assign Stratum
idx = match(paste(Data_YY_Trend$SiteName, 2017), paste(pop.frame$SiteName, pop.frame$Year))
Data_YY_Trend$Stratum = pop.frame$Stratum[idx]
#Data_YY_Trend = Data_YY_Trend[(is.na(Data_YY_Trend$Stratum) == F),]
#Data_YY_Trend = Data_YY_Trend[Data_YY_Trend$Stratum !="",]

# write the mean and trends to files
write.csv(Data_YY_Trend, "METRICS_Trends.csv")
write.csv(Data_Mean, "METRICS_Ave_of_All_Years.csv")


# all.years is the name of the file with all years data, average
# over all years.  Now I've got to combine site-eval file for all years

################################################################
################################################################
# Figure out Extents for each Strata, for weights
#####################################################
# Find weights for data (by year) and mean and trend 
#Need to uniquely find Weights for each set
#########################################################
#############################################################
# Read Shape File from which strata extents are obtained

shape = read.dbf(paste("Frame Files/",header$Frame.File.Names[2],".dbf",sep="" ))

#####################################################################
####### Checks - frame length by watershed x stratum ################
# Note - not yet sorted by "target".  That happens below, by year ###
#tapply(shape$dbf$LengthKM, shape$dbf$CHaMPshed, sum)

#Lem=shape$dbf[shape$dbf$CHaMPshed=="Lemhi",]
#Wen=shape$dbf[shape$dbf$CHaMPshed=="Wenatchee",]
#Tuc=shape$dbf[shape$dbf$CHaMPshed=="Tucannon",]
#SFS=shape$dbf[shape$dbf$CHaMPshed=="South Fork Salmon",]
#UGR=shape$dbf[shape$dbf$CHaMPshed=="Upper Grande Ronde",]
#JD=shape$dbf[shape$dbf$CHaMPshed=="John Day",]
#Ent=shape$dbf[shape$dbf$CHaMPshed=="Entiat",]
#Met=shape$dbf[shape$dbf$CHaMPshed=="Methow",]
#YF=shape$dbf[shape$dbf$CHaMPshed=="Yankee Fork",]
#Min=shape$dbf[shape$dbf$CHaMPshed=="Minam",]
#names(shape$dbf)

######################### end of checks ################################

# Assign Strata to combine watershedname and stratum.
shape$dbf$WS_Strata2011= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2011)
shape$dbf$WS_Strata2012= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2012)
shape$dbf$WS_Strata2013= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2013)
shape$dbf$WS_Strata2014= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2014)
shape$dbf$WS_Strata2015= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2015)
shape$dbf$WS_Strata2016= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2016)
shape$dbf$WS_Strata2017= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2017)
shape$dbf$WS_Strata2018= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2017)
shape$dbf$WS_Strata2019= paste(shape$dbf$CHaMPshed, shape$dbf$AStrat2017)

dbf2011 = shape$dbf
dbf2012 = shape$dbf
dbf2013 = shape$dbf
dbf2014 = shape$dbf
dbf2015 = shape$dbf
dbf2016 = shape$dbf
dbf2017 = shape$dbf
dbf2018 = shape$dbf
dbf2019 = shape$dbf

# Screen to Target Only
dbf2011 = shape$dbf[shape$dbf$Target2011 == "Target",]
dbf2012 = shape$dbf[shape$dbf$Target2012 == "Target",]
dbf2013 = shape$dbf[shape$dbf$Target2013 == "Target",]
dbf2014 = shape$dbf[shape$dbf$Target2014 == "Target",]
dbf2015 = shape$dbf[shape$dbf$Target2015 == "Target",]
# Changed 2016 to 2017 Here!!!! ... Need to update this every new year this is run!!!
dbf2016 = shape$dbf[shape$dbf$Target2016 == "Target",]
dbf2017 = shape$dbf[shape$dbf$Target2017 == "Target",]
dbf2018 = shape$dbf[shape$dbf$Target2017 == "Target",]
dbf2019 = shape$dbf[shape$dbf$Target2017 == "Target",]


#frame lengths by stratum for target only
# Quick Check
tapply(dbf2014$LengthKM, dbf2014$CHaMPshed, sum)
tapply(dbf2017$LengthKM, dbf2017$CHaMPshed, sum)


k=1
for (k in 1:3) {
if (k==1) {temp.data = data}
if (k==2) {temp.data = Data_Mean
           temp.data$VisitYear = rep(2017, nrow(temp.data))
} #Use most recent Frame Info for means and trends 
# Note - Need to update this each year!!!
# changed from 2014
if (k==3) {temp.data = Data_YY_Trend
           temp.data$VisitYear = rep(2017, nrow(temp.data))
} #Use 2017 Frame Info for trends
# changed from 2014

i=1
for (i in 1:nrow(temp.data)){
print(paste("Assigning Stratum", i, "of", nrow(temp.data)))
strat = paste(temp.data$Watershed[i], temp.data$Stratum[i])
strat
# Find strata extents and n, repeat for each year since it's
# a different metric name for each year.  A bit of brute-force
# programming here that could be streamlined quite a bit.


levels(factor(dbf2011$WS_Strata2011))
levels(factor(dbf2011$AStrat2011))
if (temp.data$VisitYear[i] == 2011) {
   strat.dbf = dbf2011[dbf2011$WS_Strata2011 == strat,]}

if (temp.data$VisitYear[i] == 2012) {
   strat.dbf = dbf2012[dbf2012$WS_Strata2012 == strat,]}

if (temp.data$VisitYear[i] == 2013) {
   strat.dbf = dbf2013[dbf2013$WS_Strata2013 == strat,]}


if (temp.data$VisitYear[i] == 2014) {
   strat.dbf = dbf2014[dbf2014$WS_Strata2014 == strat,]}

if (temp.data$VisitYear[i] == 2015) {
   strat.dbf = dbf2015[dbf2015$WS_Strata2015 == strat,]}

# Changed to 2016 from 2015 Here!!!
if (temp.data$VisitYear[i] == 2016) {
   strat.dbf = dbf2016[dbf2015$WS_Strata2016 == strat,]}

if (temp.data$VisitYear[i] == 2017) {
   strat.dbf = dbf2017[dbf2017$WS_Strata2017 == strat,]}


if (temp.data$VisitYear[i] == 2018) {
   strat.dbf = dbf2017[dbf2017$WS_Strata2017 == strat,]}

if (temp.data$VisitYear[i] == 2019) {
   strat.dbf = dbf2017[dbf2017$WS_Strata2017 == strat,]}

   temp.data$Strata.extent[i] = sum(strat.dbf$LengthKM, na.rm=T)

   temp.data$Strata.n[i] =  sum(1* 
     (paste(temp.data$Watershed, temp.data$Stratum, temp.data$VisitYear)==
      paste(strat, temp.data$VisitYear[i])))
}

temp.data$wgt = temp.data$Strata.extent / temp.data$Strata.n



if (k==1) {data$wgt= temp.data$wgt}
if (k==2) {Data_Mean$wgt = temp.data$wgt}
if (k==3) {Data_YY_Trend$wgt = temp.data$wgt}
}
#########################################################333



#
# cycle through each year, plus one extra time where we'll combine all years
# looping through all the years to get
# each year, plus an extra each
# for site mean across years, and site level trend

# the "+2" includes 1) average across years, and 2) trend across years
# Note: if data is missiung in any years, program will crash.
# if not 2011 data, run from year 2 to n.years +2

for (year in 1:(n.years+2)){

print(paste("setting eval status.  year=", year))

if (year <= n.years) {
	Eval.File.Name = as.character(header$Evaluation.File.Names[year])
	Eval = read.csv(Eval.File.Name, header=T)
	idx = match(paste(data$SiteName, data$VisitYear), paste(Eval$Site.ID, (year+minyear-1)))

	for (k in 1:length(idx)){
	 if (is.na(idx[k]) ==F) {data$GRTS.Eval.Status[k] = Eval$GRTS.Eval.Status[idx[k]]} 
	}
}

if (year > n.years) {
	Eval.File.Name = "SITEEVALUATION_AllYears.csv"
	Eval = read.csv(Eval.File.Name, header=T)
	idx = match(Data_Mean$SiteName, Eval$Site.ID)
	Data_Mean$GRTS.Eval.Status = Eval$GRTS.Eval.Status[idx]
	Data_YY_Trend$GRTS.Eval.Status = Eval$GRTS.Eval.Status[idx]
	}
}

#Now set weights to zero for any non-target sites.  This WILL REDUCE the
#total frame length, as it should!

data$wgt[data$GRTS.Eval.Status == "Non-Target"]=0
Data_Mean$wgt[Data_Mean$GRTS.Eval.Status == "Non-Target"]=0
Data_YY_Trend$wgt[Data_YY_Trend$GRTS.Eval.Status == "Non-Target"]=0


########################
## Now set wgt to zero for all sites not in the Chinook data frame if we're doing 
## a Chinook Rollup
#	if (header$Subgroup.s.[1]=="AU_Code_CH"){
		# data$wgt[data$chnk == "FALSE"] = 0
#  		Data_Mean$wgt[Data_Mean$chnk == "FALSE"] = 0
#  		Data_YY_Trend$wgt[Data_YY_Trend$chnk == "FALSE"]= 0
#	}
##############################################




# Now I have weights, and "non-target" sites have been removed and
# total stratum extents similarly reduced.
# Now time to go metric by metric, adjusting weights as needed for missing data
# on a per-metric basis.  Will make files corresponding to data files with
# just adjusted weights for each metric.


################################################
################################################



# Again, we go n.years + 2.  The "+2" is for mean and trend.
for (year in 1:(n.years + 2)) {

Results=NULL
Watershed.Results=NULL

n.years
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






metrics.saved = metrics

i=1
first_time=TRUE
for (i in 1: length(metric.list)) {

metrics = metrics.saved
metric.name = metric.list[i]
print(metric.name)


values = metrics[,colnames(metrics) == metric.name]
metrics$WS_Stratum = paste(metrics$Watershed, metrics$Stratum)


Strat.totals = tapply(metrics$wgt, as.factor(as.character(metrics$WS_Stratum)), sum)
use = 1*(is.na(values) == F)


n.by.strat = tapply(use, as.factor(as.character(metrics$WS_Stratum)), sum)

new.adjwgt.by.strat = Strat.totals / n.by.strat
new.adjwgt.by.strat[n.by.strat ==0] = 0

metric.adjwgt = rep(0, length(metrics$WS_Stratum))


for (j in 1:length(metrics$Stratum)){
metric.adjwgt[j] = 
new.adjwgt.by.strat[names(n.by.strat) == metrics$WS_Stratum[j]]
}


metric.adjwgt = metric.adjwgt * use
metrics$wgt = metric.adjwgt

# Record all the adjusted weights, on a per-metric basis, to a data.frame that
# matches, exactly, the size (rows, columns) and format of the orginal data frame.
# Will write these files for later use.


if (year < (n.years+1))  {
col=match(metric.name,colnames(Data.adj.wgt))
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


watershed.metrics = metrics
use = use * (watershed.metrics$wgt != 0)
use
data.frame(watershed.metrics$wgt, use)
# Now build spsurvey data.frames and run spsurvey analysis tools with calculated weights

my.sites <- data.frame(siteID=watershed.metrics$SiteName, Use=(use==1))
my.sites


my.design <- data.frame(siteID=watershed.metrics$SiteName,
                        wgt=watershed.metrics$wgt,
                         xcoord=watershed.metrics$x_albers,
                         ycoord=watershed.metrics$y_albers)

subgroups = (header$Subgroup.s.)
subgroups = subgroups[subgroups != ""]
subpop.index = match(subgroups, names(watershed.metrics))


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


my.subpop <- data.frame(siteID=watershed.metrics$SiteName, Subgroups)
#				  All.Sites= rep("All.Sites",length(watershed.metrics$SiteName)),
#                          Watershed = metrics$Watershed,
#				  Stratum = paste(metrics$Watershed, ".",watershed.metrics[,subpop.index],sep=""))
my.subpop


# Note - do NOT log metric for sp.survey, in most cases.  spsurvey does not require
# assumption of normality.  log metric is specified in header for variance decomposition,
# which is not part of this script.

cont.data.values = values
cont.data.values

my.cont.data <- data.frame(siteID=watershed.metrics$SiteName,cont.data.values)   
colnames(my.cont.data)[2] = metric.name


my.cont.data

Results = 0
# Don't run if data is empty
length(is.na(my.cont.data[,2])==F)

# Need at least 1 per subgroup, perhaps?  
nrow(my.cont.data[is.na(my.cont.data[,2])==F,])

nrow(my.cont.data[is.na(my.cont.data[,2])==F,])
# changed to <4 from <2.  No sense summarizing on just 3 points.
if (nrow(my.cont.data[is.na(my.cont.data[,2])==F,]) < 2) {  # was < 4
print(paste("i=", i," empty my.cont.data")) }

if (nrow(my.cont.data[is.na(my.cont.data[,2])==F,]) > 1) {  # was > 3
print("yes")


subgroups = levels(factor(my.subpop$AU_Code_ST))

my.sites
my.subpop
my.design
#my.cont.data$Max7dAM_80PctCuttoff
my.cont.data

Results <- cont.analysis(sites=my.sites,
 subpop = my.subpop,
design = my.design, data.cont = my.cont.data, total=FALSE)


###############################################
# Change names in "Results" to "Display Names"

Results$CDF$Indicator=header$Display.Name[match(Results$CDF$Indicator, header$Metric.List)]

###############################################

names(my.subpop)
################################################################
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

############################################################


##Make pdf plots
#outputfile
#pdf.filename = paste("pdf plots/",gsub(".csv", "",outputfile),"_",metric.name,"_CDF.pdf",sep="")
#
#if (year == 4) {
#cont.cdfplot(pdf.filename, Results$CDF, cdf.page=1)
#}

Results_to_write = Results$Pct[Results$Pct$Statistic == "Mean",]
#Results_to_write = Results$Pct[Results$Pct$Statistic == "Total",]
Results_to_write$Var = Results$Pct$Estimate[Results$Pct$Statistic == "Variance"]
Results_to_write$Var_Std_Error = Results$Pct$StdError[Results$Pct$Statistic == "Std. Deviation"]
#Results_to_write$Watershed = rep(watershed, nrow(Results_to_write))
Results_to_write$Year = rep(year, nrow(Results_to_write))
Results_to_write$Metric.Name = rep(metric.name, nrow(Results_to_write))
index =match(Results_to_write$Subpopulation, names(subpop.extents))
Results_to_write$Subpop.Extent = subpop.extents[index]

print(paste("year=",year))

#Results_to_write
#Summary_Results
#rbind(Results_to_write,Results_to_write)

#


first_time
if(i==1) {
Watershed.Results = Results$Pct
Watershed.Results = Watershed.Results[NULL,]}

if (nrow(Watershed.Results)==0) {
	 Watershed.Results = Results$Pct

    if(first_time==TRUE)
{
   first_time == FALSE
   Summary_Results = Results_to_write } else {
     Summary_Results = data.frame(rbind(Summary_Results, Results_to_write))
}
} else {
      Summary_Results = data.frame(rbind(Summary_Results, Results_to_write))

 	Watershed.Results$Indicator <-  factor(Watershed.Results$Indicator,levels = levels(as.factor(metric.list)) )

      Watershed.Results[(nrow(Watershed.Results)+1):(nrow(Watershed.Results)+nrow(Results$Pct)),1:9]=Results$Pct
}
} # end of "don't run cont.analysis if no data or only 1 row of data"
Watershed.Results
} # end of loop through metrics


if(is.null(Watershed.Results)==FALSE) {

Watershed.Results$Indicator = as.character(Watershed.Results$Indicator)
Watershed.Results$Indicator =header$Display.Name[match(Watershed.Results$Indicator, header$Metric.List)]
write.csv(Watershed.Results, paste("results files/",outputfile,sep=""))
}
} # end of cycle through years





# Write file of adjusted weights corresponding to data, Data_Mean, and Data_YY_Trend

write.csv(data, "Metrics_and_Covariates.csv")
write.csv(Data.adj.wgt, "CHaMP_Data_All_AdjWgt_by_Metric.csv")
write.csv(Data_Mean.adj.wgt, "CHaMP_Data_Mean_AdjWgt_by_Metric.csv")
write.csv(Data_YY_Trend.adj.wgt, "CHaMP_Data_Trend_AdjWgt_by_Metric.csv")





  