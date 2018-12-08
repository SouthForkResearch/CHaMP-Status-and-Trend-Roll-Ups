# Example of analysis of GRTS data using spsurvey
# functions "cat.analysis", "cont.analysis"
# Matt Nahorniak, August 2012

rm(data)
# Load Required Libraries
	library(xlsx)
	library(spsurvey)

# "read data from worksheet 2 (2010 data) fron "FishAnalysisGRTSTable.xlsx"
	data <- read.xlsx("Lemhi2010Data.xlsx", 1)    


# Remove duplicate site entries - keep only the first site listed.  This
# is for example only... user needs to determine what to do with duplicate
# measurements and if/how to use them.  cont.analysis and cat.analysis take
# only one set of measurements per site as inputs.
     	data <- data[!duplicated(data$siteid), ]
data

# Add x-coordinates, and y-coordinates to the cdf data frame; given the code asks for xalbers vs. x_albers, set up

# create Albers xy coords for use in analysis
# spsurvey functioins need lat/lon in this format
	latlon <- geodalbers(data$lon, data$lat)
	data$xalbers <- latlon$xcoord
	data$yalbers <- latlon$ycoord 


##################################################
# Make Some simple Exploratory Plots

# Read shapefile for Lemhi watershed
	shape = read.shape(filename = 
   		"LemhiFrameFinal/LemhiFrameFinal_20120527.shp")

names(shape)
 
# Create Rep.unit in a way that I think matches the data,
# so I can get total frame lengths

	Rep.unit= shape$HU_10_NAME
	levels(Rep.unit)
	Rep.unit[Rep.unit=="Hawley Creek"] = "Upper Lemhi River"
	Rep.unit[Rep.unit=="Middle Lemhi River"] = "Lower Lemhi River"
	Rep.unit[Rep.unit=="Timber Creek"] = "Upper Lemhi River"
	Rep.unit[Rep.unit=="Texas Creek"] = "Upper Lemhi River"	
	Rep.unit[Rep.unit=="Eighteenmile Creek"] = "Upper Lemhi River"


# Calculate sream length by stratum/reporting unit for "popsize"
# in stratified design

	ss.x.rep = paste(Rep.unit, shape$PWatershed)
	levels(as.factor(ss.x.rep))
	framelengths_strat = by(shape$FrameLengt, ss.x.rep, sum)
      framelengths_strat

#Calculate stream length by reporting unit
	framelengths_rep_unit = by(shape$FrameLengt, Rep.unit,sum)
	framelengths_rep_unit
	names(framelengths)
	sum(framelengths)

#Calculate total stream length
	sum(shape$LENGTHKM)



#######################################
# Do some preliminary Graphical Analysis
# Make a plot using the shapefile, and overlay sample
# points on top.

# Set color levels by reporting unit
	WSlevels = c("Hayden Creek", "Lower Lemhi River", "Upper Lemhi River")
	WScolors = match(Rep.unit, WSlevels)

# See what the max and min values are
	max(data$xalbers)
	min(data$xalbers)

# plot the shapefile
	plot(shape, col=1+WScolors, xlim=c(-1400000,-1300000))

# make a legend
	legendtext = WSlevels
	legend("topright", legendtext, col=(1+seq(1:length(WSlevels))), 
             ncol=, title="Reporting Unit",lty=1)


# For the individual sample points, set Color by Spatial Strata,
# and add these points to the plot.
	strat_levels = levels(data$SpatialStrata)
	strat_levels
	colors=match(data$SpatialStrata, strat_levels)+1
      legend_col = seq(1:length(strat_levels))+1
	legend_symbol = legend_col+10
	points(latlon, col=colors, 
              pch=symbol, main = "Spatial Location of Sites, by SpatialStrata",
              xlab="longitude", ylab="latitude")
	legendtext = strat_levels
	legend("bottomrig", legendtext, col=legend_col, pch=legend_symbol,
            , ncol=1, title="Spatial Strata")


####################################
# Make some other plots

	hist(data$OmykissPerMeter, main="O.MykissPerMeter", xlab="O.MykissPerMeter", col=5)

	boxplot(data$OmykissPerMeter ~ data$SpatialStrata, main=
	  "OmykissPerMeter by SpatialStrata", las=2, col=5, ylim=c(0, 300) )

	boxplot(data$OmykissPerMeter ~ data$ReportingUnit, main=
	  "OmykissPerMeter by Reporting Unit", las=1, col=5)



	boxplot(data$OmykissPerMeter ~ data$priorityHU, main = "OmykissPerMeter by PriorityHU")
	boxplot(log(data$OmykissPerMeter) ~ data$priorityHU, main = "ln(OmykissPerMeter) by PriorityHU")

# Issue - probably wouldnt want to log transform resource that may be "added up"
# to sum to resource total




###################################################
#
# Start the analysis with spsurvey functions here!
#
###################################################

# Start Buidling dataframes for cont.analsys and cat.analysis
# First time through, we'll analyze as though sample plan was
# unstratified


# get number of rows
	nr <- nrow(data)
nr
data$final.wgt
# Input the adjusted weights 
	data$final.wgt <- data$SSFrameWeight*1000 # convert to meters


# Create the sites data frame, which identifies sites to use in the analysis
	my.sites <- data.frame(siteID=data$siteid, Use=(
data$CHaMPstudyframe
=='Yes'
))     
my.sites                   
data$CHaMPstudyframe
# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired

	my.subpop <- data.frame("siteID"=data$siteid,
				  All.Sites= rep("All.Sites",nr),
                          ReportingUnit = data$ReportingUnit)


# Create the popsize data frame, based on the calculation done earlier.
# The numbers represent the total stream length by subgroups

	my.popsize = list(All.Sites = 308836,
	   ReportingUnit=list(
			"Hayden Creek"=24417, 
                  "Lower Lemhi"= 121266,      
                  "Upper Lemhi" = 163153))


# Create the design data frame, which identifies the stratum code, weight,
# x-coordinate, and y-coordinate for each site ID
	my.design <- data.frame(siteID=data$siteid,
                        wgt=data$final.wgt,
                         xcoord=data$xalbers,
                         ycoord=data$yalbers)

                 

# Now make the "data" data frame for our categorical variable
	my.cat.data <- data.frame(siteID=data$siteid, ValleyClass=data$ValleyClass)

# Now make the "data" data frame for our continuous variable
	my.cont.data <- data.frame(siteID=data$siteid,
      	                   OmykissPerMeter=data$OmykissPerMeter)



####################################
# Now, let's use "cat.analysis" to analyze the categorical data
	Results.Cat <- cat.analysis(site=my.sites, subpop = my.subpop,
                           design = my.design, data.cat = my.cat.data, 
                           popsize=my.popsize)


# And see the results
	names(Results.Cat)
# Print all results
	print(Results.Cat)

# Write results to a file
	write.csv(Results.Cat, "Results.Cat.csv")

# Print results in percentiles
	print(Results.Cat[,c(1:9)])

# Print results scaled to total lengths
	print(Results.Cat[,c(1:4,10:13)])



###############################################################
# Analyze Continuous Data usisng "cont.analysis" function
              
	Results <- cont.analysis(sites=my.sites, subpop = my.subpop,
                           design = my.design, data.cont = my.cont.data, 
                           total=TRUE,
				   popsize=my.popsize)

# And see the results
	Results$CDF
	Results$Pct


# Write results to file(s)
	write.csv(Results$Pct, "ResultsPCT.csv")
	write.csv(Results$Pct[Results$Pct$Statistic=="Total",], "ResultsPCT.Total.csv")


# print some results to the screen
	names(Results)
	with(Results$CDF, data.frame(Type, 
     		Subpopulation, Indicator, NResp, Estimate.P,LCB95Pct.P, UCB95Pct.P, Estimate.U, LCB95Pct.U, UCB95Pct.U))



# Select Subpopulation for plotting cdf plots to screen
	Plot.Subpop= Results$CDF[Results$CDF$Subpopulation=="All.Sites",]
	Plot.Subpop= Results$CDF[Results$CDF$Subpopulation=="Lower Lemhi",]
	Plot.Subpop= Results$CDF[Results$CDF$Subpopulation=="Upper =Lemhi",]

# Manually make some plots
	head(Plot.Subpop)
	title = Plot.Subpop$Subpopulation[1]
	metric = Plot.Subpop$Indicator[1]
	

# Generate a CDF Plot for Estimate.P
	with(Plot.Subpop, { 
	plot(Value[Indicator=="OmykissPerMeter"],Estimate.P[Indicator=="OmykissPerMeter"],
	 type="l",	main = paste(title, "O.Mykiss Per Meter CDF Estimate"),
	xlab="Total Abundance",	ylab="Cumulative Pct")

	lines(Value[Indicator=="OmykissPerMeter"],LCB95Pct.P[Indicator=="OmykissPerMeter"],
 		col="red")
	lines(Value[Indicator=="OmykissPerMeter"],UCB95Pct.P[Indicator=="OmykissPerMeter"],
		 col="red")
	})

# add a legend
	legend.text = c("CDF Estimate", "95% Confidence Limits")
	legend("bottomright", legend.text,col = c("black", "red"), pch=19)

## Let's compare the pct estimates
#points(Results$Pct[Results$Pct$Type=="All.Sites",]$Estimate[1:7],c(5,10,25,50, 75, 90, 95))
#points(Results$Pct[Results$Pct$Type=="All.Sites",]$LCB95Pct[1:7],c(5,10,25,50, 75, 90, 95), col="blue")
#points(Results$Pct[Results$Pct$Type=="All.Sites",]$UCB95Pct[1:7],c(5,10,25,50, 75, 90, 95), col="blue")



# Use function "cont.cdfplot"
#Create a PDF file containing plots of the CDF estimates--the function generates the cdf plots from the data in Janish_CDF_Estimates$CDF
	cont.cdfplot("Example_Omykiss_CDF.pdf", Results$CDF)



# Conduct a WALD test for differences among subpopulations
# (Note the change in the "popsize" argument" to list only subpopulations
# on the same heirarchical level	
	CDF_Tests <- cont.cdftest(sites=my.sites, subpop = my.subpop[,c(1,3)],
	   design= my.design, data.cont= my.cont.data,
	   popsize = list(ReportingUnit=my.popsize$ReportingUnit))

	print(CDF_Tests)
	write.csv(CDF_Tests, "CDF_Tests_Example.csv")




########################################
## Re-run analysis, assuming "SpatialStrata" was used as a design stratification
# Need to change "design" and "popsize" arguments and re-run cat.analysis
# and cont.analysis


# Add stratum code to my.design
	my.design <- data.frame(siteID=data$siteid,
                        wgt=data$final.wgt,
                        stratum = data$SpatialStrata,
                         xcoord=data$xalbers,
                         ycoord=data$yalbers)

  
	my.popsize = list( 
	"All.Sites" =c(
	  "Agency"=18361, "Big Timber"=29710, "Canyon"=21016, "Hawley"=23226, "Hayden"=24417,
	  "Kenney"=8380, "Lemhi Mainstem"=92487, "Mill"=1111,"Wimpey"=13340) ,      

	"ReportingUnit" = list(	
	 "Hayden Creek"= c(
	  "Agency"=0.01, "Big Timber"=0.01, "Canyon"=0.01, "Hawley"=0.01, "Hayden"=24417, 
	  "Kenney"=0.01, "Lemhi Mainstem"=0.01, "Mill"=0.01, "Wimpey"=0.01) , 

	 "Lower Lemhi"= c(
	   "Agency"=18361, "Big Timber"=0.01, "Canyon"=0.01, "Hawley"=0.01, "Hayden"=0.01, 
	   "Kenney"=8380, "Lemhi Mainstem"=51702, "Mill"=0.01 ,"Wimpey"=13340) , 

	"Upper Lemhi"= c(
	   "Agency"=0.01, "Big Timber"=29710, "Canyon"=21016, "Hawley"=23226, "Hayden"=0.01, 
	  "Kenney"=0.01, "Lemhi Mainstem"=40785, "Mill"=1111, "Wimpey"=0.01)    ))


# Re-run the categorical analysis
	Results.Cat.Stratified <- cat.analysis(site=my.sites, subpop = my.subpop,
                           design = my.design, data.cat = my.cat.data, 
                           popsize=my.popsize)

# Print all results
	print(Results.Cat.Stratified)
write.csv(Results.Cat.Stratified, "Results.cat.stratified.csv")
# Print results in percentiles
	print(Results.Cat.Stratified[,c(1:9)])
# Print results scaled to total lengths
	print(Results.Cat.Stratified[,c(1:4,10:13)])


#Re-run the continuous analysis
	Results.Stratified <- cont.analysis(sites=my.sites, subpop = my.subpop,
                           design = my.design, data.cont = my.cont.data, 
                           total=TRUE,
				   popsize=my.popsize)


# print some results to the screen
	Results.Stratified$CDF
	Results.Stratified$Pct[Results.Stratified$Pct$Statistic=="Total",]
	warnprnt()
# mostly warnings about small sample sizes


	with(Results.Stratified$CDF, data.frame(Type, 
	     Subpopulation, Indicator, NResp, Estimate.P,LCB95Pct.P, UCB95Pct.P, Estimate.U, LCB95Pct.U, UCB95Pct.U))


#Create a PDF file containing plots of the CDF estimates--the function generates the cdf plots from the data in Janish_CDF_Estimates$CDF
	cont.cdfplot("Example_Omykiss_Stratified_CDF.pdf", Results.Stratified$CDF)

# Write results to file(s)
	write.csv(Results$Pct, "ResultsPCT_stratified.csv")
	write.csv(Results$Pct[Results$Pct$Statistic=="Total",], "ResultsPCT_stratified.Total.csv")
# Same as unnstratified results.  That's OK and as expected.  Only the
# popsize argument changes, so the cat.analysis function will force a change in the rollup
# for total population sizes, I think...



