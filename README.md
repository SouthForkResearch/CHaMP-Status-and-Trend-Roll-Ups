# CHaMP Metric Status and Trend Estimation
## a.k.a. "GRTS Rollup" 

# Description #
This set of R-code files is used to
* provide design based status and trend estimates for CHaMP metrics, by watershed and (opionally) other subgroups
* Estimate the components of variance for CHaMP metrics, including measurement noise
* Generate a set of figures for annual reporting, etc.

Additional background material, presentations, and sample code from a [Generalized Random Tessellation Stratified (GRTS) Design Workshop](Spatially_Balanced_Survey_Designs.md) led by Dr. Don Stevens in 2012 is in the "GRTS Workshop 2012" folder of this repository.  


# Overview #

CHaMP sampling designs incorporate spatially balanced, stratified random sampling, where (in most CHaMP watersheds) strata are defined as combinations of valley class (source, transport, or depositional) and ownership type (public or private).  Within each stratum, equally probable, spatially balanced sampling is done.  Sample inclusion probability may vary across the different strata. 

Spatial balance in sample design is achieved via use of a Generalized Random Tessellation Stratified (GRTS) sample selection algorithm (Stevens and Olsen, 2004).  A spatially balanced sample tends to spread out the sample points more uniformly across space, increasing the amount of independent information present in each individual sample point.  GRTS sampling, specifically, provides a spatially balanced sample while also maintaining the robust sample properties of simple random sampling.  The spsurvey package (Kincaid and Olsen, 2013) for the R statistical programming language is used to analyze status and trend for GRTS sampling performed by CHaMP. 

For CHaMP data analysis, we define status as the distribution of a CHaMP metric over a specified spatial domain and time range.  For spatial domain, we here present selected results at the watershed level.  However, status can also be defined at sub watershed levels (HUC5 within a watershed, individual tributary creeks, etc.) or across multiple watersheds (i.e. the entire interior Columbia basin covered by CHaMP sampling).  Time ranges considered may include individual years, as well as time averaged status over each year completed thus far in CHaMP sampling (2011-2015).  When estimating status over multiple years, we first average the metric of interest at the site level to obtain a single average response at each site over the time period of interest, then analyze the data using spsurvey using the single average response for each site.
Trend is defined as the average of site level linear trend over time, for a give metric, over a specified spatial domain. 

For all sites that contain more than one visit year, we can estimate a linear trend by regressing the metric as measured at each site against time (in years).  Note that, at the site level, there is high uncertainty in a trend estimate made from a regression of either 2 or 5 data points.  These individual site level trend estimates are then analyzed using spsurvey, just as is done for status, as described above, to estimate a distribution of trends across the spatial domain of interest.  Extreme caution should be applied when interpreting estimates of trend, given that only fuve years of data are available.  Small year to year differences may show up as trends, but in reality these “trends” may only reflect short term aberrations year to year, rather than long term linear changes.  With only five years’ of data, it is not possible to distinguish short term year-year aberrations from long term trends. 


# Required Data Sources #

### Program Metrics database  
* Source: champmonitoring.org data exports tab
* filename(s): CHaMP_Program Metrics_1_26_2016.MDB (ms access database)

### Site evalutions by Year ###
* Source: champmonitoring.org site evalations tab
* filename(s): SITEEVALUATION_2011.csv, SITEEVALUATION_2012.csv, etc.
* Notes: One file required for each year of analysis to be done

### CHaMP Frames Shapefile ###
* Source: Carol Volk
* filename: CHaMP_Frames_All_20150415_1300

### Strata listing by CHaMP Site_ID
* Source: Carol Volk
* filename CHaMPSites_AnalysisStrata_20150415.csv



# Analysis Steps #
The controlled R-files need to be put into a directory on you local PC.  That directory must also
contain the following sub-directories:

* Metrics Database
* Frame Files

The first step in the analysis is to export two tables from the database file CHaMP_Program "Metrics_M_DD_YYYY.MD":  "metrics_and_covariates" and "metric_visit_information".  These need to
be exported as Excel files, then opened in Excel and saved as .csv files.  These .csv files need to be
put in the directory "Metrics Database".  Additional metrics may be added to "metric_visit_information.csv" manually:  HSI and NREI metrics, for example, are not currently in cm.org (as of 2/25/2016) so these, if needed, needed to be added manually.  Temperature metrics, also, need to be manually added to "metric_visit_information.csv" if these are to be rolled up.  Always use VISIT_ID as the unique identifier to link data from other sources to "metric_visit_information.csv".  If subgroups not listed in either of these data files are to be used for the analysis, they also need to be manually added here.

Next, the shape files comprising "CHaMP_Frames_All_20150415_1300" need to be put into the directory "Frame Files".  If there is a newly named updated version of the shape files, the hard-coded file name in the R-scripts "CHaMP_Analysis_Automation.R" will have to be updated as well.

In the root directory, the site evaluation files and the file CHaMPSites_AnalysisStrata_20150415.csv
need to be stored.
 


## CHaMP_Analysis_Automation.R ##
This is the primary scripts for design based status and trend estimates.  It depends primarily on the
spsurvey R package.  Basic steps are as follows:

### Read header file "header.csv" ###
The header file lists the metric(s) to be analyzed, the year(s) over which to analyze the metrics (which will be analyzed both by individual year, averaged over all years, and by annual trend), watersheds to include in the analysis, and subgroups over which to break down the results.  It also includes file names for other key input files to be read.

### Read data files  ###
Read the data files: "metricvisitinformation.csv", "metricsandcovariates.csv", and link these two data sources into a single dataframe within R.

Read the file containing strata by site ID and link information with data from above.

Screen data to only primary visits

Read site-evaluation files

Read Stratum identifier file and combine with data.  Remove data not assigned to a stratum.

Calculate site level mean and trend across years for all sites

Calculate strata extents and site-level design weights by year and for the average and trend site level values.  Design weights are calculated as the strata extents divided by the number of sites within each strata for each year and for the mean and trend site level values.  Design weights will differ by year and for the mean and trend values as the number of sites per strata will vary due to the rotating panel and somewhat inconsistent sampling design.  N/A values are dealt with later in the code.


For any site-evals determined to be non-target, remove site data and reduce total stratum extent by design weight of non-target site.

Loop through each year to be processed (as well as extra times through the loop for mean and trend), and then loop through each metric to be rolled up.  Within each loop:

* If N/A values exist, set the adjusted design weight to zero and re-calculate all other design weights such that the sum of design weights, within each strata, is maintained.  This always assumes that missing data are random with a strata, which may or may not be a good assumption.  (If another assumption is warranted, code modification will be necessary).

* Build the necessary input data frames to run the spsurvey function cont.analysis.  The information in these dataframes include design weights, subgroup identifiers, and metric data.
 
* Run the spsurvey function cont.analysis
 
* Pull out the relevant results from the cont.analysis function and store them in a data frame, to which all results for all metrics and all years will be added.

Complete looping through all years and metrics within each year.
Write output file of results.  

Write additional output file containing adjusted design weights.

## format.GRTS.results.R ##
This code re-formats the results consistent with the latest format needed for regular reporting needs, and makes a pdf file containing a set of plots and large table of results.  It's a separate file due to the every-changing needs and tastes of end-users.  CHaMP_Analysis_Automation.R must be run prior to this code. 

## variance_decomp.R ##
This code performs a variance decomposition to estimates the proportion of variation that can be attributed to year-year variability, watershed-watershed variability, stratum-stratum variability within each watershed, site-site variability within each watershed, and measurement noise.  Estimates are usually made on the natural log of the response metrics (although it need not be the case; users can specify this in the header.csv file, but the specification needs to be consistent with the shape of the residual distributions rather than the preference of the user).  CHaMP_Analysis_Automation.R must be run prior to this code. 

## BarPlots by Subpopulation.R ##
This code generates results, as preferred by for the expert panel process, of bar plots by watershed x subpopulation.  The subpopulation is as was specified in header.csv. CHaMP_Analysis_Automation.R and format.GRTS.results.R must be run prior to this code. 

Link to themed documentation
https://southforkresearch.github.io/CHaMP-Status-and-Trend-Roll-Ups/
