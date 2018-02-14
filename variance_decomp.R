windows(record=T)
# Need to run GRTS automation code first to get all strata extents for all
# sites across all watersheds included.  That scripts creates the following 
# files.  Once these files are created, you won't need to keep running the GRTS
# Automation scripts.

# data = all target data from all years
# Data.adj.wgt = adjusted weights on a per-metric basis for all years


library(lme4)

header = read.csv("header.csv", header=T)
header
names(header)
metric.use.idx = (header$Metric.List != "") | (is.na(header$Metric.List) == TRUE)
metric.use.idx


test = read.csv("Metrics_and_Covariates.csv", header=T)
test$Grad
sites = levels(factor(test$SiteName))
sites



nrow(test)

Data.adj.wgt = read.csv("CHaMP_Data_All_AdjWgt_by_Metric.csv")
Data.adj.wgt
metric.list = as.character(header$Metric.List)
metric.list = metric.list[metric.list != ""]


metric.list = metric.list[metric.use.idx]

#metric.list = na.omit(factor(header$Metric.List))
#metric.list = metric.list[metric.list != ""]
metric.list

metric.index =na.omit(match(metric.list,names(test)))
metric.index
test[,metric.index]
test$VisitYear
metric.index.log = header$log.metric.for.Variance.Decomposition[metric.use.idx]
names(metric.index) = metric.list
metric.index
metric.list


# read all data, including repeat site visits.  The repeat sites
# will be used to estimate measurement noise.  Becausse this is such
# a small subset, this isn't luped with the total mnodel.


all.data = read.csv("Metrics_and_Covariates_All_Data.csv", header=T)


sites = levels(as.factor(as.character(test$SiteName)))

################################################



metric.index

######################################################
metric.list
meas.noise = rep(0, length(metric.list))



results = array(0, c(length(names(metric.index)),4))
Intercept = rep(0, length(names(metric.index)))

rownames(results) = names(metric.index)
#rownames(results.mean.response) = names(metric.index)
k=1

#metric.index.log
#header= read.csv("header.csv", header=T)

watersheds = header$watersheds





#########################################################
# loop through metrics
k=15
k=80
k=72
k=1
length(metric.index)

for (k in 1:length(metric.index)){
print(paste("k=",k, "metric:", names(metric.index)[k]))
k
metric.list[k]

test$metric = test[,metric.index[k]]
test$metric.adj.wgt = Data.adj.wgt[,metric.index[k]]
test$metric.adj.wgt
if (metric.index.log[k] == "Yes") { test$metric = log(.1+test$metric)}

metric.name = names(metric.index)[k]
metric.name

# Just use data that is not na
#data = test[is.na(test$metric)==F,]
#test = test[is.na(test$metric)==F,]


test$metric.adj.wgt
prob =test$metric.adj.wgt/sum(test$metric.adj.wgt,na.rm=T)
prob[is.na(prob)] = 0
###################
its= 25


# temp
#############!!!!!!!!!!!!!!!!!


SigSq = array(0, c(its, 4))
Intercept.Est = rep(0, its)

test$VisitYear
nc = length(levels(as.factor(as.character(paste(test$WatershedName, test$VisitYear)))))
nc

Estimate = array(-9999, c(its, nc))
colnames(Estimate) = levels(as.factor(as.character(paste(test$WatershedName, test$VisitYear))))
colnames(Estimate)

iter=1
iter
#### IPB Iterations
for (iter in 1: its) {
print(iter)


################!!!!!!!!!!!!!!!################
samp.index= sample(1:nrow(test),nrow(test), replace=T, prob= prob)
sample= test[samp.index,]
sample$VisitYear
#boxplot(sample$metric ~ sample$Watershed)
sample$ValleyClass
sample$OwnerType
sample$Stratum = paste(sample$ValleyClass, sample$OwnerType)

sample$metric


# Make sure there's more than one data point for each level tested
if (
(length(levels(factor(sample[is.na(sample$metric)==F,]$Stratum))) > 1) &
(length(levels(factor(sample[is.na(sample$metric)==F,]$VisitYear))) > 1)&
(length(levels(factor(sample[is.na(sample$metric)==F,]$Watershed))) >1)
) {



#mod = lmer((metric+1)~ 1+ (1|VisitYear) +  (1|Watershed) + (1|Stratum), data = sample)
#Intercept.Est[iter]=summary(mod)$coefficients[1]
#variance.comps = c(as.numeric(VarCorr(mod)) ,attr(VarCorr(mod), "sc")^2 )

# Shot in the dark....otherwise remove any rows with "NA"
#sample$VisitYear = factor(sample$VisitYear)
#sample = sample[complete.cases(sample),]
#boxplot(sample$metric~ sample$VisitYear, main=metric.list[k])


data.frame(sample$metric, sample$VisitYear)
sample = sample[is.na(sample$metric) == F,]
sample$VisitYear
max(sample$metric)

mod = lmer((metric+1)~ 1+ (1|VisitYear) +  (1|Watershed) + (1|ValleyClass), data = sample)
summary(mod)
Intercept.Est[iter]=summary(mod)$coefficients[1]
variance.comps = c(as.numeric(VarCorr(mod)) ,attr(VarCorr(mod), "sc")^2 )
variance.comps
summary(mod)
anova(mod)

sample$VisitYear
sample$Watershed
names(sample)
sample$V_Class

mod = lmer((metric+1)~ 1+ VisitYear +  (1|VisitYear) +  (1|Watershed) + (1|ValleyClass), data = sample)
summary(mod)
Intercept.Est[iter]=summary(mod)$coefficients[1]
variance.comps = c(as.numeric(VarCorr(mod)) ,attr(VarCorr(mod), "sc")^2 )
summary(mod)
variance.comps

sample$VisitYear = as.numeric(sample$VisitYear)

#sample$VisitYear = factor(sample$VisitYear)

} else {
mean_log_value = NA
variance.comps = c(NA, NA, NA, NA)
}


#names(variance.comps) = c("Stratum", "Watershed", "VisitYear", "Site")
# Corrected!
names(variance.comps) = c("Watershed","VisitYear", "ValleyClass","Site")

 # }

variance.comps
#print(names(variance.comps))
#print(variance.comps)
sample$WatershedName

sample$VisitYear

colnames(Estimate)
Est = tapply(sample$metric, as.factor(as.character(paste(sample$WatershedName, sample$VisitYear))), mean)
Est
Est.idx = match(names(Est), colnames(Estimate))
Est.idx
Estimate[iter,]
Estimate[iter, Est.idx] = Est

Estimate
#colnames(Estimate) = 
#levels(as.factor(as.character(paste(sample$WatershedName, sample$VisitYear))))
#data.frame(levels(as.factor(as.character(paste(sample$WatershedName, sample$VisitYear))))
#,Estimate[iter,])

SigSq
if (iter == 1) {colnames(SigSq) = names(variance.comps)}
SigSq[iter,1:4] = variance.comps

} # end of iterating for metric


SigSq
####################







#colnames(results.mean.response) = 
#c(paste("mean",levels(as.factor(as.character(paste(sample$WatershedName, sample$VisitYear))))),
#paste("sd",levels(as.factor(as.character(paste(sample$WatershedName, sample$VisitYear))))))

results[k,]=apply(SigSq, 2, mean)
Intercept[k] = mean(Intercept.Est)

colnames(results) = colnames(SigSq)
results
#results.mean.response

# Now we have results.  residuals MSE is site-site within a stratum, confounded with 
# noise.  Noise estimate obtained separately



# Now calculate Measurement Noise for the Metric

revisit = all.data
revisit$SiteID = revisit$SiteName
revisit$SiteID
revisit = revisit[revisit$Watershed%in%watersheds,]
revisit$SiteYear = paste(revisit$SiteID, revisit$VisitYear)

revisit
revisit$nsites = rep(0, nrow(revisit))
for (i in 1:nrow(revisit)){
  revisit$nsites[i] = 
length(revisit$SiteID[revisit$SiteYear == revisit$SiteYear[i]])
}

revisit = revisit[revisit$nsites > 1,]
nrow(revisit)

revisit$measure = revisit[,metric.index[k]]
revisit$measure
names(revisit)[metric.index[k]]

revisit
if (metric.index.log[k] == "Yes") {revisit$measure = log(.1+revisit$measure)}
noise.mod = lmer(measure ~ (1|SiteID), data=revisit)
summary(noise.mod)

meas.noise[k] = attr(VarCorr(noise.mod), "sc")^2 
meas.noise[k]

levels(as.factor(revisit$SiteYear))



} # end of iterations through k (metrics)




dir.create("Variance Decomposition")
meas.noise

nrow(results)
rownames(results)

display.idx=match(row.names(results), header$Metric.List)
display.idx


var.decomp = data.frame(results,meas.noise)
var.decomp
rownames(var.decomp)
#var.decomp$Residuals = rep(NA, nrow(var.decomp))
#row.names(var.decomp)
#row.names(results)

(as.character(header$Display.Name[display.idx]))


row.names(var.decomp)=
as.character(header$Display.Name[display.idx])

paste("log(",row.names(var.decomp)[display.idx][header$log.metric.for.Variance.Decomposition=="Yes"],")", sep="")


header$log.metric.for.Variance.Decomposition[metric.use.idx]
if
 ("Yes" %in% header$log.metric.for.Variance.Decomposition) 
{

row.names(var.decomp)[display.idx][header$log.metric.for.Variance.Decomposition[metric.use.idx]=="Yes"]= 
 paste("log(",row.names(var.decomp)[display.idx][header$log.metric.for.Variance.Decomposition=="Yes"],")", sep="")
}

row.names(var.decomp)
var.decomp$meas.noise
#var.decomp$Residual
#var.decomp$Residuals = var.decomp$Residuals - var.decomp$meas.noise
#var.decomp$Residuals
#var.decomp$Residuals[var.decomp$Residuals <0] = 0

#colnames(var.decomp) = colnames(SigSq)
#var.decomp
#colnames(var.decomp)[5] = "Meas. Error"
#colnames(var.decomp)=c("Valley Class", "Watershed", "Year", "Site", "Meas.Error")
#colnames(var.decomp)=c("Watershed","Valley Class", "Year", "Site", "Meas.Error")

write.csv(var.decomp[,c(3,2,1,4,5)], "Variance Decomposition/vardecomp.csv")



normalized.var = var.decomp*0


##################################################
# Make Bar Plots for Variance Decomposition



metric.groups = length(levels(header$metric.group.for.Variance.Decomposition[1:length(metric.list)]))
#length(na.omit(factor(levels(header$metric.group.for.Variance.Decomposition)))

metric.groups

var.decomp
var.sum = apply(var.decomp, 1, sum)
var.sum
normalized.var$metric.group = header$metric.group.for.Variance.Decomposition[1:nrow(var.decomp)]

ncol(var.decomp)
for (j in 1:ncol(var.decomp)) {
normalized.var[,j] = var.decomp[,j]/ var.sum}


#sort by % measurement noise of total variance.
sort.idx  = order(normalized.var[,5],decreasing = TRUE)
normalized.var = normalized.var[sort.idx,]
normalized.var = normalized.var[is.na(normalized.var[,1])==F,]

z=1
z=2
for (z in 1:2) { # make plots 2x.  One with ALL the metrics.  Another by subgroup.


if (z==1) { mg = "All CHaMP Metrics"; levs = factor("All CHaMP Metrics")}
if (z==2) { mg = header$metric.group.for.Variance.Decomposition[metric.use.idx]; levs = levels(factor(normalized.var$metric.group))}


normalized.var$metric.group
names(normalized.var)
levs

for (mg in levs) {
#for (metric.group in levels(header$metric.group)){


jpeg(paste("c:Variance Decomposition/CHaMP Variance Decomposition_", mg,".jpg",sep="")
, 8,6, units='in', res=600)

if( z==2) {bar.idx = (1:length(normalized.var$metric.group))[normalized.var$metric.group== mg]}
if (z==1){bar.idx = (1:length(normalized.var$metric.group))}

par(mfrow = c(3,1))
if (z==2) {layout(matrix(c(1,2,3),3,1), heights=c(1,6,1)) } # put legend on bottom 1/8th of the chart}
if (z==1) {layout(matrix(c(1,2,3),3,1), heights=c(1,7,1)) } # put legend on bottom 1/8th of the chart}

par(mar=c(0,0,5,0))
plot.new()
title(main=paste("CHaMP: Estimated Components of Variance
Metric Group:", mg), cex.main=2)

label.size= .35*(z==1) + .35*(z==2)
label.size= .6

names(normalized.var)

par(mar=c(3,22,0,1))
#barplot(t(normalized.var[bar.idx,c(3,2,1,4,5)]), las=2, 
#Needed to change order when changing to Valley Class instead of Stratum
#barplot(t(normalized.var[bar.idx,c(3,1,2,4,5)]), las=2, 
barplot(t(normalized.var[bar.idx,c(2,1,3,4,5)]), las=2, 
cex.names =label.size,
#cex.names=min(1, 1*40/length(metric.list)), 
horiz=T, col=c(2,3,4,5,6),
#main=paste("Estimated Components of Variance for:", mg),
cex.axis=1)

par(mar=c(0,0,0,0))
plot.new()
legend("center",c("year", "watershed", "valley class","site", "meas. noise"),
pch=19, lt=1.2, col = c(2,3,4,5,6), cex=1.6, ncol=3, border=T)

dev.off()
 }

 }


#################################################################
data.frame("Intercept"=Intercept, var.decomp[,c(3,2,1,4,5)])
#################################################################
