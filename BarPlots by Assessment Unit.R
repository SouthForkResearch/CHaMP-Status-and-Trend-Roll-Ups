# Script to make barplots by assessment unit for Tucannon 
# Expert Panel Process review.  First run "Format Results"
#############################################################

Results.Formatted = read.csv("Formatted.Results.csv",header=T)
header = read.csv("header.csv", header=T)


mets = levels(Results.Formatted$Metric)
met=mets[1]


##############################################################
# Barplots by Assessment Unit 8/14/2015
dev.off()
mets
windows(record=T)

for (met in mets){

print(met)

names(header)
filename=paste(met,"by", header$Subgroup.s.[1])
filename=gsub("<", "", filename)
filename=gsub(":","",filename)
jpeg(paste("c:AU_Barplots/",filename,".jpg",sep=""), 12,10, units='in', res=600)


names(bar.data)
bar.data = Results.Formatted[Results.Formatted$Metric == met,]
data.frame(bar.data$Metric, bar.data$Sub.Population)
is.na(bar.data$Metric)

bar.data = bar.data[is.na(bar.data$Metric) == "FALSE",]
nrow(bar.data)

names(bar.data)
AUs=levels(bar.data$Sub.Population)
AUs
AUs = AUs[AUs != " "]
AUs = AUs[AUs != "#N/A"]
AUs = AUs[AUs != "All.Sites"]
AUs = AUs[AUs %in%wsheds == "FALSE"]
AUs


#for (AU in AUs) {
AU.bar.data = bar.data[bar.data$Sub.Population %in% AUs,]
AU.bar.data = AU.bar.data[is.na(AU.bar.data$Metric)=="FALSE",]
AU.bar.data

nrow(AU.bar.data)
AU.bar.data

if (nrow(AU.bar.data) > 0) {

idx = 
match(max(AU.bar.data$Mean+2*AU.bar.data$Std.Error.of.Mean.Estimate)
,( AU.bar.data$Mean+2*AU.bar.data$Std.Error.of.Mean.Estimate)
)
idx

AU.bar.data$Sub.Population
AU.bar.data$Visit.Year


names = paste(AU.bar.data$Sub.Population,"", as.character(AU.bar.data$Visit.Year))
names = paste(as.character(AU.bar.data$Visit.Year),"",AU.bar.data$Sub.Population)

names=gsub("Average of All Years", "Ave All Years", names)

space = rep(.2, length(names))
for (i in 1:(length(space)-1)){
 AU.bar.data$Visit.Year[i]
   if (AU.bar.data$Visit.Year[i] == "Average of All Years") 
     {  space[i+1] = 1}}


par(mar = c(15,7,4,4))

names(header)
header$Subgroup.s[1]

names(AU.bar.data)
offset = .05 * max(AU.bar.data$Mean[idx]+2*AU.bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)

a=barplot(AU.bar.data$Mean, names=names,
space=space,
main=paste(met,"by",header$Subgroup.s.[1]),
ylab=met,
cex.names=1.5,cex.lab=1.5, # changed from 1.5 to 1.2 for UGR
cex.axis=1.5, cex.main=2, las=2, 
ylim=c(0, 2*offset+max(AU.bar.data$Mean[idx]+2*AU.bar.data$Std.Error.of.Mean.Estimate[idx],na.rm=T)))

AU.bar.data$Mean
AU.bar.data$Std.Error.of.Mean.Estimate

# changed size to cex=1 from 1.5
for (i in (1:length(a))){
text(a[i],
offset+AU.bar.data$Mean[i]+1.96*AU.bar.data$Std.Error.of.Mean.Estimate[i]
, paste("n=",AU.bar.data$N[i],sep=""),cex=1
)
}


for (i in (1:length(a))){
lines(c(a[i],a[i]),
c(AU.bar.data$Mean[i]+1.96*AU.bar.data$Std.Error.of.Mean.Estimate[i],
  max(0,AU.bar.data$Mean[i]-1.96*AU.bar.data$Std.Error.of.Mean.Estimate[i])),
lw=2)}


dev.off()
} # if nrow AU.bar.data > 1
#} # cycle through assessment units
} # cycle through metrics
######################
