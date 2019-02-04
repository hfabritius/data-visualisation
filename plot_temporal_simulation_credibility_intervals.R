# *********************************************************************************************************
# Plot simulation results for multiple simulated objects. -Henna Fabritius
# *********************************************************************************************************
library(data.table) # fread, data.table data type
library(matrixStats) # colMedians

# --------------------------------------------------------------------------------------------------------
# SUMMARIZE & PLOT RESULTS
# --------------------------------------------------------------------------------------------------------
setwd(choose.dir()) # choose directory of simulation results
nr_objects<-5 # nr. of simulated objects to plot simultaneously
object_names<-c("Object_1","Object_2","Object_3","Object_4","Object_5") # give identifiers of simulation result files
results_summary<-list() # store object-specific results in a list of matrices
time_steps<-11 # time steps in the simulation

# Summarize n. of occupied trees over simulation replicates
for(y in 1:nr_species){
DataFiles=list.files(recursive=TRUE,pattern=object_names[y]) # read  in simulation result files 
results<-matrix(ncol=time_steps,nrow=length(DataFiles)) # initialize an object-specific results matrix
for(i in 1:length(DataFiles)){
  data<-fread(DataFiles[1],header=F,skip=1)[,-1] # read in one results file at a time
  results[i,]<-aggregate(ifelse(data$observation>0,1,0),by=list(data$year),FUN=sum)$x # aggregate number of sightings per year
}
results_summary[[y]]<-colMedians(results) # store median occupancy per year for each object
}

# Plot results
par(mar=c(4,6,1,1),las=1)
max_occupied<-max(unlist(results_summary))
plot(y=results_summary[1],x=seq(from=0,to=100,by=10),type="l",xlab="Year",ylim=c(0,140000),lwd=2,bty="n",cex.main=1.5,cex.axis=1.5,cex.lab=1.5,ylab="",main="Occupancy")
for(y in 1:length(results_summary)) points(y=results_summary[y],x=seq(from=0,to=100,by=10),type="l",col=palette()[y],lwd=2)
legend(y=max_occupied+10000,x=45,legend=object_names,lwd=c(2,2,2),col=palette()[1:length(results_summary)],bty="n",cex=1.5)
