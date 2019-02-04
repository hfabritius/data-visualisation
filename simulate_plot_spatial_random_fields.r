setwd(choose.dir())
polygonProjection = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # a Finnish projection

library(raster)
library(graphics)
library(utils)
library(boot)
library(INLA)
library(spdep)
library(akima)  # interp
library(rgl)    # rgl
library(lattice)
library(grid) 

source("duplicate_patches.r")
source("patch_colonization_rates.r")
source("dynamics_01_patch_colonization.r")
source("dynamics_02_patch_extinction.r")
source("dynamics_03_patch_emergence.r")
source("dynamics_04_patch_destruction.r")
source("dynamics_05_landscape_change_annual.r")

# DEFINE PARAMETERS
# **********************************************************************************

# simulation parameters
repeatnumber<-100 # how many times each analysis is repeated
LandscRates <-c(0,0.002,0.004,0.006,0.008,0.010,0.012,0.014,0.016,0.018,0.020)
extent<-extent(199995, 236015, 6874500, 6930005) # spatial extent of analysis
phis<-c(0.9,0.5,0.2) # tested values of phi (how fast the landscape changes), smallest phi means fastest change

# LOAD & PREPARE BACKGROUND DATA
# **********************************************************************************

# Create an spde object
suitability = raster("suitability.tif") # we use the landscape suitability raster as a base raster
data<-read.csv2("randomdata_100000.csv")[,2:9] # randomize locations in the landscape
locations <- as.matrix(data[,c("x","y")])
locations <- locations * 1e-6
mesh <- inla.mesh.2d(locations, max.edge=c(0.005,2), cutoff=0.001) # mesh <- inla.mesh.create.helper(points.domain=locations, max.edge=c(50,10000))  
spde <- inla.spde2.matern(mesh, alpha=2) # precision matrix # A <- inla.spde.make.A(mesh,locations) # observation matrix #image(spde$param.inla$M2), #image(spde$param.inla$M0) #image(A)

# define parameters for creating & plotting Matern spatial random fields
phi<-0.2
theta1<-(-7.8991) # locations model
theta2<-5.979
repeats<-8

samplemap<-Landscape<-list()
Q <- inla.spde2.precision(spde, theta=c(theta1,theta2))

# generate random fields & plot different realized landscape variants
sample<-inla.qsample(n=repeats, Q=Q) # inla.qsample(n = 1L, Q=Q, b, mu, sample, constr, reordering = inla.reorderings(), seed = 0L, logdens = ifelse(missing(sample), FALSE, TRUE))
for(i in 1:repeats){
  # interpolate from the mesh to a raster map
  samplemap[[i]] <-interp(x=(mesh$loc[,1]*1000000),y=(mesh$loc[,2]*1000000),z=sample[,i] ,xo=seq(xmin(suitability),xmax(suitability),length=ncol(suitability)), yo=seq(ymin(suitability),ymax(suitability),length=nrow(suitability)), linear=FALSE, extrap=TRUE)
  samplemap[[i]] <-flip(raster(t(samplemap[[i]]$z)), direction='y') # transformation related to format change
  plot(samplemap[[i]])
}
