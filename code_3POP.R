# Load the package, and set a global
# random-number seed for the reproducible
# generation of fake data later on.
library(pavo)
library(ggplot2)
library(readr)
library(dplyr)

specs <- getspec(
  where = getwd(),
  ext = "txt",
  lim = c(300, 700),
  decimal = ",",
  sep = NULL,
  subdir = FALSE,
  subdir.names = FALSE,
  ignore.case = TRUE)

vismodel <- vismodel()

coldist
colorspac
vol 

# Create custom palette based on the R chart as follow:
mycols <- colors()[c(555, 57, 451, 614, 48, 29)] #
# or you could enter the color names directly
# mycols <- c("red3", "coral", "magenta1", "springgreen4", "chartreuse1", "blue3")

# save .pdf with raw plots, default colors palette
pdf(file = "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Plots/plotsraw" )
par(mfrow=c(2,3))
explorespec(specs[1:1207], by = 3, lwd = 2)


#create data subset for each part of body
par(mfrow=c(2,3))

mspecs <- aggspec(specs, by = 3, FUN = mean)
write.csv(mspecs)
write.csv(mspecs, "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/Article/__OCTOBER/mspecs.csv", row.names=TRUE)

mspecs <- as.rspec(mspecs_clear)
is.rspec(mspecs)
is.rspec(mspecs_clear)

#create category vector 

#Upperbreast
upperbreast <- subset(mspecs, '1neck')
popupperbreast <- do.call(rbind, strsplit(names(upperbreast), "\\."))[, 2]
aggplot(upperbreast, popupperbreast,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Upper breast", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])

#Belly
belly <- subset(mspecs, '2abdo')
popbelly <- do.call(rbind, strsplit(names(belly), "\\."))[, 2]
aggplot(belly, popbelly,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Belly", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])
#Rump
rump <- subset(mspecs, '3rump')
poprump <- do.call(rbind, strsplit(names(rump), "\\."))[, 2]
aggplot(rump, poprump,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Rump", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])
#Coverts
coverts <- subset(mspecs, '4side')
popcoverts <- do.call(rbind, strsplit(names(coverts), "\\."))[, 2]
aggplot(coverts, popcoverts,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Coverts", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])
#Mantle
mantle <- subset(mspecs, '5back')
popmantle <- do.call(rbind, strsplit(names(mantle), "\\."))[, 2]
aggplot(mantle, popmantle,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Mantle", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])
#Nape
nape <- subset(mspecs, '6head')
popnape <- do.call(rbind, strsplit(names(nape), "\\."))[, 2]
aggplot(nape, popnape,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Nape", lcol = colors()[c(68, 654, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 654, 451)])



#PODZIAŁ NA DWIE POPULACJE I CZESCI CIAŁA
mspec2 <- as.rspec(mspecs_clear_2pop)
#Upperbreast
upperbreast2 <- subset(mspec2, '1neck')
popupperbreast2 <- do.call(rbind, strsplit(names(upperbreast2), "\\."))[, 2]
aggplot(upperbreast2, popupperbreast2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Upper breast", lcol = colors()[c(68, 451, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])

#Belly
belly2 <- subset(mspec2, '2abdo')
popbelly2 <- do.call(rbind, strsplit(names(belly2), "\\."))[, 2]
aggplot(belly2, popbelly2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Belly", lcol = colors()[c(68, 451, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])
#Rump
rump2 <- subset(mspec2, '3rump')
poprump2 <- do.call(rbind, strsplit(names(rump2), "\\."))[, 2]
aggplot(rump2, poprump2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Rump", lcol = colors()[c(68, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])
#Coverts
coverts2 <- subset(mspec2, '4side')
popcoverts2 <- do.call(rbind, strsplit(names(coverts2), "\\."))[, 2]
aggplot(coverts2, popcoverts2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Coverts", lcol = colors()[c(68, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])
#Mantle
mantle2 <- subset(mspec2, '5back')
popmantle2 <- do.call(rbind, strsplit(names(mantle2), "\\."))[, 2]
aggplot(mantle2, popmantle2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Mantle", lcol = colors()[c(68, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])
#Nape
nape2 <- subset(mspec2, '6head')
popnape2 <- do.call(rbind, strsplit(names(nape2), "\\."))[, 2]
aggplot(nape2, popnape2,
        UN.center = mean,
        ylim = c(0, 65),
        alpha = 0.05, legend = TRUE, main="Nape", lcol = colors()[c(68, 451)], lwd = 2, lty = "solid",  shadecol = colors()[c(68, 451)])



#bodyparts together
#'population' - vector
pop <- do.call(rbind, strsplit(names(mspecs), "\\."))[, 2]   #vector
# and  'body parts' - for each subset

aggplot()


par(mfrow=c(1,1))
EAST <- subset(mspecs, 'EAST')
sppEAST <- do.call(rbind, strsplit(names(EAST), "\\."))[, 3]   #vector
aggplot(EAST, sppEAST,
        FUN.center = mean,
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE, main="East", lcol = mycols, shadecol = mycols
        )

CENTRAL <- subset (mspecs, 'MIDD')
sppCENTRAL <- do.call(rbind, strsplit(names(CENTRAL), "\\."))[, 3]   #vector
aggplot(CENTRAL, sppCENTRAL,
        FUN.center = mean,
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE, main="Central", lcol = mycols, shadecol = mycols
)


SOUTHWEST <- subset(mspecs, 'WEST')
sppSOUTHWEST <- do.call(rbind, strsplit(names(SOUTHWEST), "\\."))[, 3]   #vector
aggplot(SOUTHWEST, sppSOUTHWEST,
        FUN.center = mean,
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE, main="WEST", lcol = mycols, shadecol = mycols
)



WEST <- merge(subset(mspecs, 'WEST'), subset(mspecs, 'MIDD'))

aggplot(WEST, sppWEST,
        FUN.center = mean,
        ylim = c(0, 65),
        alpha = 0.1, legend = TRUE, main="West", lcol = mycols, shadecol = mycols
        )

WEST2 <- merge(FARWEST,MIDD)
all.equal(WEST,WEST2)



#save .pdf with individuals specs of all patches
pdf(file = "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Plots/individuals")
meanspecs <- aggspec(specs, by=3, FUN=mean)
par(mfrow=c(2,2))
explorespec(meanspecs[1:403], by=6, lwd=2, col=mycols)


########### SUBSETTING AND AVERAGING DATA ##### by population or feathers patch
# subset EAST
specs.EAST <- subset(mspecs, "EAST")
specs.EAST.rump <- subset(specs.EAST, "3rump")
mean.Erump <- aggspec(specs.EAST.rump, FUN=mean)
# plot EAST RUMP
plot(specs.EAST.rump)
plot(mean.Erump)

# subset MIDD
specs.MIDD <- subset(meanspecs, "MIDD")
specs.MIDD.rump <- subset(specs.MIDD, "3rump")
mean.Mrump <- aggspec(specs.MIDD.rump, FUN=mean)
#plot MIDD rump
plot(specs.MIDD.rump)
plot(mean.Mrump)

# subset WEST
specs.WEST <- subset(meanspecs, "WEST")
specs.WEST.rump <- subset(specs.WEST, "3rump")
mean.Wrump <- aggspec(specs.WEST.rump, FUN=mean)
#plot WEST rump
plot(specs.WEST.rump)
plot(mean.Wrump)

#subset WEST+MIDD
specs.WEMI <- merge(specs.WEST, specs.MIDD)
specs.WEMI.rump <- subset(specs.WEMI, "3rump")
mean.WM.rump <- aggspec(specs.WEMI, FUN=mean)
#plot WEST+MIDD rump
plot(specs.WEMI.rump)
plot(mean.WM.rump)
 
#create a vector "patches" to label each part of feathers  (it is the third part of the name ID.POPULATION.PATCH.MEASUREMENT) each population requires dedicated vector
#EAST
patchesE <- do.call(rbind, strsplit(names(specs.EAST), "\\."))[, 3]   #vector
specs.by.patches.E <- aggspec(specs.EAST, by=patchesE, FUN=mean)
#MIDD
patchesM <- do.call(rbind, strsplit(names(specs.MIDD), "\\."))[, 3]   #vector
specs.by.patches.M <- aggspec(specs.MIDD, by=patchesM, FUN=mean)
#WEST
patchesW <- do.call(rbind, strsplit(names(specs.WEST), "\\."))[, 3]   #vector
specs.by.patches.W <- aggspec(specs.WEST, by=patchesW, FUN=mean)
#WEST+MIDD
patchesWM <- do.call(rbind, strsplit(names(specs.WEMI), "\\."))[, 3]  #vector
specs.by.patches.WM <- aggspec(specs.WEMI, by=patchesWM, FUN=mean)
explorespec(specs.by.patches.WM[1:7], by=6, lwd=2, col=mycols)

par(mfrow=c(2,2))

plot(specs.by.patches.E[1:7], xlim=c(300,700), ylim=c(0,100), lwd=2, col=mycols, main = "East")
plot(specs.by.patches.M[1:7], xlim=c(300,700), ylim=c(0,100), lwd=2, col=mycols, main = "Central")
plot(specs.by.patches.W[1:7], xlim=c(300,700), ylim=c(0,100), lwd=2, col=mycols, main = "Southwest")
plot(specs.by.patches.WM[1:7],xlim=c(300,700), ylim=c(0,100),  lwd=2, col=mycols, main = "WEST+MIDDLE")

par(mfrow=c(1,1))
explorespec(specs.EAST[1:127], by=patchesE, xlim=c(300,700), ylim=c(0,70), lwd=2, col="black")
explorespec(specs.WEMI[1:127], by=patchesWM, xlim=c(300,700), ylim=c(0,70), lwd=2, col="black")


write.csv(mspecs, "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Analysis/mspecs.csv")


var <- summary(mspecs)
write.csv(var, "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Analysis/variables.csv")

plot(mspecs, xlim=c(300,700), ylim=c(0,100))




