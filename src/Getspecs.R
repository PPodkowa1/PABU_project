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


# 36 spectra plus the first (wl) column



# save .pdf with raw plots, default colors palette
pdf(file = "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/Article/Oct.pdf" )
par(mfrow=c(2,3))
explorespec(specs[, 1:1585], by = 3, lwd = 2)


#create selection vectors 
#'population' - vector
pop <- do.call(rbind, strsplit(names(specs), "\\."))[, 2]   #vector

# and  'body parts' - for each subset
sppEAST <- do.call(rbind, strsplit(names(EAST), "\\."))[, 3]   #vector
sppWEST <- do.call(rbind, strsplit(names(WEST), "\\."))[, 3]   #vector

#create data subset for each part of body
par(mfrow=c(2,3))

#neck
neck <- subset(specs, '1neck')
mneck <- aggspec(neck, by=3, FUN = mean)
#create category vector 
popneck <- do.call(rbind, strsplit(names(mneck), "\\."))[, 2]
aggplot(mneck, popneck,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="NECK", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
)

#abdomen
abdo <- subset(specs, '2abdo')
mabdo <- aggspec(abdo, by=3, FUN = mean)
popabdo <- do.call(rbind, strsplit(names(mabdo), "\\."))[, 2]
aggplot(mabdo, popabdo,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="ABDOMEN", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
)

#rump
rump <- subset(specs, '3rump')
mrump <- aggspec(rump, by=3, FUN = mean)
poprump <- do.call(rbind, strsplit(names(mrump), "\\."))[, 2]
aggplot(mrump, poprump,
        UN.center = mean,
        ylim = c(0, 50),
        alpha = 0.05, legend = TRUE, main="RUMP", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
        )

#side
side <- subset(specs, '4side')
mside <- aggspec(side, by=3, FUN = mean)
popside <- do.call(rbind, strsplit(names(mside), "\\."))[, 2]
aggplot(mside, popside,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="SIDE", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
)


#back
back <- subset(specs, '5back')
mback <- aggspec(back, by=3, FUN = mean)
popback <- do.call(rbind, strsplit(names(mback), "\\."))[, 2]
aggplot(mback, popback,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="BACK", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
)


#head
head <- subset(specs, '6head')
mhead <- aggspec(head, by=3, FUN = mean)
pophead <- do.call(rbind, strsplit(names(mhead), "\\."))[, 2]
aggplot(mhead, pophead,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="HEAD", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)]
)

#summary of plots
par(mfrow=c(2,3))
aggplot(mneck, popneck,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="THROAT", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])
aggplot(mabdo, popabdo,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="ABDOMEN", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])
aggplot(mrump, poprump,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="RUMP", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])
aggplot(mside, popside,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="SIDE", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])
aggplot(mback, popback,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legend = TRUE, main="BACK", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])
aggplot(mhead, pophead,
        UN.center = mean,
        ylim = c(0, 70),
        alpha = 0.05, legen= TRUE, main="HEAD", lcol = colors()[c(29, 555)], shadecol = colors()[c(29, 555)])


par(mfrow=c(1,1))
EAST <- subset(specs, 'EAST')
aggplot(EAST, sppEAST,
        FUN.center = mean,
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE, main="EAST", lcol = mycols, shadecol = mycols
)

WEST <- subset(specs, 'WEST')
aggplot(WEST, sppWEST,
       FUN.center = mean,
        ylim = c(0, 70),
        alpha = 0.3, legend = TRUE, main="WEST", lcol = mycols, shadecol = mycols
)

# Create custom palette based on the R chart as follow:
mycols <- colors()[c(555, 57, 451, 614, 48, 29)] #
# or you could enter the color names directly
# mycols <- c("red3", "coral", "magenta1", "springgreen4", "chartreuse1", "blue3")

#save .pdf with individuals specs of all patches
pdf(file = "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Plots/individuals")
meanspecs <- aggspec(specs, by=3, FUN=mean)
par(mfrow=c(2,2))
explorespec(meanspecs[1:403], by=6, lwd=2, col=mycols)


########### SUBSETTING AND AVERAGING DATA ##### by population or feathers patch
# subset EAST
specs.EAST <- subset(meanspecs, "EAST")
specs.EAST.rump <- subset(specs.EAST, "3rump")
mean.Erump <- aggspec(specs.EAST.rump, FUN=mean)
# plot EAST RUMP
plot(specs.EAST.rump)
plot(mean.Erump)

# subset WEST
specs.WEST <- subset(meanspecs, "WEST")
specs.WEST.rump <- subset(specs.WEST, "3rump")
mean.Wrump <- aggspec(specs.WEST.rump, FUN=mean)
#plot WEST rump
plot(specs.WEST.rump)
plot(mean.Wrump)



#create a vector "patches" to label each part of feathers  (it is the third part of the name ID.POPULATION.PATCH.MEASUREMENT) each population requires dedicated vector
#EAST
patchesE <- do.call(rbind, strsplit(names(specs.EAST), "\\."))[, 3]   #vector
specs.by.patches.E <- aggspec(specs.EAST, by=patchesE, FUN=mean)
explorespec(specs.by.patches.E[1:7], by=6, lwd=2, col=mycols)

#WEST
patchesW <- do.call(rbind, strsplit(names(specs.WEST), "\\."))[, 3]   #vector
specs.by.patches.W <- aggspec(specs.WEST, by=patchesW, FUN=mean)
explorespec(specs.by.patches.W[1:7], by=6, lwd=2, col=mycols)

par(mfrow=c(2,2))

plot(specs.by.patches.E[1:7], xlim=c(300,700), ylim=c(0,100), lwd=2, col=mycols, main = "EAST")
plot(specs.by.patches.W[1:7], xlim=c(300,700), ylim=c(0,100), lwd=2, col=mycols, main = "WEST")


par(mfrow=c(1,1))
explorespec(specs.EAST[1:127], by=patchesE, xlim=c(300,700), ylim=c(0,70), lwd=2, col="black")
explorespec(specs.WEMI[1:127], by=patchesWM, xlim=c(300,700), ylim=c(0,70), lwd=2, col="black")

mspecs <- aggspec(specs, by = 3, FUN = mean)
variables

write.csv(mspecs, "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/__PABU/Analysis/mspecs.csv")


var <- summary(mspecs)
write.csv(var, "C:/Users/Pawel/OneDrive - Uniwersytet im. Adama Mickiewicza w Poznaniu/Auburn University/Article/last_variables.csv")


