# STA 250 Project 4 Notes

setwd("~/Dropbox/Work/STA 250 Files & Project/STA250_HW4_Files")

# Load in offline and AccessPointLocations files

off.samp = offline[sample(nrow(offline),size=5000,replace=FALSE),]

test.plot = rPlot(y~x, color = "channelFrequency",
                  data = offline.samp, type = "point")

# NOTE: The picture in question's boundaries are
# roughly x:(0,14), y:(0,34)

# These are the access points we care about
rownames(AccessPointLocations)
apl.x = AccessPointLocations[,1]
apl.y = AccessPointLocations[,2]

# Let's take our building image
library('png')
bg <- readPNG("building.png")
plot(1:2, type='n', main="Building Background",
     xlab="x", ylab="y", xlim=c(0.5,33), ylim=c(-3,14))
lim <- par()
rasterImage(bg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

points(apl.x,apl.y, col = "red", pch = 15)
max(offline$x)
min(offline$x)
max(offline$y)
min(offline$y)


# GOAL: Need a separate plot for each router
# so want to strsplit the data accordingly

# Also want to consider the presence of multiple entries at the same point
# and orientation

# Separate plots for signal and for orientation?

routers = rownames(AccessPointLocations)

# Initialize list to contain each router's data
router.dat = list()
for(r in 1:6)
{
    router.dat[[r]] = data.frame(mac = integer(0), signal = double(0),
                                 channelFrequency = integer(0), mode = integer(0),
                                 time = character(0), id = character(0), x = double(0),
                                 y = double(0), orientation = double(0))
    
    rbind(router.dat,setNames(as.list(off.samp[which(off.samp$mac==routers[1]),]),
                              names(router.dat)))
}

# Sample Version
router1 = off.samp[which(off.samp$mac==routers[1]),]
router2 = off.samp[which(off.samp$mac==routers[2]),]
router3 = off.samp[which(off.samp$mac==routers[3]),]
router4 = off.samp[which(off.samp$mac==routers[4]),]
router5 = off.samp[which(off.samp$mac==routers[5]),]
router6 = off.samp[which(off.samp$mac==routers[6]),]

# Actual Version
router1 = offline[which(offline$mac==routers[1]),]
router2 = offline[which(offline$mac==routers[2]),]
router3 = offline[which(offline$mac==routers[3]),]
router4 = offline[which(offline$mac==routers[4]),]
router5 = offline[which(offline$mac==routers[5]),]
router6 = offline[which(offline$mac==routers[6]),]

router.all = rbind(router1,router2,router3,router4,router5,router6)





# Example with a portion of the assignment data
example = rPlot(y ~ x, data = router1, color="signal", type = "point",
                main = "Sample of Full Data")

example$addControls("x", value = "x", values = names(router1))
example$addControls("y", value = "y", values = names(router1))
example$addControls("color", value = "signal", values = names(router1))

example

example$show('inline') # Displays javascript
print(example) # Displays graph
example$print("chart1")

# Publishes the plot for public viewing on Rpubs
example$publish('Scatterplot', host = 'rpubs')




# Need to account for multiple points at same location - signal will vary
# depending on 

rPlot(signal~orientation, data = router1, color = "mac", type = "point")


plot(y~x, data = router1)


# gridSVG material

library(shiny)
runExample("05_sliders")
