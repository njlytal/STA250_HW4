# STA 250 Project 4 Notes

setwd("~/Dropbox/Work/STA 250 Files & Project/STA250_HW4_Files")

# Load in offline and AccessPointLocations files

off.samp = offline[sample(nrow(offline),size=5000,replace=FALSE),]

test.plot = rPlot(y~x, color = "channelFrequency",
                  data = offline.samp, type = "point")

# NOTE: The picture in question's boundaries are
# roughly x:(0.5,33), y:(-3,14)

# These are the six access points we care about
# and their physical coordinates
rownames(AccessPointLocations)
apl.x = AccessPointLocations[,1]
apl.y = AccessPointLocations[,2]

# Rough idea of boundaries for the data
summary(offline$x)
summary(offline$y)

# Let's take our building image as a background.
# This confirms the coordinates we're working with, more or less
library('png')
bg <- readPNG("building.png") # If picture is in current dir
plot(1:2, type='n', main="Building Background",
     xlab="x", ylab="y", xlim=c(0.5,33), ylim=c(-3,14))
lim <- par()
rasterImage(bg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

points(apl.x,apl.y, col = "red", pch = 15)


# GOAL: Need a separate plot for each router
# so want to split the data accordingly

# Also want to consider the presence of multiple entries at the same point
# and orientation

# Separate plots for signal and for orientation?

# Names of the six routers we want to consider
routers = rownames(AccessPointLocations)


# Data for each router, separated
router1 = offline[which(offline$mac==routers[1]),]
router2 = offline[which(offline$mac==routers[2]),]
router3 = offline[which(offline$mac==routers[3]),]
router4 = offline[which(offline$mac==routers[4]),]
router5 = offline[which(offline$mac==routers[5]),]
router6 = offline[which(offline$mac==routers[6]),]

# offline data for only the six desired routers
router.all = rbind(router1,router2,router3,router4,router5,router6)


# Interactive plot with the assignment data

all.plots = function(router, router.all)
{
    plot1 = rPlot(router$y ~ router$x, data = router, color="signal", type = "point",
                  main = "Signal Data")
    
    # Adds simple menus to choose which variables to use for x, y, and color
    # (signal, orientation, x) and (signal, orientation, y) can tell us a bit
    # more about the data in question.
    plot1$addControls("x", value = "x", values = names(router.all))
    plot1$addControls("y", value = "y", values = names(router.all))
    plot1$addControls("color", value = "signal", values = names(router.all))
    
    print(plot1)
    
}

plot1 = rPlot(y ~ x, data = router1, color="signal", type = "point",
                main = "Signal Data")

# Adds simple menus to choose which variables to use for x, y, and color
# (signal, orientation, x) and (signal, orientation, y) can tell us a bit
# more about the data in question.
plot1$addControls("x", value = "x", values = names(router.all))
plot1$addControls("y", value = "y", values = names(router.all))
plot1$addControls("color", value = "signal", values = names(router.all))

plot1

plot1$show('inline') # Displays javascript
print(plot1) # Displays graph
plot1$print("chart1")

# Publishes the plot for public viewing on Rpubs
plot1$publish('Scatterplot', host = 'rpubs')

