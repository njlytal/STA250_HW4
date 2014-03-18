# STA 250 HW 4 Tests

# This contains various samples and examples in attempts to make
# different methods work.

#  ***** Testing out rCharts *****
rChart <- rCharts$new()


# ***** rCharts EXAMPLES *****

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')


# Example with a portion of the assignment data
rPlot(y ~ x|mac, data = off.samp, color="signal", type = "point")
# Need to account for multiple points at same location - signal will vary
# depending on orientation and other factors

rPlot(signal~orientation, data = off.samp, color = "mac", type = "point")


# ******* SAMPLE CODE FOR RgoogleMaps ********
lat = c(37.6,38.6,38.1);
lon = c(-121.5,-121,-122);
center = c(mean(lat), mean(lon));
zoom <- min(MaxZoom(range(lat), range(lon)));

MyMap <- GetMap(center=center, zoom=zoom, 
                markers = '&markers=color:blue|label:H|37.6,-121.5&markers=color:green|label:G|38.6,-121&markers=color:red|color:red|label:F|38.1,-122', destfile = "MyTile1.png");

sample.map = GetMap(center =c(lat=38.6,long=-121.5), zoom = 12, destfile = "maptile.png");

usa.map = GetMap(center = "United States", zoom = 4, destfile = "usamap.png")



# ******* SVG Attempts *********

# Background image


s = svg("offline.svg")
# Define and plot background image
bg <- readPNG("building.png")
plot(1:2, type='n', main="Router Signals",
     xlab="x", ylab="y", xlim=c(0.5,33), ylim=c(-3,14))
lim <- par()
rasterImage(bg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

svgPlot(points(router1$x, router1$y, xlim=c(0.5,33), ylim=c(-3,14), xlab = "X",
               ylab = "Y", col = "blue"))
addToolTips(s)

dev.off()


# ********* EXAMPLE FROM omegahat.org **********

# NOTE: findJScripts is a function in the official example
# that doesn't seem to exist. The line below has omitted
# that section, but does not keep unused graphs invisible.

alpha = seq(.01, by = 0.05, length = 30)
beta = seq(.01, by = 0.05, length = 30)

grid = expand.grid(alpha, beta)

f = 'beta.svg'
svg(f)
plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1.5), xlab = "X", ylab = "density",
     main = "Density of beta distribution")
apply(grid, 1, function(p) curve(dbeta(x, p[1], p[2]), 0, 1, n = 300, add = TRUE))
#curve(dbeta(x, .3, .7), 0, 1, add = TRUE)
#curve(dbeta(x, .7, .3), 0, 1, add = TRUE, col = "red")
dev.off()

#######
# Now post-process.

doc = xmlParse(f)
box = getViewBox(doc)
p = getPlotRegionNodes(doc)[[1]]

grid = expand.grid(seq(along = alpha), seq(along = beta))
ids = paste("curve", grid[,1], grid[,2], sep = "-")
invisible(
    sapply(seq(along = ids),
           function(i)
               addAttributes(p[[i]], .attrs = c(id = ids[i], visibility = "hidden"))))

addAttributes(p[[1]], .attrs = c(visibility = "visible"))

##########

svg = xmlRoot(doc)

enlargeSVGViewBox(doc, y = 100, svg = svg)

newXMLNode("g", attrs = c(id = "slider-alpha"), parent = svg)
newXMLNode("g", attrs = c(id = "slider-beta"), parent = svg)

newXMLNode("text",  attrs = c(x = "20",  y = box[2, 2], id = "statusText"), "", parent = svg)

addAttributes(svg, onload = sprintf("init(evt, %d, %d);", length(alpha), length(beta)))

addECMAScripts(doc, c("mapApp.js", "helper_functions.js", "slider.js", "betaSlider.js"))

addCSS(doc)

defs = getNodeSet(doc, "//x:defs", "x")[[1]]

newXMLNode("symbol", attrs = c(id = "sliderSymbol",  overflow = "visible"),
           newXMLNode("line", attrs = c(x1 = "0",  y1 = "-10",  x2 = "0",  y2 = "10",
                                        stroke = "dimgray", 'stroke-width' = "5",
                                        'pointer-events' = "none")),
           parent = defs)

saveXML(doc, docName(doc))

