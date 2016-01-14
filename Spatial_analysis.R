#Summary 
#Part 1 Load Packages , setting WD, & Load dataset
#Part 2 Load data , Assigning Variables & Summary Statistics
#Part 3 Load data , Assigning Variables & Summary Statistics



##################Part 1 #############################
#install.packages("spdep")
#install.packages("Hmisc")  
#install.packages("maptools") 

	library(spdep)
	library(Hmisc)
	library(maptools)
	setwd("C:/Users/Owner/Documents/R/win-library/3.0/spdep/shapes


##################Part 2 #############################
#Loading Data and Summary statistics

	data(boston)
	str(boston.c)
	summary(boston.c)
	
#Defining the Variables 

	dep = boston.c[["MEDV"]]
	logdep = log(dep)
	RM = boston.c[["RM"]]
	AGE = boston.c[["AGE"]]
	CRIM = boston.c[["CRIM"]]
	TAX = boston.c[["TAX"]]

##################Part 3 #############################

	boston.map = readShapePoly(system.file("etc/shapes/boston_tracts.shp", package = "spdep")[1], ID = "poltract", proj4string = CRS(paste("+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66", "+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")))
	colours = rev(bpy.colors(n = 5, cutoff.tails = 0.3))
	colours2 = rev(bpy.colors(n = 6, cutoff.tails = 0.1))

	old.mar = par("mar")
	coords = coordinates(boston.map)


#This defines the margins
	
	old.mar = c(0.4, 0.4, 0.4, 0.4)
	old.par = par(mar = old.mar)
	old.par = par(family = "serif")

#Plotting the Median Prices Map- I use intervals based on the quantiles

	plot(boston.map, col = colours[findInterval(dep, brks <- c(0 , 11, 20, 30, 40, 45), all.inside = TRUE)])
	legend("bottomleft", legend = c("0-10", "11-19", "20-29", "30-39", "40-55", "46-50"), fill = colours, bty = "n", title = "Legend:
	Housing:
	Prices in Boston
	Moran's I .31")


#Plotting the CRIME Map- Out of Curiosity
	plot(boston.map, col = colours2[findInterval(CRIM, brks <- c(0 , 5, 10, 15, 20), all.inside = TRUE)])
	legend("bottomleft", legend = c("0-4", "5-10", "11-15", "16-20","21+"), fill = colours, bty = "n", title = "Legend:
	Crime:
 	in Boston
	Moran's I .29")


#Doing a graph to view the data

	plot(dep)
	plot(AGE, dep)
	plot(boston.map)
###---------------------------------------------------------------------------------------------------------------------####

#CREATING THE SPATIAL WEIGHT MATRIX- I used only 1st order contiguity neighbor.
#It seems appropiate as the dependant variable is housing prices. 
	
	boston.cont= poly2nb(boston.map, row.names = NULL, queen = TRUE, snap = sqrt(.Machine$double.eps)*1000000)
	boston.cont2= poly2nb(boston.map, row.names = NULL, queen = FALSE, snap = sqrt(.Machine$double.eps)*1000000)

	W.listW=nb2listw(boston.cont, style="B") #Converts the square matrix as a sequence of numbers
	W.listc=nb2listw(boston.cont, style="C") #This takes the matrix above and gives it weights Here we do Global Standarized
	W.listw=nb2listw(boston.cont, style="W") #This takes the matrix above and gives it weights Here we do Row Standarized
	write.table(W.listw, "C:/Users/Owner/Desktop/SWM.txt", sep="\t") #This writes a table for future. 

	moran.test(dep, W.listc, zero.policy = FALSE)
	moran.test(CRIM, W.listc, zero.policy = FALSE)


#This are 3 different type of Weights on Basic binary, Row and Globally Standarized

	summary(W.listW)
	summary(W.listc)
	summary(W.listw)
	summary(boston.cont)	#THERE ARE NO REGIONS WITHOUT ANY NEIGHBORS

#This seems a bit strange, there is 1 region with 44 neighbors- Double Check 

	par(mar = c(0, 0, 0, 0) + 0.00001)
	plot(boston.map, axes = FALSE)
	plot(boston.cont, coords, add = TRUE)
	
	plot(boston.map, axes = FALSE)
	plot(boston.cont2, coords, add = TRUE)

#I only use this weights. I do not include distance , or K- nearest neighbors

###---------------------------------------------------------------------------------------------------------------------####


# MI Test and plot of dependent variable
	
	moran.test(dep, W.listc, zero.policy = FALSE)
	moran.test(dep, W.listw, zero.policy = FALSE) #Results on .41
	moran.plot(dep, W.listc, zero.policy = NULL)

geary.test(dep, W.listw, randomisation=TRUE, zero.policy=NULL,
    alternative="greater", spChk=NULL, adjust.n=TRUE)


# 1)Transformed the dependant variable to log to measure %
# 2)Used a 3rd degree polynomial for crimed

	dep.lm = lm(dep ~ RM + AGE + TAX + poly(CRIM, degree = 3, raw = FALSE))
	dep.lm2 = lm(logdep ~ RM + AGE + TAX+ poly(CRIM, degree = 3, raw = FALSE))
	

	summary(dep.lm)
	lm.morantest(dep.lm, W.listc)
	lm.morantest(dep.lm, W.listw)
	lm.morantest(dep.lm2, W.listc)
	lm.morantest(dep.lm2, W.listw)
	
# plots residuals

	par(mar = c(0, 0, 0, 0) + 0.00001)
	plot(boston.map, col = colours[findInterval(residuals(dep.lm), brks <- round(quantile(residuals(dep.lm), probs=seq(0,1,0.2)), digits=2), all.inside=TRUE)])
	legend("bottomright", legend = leglabs(brks), fill = colours, bty = "n", title = "Legend:
	Residuals from
	linear model")



#We do see that there is some autocorrelation. 

# plot normality of residuals
	par(mfrow = c(2, 2))
	plot(dep.lm)

# moran scatterplot
	moran.plot(dep, listw = W.listW)

# LM tests
	lm.LMtests(dep.lm, listw = W.listw, zero.policy=FALSE, test="all")

#Using a GLS & Spatial Durbin Model 


slag.lm.cont1 = lagsarlm(dep ~ RM + AGE + TAX + poly(CRIM, degree = 3, raw = FALSE), listw = W.listc)
slag.lm.cont2 = lagsarlm(dep ~ RM + AGE + TAX + poly(CRIM, degree = 3, raw = FALSE), listw = W.listw)


summary(slag.lm.cont1)
summary(slag.lm.cont2)

anova(slag.lm.cont1, slag.lm.cont2) # best is contiguity of order one!!!
moran.test(residuals(slag.lm.cont1), W.listw, zero.policy = FALSE)




#Using the SPATIAL FILTERING 


par(mar = c(0, 0, 0, 0) + 0.00001)
	plot(boston.map, col = colours[findInterval(residuals(slag.lm.cont1), brks <- round(quantile(residuals(slag.lm.cont1), probs=seq(0,1,0.2)), digits=2), all.inside=TRUE)])
	legend("bottomright", legend = leglabs(brks), fill = colours, bty = "n", title = "Legend:
	Residuals from
	Spatial Durbin
	MI .28")



par(mar = c(0, 0, 0, 0) + 0.00001)
	plot(boston.map, col = colours2[findInterval(residuals(slag.lm.cont1), brks <- round(quantile(residuals(slag.lm.cont1), probs=seq(0,1,0.2)), digits=2), all.inside=TRUE)])
	legend("bottomright", legend = leglabs(brks), fill = colours, bty = "n", title = "Legend:
	Residuals from
	Spatial Durbin
	MI .28")


moran.test(residuals(slag.lm.cont1), W.listw, zero.policy = FALSE)


par(mar = c(0, 0, 0, 0) + 0.00001)
	plot(boston.map, col = colours[findInterval(residuals(slag.lm.cont2), brks <- round(quantile(residuals(slag.lm.cont1), probs=seq(0,1,0.2)), digits=2), all.inside=TRUE)])
	legend("bottomright", legend = leglabs(brks), fill = colours, bty = "n", title = "Legend:
	Residuals from
	Spatial Durbin
	MI .08")

moran.test(residuals(slag.lm.cont2), W.listw, zero.policy = FALSE)


