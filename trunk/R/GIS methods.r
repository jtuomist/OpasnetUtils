########################################################################
# This section contains a couple of general GIS related functions
########################################################################

# Earth radius: quadratic mean or root mean square approximation of the average great-circle  
# circumference derives a radius of about 6372.8 km (Wikipedia).

earth.radius <- 6372.8

# Mathematical function to calculate the central angle of a great circle defined by it's endpoints given as spherical coordinates 
# excluding the radius which is constant by definition. 

central.angle <- function(theta1, phi1, theta2, phi2) {
	2 * asin((sin((theta1 - theta2) / 2)^2 + cos(theta1) * cos(theta2) * sin((phi1 - phi2) / 2)^2)^0.5)
}

# Difference in theta per difference of surface projected y on a spherical surface in degrees / km
dtheta.dy <- function(r) 1 / r * 180 / pi

# Difference in phi per difference of surface projected x on a spherical surface in degrees / km
dphi.dx <- function(r, theta) 1 / (cos(theta * pi / 180) * r) * 180 / pi

##############################
# GIS.Exposure
##########################
# Exposure computes exposure using a given concentration matrix and protected population data from Heande.
# Inputs: 
#	Concentration.matrix - A matrix containing spatially dependent concentratio data; 
#	LO & LA - coordinates of the center of the concentration matrix; 
#	distx & disty - maximum displacement from center of concentration matrix, assumed symmetrical, defaults to 10.5 km (PLTTI matrix);
#	resolution - resolution of concentration matrix, length of side of grid element which are assumed squares, defaults to 1 km (PILTTI matrix)
# Output:
#	An ovariable containing result of Population * Concentration. Output and marginal slots are defined. Spatial information lost in 
# 	summation (though there is an easy way around it). 
#########################################

GIS.Exposure <- function(
	Concentration.matrix, 
	LO, 
	LA, 
	distx = 10.5, 
	disty = 10.5, 
	resolution = 1,
	dbug = FALSE,
	...
) {
	# Ideally Longitude per kilometer would be calculated for each horizontal gridline separately, but satisfactory accuracy is achieved
	# by just approximating it at the center. 
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
	if(dbug) {
		cat("LaPerKm = ", LaPerKm, "\n")
		cat("LoPerKm = ", LoPerKm, "\n")
	}
	
	
	# Population. A function that searches and returns only relevant data from the database. Defined inside of GIS.Exposure to make it 
	# inaccessible outside of this function as the data involved is protected. Parameters are self explanatory or have been discussed above. 
	
	Population <- function(LO, LA, LaPerKm, LoPerKm, distx = 10.5, disty = 10.5, dbug = FALSE) {
		# Small wrapper functions used inside this function
		#GetPopLocs <- function(...) {
		#	return(opbase.old.locations.read("heande_base", "Heande3182", use.utf8 = TRUE, apply.utf8 = FALSE, ...))
		#}
		GetPopLocs <- function(index_name)
		{
			return(opbase.locations('Heande3182', index_name, username='heande', password=opbase.read_auth('heande')))
		}
		
		GetPopData <- function(...) {
			#return(opbase.old.read("heande_base", "Heande3182", use.utf8 = TRUE, apply.utf8 = FALSE, ...))
			return(opbase.data('Heande3182', username='heande', password=opbase.read_auth('heande'), ...))
		}
		# Download list of locations in data. 
		#pop.locs <- GetPopLocs()
		if (dbug) print("Fetching latitudes and longitudes...")
		locs.la <- GetPopLocs('Latitude')
		locs.lo <- GetPopLocs('Longitude')
		if (dbug) print('Done!')
		
		# Define selection where latitude falls within disty km of given coordinates.
		pop.slice.la <- locs.la[locs.la < LA + disty * LaPerKm & locs.la > LA - disty * LaPerKm]
		#pop.slice.la <- pop.locs$loc_id[
		#	pop.locs$ind == "Latitude" & 
		#	pop.locs$loc < LA + disty * LaPerKm & 
		#	pop.locs$loc > LA - disty * LaPerKm
		#]
		# Define selection where longitude is beyond distx.
		pop.slice.lo <- locs.lo[locs.lo < LO + distx * LoPerKm & locs.lo > LO - distx * LoPerKm]
		#]
		#pop.slice.lo <- pop.locs$loc_id[
		#	pop.locs$ind == "Longitude" &
		#	pop.locs$loc < LO + distx * LoPerKm &
		#	pop.locs$loc > LO - distx * LoPerKm
		#]
		# Define inverse of that because of the current database structure. 
		#pop.slice.lo.inverse <- pop.locs$loc_id[
		#	pop.locs$ind == "Longitude" &
		#	!pop.locs$loc_id %in% pop.slice.lo
		#]
	
		if (length(pop.slice.lo) == 0 || length(pop.slice.la) == 0) stop('No population on selected LA + LO')
		
		if(dbug) {
			cat("Matching LA locations in population data: ", paste(pop.slice.la, collapse = ", "), ".\n")
			cat("Matching LO locations in population data: ", paste(pop.slice.lo, collapse = ", "), ".\n")
		}
		# Download data within defined selection.
		#Population <- tidy(GetPopData(include = pop.slice.la, exclude = pop.slice.lo.inverse))
		if (dbug) print('Fetching the population data...')
		Population <- tidy(GetPopData(include = list('Latitude' = pop.slice.la, 'Longitude' = pop.slice.lo))) 
		if (dbug) print('Done!')
		# Convert textual values into numbers. 
		Population$Longitude <- as.numeric(as.character(Population$Longitude))
		Population$Latitude <- as.numeric(as.character(Population$Latitude))
		return(Population)
	}
	# Use function defined above
	Population <- Population(LO, LA, LaPerKm, LoPerKm, dbug = dbug)
	# Bin Population data into the defined grid. 
	Population$LObin <- cut(Population$Longitude, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	Population$LAbin <- cut(Population$Latitude, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	Population <- new(
		"ovariable",
		output = Population
	)
	# Define population marginal. 
	Population@marginal <- colnames(Population@output) %in% c("Iter", "LObin", "LAbin")
	
	if(dbug) {
		cat(colnames(Concentration.matrix@output), "\n")
		cat(colnames(Population@output), "\n")
	}
	# Calculating exposure. 
	temp <- Population * Concentration.matrix
	
	if(dbug) cat(colnames(temp@output), "\n")
	
	#temp <- oapply(temp, cols = c("LObin", "LAbin"), sum)
	# Sum over spatial data.
	out <- tapply(
		temp@output$Result, 
		temp@output[,colnames(temp@output)[temp@marginal & !colnames(temp@output) %in% c("LObin", "LAbin")]], 
		sum,
		na.rm = TRUE
	)
	
	out <- as.data.frame(as.table(out))
	
	colnames(out)[colnames(out) == "Freq"] <- "Result"
	
	temp@marginal <- colnames(out) %in% colnames(temp@output)[temp@marginal] & !colnames(out) %in% c("LObin", "LAbin")
	
	temp@output <- out
	
	return(temp)
}

######################################################
# GIS.Concentration.matrix 
##############################################
# Computes a concentration matrix from given emission and coordinates, based on random sampling PILTTI source-receptor-matrices.
# Inputs:
#	Emission - emission of substance in Mga^-1; 
#	LO & LA - coordinates where emission occurs; 
#	distx & disty - maximum displacement in kilometers from center of desired matrix, assumed symmetrical, 
#		defaults to 10.5 km (PLTTI matrix);
#	resolution - resolution of desired matrix (length in kilometers of side of grid element which are assumed squares), 
#		defaults to 1 km (PILTTI matrix);
#	N - number of iterations to be run
# Output:
#	An ovariable containing spatially dependent concentration data. Output and marginal slots are defined. 
##################################

GIS.Concentration.matrix <- function(
	Emission, 
	LO, 
	LA, 
	distx = 10.5, 
	disty = 10.5, 
	resolution = 1, 
	N = 1000, 
	dbug = FALSE, 
	...
) {
	LaPerKm <- dtheta.dy(earth.radius)
	LoPerKm <- dphi.dx(earth.radius, LA)
	
	# PILTTI source-receptor-matrices
	
	PILTTI.matrix <- tidy(op_baseGetData("opasnet_base", "Op_en5797")) # unit: ugm^-3/Mga^-1
	
	PILTTI.matrix$dy <- as.numeric(as.character(PILTTI.matrix$dy))
	PILTTI.matrix$dx <- as.numeric(as.character(PILTTI.matrix$dx))
	
	colnames(PILTTI.matrix)[colnames(PILTTI.matrix)=="Result"] <- "PILTTI.matrixResult"
	
	# Sampling; first make lists containing row numbers of individual matrices defined in the data. 
	ID.list <- tapply(1:nrow(PILTTI.matrix), PILTTI.matrix[,c("Kaupunki", "Vuosi", "Tyyppi")], list)
	# Then randomly pick N elements of that list to a new list. 
	ID.list.samples <- sample(ID.list, N, replace = TRUE)
	# For which we find the length of each individual list.
	ID.sample.lengths <- sapply(ID.list.samples, length)
	# Take all the values in the list and make one big vector out of it. 
	ID.vec <- unlist(ID.list.samples)
	# Use that vector to select corresponding rows from the original data. 
	PILTTI.matrix <- PILTTI.matrix[ID.vec,]
	# Add iteration indicator by repeating the numbers 1 to N according to the lengths of the list elements. 
	PILTTI.matrix$Iter <- rep(1:N, times = ID.sample.lengths)
	
	# dx and dy in PILTTI matrix is given in meters
	PILTTI.matrix$LObin <- cut(PILTTI.matrix$dx / 1000 * LoPerKm + LO, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
	PILTTI.matrix$LAbin <- cut(PILTTI.matrix$dy / 1000 * LaPerKm + LA, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
	
	PILTTI.matrix <- new(
		"ovariable",
		name = "PILTTI.matrix",
		output = PILTTI.matrix,
		marginal = colnames(PILTTI.matrix) %in% c("LObin", "LAbin", "Iter")
	)
	
	if(dbug) {
		cat(colnames(PILTTI.matrix@output), "\n")
		cat(colnames(Emission@output), "\n")
	}
	# Calculate concentratios based on emission and source-receptor-matrix
	out <- PILTTI.matrix * Emission
	return(out)
}