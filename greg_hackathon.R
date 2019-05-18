# Data competition

# setwd("/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon")

# setwd("/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon/Data")



# rm(list=ls())   


rm(list=ls()[!ls() %in% c("raw.covariate.dat", "raw.usage.dat", "citycounty",
	"n.covariate", "n.usage", "region")])
# rm(list=ls()[!ls() %in% c("raw.dat", "data", "ruralurban",
	# "citycounty", "employer.names", "fortune500", desc.files)])

# Whether to re-load data
load.dat <- TRUE

############# Load Libraries #################

print("Loading libraries...")


# library(leaps)
# library(RANN)
# library(gpls)
# library(vcd) # used for assocstats (calculating cramer's v)

library(lattice)
library(grid)
# library(Amelia)
# library(ggplot2)
# library(caret)
# library(pls)
library(plyr)
library(forecast)
# library(DMwR)
# library(penalized)
# library(elasticnet)
# library(gdata)
# library(MatchIt)
library(dplyr)
library(ggplot2)
library(glmnet)
# library(googleway)
# library(lsr)

################ Parameters ################

print("Storing parameters...")

# # directory where output should be stored
# dir.out <- "/Users/gregoryfaletto/Documents/R/Citadel/Models"

# directory where this R file lives
dir.main <- "/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon"

# directory where R raw, processed data files live
dir.dat <- "/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon/Data"

# # directory where geographic data files live
# dir.geo <- "/Users/gregfaletto/Documents/R/ZipRecruiter/Geographic data"

# # directory for models
# dir.mod <- "/Users/gregoryfaletto/Documents/R/Citadel/Models"
# dir.mod <- "/Users/gregfaletto/Dropbox"

# # name of current unprocessed data file with covariates (for propensity score
# # prediction)
# dat.prop <- "msa_data.csv"

# name of current unprocessed data file with water usage (responses)
dat.chems <- "cal_chemicals.csv"

# # name of processed data file
# dat.proc <- paste("processed_", substr(dat.resp, 1, nchar(dat.resp)-3), "R",
# 	sep="")


# # ############# Load Data ####################

if(load.dat){
	print("Loading data...")

	t0 <- Sys.time()

	## Read in training and data data, the latter may take a few minutes to run
	setwd(dir.dat)

	# Chemicals data
	raw.cal.chemicals.dat <- read.csv(file=dat.chems)
	# 2010 health outcomes
	health.2010 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2011 health outcomes
	health.2011 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2012 health outcomes
	health.2012 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2013 health outcomes
	health.2013 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2014 health outcomes
	health.2014 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2015 health outcomes
	health.2015 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2016 health outcomes
	health.2016 <- read.csv(file="2016_health_outcomes_clean.csv", header=TRUE)
	# 2017 health outcomes
	health.2017 <- read.csv(file="2017_health_outcomes_clean.csv", header=TRUE)
	# 2018 health outcomes
	health.2018 <- read.csv(file="2018_health_outcomes_clean.csv", header=TRUE)

	# # Propensity score matching data
	# raw.covariate.dat <- read.csv(file=dat.prop, head=TRUE, sep=",")
	# # Water usage (desired response)
	# raw.usage.dat <- read.csv(file=dat.resp, head=TRUE, sep=",")
	# # Earnings data
	# raw.earnings.dat <- read.csv(file="earnings.csv", head=TRUE, sep=",")
	# # City/county data
	# citycounty <- read.csv("us_cities_states_counties.csv", sep="|")
	# # make all county name lowercase
	# citycounty$County <- tolower(citycounty$County)
	# citycounty$City <- tolower(citycounty$City)
	# raw.usage.dat$COUNTY <- tolower(gsub(" County", "", raw.usage.dat$COUNTY))
	print("Data loaded!")
	setwd(dir.main)

	print("Time to load data:")
	print(Sys.time() - t0)
	
}

############ Duplicating data (so I keep a copy of raw unprocessed data file)

chemical_names <- levels(raw.cal.chemicals.dat$chemical_species)

years <- sort(unique(raw.cal.chemicals.dat$year))

fips <- sort(unique(raw.cal.chemicals.dat$fips))

# Construct data.frame for average chemical levels over 2000 - 2016
X.dat <- data.frame(fips)


# Average chemical levels over all years
for(i in 1:length(chemical_names)){
	# create a data.frame to temporarily hold averages for each county
	averages <- numeric(length(fips))
	for(j in 1:length(fips)){
		# For each fips, take the average level of this chemical over all years
		averages[j] <- mean(raw.cal.chemicals.dat[raw.cal.chemicals.dat$chemical_species==chemical_names[i] &
			raw.cal.chemicals.dat$fips==fips[j], "value"])
	}

	X.dat[, chemical_names[i]] <- averages
}

colnames(X.dat) <- c("FIPS", chemical_names)

# Remove NaN rows
for(i in 1:nrow(X.dat)){
	if(any(is.nan(as.matrix(X.dat[i,])))){
		X.dat <- X.dat[-i, ]
	}
}

# Re-name fips vector to only include remaining fips
fips <- X.dat$FIPS

# Response vector

Y <- numeric(length(fips))

for(j in 1:length(fips)){
	Y[j] <- health.2018[health.2018$FIPS==fips[j], "X..Fair.Poor"]
}

# # Save data
# write.csv(X.dat, file="X.csv")
# write.csv(Y, file="Y.csv")


# # plot data
# data.ggplot <- data.frame(X.dat, Y)

# ggplot(data=data.ggplot, aes(x=Arsenic, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=DEHP, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=Nitrates, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=Uranium, y=Y)) + geom_point()

# Fit lasso model
model <- cv.glmnet(x=as.matrix(X.dat[, 2:ncol(X.dat)]), y=Y)