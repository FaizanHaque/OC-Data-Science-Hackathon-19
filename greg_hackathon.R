# Data competition

# setwd("/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon/Data")

# setwd("/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon")

# setwd("/Users/gregfaletto/Documents/GitHub/OC-Data-Science-Hackathon-19")



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
# library(forecast)
# library(DMwR)
# library(penalized)
# library(elasticnet)
# library(gdata)
# library(MatchIt)
library(dplyr)
library(ggplot2)
library(glmnet)
library(gglasso)
library(stabs)
# library(googleway)
# library(lsr)

################ Parameters ################

print("Storing parameters...")

# # directory where output should be stored
# dir.out <- "/Users/gregoryfaletto/Documents/R/Citadel/Models"

# # directory where this R file lives
# dir.main <- "/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon"

# # directory where R raw, processed data files live
# dir.dat <- "/Users/gregfaletto/Google Drive/Data Science/R/OCRUG Hackathon/Data"

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
	# setwd(dir.dat)
	dir.main <- getwd()

	# Chemicals data
	setwd("External Data/Chemicals/")
	raw.cal.chemicals.dat <- read.csv(file=dat.chems)
	# 2010 health outcomes
	setwd(dir.main)
	setwd("External Data/Health Outcomes/")
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

	# rurality data
	setwd(dir.main)
	setwd("External Data/Rurality/")
	rurality <- read.csv(file="ruralurbancodes2013.csv", header=TRUE)

	# agricultural use data
	setwd(dir.main)
	setwd("External Data/agricultural_use/")
	agricultural <- read.csv(file="data_213608.csv", header=TRUE)

	# earnings data
	setwd(dir.main)
	setwd("External Data/Earnings/")
	earnings <- read.csv(file="earnings.csv", header=TRUE)

	# Demographics data
	setwd(dir.main)
	setwd("External Data/Demographics/")
	demographics <- read.csv(file="cc-est2017-alldata-06.csv", header=TRUE)

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

# Add rurality levels to X
rurality.fips <- integer(length(fips))
for(j in 1:length(fips)){
	# For each fips, take the average level of this chemical over all years
	rurality.fips[j] <- rurality[rurality$FIPS==fips[j], "RUCC_2013"]
}

# Plot histogram of rurality levels
df.rurality <- data.frame(rurality.fips)

rurality.plot <- ggplot(df.rurality, aes(x=rurality.fips)) +
	geom_histogram(binwidth=1)
print(rurality.plot)

# Use coarser classification for rurality
rurality.fips[rurality.fips %in% c(3, 4, 5)] <- 3
rurality.fips[rurality.fips %in% c(6, 7, 8, 9)] <- 4

rurality.fips <- factor(rurality.fips, ordered=TRUE)

X.dat <- data.frame(X.dat, rurality.fips)
colnames(X.dat)[ncol(X.dat)] <- "rurality"

# X.dat$rurality <- as.factor(X.dat$rurality)

# Add percentage of land use for agriculture to X
percent.agricultural.fips <- integer(length(fips))
for(j in 1:length(fips)){
	# For each fips, take the average level of this chemical over all years
	percent.agricultural.fips[j] <- agricultural[agricultural$countyFIPS==fips[j],
	"Value"]
}

X.dat <- data.frame(X.dat, percent.agricultural.fips)
colnames(X.dat)[ncol(X.dat)] <- "pct.agricultural"

# Add earnings data to X
earnings.fips <- integer(length(fips))
for(j in 1:length(fips)){
	# For each fips, take the average level of this chemical over all years
	earnings.fips[j] <- earnings[earnings$fips==fips[j] & earnings$year==2016,
		"total_med"]
}

X.dat <- data.frame(X.dat, earnings.fips)
colnames(X.dat)[ncol(X.dat)] <- "earnings"

# Demographic data: remove all but last year, California data
demographics <- demographics[demographics$STNAME=="California" &
	demographics$YEAR==10, ]
# Add FIPS numbers to demographic data
raw.cal.chemicals.dat$county  <- tolower(gsub(" County", "",
	raw.cal.chemicals.dat$county))
demographics$CTYNAME <- tolower(gsub(" County", "", demographics$CTYNAME))

counties <- unique(demographics$CTYNAME)

fips <- numeric(nrow(demographics))
demographics <- data.frame(demographics, fips)
for(i in 1:length(counties)){
	if(any(raw.cal.chemicals.dat$county==counties[i])){
		demographics[demographics$CTYNAME==counties[i], "fips"] <-
			raw.cal.chemicals.dat[raw.cal.chemicals.dat$county==counties[i],
			"fips"][1]
	} else{
		print(paste("ERROR: no match for county", counties[i]))
	}
	
}

fips <- X.dat$FIPS

# Add over.65 (percentage of population over 65) data to X
over.65.fips <- integer(length(fips))
for(j in 1:length(fips)){
	# Find proportion of population over 65
	tot.over.65 <- sum(demographics[demographics$fips==fips[j] &
		demographics$AGEGRP>13, "TOT_POP"])
	tot.pop <- sum(demographics[demographics$fips==fips[j], "TOT_POP"])
	over.65.fips[j] <- tot.over.65/tot.pop
}

X.dat <- data.frame(X.dat, over.65.fips)
colnames(X.dat)[ncol(X.dat)] <- "pct.over.65"


# Add race data to X
white.fips <- integer(length(fips))
for(j in 1:length(fips)){
	# Find proportion of population over 65
	tot.white <- sum(demographics[demographics$fips==fips[j], c("WA_MALE",
		"WA_FEMALE")])
	tot.pop <- sum(demographics[demographics$fips==fips[j], "TOT_POP"])
	white.fips[j] <- tot.white/tot.pop
}

X.dat <- data.frame(X.dat, white.fips)
colnames(X.dat)[ncol(X.dat)] <- "pct.white"



# Response vector

Y <- numeric(length(fips))

for(j in 1:length(fips)){
	Y[j] <- health.2018[health.2018$FIPS==fips[j], "X..Fair.Poor"]
}

# # Save data
# write.csv(X.dat, file="X.csv")
# write.csv(Y, file="Y.csv")

# plot data
data.ggplot <- data.frame(X.dat[, -1], Y)

# ggplot(data=data.ggplot, aes(x=Arsenic, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=DEHP, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=Nitrates, y=Y)) + geom_point()
# ggplot(data=data.ggplot, aes(x=Uranium, y=Y)) + geom_point()

# # Fit lasso model
# model <- cv.glmnet(x=as.matrix(X.dat[, 2:ncol(X.dat)]), y=Y)

linear.model <- lm(Y~Arsenic+Nitrates+Uranium+rurality+pct.agricultural+
	earnings+pct.over.65 + pct.white, data.ggplot)
linear.model.ints <- lm(Y~Arsenic+Nitrates+Uranium +rurality + pct.agricultural
	+ earnings +pct.over.65 + Arsenic:Nitrates + Arsenic:Uranium 
	+ Nitrates:Uranium + rurality:Arsenic + rurality:Nitrates 
	+ rurality:Uranium, data.ggplot)

linear.model.ints.2 <- lm(Y~Nitrates +rurality + earnings + pct.over.65
	+ Uranium + Arsenic:Uranium + Nitrates:Uranium + Arsenic:rurality
	+ Uranium:rurality, data.ggplot)

linear.model.ints.3 <- lm(Y~Nitrates + earnings + pct.over.65, data.ggplot)

# X.pred <- model.matrix(X.dat[, -1])[, -1]

# Lasso model
formula <- as.formula(Y ~ .)
# Second step: using model.matrix to take advantage of f
X.pred <- model.matrix(formula, data.ggplot)[, -1]
lasso.fit <- cv.glmnet(x=X.pred, y=Y)

# Lasso model with interactions
formula.int <- as.formula(Y ~ .*.)
# Second step: using model.matrix to take advantage of f
X.pred.int <- model.matrix(formula.int, data.ggplot)[, -1]
lasso.fit.ints <- cv.glmnet(X.pred.int, Y)

# # Stability selection
# stabs.selec <- stabsel(X.pred, Y, cutoff=0.55, q=5)

# Group lasso
groups <- c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9)
g.lasso.fit <- cv.gglasso(x=X.pred, y=Y, group=groups, nfolds=10)

# Group lasso with interactions
groups.ints <- c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 11, 12, 13, 13, 13, 14,
	15, 16, 17, 18, 19, 20, 20, 20, 21, 22, 23, 24, 25, 26, 26, 26, 27, 28, 29,
	30, 31, 31, 31, 32, 33, 34, 35, 36, 36, 36, 37, 37, 37, 38, 38, 38, 39, 39,
	39, 40, 41, 42, 43, 44, 45)
g.lasso.fit.int <- cv.gglasso(x=X.pred.int, y=Y, group=groups.ints, nfolds=10)
