##################################################################
##                             Libs                             ##
##################################################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)

##########################################################################
##                           IMPORT FUNCTIONS                           ##
##########################################################################

source("lickMicroStructureFunctions.R")

##########################################################################
##                    LOAD FILES INTO DATAFRAME LIST                    ##
##########################################################################

# define path asking user
path <- rstudioapi::selectDirectory(caption = "Select folder with txt files")
# if this doesn't work assign it manually
path <- "/home/nicoluarte/nbolab_LICKMS/testFolder"

# load all text files into memory
# ! make sure only relevant .txt files are present
files <- dir(path, pattern = "*.txt")
# all text files are read as tibbles
tibbleListRaw <- files %>%
        map(~ getTextIntoTibble(file.path(path, .)))
# add a column that indicates the filename
numbers <- seq(from = 1, to = length(tibbleListRaw), by = 1)
tibbleListRaw <- map2(tibbleListRaw, numbers, function(.x, .y){
	     .x %>% mutate(
			   fileName = rep(files[.y], dim(.x)[1])
			   )

})
# load look up table
# change this path to the correct one
lookUptablePath <- "/home/nicoluarte/nbolab_LICKMS/lookUpTable.txt"
lookUpTable <- read.csv(lookUptablePath, header = FALSE, sep = ":")

##################################################################
##                    Initial pre-processing                    ##
##################################################################

# remove empty tibbles
tibbleListRaw <- purrr::keep(tibbleListRaw, ~ nrow(.x) > 0)
# change headers and datatypes
headers <- c(
        "date", "pc_time", "secStart", "msStart",
        "arduino", "spout", "licksCum", "eventsCum", "fileName"
)
dataTypes <- c(
        "character", "character", "integer", "integer",
        "factor", "factor", "integer", "integer", "character"
)
tibbleListRaw <- tibbleListRaw %>%
        map(~ headersIntoList(headers, .x))
# set variables data types
tibbleListRaw <- tibbleListRaw %>% map(function(x) {
        x %>% mutate(
                secStart = as.integer(secStart),
                msStart = as.integer(msStart),
                arduino = as.factor(arduino),
                spout = as.factor(spout),
                licksCum = as.integer(licksCum),
                eventsCum = as.integer(eventsCum)
        )
})
# get animal names corresponding to each arduino
# WARNING!: code 999 means that no corresponding value was found in the lookUpTable
tibbleListRaw <- map(tibbleListRaw, function(x){
	    r <- c()
	    for (i in 1:dim(x)[1]){
		    if (length(compareReturn(
					  x$fileName[i], as.integer(as.character(x$arduino[i])),
					  lookUpTable$V1, lookUpTable$V2,
					  lookUpTable$V3
					  )) == 0){
			    r[i] <- 999
		    }
		    else{
			    r[i] <- compareReturn(
					  x$fileName[i], as.integer(as.character(x$arduino[i])),
					  lookUpTable$V1, lookUpTable$V2,
					  lookUpTable$V3
						  )
		    }
	    }
	    x %>% mutate(
			 animalCode = r
			 )
})

##################################################################
##                      Feature extraction                      ##
##################################################################

# add basic features
tibbleList <- tibbleListRaw %>%
	map(function(x){ x %>%
	group_by(arduino, spout) %>%
	mutate(
	       ms = getMillis(secStart, msStart),
	       isEvent = getIsEvent(eventsCum),
	       isLick = getIsLick(eventsCum),
	       msFromEvent = getmsFromEvent(isEvent, ms),
	       isTimeout = getIsTimeout(msFromEvent, 20000, eventsCum)
	       ) %>%
	ungroup()
})
# add main features
tibbleList <- tibbleList %>%
	map(function(x) { x %>%
	group_by(arduino, spout, isLick) %>%
	mutate(
	       ILI = getILI(ms)
	       ) %>%
	ungroup()
})

##################################################################
##                      Data visualization                      ##
##################################################################

##################################################################
##                          Statistics                          ##
##################################################################
