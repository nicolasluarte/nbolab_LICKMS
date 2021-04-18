library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)

parseLickometerRaw <- function(filePath,
                                  headers = c(),
                                  dataTypes = c()){
  unParsed <- readLines(filePath)
  unParsed <- gsub(pattern = '(-[0-9]*)( -)', replace = '\\1\\20,', x = unParsed)
  unParsed <- gsub(pattern = "-", replace = ",", x = unParsed)
  dataRaw <- as_tibble(do.call(rbind, strsplit(unParsed, split=",")))
  if (length(headers) != 0){
    colnames(dataRaw) <- headers
  }
  if (length(dataTypes) == 0){
    dataRaw[,3] <- as.integer(dataRaw[,3])
    dataRaw[,4] <- as.integer(dataRaw[,4])
    dataRaw[,5] <- as.factor(dataRaw[,5])
    dataRaw[,6] <- as.factor(dataRaw[,6])
    dataRaw[,7] <- as.integer(dataRaw[,7])
    dataRaw[,8] <- as.integer(dataRaw[,8])
  }
  else{
    dataRaw[] <- map2(dataRaw, str_c("as.", dataTypes), ~ get(.y)(.x))
  }
  dataRaw <- dataRaw %>% drop_na()
  return (dataRaw)
}

tagEvents <- function(dataRaw,
                      ECUM="eventsCum",
                      LCUM = "licksCum",
                      SEC="secStart",
                      MS="msStart",
                      ARDUINO="arduino",
                      SPOUT="spout"){
  # create milliseconds from start
  dataRaw <- dataRaw %>% mutate(ms = (!!sym(SEC) * 1000 + !!sym(MS)))
  # for each arduino ms starts from 0
  dataRaw <- dataRaw %>% group_by(!!sym(ARDUINO), !!sym(SPOUT)) %>%
    mutate(ms = ms - ms[1]) %>%
    mutate(isEvent = !!sym(ECUM) - lag(!!sym(ECUM), default = first(!!sym(ECUM)))) %>% 
    mutate(isLick = !!sym(LCUM) - lag(!!sym(LCUM), default = first(!!sym(LCUM)))) %>% 
    mutate(msFromEvent = countMsFromEvent(isEvent, ms)) %>% 
    mutate(isTimeOut = isTimeOut(msFromEvent, 20000, !!sym(ECUM)))
  dataRaw <- dataRaw %>% group_by(isLick, !!sym(ARDUINO), !!sym(SPOUT)) %>% 
    mutate(ILI = abs(ms - lag(ms, default = first(ms))))
  return(dataRaw)
}

countMsFromEvent <- function(isEvent, ms){
  timeVector <- c()
  anchor <- c()
  timeOut <- c()
  for (i in 1:length(isEvent)){
    if (isEvent[i] == 0){
      timeVector[i] <- ms[i]
      if (length(anchor != 0)){
        timeVector[i] <- ms[i] - anchor 
      }
    }
    else if (isEvent[i] == 1){
      anchor <- ms[i]
      timeVector[i] <- 0
    }
  }
  return(timeVector)
}

isTimeOut <- function(countMsFromEvent, timeOut, eventsCum){
  isTimeOut <- c()
  for (i in 1:length(countMsFromEvent)){
    if (countMsFromEvent[i] <= timeOut && eventsCum[i] >= 1){
      isTimeOut[i] <- 1
    }
    else{
      isTimeOut[i] <- 0
    }
  }
  return(isTimeOut)
}

latencyToFirstLick <- function(){
  # to any spout
  
}

## este es el ejemplo de la funcion
nLicks <- function(dataFrame){
  dataFrame$var1 = c(3,3,3)
  return(dataFrame)
}

df <- data.frame(var1 = c(3, 2, 1))
df2 <- nLicks(df)

# nLicksInTimeOut
# @isLick: a vector of 0 and 1 indicating if a lick was detected or not
# @isTimeout: a vector of 0 and 1 indicating if it was time out or not
nLicksInTimeOut <- function(isLick, isTimeout){
	# check for proper vector format
	if (!all(isLick %in% c(1, 0))) { stop("isLick vector not correct")}
	if (!all(isTimeout %in% c(1, 0))) { stop("isTimeOut vector not correct")}
	if (!length(isLick) == length(isTimeout)) { stop("Vectors need to be of the same size")}
	isLickVector <- as.vector(isLick)
	isTimeoutVector <- as.vector(isTimeout)
	v <- isLickVector * isTimeoutVector
	returnList <- list(licksInTimeout = v,
			   nLicksInTimeout = sum(v),
			   nLicksOutOfTimeout = sum(isLickVector) - sum(v))
	return(returnList)
}

# nEvents
# @eventsCum: a vector which contains the cumulative count if events
nEvents <- function(eventsCum){
	if(!is.numeric(eventsCum)) { stop("Vector should be numeric")}
	return(list(nEvents = max(as.vector(eventsCum))))
}

# binsMs
# @ms: a vector containg the ms from start, starting from 0
# @windowsSize: how many ms per bin
# return: a vector containing the bins of ms, according to range
binsMs <- function(ms, windowsSize){
	if(!is.numeric(ms)) { stop("Vector should be numeric")}
	return(list(msBins = cumsum(ms %% windowsSize == 0)))
}

tagBurst <- function(){
  
}

tagPauses <- function(){
  
}

licksPerMs <- function(){
}

path <- "/home/nicoluarte/drive/Resultados OBJ 4/PR RATONES CANULADOS MARZO 2021 (206 al 216)/PR"
files <- dir(path, pattern = "*.events_.txt")
dfList <- files %>%
  map(~ parseLickometerRaw(file.path(path, .),
                           headers = c("date", "pc_time", "secStart", "msStart",
                                   "arduino", "spout", "licksCum", "eventsCum"),
                           dataTypes = c("character", "character", "integer", "integer", "factor", "factor", "integer", "integer")))


## AQUI SE CARGA EL ARCHIVO
dataFrame <- parseLickometerRaw("aqui va nombre del archivo.txt",
                           headers = c("date", "pc_time", "secStart", "msStart",
                                   "arduino", "spout", "licksCum", "eventsCum"),
                           dataTypes = c("character", "character", "integer", "integer", "factor", "factor", "integer", "integer"))

# check for empty datasets
dfList <- purrr::keep(dfList, ~ nrow(.x) > 0)

dfListTagged <- dfList %>% map(~ tagEvents(.))
dfTagged <- dfList %>% map_dfr(~ tagEvents(.))

# break to categorize ms
dfTagged$cat <- cut(dfTagged$ms, 
                   breaks=c(-Inf, 1500000, 3000000, Inf), 
                   labels=c("inicio","medio","fin"))






## N of licks

dfTagged %>% ggplot(aes(x = ms, y = licksCum, color = spout)) +
  geom_smooth()

dfTagged %>% ggplot(aes(x = ms, y = ILI, color = as.factor(isTimeOut))) +
  geom_smooth(method = "lm")

dfTagged %>% ggplot(aes(ILI, fill = as.factor(eventsCum))) +
  geom_density(alpha = 0.5) + xlim(0, 500) +
  theme_bw()

dfTagged %>% filter(isLick == 1) %>% ggplot(aes(ILI, fill = as.factor(cat))) +
  geom_density(position = "stack", adjust = 2) + xlim(0, 2500) +
  theme_bw()




