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

getTextIntoTibble <- function(textFilePath){
	tmp <- readLines(textFilePath)
	tmp <- gsub(pattern = '(-[0-9]*)( -)', replace = '\\1\\20,', x = tmp)
	tmp <- gsub(pattern = "-", replace = ",", x = tmp)
	tibbleOut <- as_tibble(do.call(rbind, strsplit(tmp, split = ",")))
	return(tibbleOut)
}

headersIntoList <- function(headers, tibbleList){
	names(tibbleList) <- headers
	return(tibbleList)
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


# getMillis
# @seconds: vector containing seconds from start
# @milliseconds: vector containing milliseconds from start
getMillis <- function(seconds, milliseconds){
	ms <- (seconds * 1000) + milliseconds
	return(ms - ms[1])
}

# getIsEvent
# @cumEvents: cummulative events or rewards
getIsEvent <- function(cumEvents){
	isEvent <- cumEvents - lag(cumEvents, default = first(cumEvents))
	return(isEvent)
}

# getIsLick
# @cumLicks: cummulative events or rewards
getIsLick <- function(cumLicks){
	isLick <- cumLicks - lag(cumLicks, default = first(cumLicks))
	return(isLick)
}

# msFromEvent}
# @isEvent: vector that defines for each row is a lick was done or not
# @ms: vector containing milliseconds from start, starting at 0
# countMsFromEvent does all the work
getmsFromEvent <- function(isEvent, ms){
	msFromEvent <- countMsFromEvent(isEvent, ms)
	return(msFromEvent)

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

# getIsTimeout
# @msFromEvent: milliseconds from every event
# @timeout: timeout used in milliseconds
# @cumEvents: cummulative events
# is timeout makes all the work
getIsTimeout <- function(msFromEvent, timeout, cumEvents){
	isTimeout <- isTimeout(msFromEvent, timeout, cumEvents)
	return(isTimeout)
}
isTimeout <- function(countMsFromEvent, timeOut, eventsCum){
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

# getILI
# @ms: milliseconds from the start
getILI <- function(ms){
	ILI <- abs(ms - lag(ms, default = first(ms)))
	return(ILI)
}

# getBurst
# ILI: interlick interval
# windowSize: the milliseconds passed in which you consider
# a lick as pertaining to a given burst
getBursts <- function(ILI, windowsSize){
	clusters <- c(0, cumsum(abs(diff(ILI <= windowsSize))))
	return(clusters * (ILI <= windowsSize))
}

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

# compareReturn
# @hashTable all relevant categories are a paste0, this is here for the key
# @value what to return from the hashtable
compareReturn <- function(hashTable, key){
	return(map_dbl(key, function(x){
		    hashTable$value[hashTable$key %in% x]
			   }))
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

latencyToFirstLick <- function(){
  # to any spout
  
}

