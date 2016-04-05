rankall <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[, 7] <- as.factor(data[, 7])    # state as a factor variable
    data[, 11] <- as.numeric(data[, 11]) # heart attack
    data[, 17] <- as.numeric(data[, 17]) # heart failure
    data[, 23] <- as.numeric(data[, 23]) # pneumonia
    
    #handling invalid outcomes
    posOutcomes <-c("heart attack", "heart failure", "pneumonia")
    
    if (is.element(outcome, posOutcomes)==FALSE) {
        stop("invalid outcome")
    }
    
    if (outcome=="heart attack"){
        outCol=11
    } else if(outcome=="heart failure"){
        outCol=17
    } else {
        outCol=23
    }
    
    cleanData <- data[!is.na(data[[outCol]]),] # removing NA's
    
    
    outFrame <- data.frame(hospital=character(54), state=character(54), stringsAsFactors = FALSE)
    
    for (i in 1:length(levels(data$State))) {
    
        stateData <- subset(cleanData, cleanData$State==levels(data$State)[i]) # removing other states
        orderedSet <- stateData[order(stateData[[outCol]],stateData$Hospital.Name),]
        validRows <- nrow(orderedSet)
        
        if (num=="best"){
            reqRow = 1
        } else if(num=="worst"){
            reqRow = validRows
        } else {
            reqRow = num
        }
        
        hosp <- as.character(orderedSet[reqRow,"Hospital.Name"])  # name of 'num' ranked hospital
        name <- as.character(levels(data$State)[i])               # name of current state
        
        outFrame[i,1] <- hosp
        outFrame[i,2] <- name
    }
    
    outFrame

}
