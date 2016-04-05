rankhospital <- function(state, outcome, num) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[, 7] <- as.factor(data[, 7])    # state as a factor variable
    data[, 11] <- as.numeric(data[, 11]) # heart attack
    data[, 17] <- as.numeric(data[, 17]) # heart failure
    data[, 23] <- as.numeric(data[, 23]) # pneumonia
    
    #handling invalid arguments
    posStates <- levels(data$State) #distinct states (54)
    posOutcomes <-c("heart attack", "heart failure", "pneumonia")
    
    if (is.element(state, posStates)==FALSE) {
        stop("invalid state")
    } else if (is.element(outcome, posOutcomes)==FALSE){
        stop("invalid outcome") 
    }
    
    #body of the function
    if (outcome=="heart attack"){
        outCol=11
    } else if(outcome=="heart failure"){
        outCol=17
    } else {
        outCol=23
    }
    
    cleanData <- data[!is.na(data[[outCol]]),]                  # removing NA's
    cleanData <- subset(cleanData, cleanData$State==state)      # removing other states
    orderedSet <- cleanData[order(cleanData[[outCol]],cleanData$Hospital.Name),]
    validRows <- nrow(orderedSet)
    
        if (num=="best"){
            reqRow = 1
        } else if(num=="worst"){
            reqRow = validRows
        } else {
            reqRow = num
        }
    
    orderedSet[reqRow,"Hospital.Name"]
}
