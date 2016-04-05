# 1. exercise
# data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# data[, 11] <- as.numeric(data[, 11])
# hist(data[, 11], ylim=c(0,900))

# 2.exercise
best <- function(state, outcome) {
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
    
    cleanData <- data[!is.na(data[[outCol]]),]             # removing NA's
    cleanData <- subset(cleanData, cleanData$State==state) # removing other states
    
    # getting minimum death rate
    lowestRate <- min(cleanData[[outCol]]) 
    # getting hospitals having the min. rate
    bestSet <- subset(cleanData, cleanData[[outCol]]==lowestRate)
    # order the best hospitals alphabetically
    bestSet <- bestSet[order(bestSet$Hospital.Name),]
    # the name of the first hospital
    bestSet[1,"Hospital.Name"]
    
}
