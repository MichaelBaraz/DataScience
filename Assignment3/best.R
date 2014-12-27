best <- function(state, outcome) {
    ## Purpose: 
#    Take two arguments: 1) 2-character abbreviated name of a state, 2) an outcome name.
#    . 
#    The function reads the outcome-of-care-measures.csv le and returns a character
#    vector with the name of the hospital that has the best (i.e. lowest) 30-day
#    mortality for the specied outcome in that state. The hospital name is the 
#    name provided in the Hospital.Name variable. The outcomes can be one of
#    \heart attack", \heart failure", or \pneumonia". Hospitals that do not have
#    data on a particular outcome should be excluded from the set of hospitals when
#    deciding the rankings.
#    
#    Handling ties. If there is a tie for the best hospital for a given outcome, 
#    then the hospital names should be sorted in alphabetical order and the rst 
#    hospital in that set should be chosen (i.e. if hospitals \b", \c",
#    and \f" are tied for best, then hospital \b" should be returned).    

    ## Author: Michael Baraz
    
    ## Decide if looking for best or worst
    arg_best <- TRUE
    
    ## Read outcome data into data frame
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (!(state %in% df[,7])) 
    {
        stop("invalid state parameter (#1) specified, please provide a valid state code and retry")
    }

    ## Set column number index based on input parm
    colIdx <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)

    ## Validate input parm, if not found, stop and report error
    if (colIdx == 0)
    {
        stop("invalid outcome parameter (#2) specified, valid values are: heart attack, heart failure, pneumonia")
    }

    ## get subset for the select state only
    df <- subset(df,df$State == state)

    ## Convert column values to numetic
    df[, colIdx] <- suppressWarnings(as.numeric(df[, colIdx]))

    ## based on best/worst choice, get min or max
     if (arg_best)
     {
         valMin <- min(df[[colIdx]], na.rm=TRUE)
         df <- subset(df, df[[colIdx]] == valMin)
     }
     else
     {
         valMax <- max(df[[colIdx]], na.rm=TRUE)
         df <- subset(df,df[[colIdx]] == valMax)
     }
    
    ## If tie, sort hospitals by name
    if (nrow(df) > 1)
    {
        df <- df[order(df[["Hospital.Name"]]),]
    }
    
    ## return only 1 result
    return(df[1,"Hospital.Name"])
    
}
