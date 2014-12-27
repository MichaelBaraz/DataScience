best <- function(state, outcome, isBest = TRUE) {
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
    
    
    ## Set column number index based on input parm
    colIdx <- validateParm_Outcome(outcome)

    
    ## Read outcome data into data frame
    ## Load File and prepare data
    myFileName <- "C:/Users/Michael.Baraz/Documents/Data Science/R/hospital/outcome-of-care-measures.csv"
    df <- loadAndPrepareData(myFileName, colIdx)

    ## now filter based on specified state 
    df <- filterByState(df, state)
    
    
    ## based on best/worst choice, get min or max
     if (isBest)
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




## Function: will load and prepare data.
loadAndPrepareData <- function(arg_filename, arg_NumColumnIdx)
{
    ## Load data from file as character
    df <- read.csv(arg_filename, colClasses = "character")
    
    ## Convert column values to numetic
    df[, arg_NumColumnIdx] <- suppressWarnings(as.numeric(df[, arg_NumColumnIdx]))

    return(df)
}


## Function: will load and prepare data.
filterByState <- function(arg_df, arg_State)
{
    ## Check that state and outcome are valid
    if (!(arg_State %in% arg_df$State)) 
    {
        stop("invalid state parameter (#1) specified, please provide a valid state code and retry")
    }

    ## get subset for the select state only
    arg_df <- subset(arg_df, arg_df$State == arg_State)

    return( arg_df )
}



## Function: will validate paramter "outcome" and return column index
validateParm_Outcome <- function(outcome)
{    
    ## Set column number index based on input parm
    colIdx <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23, 0)
    
    ## Validate input parm, if not found, stop and report error
    if (colIdx == 0)
    {
        stop("invalid outcome parameter (#2) specified, valid values are: heart attack, heart failure, pneumonia")
    }

    return( colIdx )
}

## Tests:
# best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
# best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
# best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
# best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome
