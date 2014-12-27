rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    ## Author: Michael Baraz
    
    ##USe best function
    source("best.R")

    ## Set column number index based on input parm
    colIdx <- validateParm_Outcome(outcome)

    
    ## Read outcome data into data frame
    ## Load File and prepare data
    myFileName <- "C:/Users/Michael.Baraz/Documents/Data Science/R/hospital/outcome-of-care-measures.csv"
    df <- loadAndPrepareData(myFileName, colIdx)
    
    ## now filter based on specified state 
    df <- filterByState(df, state)
    
    
    ##check rank vs quantity
    ##will return subset of original data.

    num_quantityOfCases <- length(df[["State"]])
    ##if rank>quantity return NA
    if (is.numeric(num) && (num>num_quantityOfCases))
    {
        return("NA")
    }
    
    ##if num is "best" - return best
    if (!is.numeric(num) && (num=="best"))
    {
        return(best(state,outcome,TRUE))
    }
    
    ##if num is "worst" - return worst
    if (!is.numeric(num) && (num=="worst"))
    {
        return(best(state,outcome,FALSE))
    }

    
    ##Sort hospitals by names (to deal with ties)
    df <- df[order(df[[colIdx]], df[["Hospital.Name"]]),]
    df <- subset(df, subset=(!is.na(df[[colIdx]])))
    
    if(is.numeric(num))
    {
        return(df[num,"Hospital.Name"])
    }
    
}
