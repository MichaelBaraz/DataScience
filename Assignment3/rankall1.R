rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## Author: Michael Baraz
    
    ##USe best function
    source("rankhospital.R")
    
    ## Set column number index based on input parm
    colIdx <- validateParm_Outcome(outcome)
    
    
    ## Read outcome data into data frame
    ## Load File and prepare data
    myFileName <- "outcome-of-care-measures.csv"
    df <- loadAndPrepareData(myFileName, colIdx)
    
    ##Load File and prepare data
    myFileName <- "outcome-of-care-measures.csv"
    df <- loadAndPrepareData(myFileName, colIdx)
    
    ##Get vector of state names:
    vec_StatesNames <- listGroupNames(df, "State")
    
    vec_HospNames <- sapply(vec_StatesNames, rankhospital, outcome = outcome, num = num)
    
    df_HospForState <- data.frame(hospital = vec_HospNames, state = vec_StatesNames)
    
    return(df_HospForState)
}



##Function: will return vector of group names
listGroupNames <- function(arg_Data, arg_GroupColumnName)
{
    ##build a contingency table with counts per group
    ##(group = "State" in this case; counts is num of hospitals per state)
    tbl_quantitiesPerGroup<-table(arg_Data[[arg_GroupColumnName]])
    
    ##make data frame from a contingency table
    df_quantitiesPerGroup<-data.frame(tbl_quantitiesPerGroup)
    
    ##return 1st column with group names
    return(df_quantitiesPerGroup[[1]])
}
