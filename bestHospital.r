best<-function(state,outcome){
    hospitalsRawData<-read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    #Assigning column names to variables
    heartAttack<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    heartFailure<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    pneumonia<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    
    stateColumn<-"State"
    hospitalName<-"Hospital.Name"
    
    #Defigning what outcome column to use from user's intput+validation of input
    if(outcome=="heart attack"){
          outcomeUsed<-heartAttack
    }else if(outcome=="heart failure"){
          outcomeUsed<-heartFailure
    }else if(outcome=="pneumonia"){
          outcomeUsed<-pneumonia
    }else{
          stop("invalid outcome")
    }    
    
    #Validation of user's input for state
    if (!state%in%hospitalsRawData[,stateColumn])
          stop("invalid state")
    
    #getting subset of the data which contains only three columns we are going to be working with
    hospitalColumnSubset<-hospitalsRawData[,c(hospitalName,stateColumn,outcomeUsed)]
    
    #subsetting our three column data frame by the state we want to have information for
    hospitalByState<-hospitalColumnSubset[hospitalColumnSubset[,stateColumn]==state,]
    
    #getting the name of the best hospital in the given state where the outcome number is the smallest
    #which.min return the raw with the smallest value in a given column
    hospitalByState[which.min(hospitalByState[,outcomeUsed]),][,hospitalName]
    
}