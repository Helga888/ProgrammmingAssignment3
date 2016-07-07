rankall <- function(outcome, num = "best")
{
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
  
  worst<-function(data){
    
    n_of_rows<-nrow(data)  
    data[n_of_rows,hospitalName]
  }
  
  
  
  #getting subset of the data which contains only three columns we are going to be working with
  hospitalColumnSubset<-hospitalsRawData[,c(hospitalName,stateColumn,outcomeUsed)]
  
  ndx<- order(hospitalColumnSubset[,stateColumn],hospitalColumnSubset[,outcomeUsed],hospitalColumnSubset[,hospitalName],na.last=NA)
  
  orderedHospitasColumnSubset <- hospitalColumnSubset[ndx,]
  splitByStateAndOrdered<-split(orderedHospitasColumnSubset,orderedHospitasColumnSubset[,stateColumn])
  
  if(num=="best"){
      num = 1
      ranked<-unlist(lapply(splitByStateAndOrdered, function(x) x[1,hospitalName]))
  }else if(num == "worst"){
      ranked<-unlist(lapply(splitByStateAndOrdered, worst))
  }else{
      ranked<-unlist(lapply(splitByStateAndOrdered, function(x) x[num,hospitalName]))
  }
  
  nm<-names(ranked)
  data.frame(hospital=ranked[nm],state=nm)
  
  
  
}