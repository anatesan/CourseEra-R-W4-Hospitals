# this file has a number of low level utility functions
# If there is an error in parameters - we just stop per instructions

# validate correctness of outcome
validateOutcome <- function(outcome) {
      # validate outcome
      
      valid_outcomes = c("Heart Attack", "Heart Failure", "Pneumonia")
      
      if (sum(toupper(valid_outcomes) == toupper(outcome)) == 0) {
            stop(sprintf("Invalid outcome specified: %s\n", outcome))
      }
      TRUE
}

# Rank should be Best, Worst or some numeric rank - otherwise error
validateRank <- function(rank) {
      if (toupper(rank) == "BEST" | toupper(rank) == "WORST") {
            return(TRUE)
      }
      
      if (is.na(as.numeric(rank))) {
            stop(sprintf("Invalid rank: %s\n", rank))
      }
      
      return(TRUE)
}

# validate all the parameters

validateState <- function(state) {
      # validate input parameters
      # state validation
      
      if (sum(state.abb == state) == 0) {
            stop(sprintf("Invalid state specified: %s\n", state))
      }
      
      TRUE
}

validateFile <- function(fname) {
      # validate file name
      if (!file.exists(fname)) {
            stop(sprintf("Invalid filename specified: %s\n", fname))
            
      }
      TRUE
      
}

# Based on outcome,  return a mapping to right Mortality col name

mapMortalityCol <- function(outcome) {
      mortality_col <-
            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      
      if (!validateOutcome(outcome)) {
            stop(sprintf("Invalid outcome: %s\n", outcome))
      }
      
      else if (toupper(outcome) == toupper("Heart Attack")) {
            mortality_col <-
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      }
      else if (toupper(outcome) == toupper("Heart Failure")) {
            mortality_col <-
                  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      }
      else  {
            mortality_col <-
                  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      }
      mortality_col
}

# translate rank into a number.  Best gets mapped to 1,  Worst to -1

mapRank <- function(rank_str) {
      numeric_rank <- 1
      
      if (toupper(rank_str) == "BEST") {
            numeric_rank <- 1
      }
      else if (toupper(rank_str) == "WORST") {
            numeric_rank <- -1
      }
      else {
            numeric_rank <- as.integer(rank_str)
      }
}

# getRankedValue - return appropriate value at ranked level.
# this is called on the data.table

getRankedValue <- function(vect = c(), rank = 1) {
      # if rank > length of vect, return NA
      # if rank == -1,  get last value of vect
      
      
      
      if (rank == -1) {
            val<- (vect[length(vect)])
      }
      else if (rank > length(vect)) {
            if (is.character(vect)) {
                  val<-as.character(NA)
            }
            else if (is.numeric(vect)) {
                  val<-is.numeric(NA)
            }
            else if (is.integer(vect)){
                  val<-(as.integer(NA))
                  
            }
            else val<-NA
      }
      else {
            val<-vect[rank]
      }
      val
}
