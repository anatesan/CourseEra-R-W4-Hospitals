# Find best hostpital in state for outcome
# check for bad input parameters

library(data.table)

rankHospital <- function(state, outcome, rank=1, 
                         fname="outcome-of-care-measures.csv",
                         dump=TRUE) { # dump entire State data
      my_data <- read.csv(fname,
                          colClasses = "character")
      
      
      # validate input parameters
      # state validation
      
      if (sum(state.abb == state) == 0) {
            stop(sprintf("Invalid state specified: %s\n", state))
      }
      
      validateOutcome(outcome)
      
      
      mortality_col <-mapMortalityCol(outcome)
      
      # filter by state
      
      state_filtered_dt <- data.table(subset(my_data, State == state))
      
      # filter out non-numeric value rows in mortaility columns
      
      # get logical vector to flag out non-numeric rows
      numeric_logical_vector<-!is.na(suppressWarnings(as.numeric(state_filtered_dt[[mortality_col]])))
      
      # now get a real row index leaving out the non-numeric ones...  First seq and then subset only non-na
      
      row_index_vect <- seq(numeric_logical_vector)[numeric_logical_vector]
      
      
      mortality_dt <- state_filtered_dt[row_index_vect]
      sorted_index_vect <- order(as.numeric(mortality_dt[[mortality_col]]), mortality_dt[["Hospital.Name"]], na.last=TRUE)
      
      # Get ranked row from sorted index.   If rank is too high,  return NA
      validateRank(rank)
      mapped_rank <- mapRank(rank)
      if (mapped_rank>length(sorted_index_vect)) {
            s <- NA
            message(sprintf("No hospital found for rank %5.0f for \"%s\" in the state of \"%s\"\n", mapped_rank, outcome, state) 
)
      } 
      else {
            
            # fix worst case rank
            
            if (mapped_rank<0) mapped_rank<-length(sorted_index_vect)
            
      
            ranked_index <-sorted_index_vect[mapped_rank]
            s<-sprintf("Ranked %3.0f for \"%s\" in the state of \"%s\" is \"%s\" with a 30-day mortality rate of %s\n", 
                       mapped_rank, outcome, state, mortality_dt[ranked_index, ]$Hospital.Name, mortality_dt[ranked_index, ][[mortality_col]])
            
            cat(s)
            
      }
      
      ## useful to dump always anyway and this gets 
      formatted_dt <- data.table(mortality_dt[sorted_index_vect]$State,
                                       mortality_dt[sorted_index_vect]$Hospital.Name,
                                       mortality_dt[sorted_index_vect, ][[mortality_col]])
      if(dump) {
            print(formattable(formatted_dt))
      }
      
      formatted_dt
      
      
}
