# Find best hostpital in state for outcome
# let rankHospital do the heavy lifting.   Best sinply specifies Rank 1

library(data.table)

best <- function(state, outcome, 
                 fname="outcome-of-care-measures.csv",
                 dump=TRUE) {
      
     rankHospital(state=state, outcome=outcome, 
                  fname=fname, rank = 1,
                  dump=TRUE)
      
      
}
