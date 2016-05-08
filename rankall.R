# RankAll hospitals across all states
#

library(data.table)
library(formattable)

rankall <- function(outcome, num="best", 
                    fname="outcome-of-care-measures.csv") {

      # validate parameters
      
      if (!(validateOutcome(outcome) & validateRank(num) &
            validateFile(fname))) {
            stop("Wrong parameters")
      }
      
      # get mapping to right col in data based on outcome
      
      mortality_col<-mapMortalityCol(outcome)
      
      # In theory, data.table should have made everything here very easy,  but I ran into various problems
      # and could not use it as it was advertised
      
      # Took the easier path around some discrepancies
      # data.frame makes column names dotted e.g. "Hospital.Name" as opposed to "Hospital Name" (no DOT)
      # kept this consistent with earlier code in Best and rankhospital.  This is why we first get df and then dt
      
      hosp_df <- read.csv(fname,
                          colClasses = "character")
      hosp_dt <- data.table(hosp_df)
      
      # get the non-numeric rows filtered out 
      
      filtered_dt<- hosp_dt[!is.na(suppressWarnings(as.numeric(hosp_dt[[mortality_col]])))]    # i: numeric only

      # sort the filtered to get row_index - 
      # ensure that the sorting is done with mortality rates as numbers, not characters
      
      sorted_row_index_vect <- order(as.numeric(filtered_dt[[mortality_col]]), filtered_dt[["Hospital.Name"]], na.last=TRUE)
      sorted_dt<-filtered_dt[sorted_row_index_vect]
      
      ## This was one place data.table seemed to work well.   
      # Look up docs on .SD and .SDCols at 
      # https://github.com/Rdatatable/data.table/blob/master/vignettes/datatable-intro.Rmd
      
      numeric_rank <- mapRank(num) # get right mapped column
      
      ## getRankedValue returns the right index to be used.  
      ## lappy traverses all columns in .SD but is restricted by list in .SDcols
      ## I could have included State as well I think,  but had some problems I did not debug - this works.
      
      ranked_dt <- sorted_dt[, lapply(.SD, getRankedValue, rank=numeric_rank), by=State,
                             .SDcols = c( "Hospital.Name", mortality_col)]
      
      ## THis is some hack since I could not figure out how to get mortality columes added correctly in the
      ## data.table syntax...  Essentially,  this is just creating yet another data.table with the columns I '
      ## need for the pretty printing
      
      ranked_printable_table<- data.table(state=ranked_dt$State, hospital=ranked_dt$Hospital.Name, 
                                     mortality_rate=ranked_dt[[mortality_col]])
      
      my_t<- ranked_printable_table[order(state)]
      print(formattable(my_t))
      
      my_t
      
      
}