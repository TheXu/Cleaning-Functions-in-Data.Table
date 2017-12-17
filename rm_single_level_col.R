#Function that subsets our columns based off if there is more than one unique value
rm_single_level_col <- function( dt ){
  #Make all our columns factors for cleaning
  dt_factor <- dt[, lapply(.SD, as.factor) ]
  
  #Find the number of factor levels for each column
  col_factor_levels <- sapply(dt_factor, nlevels)
  
  #Subset columns on our original data
  dt_clean <- dt[, col_factor_levels != 1, with = FALSE ]
  
  #Return this cleaned data
  return( dt_clean )
}