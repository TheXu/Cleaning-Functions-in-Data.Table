#Scrubs out all column names, and all categorical level names
names_scrubber <- function( data ){
  
  #The data that we will work on
  data_temp <- data
  
  #Store Old Column Names
  old_names <- names( data )
  
  ##Replace column names
  #New Column Names based off of how many columns
  new_col_names <- LETTERS[ 1:ncol(data_temp) ]
  #Replace Column Names
  data.table::setnames( data_temp, names(data_temp), new_col_names )
  
  ##Replace factor levels
  #Print the old names
  print( old_names )
  #Prompts user for column numbers that are factors
  factor_select <- readline( "Indicate by column number, which columns are factors (Commas to separate) : " )
  factor_select2 <- unlist(strsplit( factor_select, split = "," ))
  factors <- LETTERS[ as.numeric(factor_select2) ]
  #Make all our columns factors for cleaning
  data_temp[, (factors) := lapply( .SD, as.factor ), .SDcols = factors  ]
  #Find the number of factor levels for each column
  num_factor_levels <- sapply(data_temp, nlevels)
  #Store what the factor levels are; https://stackoverflow.com/questions/33662588/extracting-levels-from-data_temp-table
  #factor_levels <- Filter( Negate( is.null ),lapply( data_temp,levels ) )
  #Generate new factor levels; https://stackoverflow.com/questions/21681785/repeating-vector-of-letters
  new_factor_levels <- sapply( num_factor_levels, function(length){
    make.unique(rep(letters, length.out = length), sep='')
  } )
  #Taking out new factor levels for columns without factors
  new_factor_levels <- Filter( Negate( is.null ), lapply( new_factor_levels, function(x){
    if( !identical( x, character(0) ) ){
      x
    }
  } ) )
  
  #Replace old factor levels with new factor levels
  sapply( names(data_temp), function(col){
    data.table::setattr( data_temp[[col]], "levels", new_factor_levels[[col]] )
  })
  
}

