#Select files from a folder, then append them
appendeR <- function( work_directory, header, sep ){
  #Files in the working directory
  directory <- dir( work_directory )
  #Make selections on which files to append
  file_select <- "not stop" #Initialize
  files <- character() #Initialize
  while( file_select != "Stop This" ){
    #Print directory
    print( directory )
    #Make selections
    file_select <- readline( "Select the number associated with your file, (\"Stop This\" to stop) : " )
    #Execute when we have said we didn't want to stop
    if( file_select != "Stop This" ){
      #Extract file name associated with the number
      file <- directory[ as.numeric(file_select) ]
      #Append previous selects
      files <- append( files, file )
    }
  }
  
  #Display
  cat("You selected: \n")
  print( files )
  
  #Append by just using the files you selected
  dataset <- do.call("rbind", lapply( files, FUN = function(file) {
    data.table::fread( input = paste0( work_directory, file ), header=header, sep=sep )
  }))
  
  return( dataset )
}
