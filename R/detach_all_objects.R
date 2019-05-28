#' Detach all non-essential objects from all environments
#' 
#' This function detaches all non-essential objects from all environments and is 
#' a simple rewrite of a function available through the codes provided by
#' the Choice Modeling Centre in Leeds.
#' 

detach_all_objects <- function(){
    #   Create a subsetting vector to avoid detaching important objects
    subset_environment <- substring(search(), 1L, 8L) != "package:" &
        search() != ".GlobalEnv" & search() != "Autoloads" & 
        search() != "CheckExEnv" & search() != "tools:rstudio" &
        search() != "TempEnv"
    
    #   Get the position of the objects to detach
    object_positions <- (1L:length(search()))[subset_environment]
    
    #   Loop over objects to detach
    for(i in 1L:length(object_positions)){
        if(length(object_positions) > 0L){
            detach(pos = object_positions[1L])
            #   Update object positions
            object_positions <- (1L:length(search()))[subset_environment]
        }
    }
}