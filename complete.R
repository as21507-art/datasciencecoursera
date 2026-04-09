complete <- function(directory, id=1:332){
      
      # Initializing the list of nobs
      nobs_list <- numeric()
      
      for (i in id){
            
            # Determining the file name
            if (i < 10){
                  preceeding_zeros <- "00"
            } else if (i < 100){
                  preceeding_zeros <- "0"
            } else {
                  preceeding_zeros <-""
            }
            filename <- paste(directory, "/", preceeding_zeros, i, '.csv', sep="")
      
            # Reading the data from the file
            current_data <-read.csv(filename)
            current_nob <- sum(complete.cases(current_data))
            
            # Appending to the nobs_list
            nobs_list <- c(nobs_list, current_nob)
      }
      return(data.frame(id=id, nobs=nobs_list))
}

print(complete("specdata", 30:25))
