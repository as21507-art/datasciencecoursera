pollutantmean <- function(directory, pollutant, id=1:332){

      # Initializing the parameters
      total_particulate_matter <- 0
      number_of_data <- 0
      
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
            
            # Removing the NA values from the file
            filtered_data <- current_data[!is.na(current_data[pollutant]), ]
            
            # Increment values
            total_particulate_matter <- total_particulate_matter + sum(filtered_data[pollutant])
            number_of_data <- number_of_data + nrow(filtered_data[pollutant])
      }
      
      # Calculating and returning the value of the mean
      if (total_particulate_matter == 0) {
            return(0)
      } else {
            return(total_particulate_matter / number_of_data)     
      }
}

finalMean <- pollutantmean("specdata", "nitrate", 70:72)
print(finalMean)
