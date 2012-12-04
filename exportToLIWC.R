exportToLIWC <- function(myData, idCol, textCols) {
  # Export data from dataframes to text files for subsequent analysis with LIWC
  # Include unique identifier with every file for ease of merging results back
  
  log <- data.frame(filename = character(0), participant = character(0), variable = character(0), stringsAsFactors = F)
  for (i in 1:nrow(myData)) {
    id <- gsub(" +$", "", as.character(myData[i,idCol])) # Get participant identifier
    
    for (thisCol in textCols) {
      filename <- paste(id, "_", thisCol, ".txt", sep = "") # Create unique filename
      content <- as.character(myData[i, thisCol]) # Get cell contents as text
      cat("Saving", filename, "\n")
      writeLines(content, con = filename) # Write file
      log[nrow(log)+1,] <- c(filename, id, thisCol)
    }
  }
  
  cat("Done!\n")
  return(log)
}