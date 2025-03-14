##01_read_data

##this script reads the data into R and stores it in a R dat format for subsequent reference

##Data is read from "Responses Japanese.csv" 
##"Responses Japanese.csv" is derived from "Òâ¡Òâ╝ÒâåÒéÖÒâ╝Oé┐_1of1.csv"
##by passing it into a text editor -- in this case microsoft notepad -- and saving the ##file as a .csv file with file origin set to "Unicode (UTF-8)", with the data type set ##to "Delimited" and the delimiter set to "Comma"

##check if the RDS file already exists

if (
  file.exists(
    here("Data", "JACSIS2023", "processed", "Responses Japanese.RDS")
    )
  ) 
  {
  return(
    print("RDS file already exists, you may proceed to the next script")
    )
} else {

##read the csv file
  
raw_data <- read.csv(
  here(
    "Data", "JACSIS2023", "Responses Japanese.RDS"),
    skip = 1,
    header = TRUE
  )

##saving the raw data in RDS format if it doesn't yet exist

  saveRDS(
    raw_data, 
    here("Data", "JACSIS2023", "processed", "Responses Japanese.RDS")
    )
}

