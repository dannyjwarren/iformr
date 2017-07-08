# Pull in locations data from .RData file. Then create and push to Data folder

# Example data from .RData file (locations)
localDir <- getwd()
file <- paste(localDir, "locations.RData", sep="/")
load(file)

# Create a data directory and send locations to new directory as a .RData file
devtools::use_data(locations)
