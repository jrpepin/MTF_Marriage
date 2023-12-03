#-------------------------------------------------------------------------------
# MARRIAGE EXPECTATIONS PROJECT
# ME_00-2_data download.R
# Joanna R. Pepin
#-------------------------------------------------------------------------------
## This script only needs to be run the first time to download the data.
## Users should run ME_00-1_setup and packages.R before running this script.

# Download the data ------------------------------------------------------------

icpsr_download(file_id = c(  7927,  7928,  7929,  7930,
                             7900,  9013,  9045,  8387,  8388, 
                             8546,  8701,  9079,  9259,  9397, 
                             9745,  9871,  6133,  6367,  6517,
                             6716,  2268,  2477,  2751,  2939,
                             3184,  3425,  3753,  4019,  4264,
                             4536,  20022, 22480, 25382, 28401,
                             30985, 34409, 34861, 35218, 36263,
                             36408, 36798, 37182, 37416, 37841,
                             38156, 38503, 38882),
               download_dir = dataDir)

# To download one survey year at a time (for yearly updates):
# icpsr_download(file_id = 38882, download_dir = dataDir)

## Clean up folders -- WARNING -- This code will delete files on your hard-drive. USE WITH CAUTION

### Delete unused SAS files
#to_be_deleted <- dir(path=dataDir, pattern="\\.sas$", recursive = TRUE) # Make sure list only includes files in the sub-directory
#file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)        # Delete the files

#to_be_deleted <- dir(path=dataDir, pattern="\\.xpt$", recursive = TRUE)
#file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

### Delete unused SPSS files
#to_be_deleted <- dir(path=dataDir, pattern="\\.por$", recursive = TRUE)
#file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

#to_be_deleted <- dir(path=dataDir, pattern="\\.sps$", recursive = TRUE)
#file.remove(file.path(dataDir, to_be_deleted), recursive = TRUE)

#remove(to_be_deleted)

# Importing the (Stata) data files ---------------------------------------------

form2dta    <- list.files(path=dataDir, pattern = ".-0002-Data.dta$|.-0003-Data.dta$", recursive = TRUE) # create a list of Form 2 data files -- different folders based on the year
mtf_F2_list <- lapply(file.path(dataDir, form2dta), read.dta) # turn the list into a list of dataframes
mtfF2       <- rbindlist(mtf_F2_list, use.names=TRUE, fill=TRUE) # Convert the list of data frames into one data frame
mtf_V2      <- select(mtfF2, V1, V5, V13, ARCHIVE_WT, TABLET, starts_with("V2")) ## Keep only Form 2 variables
mtf_V2      <- subset(mtf_V2, !is.na(V2151)) ## Keep only Form 2 survey respondents

remove(mtf_F2_list) # clean up global environment
remove(mtfF2)

## Save the dataframe for easy open in the future
### Note: This data is NOT harmonized. Make frequent and judicious referral to the codebooks.
save(mtf_V2, file=file.path(dataDir, "mtf_form2.Rda"))