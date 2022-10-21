###########################################################################
# clas-general-import_data.R
###########################################################################

# Load data ---------------------------------------------------------------

TPN1_a22_clas_app <- read.csv("data/TPN1_a22_clas_app.txt", sep="")

# Convert `y` from `int` (numeric) to `factor` (nominal/categorical)
TPN1_a22_clas_app$y   <- as.factor(TPN1_a22_clas_app$y)

if(exists("TPN1_a22_clas_app")) message("The data.frame `TPN1_a22_clas_app` is imported")
