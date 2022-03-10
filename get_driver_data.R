library(neon4cast)

Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")

dir.create("drivers", showWarnings = FALSE)

sites <- c("ORNL")
for(i in 1:length(sites)){
  neon4cast::get_stacked_noaa_s3(".",site = sites[i], averaged = FALSE)
}

  
