library(neon4cast)

Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")

dir.create("drivers", showWarnings = FALSE)

