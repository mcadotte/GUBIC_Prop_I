# Run from the project R directory.
city <- read.csv(file.path("data", "urban_sf_sgubik.csv"))
city.env <- read.csv(file.path("data", "urban_env.csv"))

urban_record_totals <- local({
  records <- readRDS(file.path("data", "gbif_gubic_list_per_city_final2_20241015.rds"))

  # Include singletons. Cities with no positive observed count have no effort estimate.
  observed <- which(records$n_records_urban > 0)
  aggregate(
    list(tot.records = records$n_records_urban[observed]),
    by = list(ID_HDC_G0_2 = records$ID_HDC_G0_2[observed]),
    FUN = sum
  )
})
