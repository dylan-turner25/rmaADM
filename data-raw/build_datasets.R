# load package
devtools::load_all()


# download the ADM files to data-raw
download_adm2(
  years = 2011:2026,
  dataset_codes = c("A01010", # base rate
                    "A01040", # coverage level differential
                    "A00030", # insurance offer
                    "A01020", # beta
                    "A01030", # combo revenue factor
                    "A01110", # historical revenue capping
                    "A00200", # dates
                    "A01115", # historical yield trend
                    "A01130", # AreaCoverageLevel
                    "A01135", # Area Rate"
                    "A01005", # AreaRiskRate
                    "A00070", # subsidy percent
                    "A00810",  # price

                    # helper datasets
                    "A01090", # unit discount
                    "A00500", # organic practice,
                    "A00510", # practice
                    "A00490", # irrigation practice
                    "A00520", # state
                    "A00440", # county
                    "A00420", # commodity
                    "A00430", # commodity type
                    "A00540",  # Type"
                    "A00460" # Insurance plan
  ),
  overwrite = FALSE,
  keep_source_files = TRUE
)




# upload cleaned files as a data release
piggyback::pb_new_release(
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  name = "First data release",
  body = "This release contains cleaned files from the ADM."
)

#upload  .rds files into that release
files <- list.files("data-raw", "\\.parquet$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  overwrite = F
)



# build county yield history
build_county_yield_history(years = 2011:2025)

#upload  .rds files into that release
files <- list.files("data-raw", "county_yield_history.parquet$",
                        full.names = TRUE, recursive = TRUE)


piggyback::pb_upload(
  files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  overwrite = T
)


