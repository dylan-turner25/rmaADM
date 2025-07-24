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
                    "A00540"  # Type"
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


# build ao expense subsidy percent
ao_expense_subsidy_percent <- get_ice_data(years=2011:as.numeric(format(Sys.Date(),"%Y")),selected_ice="D00097_IceAOExpenseSubsidy")
ao_expense_subsidy_percent[,commodity_year := reinsurance_year]
ao_expense_subsidy_percent <- ao_expense_subsidy_percent[
  ,
  lapply(.SD, mean, na.rm = TRUE),
  by     = c("commodity_year","insurance_plan_code","coverage_level_percent"),
  .SDcols = c("ao_expense_subsidy_percent")]

# Generate metadata key for factor metadata
metadata_key <- paste0("ao_expense_subsidy_percent_", paste(range(ao_expense_subsidy_percent$commodity_year), collapse = "_"))

# Save as parquet file using compress_adm2 function (handles type conversion automatically)
file_name <- paste0("./data-raw", "/ao_expense_subsidy_percent.parquet")
compress_adm2(ao_expense_subsidy_percent, file_name, metadata_key)

#upload  .rds files into that release
files <-  list.files("data-raw", "ao_expense_subsidy_percent.parquet$",
           full.names = TRUE, recursive = TRUE)

piggyback::pb_upload(
  files,
  repo = "dylan-turner25/rmaADM",
  tag  = "v0.2.0",
  overwrite = T
)


