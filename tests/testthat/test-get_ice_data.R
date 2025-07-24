test_that("get_ice_data returns all expected columns and ao_expense_subsidy_percent is numeric", {
  dt <- get_ice_data(
    years = 2011,
    selected_ice = "D00097_IceAOExpenseSubsidy"
  )
  dt[,commodity_year := reinsurance_year]
  dt <- dt[
    ,
    lapply(.SD, mean, na.rm = TRUE),
    by     = c("commodity_year","insurance_plan_code","coverage_level_percent"),
    .SDcols = c("ao_expense_subsidy_percent")]

  # 1) it's a data.table
  expect_s3_class(dt, "data.table")

  # 2) required columns are present
  required_cols <- c("commodity_year","insurance_plan_code","coverage_level_percent","ao_expense_subsidy_percent")
  missing <- setdiff(required_cols, names(dt))
  expect_length(missing, 0)

  # 3) ao_expense_subsidy_percent is numeric
  expect_true(is.numeric(dt$ao_expense_subsidy_percent))
})
