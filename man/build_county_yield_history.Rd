% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_county_yield_history.R
\name{build_county_yield_history}
\alias{build_county_yield_history}
\title{Build county yield history from ADM data}
\usage{
build_county_yield_history(years, export_dir = "./data-raw", by_year = TRUE)
}
\arguments{
\item{years}{\link{numeric} Vector of years to process}

\item{export_dir}{\link{character} Directory path where the output parquet file will be saved.
Defaults to "./data-raw"}

\item{by_year}{\link{logical} Currently unused parameter (kept for compatibility)}
}
\value{
A \link[data.table]{data.table} with one row per FCIP insurance pool per year, containing
the mean \code{yield_amount}, \code{trended_yield_amount}, and
\code{detrended_yield_amount}, along with the corresponding
\code{commodity_year} and \code{yield_year}.
}
\description{
Processes insurance offer and historical yield trend data for multiple years,
merges them on the historical trend IDs, aggregates by mean within each year,
then across all years selects the earliest available trend per FCIP insurance
pool and computes average yields. Saves the result as a parquet file with
automatic type conversion and optimization.
}
\details{
\itemize{
\item Each year, the RMA rebuilds the historical yield trend table, so
yields may differ across different releases of that table.
\item This function uses information across all available tables to
construct a single, reconciled county yield history.
\item The output is automatically saved as a parquet file with automatic type
conversion and optimization in the specified directory.
\item Applies the same type conversion logic as \code{download_adm2} for consistency.
}
}
