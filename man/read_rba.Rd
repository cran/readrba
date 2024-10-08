% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_rba.R
\name{read_rba}
\alias{read_rba}
\alias{read_rba_seriesid}
\title{Download, import, and tidy statistical tables from the RBA}
\usage{
read_rba(
  table_no = NULL,
  cur_hist = "current",
  series_id = NULL,
  path = tempdir(),
  update_urls = FALSE
)

read_rba_seriesid(series_id, path = tempdir())
}
\arguments{
\item{table_no}{Character vector of table number(s),
such as \code{"A1"} or \code{c("a1.1", "g1")}.}

\item{cur_hist}{Character; valid values are \code{"current"} or \code{"historical"}.}

\item{series_id}{Optional, character. Specifying \code{series_id} is an alternative
to specifying \code{table_no}.

Supply unique RBA time series identifier(s).
For example, "GCPIAG" is the identifier for the CPI, so \code{series_id = "GCPIAG"} will
return this series. You can supply multiple series IDs as a character
vector, such as \code{series_id = c("GCPIAG", "GCPIAGSAQP")}.

Note that \code{cur_hist} is ignored if you specify \code{series_id} -
both current and historical data will be included in the output.}

\item{path}{Directory in which to save downloaded RBA Excel file(s).
Default is \code{tempdir()}.}

\item{update_urls}{Logical; default is \code{FALSE}. When \code{FALSE}, an internal
table is used to fetch the URL(s) corresponding to the requested data. If
\code{TRUE}, the RBA website is scraped to obtain updated URL(s).

Must be either a vector of either length 1 (eg. "\code{cur_hist = "current"}) or
the same length as \code{table_no} (eg. \code{cur_hist = c("current", "historical")}).

\code{cur_hist} is ignored if \code{series_id} is specified.}
}
\value{
A single tidy tibble containing the requested table(s)
}
\description{
Download, import, and tidy statistical tables from the RBA
}
\details{
\code{read_rba()} downloads, imports and tidies data from statistical
tables published by the Reserve Bank of Australia. You can specify the
requested data using the \code{table_no} or \code{series_id}.

To find the \code{table_no} or \code{series_id} that corresponds to the data you want,
use the functions \code{browse_rba_tables()} and/or \code{browse_rba_series()}. Note
that some historical exchange rate tables do not have table numbers on
the RBA website; they have been assigned table numbers -
see \verb{browse_rba_tables("exchange rates").}

To see which tables cannot currently be read, run \code{browse_rba_tables(FALSE)}.

\code{read_rba_seriesid()} is a wrapper around \code{read_rba()}.

Certain corporate networks restrict your ability to download files in an R
session. On some of these networks, the \code{"wininet"} method must be used when
downloading files. Users can  specify the method that will be used to
download files by setting the \code{"R_READRBA_DL_METHOD"} environment variable.

For example, the following code sets the environment variable for your
current session: \code{Sys.setenv("R_READRBA_DL_METHOD" = "wininet")}
You can add \code{"R_READRBA_DL_METHOD"} to your .Rprofile to have this
persist across sessions.
}
\examples{
\dontrun{
# Get a single table:
read_rba(table_no = "a1.1")

# Get multiple tables, combined in a tidy tibble:
read_rba(table_no = c("a1.1", "g1"))

# Get both the current and historical versions of a table
read_rba(table_no = c("a1.1", "a1.1"), cur_hist = c("current", "historical"))

# Get data based on the series ID:
read_rba(series_id = "GCPIAG")

# Or, equivalently, use:
read_rba_seriesid("GCPIAG")

# Get multiple series IDs:
read_rba(series_id = c("GCPIAG", "GCPIAGSAQP"))
}
}
