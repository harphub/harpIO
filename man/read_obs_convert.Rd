% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_obs_convert.R
\name{read_obs_convert}
\alias{read_obs_convert}
\title{Read observations and output to sqlite OBSTABLE file(s)}
\usage{
read_obs_convert(
  start_date,
  end_date,
  by = "3h",
  obs_path = ".",
  obs_format = "vobs",
  obsfile_template = "vobs",
  parameter = NULL,
  sqlite_path = NULL,
  sqlite_template = "obstable",
  return_data = FALSE,
  iterations_per_write = 24,
  sqlite_synchronous = c("off", "normal", "full", "extra"),
  sqlite_journal_mode = c("delete", "truncate", "persist", "memory", "wal", "off"),
  ...
)
}
\arguments{
\item{start_date}{Date of the first observations to be read in. Should be in
YYYYMMDDhh format. Can be numeric or charcter.}

\item{end_date}{Date of the last observations to be read in. Should be in
YYYYMMDDhh format. Can be numeric or charcter.}

\item{by}{The time between observations. Should be a string of a number
followed by a letter (the defualt is "6h"), where the letter gives the
units - may be d for days, h for hours or m for minutes.}

\item{obs_path}{The path for the input observation files. obs_path will, in
most cases, form part of the file template.}

\item{obs_format}{The format of the input observation files. Currently only
"vobs".}

\item{obsfile_template}{The template for the observation files. Currently
only "vobs".}

\item{parameter}{Not used for vobs.}

\item{sqlite_path}{If not NULL, sqlite files are generated and written to the
directory specified here.}

\item{sqlite_template}{The template for the sqlite observations file. The
default is "obstable", which is "{sqlite_path}/OBSTABLE_{YYYY}.sqlite".}

\item{return_data}{Whether to return the data to the calling environment. The
default is FALSE.}

\item{iterations_per_write}{The number of iterations of "by" before each
write to the sqlite file. The default is 24.}

\item{sqlite_synchronous}{The synchronus setting for sqlite files. The
defualt is "off", but could also be "normal", "full", or "extra". See
\url{https://www.sqlite.org/pragma.html#pragma_synchronous} for more
information.}

\item{sqlite_journal_mode}{The journal mode for the sqlite files. The default
is "delete", but can also be "truncate", "persist", "memory", "wal", or
"off". See \url{https://www.sqlite.org/pragma.html#pragma_journal_mode} for
more information.}

\item{...}{Arguments to read functions. Not currently used.}
}
\value{
If return_data is TRUE - a list with four data frames - one for synop
(near surface) observations, one for the units of the synop observations,
one for the temp (upper air) observations, and one for the units of the
temp observations.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function was deprecated as \link{read_obs} is much more flexible.
}
