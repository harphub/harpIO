% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_analysis.R
\name{read_analysis}
\alias{read_analysis}
\title{Read analysis data from multiple files}
\usage{
read_analysis(
  dttm,
  analysis_model,
  parameter,
  members = NULL,
  members_out = members,
  lags = NULL,
  vertical_coordinate = c("pressure", "model", "height", "depth", NA),
  file_path = getwd(),
  file_format = NULL,
  file_template = "an{YYYY}{MM}{DD}{HH}.grib",
  file_format_opts = list(),
  transformation = c("none", "interpolate", "regrid", "xsection", "subgrid"),
  transformation_opts = NULL,
  param_defs = get("harp_params"),
  output_file_opts = sqlite_opts(),
  return_data = TRUE,
  merge_lags = TRUE,
  show_progress = TRUE,
  stop_on_fail = FALSE,
  start_date = NULL,
  end_date = NULL,
  by = "6h"
)
}
\arguments{
\item{dttm}{A vector of date time strings to read. Can be in YYYYMMDD,
YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be numeric or
character. \code{\link[harpCore]{seq_dttm}} can be used to generate a
vector of equally spaced date-time strings.}

\item{analysis_model}{The name of the analysis model(s) to read. Can be
expressed as character vector if more than one model is wanted.}

\item{parameter}{The name of the forecast parameter(s) to read from the
files. Should either be harp parameter names (see
\code{\link{show_param_defs}}), or in the case of netcdf files can be the
name of the parameters in the files. If reading from vfld files, set to
NULL to read all parameters.}

\item{members}{For ensemble forecasts, a numeric vector giving the member
numbers to read. If more than one forecast model is to be read in, the
members may be given as a single vector, in which case they are recycled
for each forecast model, or as a named list, with the forecast models (as
given in \code{fcst_model}) as the names. For multimodel ensembles this
would be a named list of named lists. If file names do not include the
ensemble member, i.e. all members are in the same file, setting
\code{members} to NULL will read all members from the files.}

\item{members_out}{If the members are to renumbered on output, the new member
numbers are given in \code{members_out}. Should have the same structure as
\code{members}.}

\item{lags}{A named list of members of an ensemble forecast model that are
lagged and the amount by which they are lagged. The list names are the
names of those forecast models, as given in \code{fcst_model} that have
lagged members, and the lags are given as vectors that are the same length
as the members vector. If the lags are numeric, it is assumed that they are
in hours, but the units may be specified with a letter after each value
where d = days, h = hours, m = minutes and s = seconds. \code{lags} is
primarily used to generate the correct file names for lagged members - for
example a lag of 1 hour will generate a file name with a date-time 1 hour
earlier than the date-time in the sequence \code{(start_data, end_date, by
  = by)} and a lead time 1 hour longer.}

\item{vertical_coordinate}{For upper air data to be read the vertical
coordinate in the files must be given. By default, this is "pressure", but
may also be "height" or "model" for model levels. If reading from vfld
files, set to NA to only read surface parameters.}

\item{file_path}{The parent path to all forecast data. All file names are
generated to be under the \code{file_path} directory. The default is the
current working directory.}

\item{file_format}{The format of files to read. harpIO includes functions to
read 'vfld', 'netcdf', 'grib' and 'fa' format files. If set to NULL, an
attempt will be made to guess the format of the files. However, you may
write your own functions called read_<file_format> function and
\code{read_forecast} will attempt to use that instead. See the vignette on
writing read functions for more information.}

\item{file_template}{A template for the file names. For available built in
templates see \code{\link{show_file_templates}}. If anything else is
passed, it is returned unmodified, or with substitutions made for dynamic
values. Available substitutions are {YYYY} for year, \{MM\} for 2 digit
month with leading zero, \{M\} for month with no leading zero, and
similarly \{DD\} or \{D\} for day, \{HH\} or \{H\} for hour, \{mm\} or
\{m\} for minute. Also \{LDTx\} for lead time and \{MBRx\} where x is the
length of the string including leading zeros. Note that the full path to
the file will always be file_path/template.}

\item{file_format_opts}{A list of options specific to the file format. For
netcdf this can be generated by \code{\link{netcdf_opts}} and for grib by
\code{\link{grib_opts}}.}

\item{transformation}{The transformation to apply to the data once read in.
"none" will return the data in its original form, "interpolate" will
interpolate to points at latitudes and longitudes supplied in
\code{transformation_opts}, "regrid" will regrid the data to a new domain
given in \code{transformation_opts}, and "xsection" will interpolate to a
vertical cross sectoin betweem two points given in
\code{transformation_opts}.}

\item{transformation_opts}{Options for the transformation. For
\code{transformation = "interpolate"} these can be generated by
\code{\link{interpolate_opts}}, for \code{transformation = "regrid"} these
can be generated by \code{\link{regrid_opts}}, and \code{transformation =
  "xsection"} these can be generated by \code{\link{xsection_opts}}.}

\item{param_defs}{A list of parameter definitions that includes the file
format to be read. By default the built in list \code{\link{harp_params}}
is used. Modifications and additions to this list can be made using
\code{\link{modify_param_def}} and \code{\link{add_param_def}}
respectively.}

\item{output_file_opts}{Options for output files. \code{read_forecast} can
output data \code{transformation = "interpolate"} as sqlite files. The
options for the sqlite files can be set with \code{\link{sqlite_opts}}.
Most inportantly, the path argument in \code{link{sqlite_opts}} must not be
NULL for data to be output to sqlite files.}

\item{return_data}{By default \code{read_forecast} does not return any data,
since many GB of data could be read in. Set to TRUE to return the data read
in to the global environment.}

\item{merge_lags}{Logical. Whether to merge the lagged members when
\code{return_data = TRUE} (the default). When \code{TRUE}, the forecast
time and lead time for the lagged members are adjusted to fit with the
unlagged members.}

\item{show_progress}{Some files may contain a lot of data. Set to TRUE to
show progress when reading these files.}

\item{stop_on_fail}{Logical. Set to TRUE to make execution stop if there are
problems reading a file. Missing files are always skipped regardless of
this setting. The default value is FALSE.}

\item{start_date, end_date, by}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} The use of
\code{start_date}, \code{end_date} and \code{by} is no longer supported. \code{dttm} together
with \code{\link[harpCore]{seq_dttm}} should be used to generate equally
spaced date-times.}
}
\value{
When \code{return_date = TRUE}, a harp_analysis object.
}
\description{
\code{read_analysis} generates file names, based on the arguments given,
reads data from them, and optionally performs a transformation on those data.
Works in much the same as \code{\link{read_forecast}} except dates must be
given explictly. These can be generated from \code{\link{seq_dates}}. Also,
data are returned by default. If just want to use this function for
interpolating to points and writing the results to sqlite files, make sure to
set \code{return_data = FALSE}.
}
