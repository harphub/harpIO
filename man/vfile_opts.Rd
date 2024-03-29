% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vfile_opts.R
\name{vfile_opts}
\alias{vfile_opts}
\title{Generate options for vfld and vobs files}
\usage{
vfile_opts(
  type = c("vfld", "vobs"),
  missing_value = -99,
  synop_cols = NULL,
  temp_cols = NULL
)
}
\arguments{
\item{type}{Type of vfile - "vfld" or "vobs"}

\item{missing_value}{Missing value indicator. Default is -99.0}

\item{synop_cols}{For vfld/vobs version < 4 the column names in the files
are assumed. Use \code{synop_cols} to override the defaults for SYNOP
data. Note that the variable names used here must be valid vfld/vobs
names.}

\item{temp_cols}{For vfld/vobs version < 4 the column names in the files
are assumed. Use \code{temp_cols} to override the defaults for TEMP data.
Note that the variable names used here must be valid vfld/vobs names.}
}
\value{
A list of options
}
\description{
Generate options for vfld and vobs files
}
\examples{
vfile_opts()
vfile_opts(type = "vobs")
vfile_opts(missing_value = -555)
}
