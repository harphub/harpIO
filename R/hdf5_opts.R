#' Set Options for Reading HDF5 files
#'
#' When reading a hdf5 file in harp, the system needs to know some information
#' about the file format. These options are then used in \code{read_hdf5}, which 
#' generally assumes that the hdf5 file uses the ODIM standard. 
#' 
#' If your hdf5 file differs from the ODIM standard, you can specify specific 
#' decoding through the \code{iflag} (if it has been added to \code{read_hdf5}).
#' The only recognised option for \code{iflag} is currently 'knmi'.
#' The variable name used to recognise precipitation in the hdf5 file defaults
#' to 'ACCR'. This can be changed by using \code{pcp_quant}.
#' 
#' The default options can be seen by running \code{hdf5_opts()}.
#'
#' @param data_path This option is not respected! Notionaly this is the
#'   path to the data inside the hdf5 file, e.g. obtained from h5dump -n, but 
#'   by default all datasets in the hdf5 are read and are then matched to the 
#'   parameter specified by 'parameter' in \code{read_hdf5}.
#' @param odim Logical. This option is not functional! Notionally this indicates
#'   if the file follows the ODIM standard, but is not respected at the moment.
#' @param meta Logical. Get metavar such as domain information from the hdf5 
#'   file. If set to TRUE then a geofield will be returned, if FALSE just the 
#'   raw data will be returned. Generally should be TRUE.
#' @param invert_data Logical. Should the data be inverted i.e. transposed and
#'   put upside-down?
#' @param iflag An institute flag to indicate specific decoding of hdf5 files.
#'   The only currently recognised option is 'knmi'. If NULL then the ODIM
#'   standard is assumed. 
#' @param pcp_quant The name used for precipitation in the hdf5 file. Defaults 
#'   to ACCR i.e. the ODIM standard.
#' @param pinfo Logical. Print some info from \code{read_hdf5} for debugging. 
#'
#' @return A list of options for reading hdf5 files.
#' @export
#'
#' @examples
#' hdf5_opts()
#' hdf5_opts(invert_data = FALSE, iflag = "knmi", pcp_quant = "PRECIP_[MM]")
hdf5_opts <- function(
    data_path   = NULL,
    odim        = TRUE,
    meta        = TRUE,
    invert_data = TRUE,
    iflag       = NULL,
    pcp_quant   = "ACRR",
    pinfo       = FALSE
) {
  
  list(
    data_path   = data_path,
    odim        = odim,
    meta        = meta,
    invert_data = invert_data,
    iflag       = iflag,
    pcp_quant   = pcp_quant,
    pinfo       = pinfo
  )
  
}