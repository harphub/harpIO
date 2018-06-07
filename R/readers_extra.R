### Some reader functions.

### NEED DOCUMENTING. Not exporting for now.


# faTar -----------------------------------------
# for fa files archived in .tar files (note that FC_file should add an attribute "filename"!)
read_faTar <- function(infile, ...) {
  archname <- infile
  fafile <- attributes(infile)$filename

  read_fa(infile=fafile, archname=archname, ...)
}

#  inca_ascii -----------------------------------
read_inca_ascii <- function(infile, parameter, domain, ...) {
  # you absolutely /need/ to know nx!
  # so domain must at least have a $nx component.
  if (!file.exists(infile)) stop(paste("File",infile,"not found."))
  if (missing(domain)) stop("inca_ascii format requires domain information.\n")
  inca <- matrix(scan(infile), byrow=TRUE, nrow=domain$nx)
  if (parameter$basename == "s") {
    # we already decoded the U component
    f2 <- gsub("UU", "VV", infile)
    V <- matrix(scan(f2), byrow=TRUE, nrow=domain$nx)
    inca[, ] <- sqrt(inca^2 + V^2)
  }
  #  if (inherits(domain,"geodomain")) inca <- as.geodomain(inca, domain)
  inca
}

