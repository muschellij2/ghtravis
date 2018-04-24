#' @title Read DESCRIPTION file
#' @description Reads a DESCRIPTION file
#'
#' @param path path to the DESCRIPTION file
#' @return List of the fields and the data.frame of the description information
#' @export
#' @importFrom neuroc.deps collapser
#'
read_dcf <- function(path = "DESCRIPTION") {
  file = file(path)
  on.exit({
    close(file)
  })
  fields <- colnames(read.dcf(file))
  dcf_file <- neuroc.deps::collapser(read.dcf(file, keep.white = fields, all = TRUE), cn = c("Imports", "Suggests", "Depends"))
  dcf = as.list(dcf_file[1, ])
  return(list(fields = fields,
              dcf = dcf))
}