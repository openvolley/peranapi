#' Create match file via API
#'
#' @param guid string: the match guid as returned by \code{\link{pa_get_matches}}
#' @param filename string: the filename to use. A temporary file will be used if no \code{filename} is provided. Existing files will not be overwritten
#'
#' @return Filename
#'
#' @examples
#'
#' \dontrun{
#'   my_server_name <- "server name used in VBStats"
#'
#'   ## retrieve the ID of the first match in our collection
#'   guid <- pa_get_matches(my_server_name)$guid[1]
#'
#'   ## build the match file
#'   fn <- pa_get_match_file(guid)
#'
#'   ## load it
#'   x <- peranavolley::pv_read(fn)
#' }
#'
#' @export
pa_get_match_file <- function(guid, filename = NULL) {
    if (is.null(filename)) filename <- tempfile(fileext = ".psvb")
    assert_that(is.string(filename))
    if (file.exists(filename)) stop("file ", filename, " exists, not overwriting")
    mi <- pa_get_match_info(guid)
    ed <- pa_get_event_data(guid, last_time = 0)
    pv_write(c(mi, ed), filename)
    filename
}


## include pv_write here, avoids package dependency on peranavolley for just this one function

#' Write a Perana Sports volleyball data file
#'
#' This is somewhat experimental. It may be useful if one wants to read an existing file, modify the content, and re-write it back to a PSVB file.
#' 
#' @param x character: data to write. See e.g the \code{raw} component of the object returned by \code{\link[peranavolley]{pv_read}}
#' @param filename string: path to file
#'
#' @seealso \code{\link[peranavolley]{pv_read}}
#'
#' @examples
#'
#' \dontrun{
#'   x <- pv_read(pv_example_file())
#'   new_file_name <- tempfile(fileext = ".psvb")
#'   pv_write(x$raw, filename = new_file_name)
#' }
#'
#' @export
pv_write <- function(x, filename) {
    assert_that(is.character(x))
    x <- unname(x)
    tf <- tempfile()
    on.exit(unlink(tf))
    ## write to gzipped file
    gzcon <- gzfile(tf)
    writeLines(text = x, con = gzcon)
    close(gzcon)
    ## now read back in bytes
    gb <- readBin(tf, what = "raw", n = file.info(tf)$size*2)
    ## first four bytes are to be the buffer size
    sz <- packBits(intToBits(sum(vapply(x, nchar, FUN.VALUE = 1))), "raw")
    if (length(sz) > 4) {
        ## should not happen?
        sz <- sz[1:4]
    } else if (length(sz) < 4) {
        sz <- c(as.raw(rep(0, 4-length(sz))), sz)
    }
    gb <- c(sz, gb)
    ## now base64-encode
    gb <- base64enc::base64encode(gb)
    ## and finally write to (UTF-8) file
    writeLines(gb, filename)
}

