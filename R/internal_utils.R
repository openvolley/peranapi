b64gunz <- function(x) {
    x <- base64enc::base64decode(x)
    if (length(x) < 6 || !identical(x[5:6], as.raw(c(31, 8*16+11))))
        stop("cannot read text")
    ## first four bytes are the buffer size
    rc <- rawConnection(x[seq_along(x)[-1:-4]])
    z <- gzcon(rc)
    readLines(z, warn = FALSE)
}
