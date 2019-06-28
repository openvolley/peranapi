.onLoad <- function(libname, pkgname) {
    ## populate the options slot
    this_options <- list(
        base_url = "http://peranasports.com/PeranaWebService/PeranaWebService.asmx"
    )
    options(list(peranapi = this_options))
    invisible()
}

pa_opts <- function() getOption("peranapi")
pa_opt <- function(optname) pa_opts()[[optname]]
pa_set_opt <- function(...) {
    opts <- pa_opts()
    newopts <- list(...)
    for (nm in names(newopts)) opts[[nm]] <- newopts[[nm]]
    options(list(peranapi = opts))
}
