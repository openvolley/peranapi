#' Retrieve the sessions associated with a given server name
#'
#' @param server_name string: the server name used in VBStats
#' @param app_name string: the app name ("VBStats" for VBstatsHD) 
#'
#' @return data.frame with columns \code{guid}, \code{teams}, \code{date}
#'
#' @export
pa_get_sessions <- function(server_name, app_name = "VBStats") {
    assert_that(is.string(server_name), !is.na(server_name))
    assert_that(is.string(app_name), !is.na(app_name))
    my_url <- paste0(pa_opt("base_url"), "/", "GetSessions?appName=", URLencode(app_name), "&serverName=", URLencode(server_name))
    x <- read_xml(my_url)
    x <- xml_text(xml_contents(x)[[1]])
    x <- strsplit(x, "~")
    x <- do.call(rbind, lapply(x, function(z) read.table(text = z, sep = "|", stringsAsFactors = FALSE)))
    setNames(x, c("guid", "teams", "date"))
}


#' Retrieve the match information associated with a guid
#'
#' @param guid string: the match guid as returned by \code{\link{pa_get_sessions}}
#'
#' @return character
#'
#' @export
pa_get_match_info <- function(guid) {
    assert_that(is.string(guid), !is.na(guid))
    my_url <- paste0(pa_opt("base_url"), "/", "GetStatsFile?guid=", URLencode(guid))
    x <- read_xml(my_url)
    x <- xml_text(xml_contents(x)[[1]])
    x <- read.table(text = x, sep = "~", stringsAsFactors = FALSE)
    x <- b64gunz(x[[2]])
    if (!any(grepl("\"", x, fixed = TRUE))) x <- b64gunz(x)
    x
}
