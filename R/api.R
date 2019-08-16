#' Retrieve the matches associated with a given server name
#'
#' @param server_name string: the server name used in VBStats
#' @param app_name string: the app name ("VBStats" for VBstatsHD) 
#'
#' @return data.frame with columns \code{guid}, \code{teams}, \code{date}
#'
#' @examples
#' \dontrun{
#'  my_server_name <- "server name used in VBStats"
#'
#'  ## retrieve the matches available to us
#'  pa_get_matches(my_server_name)
#'
#' }
#'
#' @export
pa_get_matches <- function(server_name, app_name = "VBStats") {
    assert_that(is.string(server_name), !is.na(server_name))
    assert_that(is.string(app_name), !is.na(app_name))
    my_url <- paste0(pa_opt("base_url"), "/", "GetSessions?appName=", URLencode(app_name), "&serverName=", URLencode(server_name))
    x <- read_xml(my_url)
    x <- xml_contents(x)
    if (length(x) > 0) {
        x <- xml_text(x[[1]])
        x <- strsplit(x, "~")
        x <- do.call(rbind, lapply(x, function(z) read.table(text = z, sep = "|", stringsAsFactors = FALSE)))
    } else {
        x <- data.frame(guid = character(), teams = character(), date = numeric())
    }
    setNames(x, c("guid", "teams", "date"))
}


#' Retrieve the match information associated with a guid
#'
#' @param guid string: the match guid as returned by \code{\link{pa_get_matches}}
#'
#' @return Character.
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


#' Retrieve event data associated with a guid
#'
#' @param guid string: the match guid as returned by \code{\link{pa_get_matches}}
#' @param last_time datetime or numeric: only return events more recent than this
#'
#' @return Character.
#'
#' @export
pa_get_event_data <- function(guid, last_time = 0) {
    do_get_event_data(guid = guid, ws = "GetLatestStats2", last_time = 0)
}

## unexported, old version that uses double-gzipped service
pa_get_event_data0 <- function(guid, last_time = 0) {
    do_get_event_data(guid = guid, ws = "GetLatestStats", last_time = 0)
}


## GetLatestStats double b64/gzips its event data payload
## GetLatestStats2 doesn't
do_get_event_data <- function(guid, ws = "GetLatestStats2", last_time = 0) {
    assert_that(is.string(guid), !is.na(guid))
    assert_that(is.string(ws))
    ws <- match.arg(ws, c("GetLatestStats", "GetLatestStats2"))
    if (inherits(last_time, "POSIXt")) last_time <- as.numeric(last_time)/1000
    assert_that(is.numeric(last_time))
    my_url <- paste0(pa_opt("base_url"), "/", ws, "?guid=", URLencode(guid), "&lasttime=", last_time)
    x <- read_xml(my_url)
    x <- xml_text(xml_contents(x)[[1]])
    x <- read.table(text = x, sep = "~", stringsAsFactors = FALSE)
    if (ncol(x) < 4) stop("No event data is available for this match")
    x <- b64gunz(x[[4]])
    if (!any(grepl("\"", x, fixed = TRUE))) x <- b64gunz(x)
    x
}
