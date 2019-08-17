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
    assert_that(is.string(guid), !is.na(guid))
    if (inherits(last_time, "POSIXt")) last_time <- as.numeric(last_time)/1000
    assert_that(is.numeric(last_time))
    my_url <- paste0(pa_opt("base_url"), "/GetLatestStats2?guid=", URLencode(guid), "&lasttime=", last_time)
##cat("GetLatestStats URL: ", my_url, "\n")
    x <- read_xml(my_url)
    x <- xml_text(xml_contents(x)[[1]])
    x <- read.table(text = x, sep = "~", stringsAsFactors = FALSE, comment.char = "")
    if (ncol(x) < 4) stop("No event data is available for this match")
    x <- lapply(strsplit(x[[4]], "|#|", fixed = TRUE)[[1]], b64gunz) ## incremental sends are separated by |#|
    ## the first element of x will be the main info with Q, DTH, DTA, SS, etc and some E sections
    ## each subsequent element in x should contain incremental E's, a new Q, and possibly other things (?)
    ## grr, Q's don't have a guid and events aren't explicitly linked to a particular Q
    ## check each new Q against the last one, and if it has the same StartTime then assume it's the same set
    ## if it has a new StartTime then it's a new set
    out <- x[[1]]
    qidx <- grep("^Q~", out)
    if (length(qidx) < 1) stop("no Q sections")
    current_q_idx <- tail(qidx, 1)
    current_q <- out[[current_q_idx]]
    current_q_json <- jsonlite::fromJSON(sub("^Q~", "", current_q))
    current_q_start_time <- current_q_json$StartTime
    if (length(x) > 1) {
        for (inc in x[-1]) {
            if (!all(grepl("^[QE]~", inc))) stop("events increment contains something other than E and Q")
            this_q <- inc[[grep("^Q~", inc)]]
            this_q_json <- jsonlite::fromJSON(sub("^Q~", "", this_q))
            if (this_q_json$StartTime %eq% current_q_start_time) {
##cat("same set\n")
                ## replaces current q
                out[current_q_idx] <- this_q
            } else {
                ## new set
##cat("new set\n")
                out <- c(out, this_q)
                current_q_idx <- length(out)
                current_q_start_time <- this_q_json$StartTime
            }
            ## add events to end of out
            ## TODO deal with undos and replacements of old events
            out <- c(out, inc[grepl("^E~", inc)])
        }
    }
    out
}
