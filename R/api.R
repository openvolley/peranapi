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
    ##x <- read.table(text = x, sep = "~", stringsAsFactors = FALSE, comment.char = "") ## SLOOOOOOOOW
    x <- data.table::fread(text = paste0(x, "\n"), sep = "~", data.table = FALSE)
    if (ncol(x) < 4) stop("No event data is available for this match")
    x <- lapply(strsplit(x[[4]], "|#|", fixed = TRUE)[[1]], b64gunz) ## incremental sends are separated by |#|
    ## the first element of x will be the main info with Q, DTH, DTA, SS, etc and some E sections
    ## each subsequent element in x should contain incremental E's, a new Q, and possibly other things (?)
    ## grr, Q's don't have a guid and events aren't explicitly linked to a particular Q
    ## check each new Q against the last one, and if it has the same StartTime then assume it's the same set
    ## if it has a new StartTime then it's a new set
    ## also expect to see MODE (modified event) and DELE (deleted event) directives. Use the EventId in the MODE/DELE section to find the existing event to delete or replace
    out <- x[[1]]
    out_parsed <- rep(list(NULL), length(out))
    eidx <- grep("^E~", out)
    if (length(eidx) > 0) out_parsed[eidx] <- pparse_multi(out[eidx])
    qidx <- grep("^Q~", out)
    if (length(qidx) < 1) stop("no Q sections")
    current_q_idx <- tail(qidx, 1)
    current_q <- out[[current_q_idx]]
    current_q_json <- jsonlite::fromJSON(sub("^Q~", "", current_q))
    current_q_start_time <- current_q_json$StartTime
    unprocessed_events <- c()
    if (length(x) > 1) {
        allinc <- do.call(rbind, lapply(seq_along(x)[-1], function(z) data.frame(ii = z, inc = x[[z]], stringsAsFactors = FALSE)))
        allinc_parsed <- pparse_multi(allinc$inc) ## parse all at once, even though parsing individually doesn't seem much slower
        for (this_ii in sort(unique(allinc$ii))) {
            iidx <- allinc$ii == this_ii
            this <- process_increment(allinc$inc[iidx], out, inc_parsed = allinc_parsed[iidx], out_parsed = out_parsed, current_q_start_time = current_q_start_time, current_q_idx = current_q_idx)
            out <- this$out
            out_parsed <- this$out_parsed
            current_q_start_time <- this$current_q_start_time
            unprocessed_events <- c(unprocessed_events, this$unprocessed)
        }
        if (length(unprocessed_events) > 0) {
            for (inc in unprocessed_events) {
                this <- process_increment(inc, out, out_parsed = out_parsed, current_q_start_time = current_q_start_time, current_q_idx = current_q_idx, queue_unprocessed = FALSE)
                out <- this$out
                out_parsed <- this$out_parsed
            }
        }
    }
    out
}

## slow
existing_event <- function(parsed_event, existing) {
    if (!"EventId" %in% names(parsed_event)) stop("no EventId in parsed_event")
    grep(paste0("EventId\"[[:space:]]*:[[:space:]]*\"", parsed_event$EventId, "\""), existing)
}

existing_event_p <- function(parsed_event, existing_parsed) {
    if (!"EventId" %in% names(parsed_event)) stop("no EventId in parsed_event")
    tevid <- parsed_event$EventId
    which(vapply(existing_parsed, function(z) "EventId" %in% names(z) && z$EventId == tevid, FUN.VALUE = TRUE))
}

process_increment <- function(inc, out, queue_unprocessed = TRUE, inc_parsed = NULL, out_parsed, current_q_start_time, current_q_idx, debug = FALSE) {
    unproc <- c()
    if (is.null(inc_parsed)) inc_parsed <- pparse_multi(inc)
    if (length(inc) == 1 && grepl("^(DELE|MODE)", inc)) {
        ##eidx <- existing_event(inc_parsed[[1]], out)
        eidx <- existing_event_p(inc_parsed[[1]], out_parsed)
        if (length(eidx) == 1) {
            if (grepl("^DELE", inc)) {
                if (debug) cat("DELE at", eidx, "\n")
                ## change this?
                out <- out[-eidx]
                out_parsed <- out_parsed[-eidx]
            } else {
                if (debug) cat("MODE at", eidx, "\n")
                out[eidx] <- inc
                out_parsed[[eidx]] <- inc_parsed[[1]]
            }
        } else if (length(eidx) < 1) {
            if (queue_unprocessed) {
                unproc <- c(unproc, inc)
            } else {
                warning("no matching events for MODE/DELE directive, ignoring")
            }
        } else {
            stop("multiple matching events for MODE/DELE directive")
        }
    } else {
        if (!all(grepl("^[QE]~", inc))) stop("events increment contains something other than E and Q")
        ##this_q <- inc[[grep("^Q~", inc)]]
        ##this_q_json <- jsonlite::fromJSON(sub("^Q~", "", this_q))
        this_q_idx <- grep("^Q~", inc)
        this_q <- inc[[this_q_idx]]
        this_q_json <- inc_parsed[[this_q_idx]]
        if (this_q_json$StartTime %eq% current_q_start_time) {
            if (debug) cat("same set\n")
            ## replaces current q
            out[current_q_idx] <- this_q
        } else {
            ## new set
            if (debug) cat("new set\n")
            out <- c(out, this_q)
            out_parsed <- c(out_parsed, list(this_q_json))
            current_q_idx <- length(out)
            current_q_start_time <- this_q_json$StartTime
        }
        ## add events to end of out
        ## sometimes we appear to get an event that replaces an existing event, but as a standard E~ directive, not MODE~
        for (evi in grep("^E~", inc)) {
            ev <- inc[evi]
            ##eidx <- existing_event(inc_parsed[[evi]], out)
            eidx <- existing_event_p(inc_parsed[[evi]], out_parsed)
            if (length(eidx) < 1) {
                ## add
                if (debug) cat("new E at", length(out)+1, "\n")
                out <- c(out, ev)
                out_parsed <- c(out_parsed, inc_parsed[evi])
            } else if (length(eidx) == 1) {
                ## replace unless identical
                ##if (ev != out[eidx]) {
                if (debug) cat("replacing E at", eidx, ":\n", out[eidx], "\nwith\n", ev, "\n")
                out[eidx] <- ev
                out_parsed[[eidx]] <- inc_parsed[[evi]]
                ##}
            } else {
                stop("multiple existing events with same EventId as ", ev)
            }
        }
    }
    if (debug) cat("\n")
    list(out = out, out_parsed = out_parsed, unprocessed = unproc, current_q_start_time = current_q_start_time, current_q_idx = current_q_idx)
}
