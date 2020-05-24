#' Get subway data
#'
#' Take request and group specification, it returns corresponding data from the source DB.
#' In case of invalid request, it returns an NULL and prints out a warning message.
#' @param request Station codes for the request.
#' It should be a \code{numeric} or \code{character} vector indicating the station codes to get.
#' @param group Group specification.
#' Should be one of \code{"day"}, \code{"week"}, \code{"month"}, and \code{"year"}.
#' Note that \code{"week"} here may not coincide with real world's week concept.
#' For detail, refer to \code{\link{week}}. (default = "day")
#' @param FUN Function to be applied when groupping.
#' Note that if \code{group} is "day", then this will be ignored. (default = sum)
#' @param ... Extra arguments for \code{FUN}.
#' @param from Character string specifying the start day(inclusive).
#' It should be a \code{POSIXct} object or a \code{character} which can be casted to \code{POSIXct}.
#' If \code{NULL}, it will be ignored. (default = NULL)
#' @param to Character string specifiying the end day(inclusive).
#' It should be a \code{POSIXct} object or a \code{character} which can be casted to \code{POSIXct}.
#' If \code{NULL}, it will be ignored. (default = NULL)
#' @param split If \code{TRUE}, the result will be splitted by the station code.
#' Otherwise, not. (default = FALSE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A data frame holding the requested info if \code{split} is FALSE.
#' A list properly splitted by the station code if \code{split} is TRUE.
#'
#' @examples
#' # Get daily subway data of SNU and Shinchon station
#' # 228 and 240 are the station codes for SNU and Shinchon, respectively
#' data <- get.subway(c(228, 240), group = "day")
#' head(data)
#'
#' # Get montly subway data of SNU and Shinchon station
#' # We want splitted data and want to mean over every month
#' data <- get.subway(c(228, 240), group = "month", FUN = mean, split = TRUE)
#' head(data$"228")
#' head(data$"240")
#'
#' # Get weekly data of SNU station to 12/31/2019
#' data <- get.subway(228, group = "week", to = "2019-12-31")
#' tail(data)
get.subway <- function(request, group = c("day", "week", "month", "year"), FUN = sum, ..., from = NULL, to = NULL,
                       split = FALSE) {
  group <- match.arg(group)

  # Subset target
  target <- subset(subway, code %in% request)
  target$code <- droplevels(target$code)

  # If there is no requested data, print warnings
  for (it in request) {
    if (!(it %in% levels(target$code))) {
      warning(paste("request", it, "not found"))
    }
  }

  # If there is no target, just return
  if (nrow(target) == 0) return(invisible(NULL))

  # If there is date specification, subset it
  if (!is.null(from)) {
    target <- subset(target, date >= as.POSIXct(from))
  }

  if (!is.null(to)) {
    target <- subset(target, date <= as.POSIXct(to))
  }

  # If group option is not day, preprocess it properly
  if (group == "year") {
    tmp <- with(target, split(target, code))
    pivot <- lapply(tmp, function(x) year(x$date))

    for (i in seq_len(length(tmp))) {
      tmp[[i]] <- cbind(tmp[[i]][!duplicated(pivot[[i]]), 1:2],
                        with(tmp[[i]], data.frame(on = tapply(on, pivot[[i]], FUN, ...),
                                                  off = tapply(off, pivot[[i]], FUN, ...),
                                                  total = tapply(total, pivot[[i]], FUN, ...),
                                                  diff = tapply(diff, pivot[[i]], FUN, ...))))
    }

    target <- Reduce(rbind, tmp)
  } else if (group == "month") {
    tmp <- with(target, split(target, code))
    pivot <- lapply(tmp, function(x) sprintf("%04d %02d", year(x$date), month(x$date)))

    for (i in seq_len(length(tmp))) {
      tmp[[i]] <- cbind(tmp[[i]][!duplicated(pivot[[i]]), 1:2],
                        with(tmp[[i]], data.frame(on = tapply(on, pivot[[i]], FUN, ...),
                                                  off = tapply(off, pivot[[i]], FUN, ...),
                                                  total = tapply(total, pivot[[i]], FUN, ...),
                                                  diff = tapply(diff, pivot[[i]], FUN, ...))))
    }

    target <- Reduce(rbind, tmp)
  } else if (group == "week") {
    tmp <- with(target, split(target, code))
    pivot <- lapply(tmp, function(x) sprintf("%04d %02d", year(x$date), week(x$date)))

    for (i in seq_len(length(tmp))) {
      tmp[[i]] <- cbind(tmp[[i]][!duplicated(pivot[[i]]), 1:2],
                        with(tmp[[i]], data.frame(on = tapply(on, pivot[[i]], FUN, ...),
                                                  off = tapply(off, pivot[[i]], FUN, ...),
                                                  total = tapply(total, pivot[[i]], FUN, ...),
                                                  diff = tapply(diff, pivot[[i]], FUN, ...))))
    }

    target <- Reduce(rbind, tmp)
  }

  rownames(target) <- seq_len(nrow(target))

  # If split option is true, split data into a list
  if (split) {
    target <- with(target, split(target, code))
  }

  return(target)
}

#' Search for subway station code
#'
#' Take search keyword and search internal dictionary for station codes.
#' It supports regex and substring mathcing.
#' The search result will be printed out to the console.
#' @param name Keyword to be searched.
#' Accepts substring and regex.
#' @param exact If \code{TRUE}, it only finds exact match.
#' Otherwise, not. (default = FALSE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @examples
#' # What is the station code for SNU?
#' search.code("서울대")
search.code <- function(name, exact = FALSE) {
  name <- gsub(" ", "", name)

  if (exact) {
    found <- dict[name == dict$name,]
  } else {
    found <- dict[grep(name, dict$name),]
  }

  if (nrow(found) == 0) {
    print("No search result")
  } else {
    rownames(found) <- seq_len(nrow(found))
    print(found)
  }
}
