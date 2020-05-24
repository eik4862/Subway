#' Plot raw time series
#'
#' Take time series data and plot raw traces.
#' @param data Time series data or list of them to be plotted.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param date POSIXct vector or list of them for x axis.
#' If NULL, it uses integer index 1, 2, ... instead. (default = NULL)
#' @param name Character vector of data names for facet titles.
#' If NULL, it uses the name of input data argument instead. (default = NULL)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return \code{ggplot} object of the plot
plot.raw <- function(data, date = NULL, name = NULL) {
  if (class(data)[1] == "list") { # Multiple time series
    # Get ready
    data <- lapply(data, as.numeric)

    if (is.null(name)) {
      name <- names(data)

      if (is.null(name)) {
        name <- paste("Data", seq_len(length(data)), sep = "_")
      }
    } else if (length(name) != length(data)) {
      stop("number of given data and names do not match")
    }

    if (is.null(date)) {
      date <- unlist(lapply(data, seq_along))
      x.lab <- "Index"
      free <- "free_y"
    } else if (typeof(date) != "list") {
      date <- rep(date, times = length(data))
      x.lab <- "Date"
      free <- "free_y"
    } else {
      date <- do.call(c, date)
      x.lab <- "Date"
      len <- sapply(data, length)
      free <- ifelse(all(len == len[1]), "free_y", "free")
    }

    ref <- seq_len(length(data))
    names(name) <- ref
    ref.it <- unlist(lapply(seq_len(length(data)), function(i) rep(ref[i], times = length(data[[i]]))))

    # Plot
    plt <- data.frame(date = date, data = unlist(data), ref = factor(ref.it)) %>%
      ggplot(aes(x = date, y = data, color = ref)) +
      geom_line(alpha = 0.7) +
      facet_wrap(.~ref, ncol = 1, strip.position = "right", scales = free, labeller = labeller(ref = as_labeller(name))) +
      labs(title = "Plot of raw time series", x = x.lab, y = "Passengers") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  } else {  # Single time series
    # Get ready
    if (is.null(name)) {
      name <- deparse(substitute(data))
    }

    data <- as.numeric(data)

    if (is.null(date)) {
      date <- seq_along(data)
      x.lab <- "Index"
    } else {
      x.lab <- "Date"
    }

    # Plot
    plt <- ggplot(data = NULL, aes(x = date, y = data, color = "A")) +
      geom_line(alpha = 0.7) +
      labs(title = "Plot of raw time series", subtitle = paste("Data", name), x = x.lab, y = "Passengers") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  }

  return(plt)
}

#' Plot trend of time series
#'
#' Take time series data and overlay their trend over raw trace.
#' @param data Time series data or list of them whose trend is to be plotted.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param method Smoothing method.
#' Should be one of \code{"loess"}, \code{"ma"}, \code{"lm"}, \code{"gam"}, and \code{"glm"}. (default = "loess")
#' @param ... Extra arguments for smoothing.
#' Refer to \code{\link{ma}} and \code{\link{geom_smooth}}.
#' In caes of MA, if \code{order} argument is not given, 10 will be as default value.
#' @param date POSIXct vector or list of them for x axis.
#' If NULL, it uses integer index 1, 2, ... instead. (default = NULL)
#' @param name Character vector of data names for facet titles.
#' If NULL, it uses the name of input data argument instead. (default = NULL)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return \code{ggplot} object of the plot
plot.trend <- function(data, method = c("loess", "ma", "lm", "gam", "glm"), ..., date = NULL, name = NULL) {
  method <- match.arg(method)

  if (class(data)[1] == "list") { # Multiple time series
    # Get ready
    data <- lapply(data, as.numeric)

    if (is.null(name)) {
      name <- names(data)

      if (is.null(name)) {
        name <- paste("Data", seq_len(length(data)), sep = "_")
      }
    } else if (length(name) != length(data)) {
      stop("number of given data and names do not match")
    }

    if (is.null(date)) {
      date <- unlist(lapply(data, seq_along))
      x.lab <- "Index"
      free <- "free_y"
    } else if (typeof(date) != "list") {
      date <- rep(date, times = length(data))
      x.lab <- "Date"
      free <- "free_y"
    } else {
      date <- do.call(c, date)
      x.lab <- "Date"
      len <- sapply(data, length)
      free <- ifelse(all(len == len[1]), "free_y", "free")
    }

    ref <- seq_len(length(data))
    names(name) <- ref
    ref.it <- unlist(lapply(seq_len(length(data)), function(i) rep(ref[i], times = length(data[[i]]))))

    # MA smoothing is not built-in
    if (method == "ma") {
      # Compute MA
      if (!hasArg(order)) {
        args <- list(order = 10, ...)
      }

      smooth <- lapply(data, ma, unlist(args))

      # Plot
      plt <- data.frame(date = date, data = unlist(data), ref = factor(ref.it)) %>%
        ggplot(aes(x = date, y = data)) +
        geom_line(alpha = 0.4) +
        geom_line(aes(y = unlist(smooth), color = ref), na.rm = TRUE)
    } else {
      # Plot
      plt <- data.frame(date = date, data = unlist(data), ref = factor(ref.it)) %>%
        ggplot(aes(x = date, y = data)) +
        geom_line(alpha = 0.4) +
        geom_smooth(method = method, ..., aes(color = ref))
    }

    plt <- plt +
      facet_wrap(.~ref, ncol = 1, strip.position = "right", scales = free, labeller = labeller(ref = as_labeller(name))) +
      labs(title = paste("Plot of trend using", toupper(method)), x = x.lab, y = "Passengers") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  } else {  # Single time series
    # Get ready
    if (is.null(name)) {
      name <- deparse(substitute(data))
    }

    data <- as.numeric(data)

    if (is.null(date)) {
      date <- seq_along(data)
      x.lab <- "Index"
    } else {
      x.lab <- "Date"
    }

    # MA smoothing is not built-in
    if (method == "ma") {
      # Compute MA
      if (!hasArg(order)) {
        args <- list(order = 10, ...)
      }

      smooth <- ma(data, unlist(args))

      # Plot
      plt <- ggplot(data = NULL, aes(x = date, y = unlist(data))) +
        geom_line(alpha = 0.4) +
        geom_line(aes(y = smooth, color = "A"), na.rm = TRUE)
    } else {
      # Plot
      plt <- ggplot(data = NULL, aes(x = date, y = unlist(data))) +
        geom_line(alpha = 0.4) +
        geom_smooth(method = method, ..., aes(color = "A"))
    }

    plt <- plt +
      labs(title = paste("Plot of trend using", toupper(method)), subtitle = paste("Data", name),
           x = x.lab, y = "Passengers") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  }

  return(plt)
}

#' Plot sample ACF of time series
#'
#' Take time series data and plot their sample ACF and CI.
#' @param data Time series data or list of them whose ACF is to be plotted.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param lag.max Maximum length of lag to be shown.
#' If null, it uses default setting of \code{\link{acf}} function.
#' @param conf.level The confidence level for CI. (default = 0.95)
#' @param name Character vector of data names for facet titles.
#' If NULL, it uses the name of input data argument instead. (default = NULL)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return \code{ggplot} object of the plot
plot.acf <- function(data, lag.max = NULL, conf.level = 0.95, name = NULL) {
  if (class(data)[1] == "list") { # Multiple time series
    # Get ready
    if (is.null(name)) {
      name <- names(data)

      if (is.null(name)) {
        name <- paste("Data", seq_len(length(data)), sep = "_")
      }
    } else if (length(name) != length(data)) {
      stop("number of given data and names do not match")
    }

    #Compute ACF
    data.acf <- lapply(data, function(x) drop(acf(x, lag.max = lag.max, plot = FALSE)$acf))
    lag <- lapply(data.acf, function(x) seq_along(x) - 1)

    ref <- seq_len(length(data.acf))
    names(name) <- ref
    ref.it <- unlist(lapply(seq_len(length(data.acf)), function(i) rep(ref[i], times = length(data.acf[[i]]))))

    ci <- data.frame(ci = unlist(lapply(data, function(x) c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) / sqrt(length(x)))),
                     ref = rep(ref, each = 2))

    # Plot
    plt <- data.frame(lag = unlist(lag), data.acf = unlist(data.acf), ref = factor(ref.it)) %>%
      ggplot(aes(x = lag, y = data.acf, color = ref)) +
      geom_segment(aes(x = lag, xend = lag, y = 0, yend = data.acf), alpha = 0.5) +
      geom_point() +
      geom_hline(data = ci, aes(yintercept = ci), linetype = "dashed", color = "darkblue") +
      facet_wrap(.~ref, ncol = 1, strip.position = "right", scales = "free_y",
                 labeller = labeller(ref = as_labeller(name))) +
      labs(title = "Autocorrelation function", x = "Lag", y = "ACF") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  } else {  # Single time series
    # Get ready
    if (is.null(name)) {
      name <- deparse(substitute(data))
    }

    # Compute ACF
    data.acf <- drop(acf(data, lag.max = lag.max)$acf)
    lag <- seq_along(data.acf) - 1
    ci <- c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) / sqrt(length(data))

    # Plot
    plt <- ggplot(data = NULL, aes(x = lag, y = data.acf, color = "A")) +
      geom_segment(aes(x = lag, xend = lag, y = 0, yend = data.acf), alpha = 0.5) +
      geom_point() +
      geom_hline(yintercept = ci, linetype = "dashed", color = "darkblue") +
      labs(title = "Autocorrelation function", subtitle = paste("Data", name), x = "Lag", y = "ACF") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  }

  return(plt)
}

#' Plot sample PACF of time series
#'
#' Take time series data and plot their sample PACF and CI.
#' @param data Time series data or list of them whose PACF is to be plotted.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param lag.max Maximum length of lag to be shown.
#' If null, it uses default setting of \code{\link{acf}} function.
#' @param conf.level The confidence level for CI. (default = 0.95)
#' @param name Character vector of data names for facet titles.
#' If NULL, it uses the name of input data argument instead. (default = NULL)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return \code{ggplot} object of the plot
plot.pacf <- function(data, lag.max = NULL, conf.level = 0.95, name = NULL) {
  if (class(data)[1] == "list") { # Multiple time series
    # Get ready
    if (is.null(name)) {
      name <- names(data)

      if (is.null(name)) {
        name <- paste("Data", seq_len(length(data)), sep = "_")
      }
    } else if (length(name) != length(data)) {
      stop("number of given data and names do not match")
    }

    #Compute PACF
    data.pacf <- lapply(data, function(x) drop(pacf(x, lag.max = lag.max, plot = FALSE)$acf))
    lag <- lapply(data.pacf, function(x) seq_along(x) - 1)

    ref <- seq_len(length(data.pacf))
    names(name) <- ref
    ref.it <- unlist(lapply(seq_len(length(data.pacf)), function(i) rep(ref[i], times = length(data.pacf[[i]]))))

    ci <- data.frame(ci = unlist(lapply(data, function(x) c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) / sqrt(length(x)))),
                     ref = rep(ref, each = 2))

    # Plot
    plt <- data.frame(lag = unlist(lag), data.pacf = unlist(data.pacf), ref = factor(ref.it)) %>%
      ggplot(aes(x = lag, y = data.pacf, color = ref)) +
      geom_segment(aes(x = lag, xend = lag, y = 0, yend = data.pacf), alpha = 0.5) +
      geom_point() +
      geom_hline(data = ci, aes(yintercept = ci), linetype = "dashed", color = "darkblue") +
      facet_wrap(.~ref, ncol = 1, strip.position = "right", scales = "free_y",
                 labeller = labeller(ref = as_labeller(name))) +
      labs(title = "Partial autocorrelation function", x = "Lag", y = "PACF") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  } else {  # Single time series
    # Get ready
    if (is.null(name)) {
      name <- deparse(substitute(data))
    }

    # Compute ACF
    data.pacf <- drop(pacf(data, lag.max = lag.max)$acf)
    lag <- seq_along(data.pacf) - 1
    ci <- c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) / sqrt(length(data))

    # Plot
    plt <- ggplot(data = NULL, aes(x = lag, y = data.pacf, color = "A")) +
      geom_segment(aes(x = lag, xend = lag, y = 0, yend = data.pacf), alpha = 0.5) +
      geom_point() +
      geom_hline(yintercept = ci, linetype = "dashed", color = "darkblue") +
      labs(title = "Partial autocorrelation function", subtitle = paste("Data", name), x = "Lag", y = "PACF") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  }

  return(plt)
}

#' Plot studentized residuals
#'
#' Take fitted ARIMA models and plot studentized residuals.
#' @param fit Fitted ARIMA model(\code{ARIMA} object) or list of them whose residuals to be plotted.
#' @param date POSIXct vector or list of them for x axis.
#' If NULL, it uses integer index 1, 2, ... instead. (default = NULL)
#' @param name Character vector of model names for facet titles.
#' If NULL, it uses the model name of input \code{ARIMA} object instead. (default = NULL)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return \code{ggplot} object of the plot
plot.residual <- function(fit, date = NULL, name = NULL) {
  if (class(fit)[1] == "list") {  # Multiple models
    # Get ready
    if (is.null(name)) {
      name <- sapply(fit, function(x) sprintf("%s", x))
    } else if (length(name) != length(fit)) {
      stop("number of given models and names do not match")
    }

    if (is.null(date)) {
      date <- unlist(lapply(fit, function(x) seq_along(x$residuals)))
      x.lab <- "Index"
      free <- "free_y"
    } else if (typeof(date) != "list") {
      date <- rep(date, times = length(fit))
      x.lab <- "Date"
      free <- "free_y"
    } else {
      date <- do.call(c, date)
      x.lab <- "Date"
      len <- sapply(fit, function(x) length(x$residuals))
      free <- ifelse(all(len == len[1]), "free_y", "free")
    }

    ref <- seq_len(length(fit))
    names(name) <- ref
    std.resi <- lapply(fit, function(x) scale(as.numeric(x$residuals)))
    ref.it <- unlist(lapply(seq_len(length(fit)), function(i) rep(ref[i], times = length(std.resi[[i]]))))

    # Plot
    plt <- data.frame(resi = unlist(std.resi), ref = factor(ref.it)) %>%
      ggplot(aes(x = date, y = resi, color = ref)) +
      geom_line(alpha = 0.7) +
      facet_wrap(.~ref, ncol = 1, strip.position = "right", scales = free, labeller = labeller(ref = as_labeller(name))) +
      labs(title = "Plot of studentized residuals", x = x.lab, y = "Studentized residual") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  } else {  # Single model
    # Get ready
    if (is.null(name)) {
      name <- sprintf("%s", fit)
    }

    if (is.null(date)) {
      date <- seq_along(fit$residuals)
      x.lab <- "Index"
    } else {
      x.lab <- "Date"
    }

    std.resi <- scale(as.numeric(fit$residuals))

    # Plot
    plt <- ggplot(data = NULL, aes(x = date, y = std.resi, color = "A")) +
      geom_line(alpha = 0.7) +
      labs(title = "Plot of studentized residuals", subtitle = paste("Model", name), x = x.lab, y = "Studentized residual") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
  }

  return(plt)
}
