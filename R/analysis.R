#' Power spectrum analaysis for time series
#'
#' Take time series data and perform power specturm analysis.
#' Through FFT, it lists up the most probable periods for seasonal term.
#' Up to 10 most likely suggestions will be printed out to the console based on estimated PSD(Power Spectrum Density).
#' @param data Time series data or list of them to be plotted.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param plot If \code{TRUE}, the plot of estimated PSD will be presented.
#' Otherwise, not. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{period} \tab Data frame holding suggested periods and corresponding PSD. Ordered by PSD.\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' data <- get.subway(228)
#' psd(data$total)
psd <- function(data, plot = TRUE) {
  # There should be at least three data points
  if (length(data) <= 2) {
    stop("number of the data is too small")
  }

  # Get ready
  dname <- deparse(substitute(data))
  N <- length(data)
  data <- as.numeric(data)

  # FFT and find candidates
  data.fft <- abs(fft(data)) ** 2 / N
  data.fft <- data.fft[-c(1, ceiling(N / 2):N)]
  period <- N / (1:(ceiling(N / 2) - 2))

  # If plot is true, plot PSD
  if (plot) {
    plt <- data.frame(fft = data.fft, period = period) %>%
      ggplot(aes(x = period, y = fft, color = "A")) +
      geom_segment(aes(x = period, xend = period, y = 0, yend = fft), alpha = 0.5) +
      geom_point() +
      labs(title = "Estimated power spectral density", subtitle = paste("Data", dname), x = "Period", y = "Power") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))

    if (length(period) > 100) {
      plt <- plt + coord_cartesian(xlim = c(1, 100))
    }

    plot(plt)
  }

  # Order frequencies and report
  period <- period[order(data.fft, decreasing = TRUE)]

  cat("\n\tPower sepctrum analysis\n\n")
  cat(sprintf("data  : %s\n", dname))
  cat("top 10:\n")

  for (i in seq_len(min(10, length(period)))) {
    cat(sprintf("  [%02d] %.8g\n", i, period[i]))
  }

  if (plot) {
    res <- list(period = data.frame(period = period, psd = sort(data.fft, decreasing = TRUE)), plot = plt)
  } else {
    res <- list(period = data.frame(period = period, psd = sort(data.fft, decreasing = TRUE)))
  }

  return(invisible(res))
}

#' Normality check procedure for residuals
#'
#' Take fitted ARIMA model and perform normality checks.
#' It runs three well-known normality test, Shapiro-Wilk test, Jarque-Bera test, and Kolmogorov-Smirnov test
#' and prints out the result to the console.
#' If \code{plot} is TRUE, it also plots Q-Q plot of residuals and estimated density of residual for visual check.
#' @param fit Fitted ARIMA model(\code{ARIMA} object).
#' @param alpha The probability of type 1 error for normality tests. (default = 0.05)
#' @param plot If \code{TRUE}, the Q-Q plot of residuals and estimated density of residuals will be presented.
#' Otherwise, not. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{shapiro.wilk} \tab The test result of Shapiro-Wilk test(\code{htest} objects).\cr
#'   \tab\cr
#'   \code{jarque.bera} \tab The test result of Jarque-Bera test(\code{htest} objects).\cr
#'   \tab\cr
#'   \code{komogorov.smirnov} \tab The test result of Kolmogorov-Smirnov test(\code{htest} objects).\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' data <- get.subway(228)
#'
#' # Fit SARIMA model with total on and off passenger counts
#' SNU.total <- ts(data$total, frequency = 7)
#' SNU.fit <- auto.arima(SNU.total)
#'
#' # Normality check
#' normal.test(SNU.fit)
normal.test <- function(fit, alpha = 0.05, plot = TRUE) {
  model <- sprintf("%s", fit)

  # If plot is true, plot Q-Q plot and density plot
  if (plot) {
    qq.plt <- ggplot(data = NULL, aes(sample = as.numeric(fit$residuals), color = "A")) +
      geom_qq(alpha = 0.7) +
      geom_qq_line() +
      labs(title = "Q-Q plot of residuals", subtitle = paste("Model", model), x = "Theoretical", y = "Sample") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
    dense.plt <- ggplot(data = NULL, aes(x = as.numeric(fit$residuals), color = "A", fill = "A")) +
      geom_density(alpha = 0.2) +
      geom_point(aes(x = as.numeric(fit$residuals), y = 0), alpha = 0.3) +
      labs(title = "Estimated density of residuals", subtitle = paste("Model", model), x = "Residual", y = "Density") +
      scale_y_continuous(position = "right") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
    plot(ggarrange(qq.plt, dense.plt))
  }

  # Run Shapiro-Wilk test, Jarque-Bera test, and Kolmogorov-Smirnov test
  SW <- shapiro.test(as.numeric(fit$residuals))
  JB <- jarque.bera.test(as.numeric(fit$residuals))
  KS <- ks.test(as.numeric(fit$residuals), pnorm, mean = mean(fit$residuals), sd = sd(fit$residuals))
  SW.res <- ifelse(SW$p.value < alpha, "Severe non-normality is suspected", "No strong evidence for non-normality")
  JB.res <- ifelse(JB$p.value < alpha, "Severe non-normality is suspected", "No strong evidence for non-normality")
  KS.res <- ifelse(KS$p.value < alpha, "Severe non-normality is suspected", "No strong evidence for non-normality")

  # Report
  cat("\n\tShapiro-Wilk test for normality\n\n")
  cat(sprintf("model  : %s\n", model))
  cat(sprintf("p-value: %.4g %s\n", SW$p.value, sign.code(SW$p.value)))
  cat(sprintf("result : %s\n", SW.res))
  cat("\n\tJarque-Bera test for normality\n\n")
  cat(sprintf("model  : %s\n", model))
  cat(sprintf("p-value: %.4g %s\n", JB$p.value, sign.code(JB$p.value)))
  cat(sprintf("result : %s\n", JB.res))
  cat("\n\tKolmogorov-Smirnov test for normality\n\n")
  cat(sprintf("model  : %s\n", model))
  cat(sprintf("p-value: %.4g %s\n", KS$p.value, sign.code(KS$p.value)))
  cat(sprintf("result : %s\n", KS.res))
  cat("\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

  # Return test results and plots if exist
  if (plot) {
    res <- list(shapiro.wilk = SW, jarque.bera = JB, komogorov.smirnov = KS, qq.plot = qq.plt, density.plot = dense.plt)
  } else {
    res <- list(shapiro.wilk = SW, jarque.bera = JB, komogorov.smirnov = KS)
  }

  return(invisible(res))
}

#' Model adequacy check procedure
#'
#' Take fitted ARIMA model and perform adequacy checks.
#' It runs Ljung-Box test with residuals for various lags and print out the test result to the console.
#' If \code{plot} is TRUE, it also plots the test result.
#' @param fit Fitted ARIMA model(\code{ARIMA} object).
#' @param alpha The probability of type 1 error for normality tests. (default = 0.05)
#' @param lag.max Maximum length of lag to be tested. (default = \code{min(30, length(fit$residuals) - 1)})
#' @param plot If \code{TRUE}, the test result will be presented as a plot.
#' Otherwise, not. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{ljung.box} \tab A list of all test results(\code{htest} objects).\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' data <- get.subway(228)
#'
#' # Fit SARIMA model with total on and off passenger counts
#' SNU.total <- ts(data$total, frequency = 7)
#' SNU.fit <- auto.arima(SNU.total)
#'
#' # Normality check
#' adequacy.test(SNU.fit)
adequacy.test <- function(fit, alpha = 0.05, lag.max = min(30, length(fit$residuals) - 1), plot = TRUE) {
  model <- sprintf("%s", fit)

  # Run Ljung-Box test for various lags
  LB <- lapply(seq_len(lag.max), function(x) Box.test(fit$residuals, lag = x, type = "Ljung-Box"))
  p.val <- sapply(LB, function(x) x$p.value)

  if (plot) {
    plt <- ggplot(NULL, aes(x = seq_len(lag.max), y = p.val, col = "A")) +
      geom_point() +
      geom_line(alpha = 0.5) +
      geom_hline(yintercept = alpha, linetype = "dashed") +
      annotate("text", x = lag.max, y = alpha, label = paste("alpha =", round(alpha, 4)), vjust = 2, hjust = 1) +
      scale_y_continuous(labels = function(x) round(log(x), 4)) +
      labs(title = "Ljung-Box test result", subtitle = paste("Model", model), x = "Lag", y = "Log of p-value") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
    plot(plt)
  }

  # Report
  cat("\n\tLjung-Box test for model adequacy\n\n")
  cat(sprintf("model  : %s\n", model))

  if (all(p.val > alpha)) {
    cat(sprintf("result : residuals have no dependencies upto lag %d\n", lag.max))
  } else {
    lag.dep <- seq_len(lag.max)[p.val < alpha]

    if (length(lag.dep) > 5) {
      cat(sprintf("result : residuals have dependencies at lags %s ...\n", paste(lag.dep[1:5], collapse = ", ")))
    } else {
      cat(sprintf("result : residuals have dependencies at lags %s\n", paste(lag.dep, collapse = ", ")))
    }
  }

  cat("p-value:\n")

  for (i in seq_len(length(p.val))) {
    cat(sprintf("  [%02d] %10.4g %s\n", i, p.val[i], sign.code(p.val[i])))
  }

  cat("\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

  # Return test results and plots if exist
  if (plot) {
    res <- list(ljung.box = LB, plot = plt)
  } else {
    res <- list(ljung.box = LB)
  }

  return(invisible(res))
}

#' Change point detection
#'
#' Take time series data and perform change point detection.
#' It uses \code{\link{cpt.mean}} or \code{\link{cpt.var}} from \code{changepoint} package for change point detection.
#' If \code{plot} is TRUE, it also plots the point where the changes take place.
#' @param data Time series data whose change point is to be detected.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param type Type of change to be detected.
#' Should be one of \code{"mean"} and \code{"var"}. (default = "mean")
#' @param ... Extra arguments for change point detection.
#' Refer to \code{\link{cpt.mean}} and \code{\link{cpt.var}}.
#' @param plot If \code{TRUE}, the chagne points will be plotted.
#' Otherwise, not. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{change.point} \tab Change point detection result(\code{cpt} object).\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' data <- get.subway(228)
#'
#' # Change point detection in mean
#' detect.change(data$total, type = "mean")
#'
#' # Change point detection in variance
#' detect.change(data$total, type = "var")
detect.change <- function(data, type = c("mean", "var"), ..., plot = TRUE) {
  type <- match.arg(type)
  dname <- deparse(substitute(data))
  data <- as.numeric(data)

  # Change point detection
  if (type == "mean") {
    change <- cpt.mean(data, ...)
  } else {
    change <- cpt.var(data, ...)
    type <- "variance"
  }

  change.pt <- change@cpts

  if (1 %in% change.pt) {
    change.pt <- change.pt[change.pt != 1]
  }

  if (length(data) %in% change.pt) {
    change.pt <- change.pt[change.pt != length(data)]
  }

  if (plot) {
    plt <- ggplot(data = NULL, aes(x = seq_along(data), y = data, color = "A")) +
      geom_line(alpha = 0.7) +
      geom_vline(xintercept = change.pt, linetype = "dashed", color = "darkblue") +
      labs(title = "Change point detection result", subtitle = paste("Change in", type), x = "", y = "") +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
    plot(plt)
  }

  # Report
  cat("\n\tChange point detection\n\n")
  cat(sprintf("data        : %s\n", dname))
  cat(sprintf("type        : %s\n", type))

  if (length(change.pt) != 0) {
    cat(sprintf("change point: %s\n", paste(change.pt, collapse = ", ")))
  } else {
    cat("change point: there are no change points\n")
  }

  # Return test results and plots if exist
  if (plot) {
    res <- list(change.point = change, plot = plt)
  } else {
    res <- list(change.point = change)
  }

  return(invisible(res))
}

#' SARIMA model selection procedure
#'
#' Take data and candidate parameters and run grid search.
#' For efficiency, it computes in parallel with heuristics on CPU load balancing.
#' For SARIMA model fitting, it uses \code{\link{Arima}} from \code{forecast}.
#' Sometimes, the fitting results in error for specific parameter combination (e.g. failiure in stationarity).
#' In such case, it just ignores such combination.
#' If \code{plot} is TRUE, it also plots the grid search result.
#' @param data Time series data for grid search.
#' The data should be either \code{ts} object or \code{numeric} vector.
#' @param p Candidates for AR order.
#' @param d Candidates for degree of differencing.
#' @param q Candidates for MA order.
#' @param P Candidates for seasonal AR order.
#' @param D Candidates for seasonal degree of differencing.
#' @param Q Candidates for seasonal MA order.
#' @param period Candidates for seasonal term period.
#' Note that long period needs quite long computation time.
#' @param ... Extra arguments for SARIMA fitting.
#' Refer to \code{\link{Arima}}.
#' @param criterion Model selection criterion.
#' Should be one of \code{"aic"}, \code{"aicc"}, and \code{"bic"}.
#' @param plot If \code{TRUE}, the obtained values of criterion for each candidiate model will be plotted.
#' Otherwise, not. (default = TRUE)
#' @param verbose If \code{TRUE}, it prints out messages indicating the internal progress.
#' Otherwise, not.
#' Since model selection procedure can take a long time, we highly recommend to keep it \code{TRUE}. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{min} \tab Achieved minimum of the criterion.\cr
#'   \tab\cr
#'   \code{select} \tab Selected model(\code{Arima} object).\cr
#'   \tab\cr
#'   \code{tune} \tab Data frame holding the grid search result. Ordered by the criterion values.\cr
#'   \tab\cr
#'   \code{models} \tab List of all models fitted. Ordered by the criterion values.\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' data <- get.subway(228)
#'
#' # Model selection
#' # It would take some time
#' model.select(data$total, p = 1:3, d = 1:3, q = 1:3,
#'              P = 1:3, D = 1:3, Q = 1:3, period = c(1, 7),
#'              criterion = "aic")
model.select <- function(data, p, d, q, P, D, Q, period, ..., criterion = c("aic", "aicc", "bic"), plot = TRUE, verbose = TRUE, log = TRUE) {
  criterion <- match.arg(criterion)
  dname <- deparse(substitute(data))
  data <- as.numeric(data)

  # Construct param combinations
  if (verbose) message("Constructing parameter combinations...")
  param <- data.frame(p = rep(p, each = length(d) * length(q) * length(P) * length(D) * length(Q) * length(period)),
                      d = rep(rep(d, each = length(q) * length(P) * length(D) * length(Q) * length(period)), times = length(p)),
                      q = rep(rep(q, each = length(P) * length(D) * length(Q) * length(period)), times = length(p) * length(d)),
                      P = rep(rep(P, each = length(D) * length(Q) * length(period)), times = length(p) * length(d) * length(q)),
                      D = rep(rep(D, each = length(Q) * length(period)), times = length(p) * length(d) * length(q) * length(P)),
                      Q = rep(rep(Q, each = length(period)), times = length(p) * length(d) * length(q) * length(P) * length(D)),
                      period = rep(period, times = length(p) * length(d) * length(q) * length(P) * length(D) * length(Q)))
  param$cri <- vector(length = nrow(param))

  # Set for parallel computing
  if (verbose) message("Setting up for parallel computing...")
  core <- detectCores()
  options(warn = -1)
  param$ref <- factor(seq_len(core) + vector(length = nrow(param)))
  options(warn = 0)
  param <- with(param, split(param, ref))
  prog <- function(n, tag) message(sprintf("  Task %d of %d finished (from worker %d)", n, length(param), tag))
  cl <- makeCluster(detectCores())
  registerDoSNOW(cl)

  if (verbose) {
    message("Start fitting...")
    tune <- foreach(i = seq_len(length(param)), .combine = list, .multicombine = TRUE, .packages = "forecast",
                    .options.snow = list(progress = prog)) %dopar% {
                      lapply(seq_len(nrow(param[[i]])),
                             function(j) {
                               tryCatch(Arima(data, order = as.numeric(param[[i]][j,1:3]),
                                              seasonal = list(order = as.numeric(param[[i]][j,4:6]),
                                                              period = param[[i]]$period[j]), ...),
                                        error = function(e) NULL)
                             })
                    }
  } else {
    tune <- foreach(i = seq_len(length(param)), .combine = list, .multicombine = TRUE, .packages = "forecast",) %dopar% {
      lapply(seq_len(nrow(param[[i]])),
             function(j) {
               tryCatch(Arima(data, order = as.numeric(param[[i]][j,1:3]),
                              seasonal = list(order = as.numeric(param[[i]][j,4:6]),
                                              period = param[[i]]$period[j]), ...),
                        error = function(e) NULL)
             })
    }
  }

  stopCluster(cl)

  # Merging
  if (verbose) message("Merging...")
  if (criterion == "aic") {
    for (i in seq_len(length(param))) {
      for (j in seq_len(nrow(param[[i]]))) {
        param[[i]]$cri[j] <- ifelse(is.null(tune[[i]][[j]]), NA, tune[[i]][[j]]$aic)
      }
    }
  } else if (criterion == "aicc") {
    for (i in seq_len(length(param))) {
      for (j in seq_len(nrow(param[[i]]))) {
        param[[i]]$cri[j] <- ifelse(is.null(tune[[i]][[j]]), NA, tune[[i]][[j]]$aicc)
      }
    }
  } else if (criterion == "bic") {
    for (i in seq_len(length(param))) {
      for (j in seq_len(nrow(param[[i]]))) {
        param[[i]]$cri[j] <- ifelse(is.null(tune[[i]][[j]]), NA, tune[[i]][[j]]$bic)
      }
    }
  }

  param <- na.omit(Reduce(rbind, param))
  models <- unlist(tune, recursive = FALSE)
  models <- Filter(Negate(is.null), models)

  if (nrow(param) == 0) {
    stop("no valid model is fitted")
  }

  # Model selection
  if (verbose) message("Selecting best model...")
  models <- models[order(param$cri)]
  param <- param[order(param$cri),-9]
  rownames(param) <- seq_len(nrow(param))
  select <- as.numeric(param[1,-8])
  names(select) <- colnames(param)[1:7]

  if (verbose) message("Plotting...")
  if (plot) {
    plt <- ggplot(data = NULL, aes(x = seq_along(param$cri), y = param$cri, color = "A")) +
      geom_point() +
      geom_line(alpha = 0.5) +
      labs(title = "Model selection result", subtitle = paste("Using", toupper(criterion), "as criterion"),
           x = "Model index", y = toupper(criterion)) +
      theme_minimal() +
      theme(legend.position = "None") +
      theme(plot.title = element_text(face = 'bold'))
    plot(plt)
  }

  # Report
  cat("\n\tModel selection procedure\n\n")
  cat(sprintf("data          : %s\n", dname))
  cat(sprintf("crietrion     : %s\n", criterion))
  cat(sprintf("achieved min  : %.4g\n", param$cri[1]))
  cat(sprintf("selected model: %s\n", models[[1]]))

  # Return test results and plots if exist
  if (plot) {
    res <- list(min = param$cri[1], select = models[[1]], tune = param, models = models, plot = plt)
  } else {
    res <- list(min = param$cri[1], select = models[[1]], tune = param, models = models)
  }

  return(invisible(res))
}

#' Future forecasting procedure
#'
#' Take fitted ARIMA model and forecast future values.
#' If the realization of future values are given, it also computes prediction diagnostics.
#' For prediction, it supports two methods:
#' One-shot prediction predicts all future values only using the given model.
#' One-step prediction predicts one step ahead and refit the model with the realized future value for the next prediction.
#' As noticed, for one-step prediction user must provide realized future values.
#' @param fit Fitted ARIMA model(\code{ARIMA} object) which will be used for forecasting.
#' @param ndays The number of days to be forecasted. (default = 30)
#' @param method Method of prediction.
#' Should be one of \code{"one-shot"} or \code{"one-step"}. (default = "one-shot")
#' @param ... Extra parameters for SARIMA fitting.
#' Refer to \code{\link{Arima}}.
#' @param real Realizations of future values.
#' This parameter is mandatory for one-step forecasting, while it is only optional for one-shot forecasting.
#' Also, for one-step forecasting, the number of realized future values must be greater than \code{ndays}.
#' It should be either \code{ts} object or \code{numeric} vector. (default = NULL)
#' @param plot If \code{TRUE}, the predicted future values will be plotted with 95\% and 80\% CI,
#' some of the past observations, and future realizations if given.
#' Otherwise, not. (default = TRUE)
#'
#' @author Sanghyun Park, Daun Jeong, and Sehun Kim
#'
#' @return A list containing the following elements:\tabular{ll}{
#'   \code{mean} \tab Predicted future values.\cr
#'   \tab\cr
#'   \code{CI} \tab Data frame holding lower and upper bounds of 80\% and 95\% CI.\cr
#'   \tab\cr
#'   \code{statistics} \tab A numeric vector holding prediction diagnostics,
#'   RMSE(Root Mean Square Error) and MAE(Mean Absolute Error).
#'   If the number of predicted future values and the number of given future realizations differ,
#'   it just ignores extra information in the longer one. (Returned only if \code{real} is given.)\cr
#'   \tab\cr
#'   \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
#' }
#'
#' @examples
#' # 228 is the station code for SNU
#' # We want to predict the last 100 total count of passengers
#' data <- get.subway(228)
#' obs <- data$total[1:(nrow(data) - 100)]
#' real <- data$total[(nrow(data) - 99):nrow(data)]
#'
#' # Fit SARIMA model
#' fit <- auto.arima(ts(obs, frequency = 7))
#'
#' # One-shot prediction
#' future(fit, ndays = 100, method = "one-shot", real = real)
#'
#' # One-step prediction
#' future(fit, ndays = 100, method = "one-step", real = real)
future <- function(fit, ndays = 30, method = c("one-shot", "one-step"), ..., real = NULL, plot = TRUE) {
  method <- match.arg(method)
  model <- sprintf("%s", fit)

  # Get ready
  if (is.null(real) && method == "one-step") {
    stop("one-step prediction requires future realizations")
  } else if (length(real) < ndays && method == "one-step") {
    stop("the number of realized values must be greater than ndays")
  }

  if (!is.null(real)) {
    real <- as.numeric(real)
  }

  # Forecasting
  if (method == "one-shot") { # One shot prediction
    pred <- forecast(fit, h = ndays)
    obs <- fit$x
    lwr <- pred$lower
    upr <- pred$upper
    mu <- pred$mean
    CI <- data.frame(lower.80 = as.numeric(lwr[,1]), upper.80 = as.numeric(upr[,1]),
                     lower.95 = as.numeric(lwr[,2]), upper.95 = as.numeric(upr[,2]))
  } else if (method == "one-step") {  # One step prediction
    order <- fit$arma
    obs <- fit$x
    lwr <- matrix(nrow = ndays, ncol = 2)
    upr <- matrix(nrow = ndays, ncol = 2)
    mu <- vector(length = ndays)
    pred <- forecast(fit, h = 1)
    lwr[1,] <- pred$lower[1,]
    upr[1,] <- pred$upper[1,]
    mu[1] <- pred$mean[1]

    for (i in seq_len(ndays - 1)) {
      fit <- Arima(c(obs, real[seq_len(i)]), order = c(order[1], order[6], order[2]),
                   seasonal = list(order = c(order[3], order[7], order[4]), period = order[5]))
      pred <- forecast(fit, h = 1)
      lwr[i + 1,] <- pred$lower[1,]
      upr[i + 1,] <- pred$upper[1,]
      mu[i + 1] <- pred$mean[1]
      CI <- data.frame(lower.80 = as.numeric(lwr[,1]), upper.80 = as.numeric(upr[,1]),
                       lower.95 = as.numeric(lwr[,2]), upper.95 = as.numeric(upr[,2]))
    }
  }

  if (plot) {
    method <- ifelse(method == "one-shot", "One-shot", "One-step")
    N.obs <- length(obs)
    N.real <- min(length(real), ndays)
    obs.start <- max(1, N.obs - 2 * ndays + 1)

    if (!is.null(real)) {
      options(warn = -1)
      plt <- data.frame(x = c(seq_along(obs)[obs.start:N.obs], N.obs, seq_along(mu) + N.obs),
                        y = c(obs[obs.start:N.obs], obs[N.obs], mu),
                        ymin1 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(lwr[,2])),
                        ymax1 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(upr[,2])),
                        ymin2 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(lwr[,1])),
                        ymax2 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(upr[,1])),
                        type = c(rep("obs", times = N.obs - obs.start + 1), rep("pred", times = ndays + 1))) %>%
        ggplot(aes(x = x, y = y)) +
        geom_ribbon(aes(ymin = ymin1, ymax = ymax1, fill = type), alpha = 0.1) +
        geom_ribbon(aes(ymin = ymin2, ymax = ymax2, fill = type), alpha = 0.2) +
        geom_line(data = data.frame(x = seq_len(N.real + 1) + N.obs - 1, y = c(obs[N.obs], real[1:N.real])),
                  aes(x = x, y = y), color = "black", alpha = 0.4) +
        geom_line(aes(color = type)) +
        labs(title = paste(method, "forecasting result"), subtitle = paste("Model", model), x = "Index", y = "") +
        theme_minimal() +
        theme(legend.position = "None") +
        theme(plot.title = element_text(face = 'bold'))
      options(warn = 0)
    } else {
      options(warn = -1)
      plt <- data.frame(x = c(seq_along(obs)[obs.start:N.obs], N.obs, seq_along(mu) + N.obs),
                        y = c(obs[obs.start:N.obs], obs[N.obs], mu),
                        ymin1 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(lwr[,2])),
                        ymax1 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(upr[,2])),
                        ymin2 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(lwr[,1])),
                        ymax2 = c(rep(NA, times = N.obs - obs.start + 1), obs[N.obs], as.numeric(upr[,1])),
                        type = c(rep("obs", times = N.obs - obs.start + 1), rep("pred", times = ndays + 1))) %>%
        ggplot(aes(x = x, y = y)) +
        geom_ribbon(aes(ymin = ymin1, ymax = ymax1, fill = type), alpha = 0.1) +
        geom_ribbon(aes(ymin = ymin2, ymax = ymax2, fill = type), alpha = 0.2) +
        geom_line(aes(color = type)) +
        labs(title = paste(method, "forecasting result"), subtitle = paste("Model", model), x = "Index", y = "") +
        theme_minimal() +
        theme(legend.position = "None") +
        theme(plot.title = element_text(face = 'bold'))
      options(warn = 0)
    }

    options(warn = -1)
    plot(plt)
    options(warn = 0)
  }

  # Compute statistics
  if (!is.null(real)) {
    real.trim <- real[1:min(length(real), length(mu))]
    mu.trim <- mu[1:min(length(real), length(mu))]
    rmse <- sqrt(mean((real.trim - mu.trim) ** 2))
    mae <- mean(abs(real.trim - mu.trim))
    stat <- c(rmse, mae)
    names(stat) <- c("RMSE", "MAE")
  }

  # Report
  cat("\n\tFuture forecasting procedure\n\n")
  cat(sprintf("model : %s\n", model))
  cat(sprintf("method: %s\n", tolower(method)))

  if (!is.null(real)) {
    cat(sprintf("RMSE  : %.4g\n", rmse))
    cat(sprintf("MAE   : %.4g\n", mae))
  }

  # Return test results and plots if exist


  if (plot) {
    if (!is.null(real)) {
      res <- list(mean = mu, CI = CI, statistics = stat, plot = plt)
    } else {
      res <- list(mean = mu, CI = CI, plot = plt)
    }
  } else {
    if (!is.null(real)) {
      res <- list(mean = mu, CI = CI, statistics = stat)
    } else {
      res <- list(mean = mu, CI = CI)
    }
  }

  return(invisible(res))
}
