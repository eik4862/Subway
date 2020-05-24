sign.code <- function(x) {
  return(ifelse(x < 0.001, "***", ifelse(x < 0.01, "**", ifelse(x < 0.05, "*", ifelse(x < 0.1, ".", "")))))
}
