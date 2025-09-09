#' If Hz histogram is not normal, this use Boxcox transformation
#' @param hz_model tidy_df
#' @param df tidy_df
#' @importFrom fBasics boxcox
#' @return df with transformed Hz column
#' @export




hz_transform <- function(df) {
  hz_model <- lm(Hz ~ treatment * ultrasound, data = df)
  c <- abs(min(hz_model$residuals)) + 1
  residuals_p <- hz_model$residuals + c
  tran_lm <- fBasics::boxcox(residuals_p ~ 1, lambda = seq(-2, 2, 0.1), plotit = FALSE)
  lambda <- tran_lm$x[which.max(tran_lm$y)]
  if (abs(lambda) < 1e-6) {
    df$hz_trans <- log(df$Hz)
  } else {
    df$hz_trans <- (df$Hz^lambda - 1) / lambda
  }
  return(df)
}
