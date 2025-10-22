#' If Hz histogram is not normal, this use Boxcox transformation
#' @param df tidy_df
#' @param col_name name of column to transform
#' @importFrom MASS boxcox
#' @return df with transformed Hz column
#' @export




hz_transform <- function(df, col_name) {
  tran_lm <- boxcox(df$col_name ~ 1, lambda = seq(-2, 2, 0.1), plotit = FALSE)
  lambda <- tran_lm$x[which.max(tran_lm$y)]
  if (abs(lambda) < 1e-6) {
    df$trans <- log(df$col_name)
  } else {
    df$trans <- (df$col_name^lambda - 1) / lambda
  }
  return(df)
}
