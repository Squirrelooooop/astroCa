#' Tidy Excel data containing AQUA2 Max Dff
#' @param df Dataframe needed to be tidied
#' @param timeinsec Time in seconds for each recording
#' @return with cols for exp, coverslip, ultrasound, treatment, dFF


tidydata <- function(df, timeinsec) {
  df[,5][is.na(df[,5])] <- -9
  tidy_df <- df |>
    tidyr::pivot_longer(
                  cols = 5:ncol(df),
                  values_to = "dFF"
              ) |> tidyr::drop_na() |>
    tidyr::group_by(exp, coverslip, treatment, ultrasound) |>
    tidyr::summarise(
      mean_cadff = mean(dFF),
      number_active = n(),
      .groups = "drop"
    ) |>
    tidyr::mutate(
      Hz = number_active / timeinsec,
      mean_cadff = tidyr::if_else(mean_cadff == -9, 0, mean_cadff),
      number_active = tidyr::if_else(mean_cadff == 0, 0L, number_active),
      Hz = tidyr::if_else(mean_cadff == 0, 0, Hz)
    )
  tidy_df <- tidy_df[,-5]
  return(tidy_df)
}
