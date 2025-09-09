#' Get emmeans dataframe
#' @param model fitted linear mixed model
#' @return data frame of emmeans
#' @noRd
#' @importFrom emmeans emmeans
get_emm <- function(model) {
  as.data.frame(emmeans(model, ~ treatment * ultrasound))
}

#' Plot results from linear mixed effects model (recording on x-axis)
#' @param model linear mixed effects model
#' @param df Dataframe
#' @param y_var column to plot
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot geom_jitter geom_point geom_errorbar aes position_jitterdodge position_dodge
#' @importFrom ggpubr theme_pubr
emm_plot_recording <- function(model, df, y_var) {
  emm_dff <- get_emm(model)
  ggplot(df, aes(x = treatment, y = {{y_var}}, color = ultrasound)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 0.5) +
    geom_point(
      data = emm_dff,
      aes(x = treatment, y = emmean, fill = ultrasound),
      shape = 18, size = 2,
      position = position_dodge(width = 0.9),
      inherit.aes = FALSE
    ) +
    geom_errorbar(
      data = emm_dff,
      aes(x = treatment, y = emmean, ymin = lower.CL, ymax = upper.CL, fill = ultrasound),
      width = 0.2,
      position = position_dodge(width = 0.9),
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    theme_pubr(legend = "right")
}

#' Plot results from linear mixed effects model (ultrasound on x-axis)
#' @param model linear mixed effects model
#' @param df Dataframe
#' @param y_var column to plot
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot geom_jitter geom_point geom_errorbar aes position_jitterdodge position_dodge
#' @importFrom ggpubr theme_pubr
emm_plot_ultrasound <- function(model, df, y_var) {
  emm_dff <- get_emm(model)
  ggplot(df, aes(x = ultrasound, y = {{y_var}}, color = treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 0.5) +
    geom_point(
      data = emm_dff,
      aes(x = ultrasound, y = emmean, fill = treatment),
      shape = 23, size = 2,
      position = position_dodge(width = 0.9),
      inherit.aes = FALSE
    ) +
    geom_errorbar(
      data = emm_dff,
      aes(x = ultrasound, y = emmean, ymin = lower.CL, ymax = upper.CL, fill = treatment),
      width = 0.2,
      position = position_dodge(width = 0.9),
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    theme_pubr(legend = "right")
}
