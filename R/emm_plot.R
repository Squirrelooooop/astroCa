#' Plot results from linear mixed effects model with emmeans
#' @param model linear mixed effects model
#' @param df Dataframe used to fit the model
#' @import emmeans
#' @import ggplot2
#' @return plot with x axis as recording or ultrasound, y axis as response variable
#' @export


emm_plot_recording <- function(model, df, y_var) {
  emm_dff <- emmeans(model, ~ treatment * ultrasound)
  emm_dff <- as.data.frame(emm_dff)
  ggplot(df, aes(x = treatment, y = {{y_var}}, color = ultrasound)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 0.5) +
    geom_point(data = emm_dff, aes(x = treatment, y = emmean, fill = ultrasound), shape = 18, size = 2, position = position_dodge(width = 0.9), inherit.aes = FALSE) +
    geom_errorbar(data = emm_dff, aes(x= treatment, y = emmean, ymin = lower.CL, ymax = upper.CL, fill = ultrasound), width = 0.2,
                  position = position_dodge(width = 0.9), alpha = 0.5, inherit.aes = FALSE) +
    theme_pubr(legend = "none")
}

emm_plot_ultrasound <- function(model, df, y_var) {
  emm_dff <- emmeans(model, ~ treatment * ultrasound)
  emm_dff <- as.data.frame(emm_dff)
  ggplot(df, aes(x = ultrasound, y = {{y_var}}, color = treatment)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), alpha = 0.5) +
    geom_point(data = emm_dff, aes(x = ultrasound, y = emmean, fill = treatment), shape = 23, size = 2, position = position_dodge(width = 0.9), inherit.aes = FALSE) +
    geom_errorbar(data = emm_dff, aes(x= ultrasound, y = emmean, ymin = lower.CL, ymax = upper.CL, fill = treatment), width = 0.2,
                  position = position_dodge(width = 0.9), alpha = 0.5, inherit.aes = FALSE) +
    theme_pubr(legend = "right")
}
