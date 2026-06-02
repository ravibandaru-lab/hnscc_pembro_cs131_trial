library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(cowplot)

setwd("/Users/ravibandaru/Desktop/hnscc_pembro_cs131_trial/Main_Figures/F1")
df <- read.csv("../../Data/Cleaned_Survival_HNSCC_CP.csv", stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    DFS_months = Disease.Free.Months,
    OS_months  = Overall.Months,
    DFS_event  = as.integer(dfs_event),
    OS_event   = as.integer(os_event)
  ) %>%
  mutate(DFS_months = ifelse(is.na(DFS_months), OS_months, DFS_months))

surv_dfs <- survfit(Surv(DFS_months, DFS_event) ~ 1, data = df)
surv_os  <- survfit(Surv(OS_months,  OS_event)  ~ 1, data = df)

XLIM    <- 60
XBREAKS <- c(0, 12, 24, 36, 48, 60)
PAL     <- "#D8B085"

prism_theme <- theme_classic(base_size = 10) +
  theme(
    plot.title        = element_blank(),
    axis.title.x      = element_text(size = 9, margin = margin(t = 4)),
    axis.title.y      = element_text(size = 9, margin = margin(r = 4)),
    axis.text         = element_text(size = 8, colour = "black"),
    axis.line         = element_line(colour = "black", linewidth = 0.5),
    axis.ticks        = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    legend.position   = "none",
    plot.tag          = element_text(size = 11, face = "bold"),
    plot.tag.position = "topleft",
    plot.margin       = margin(8, 14, 4, 6)
  )

shared_x <- scale_x_continuous(
  limits = c(0, XLIM),
  breaks = XBREAKS,
  expand = c(0.01, 0)
)

# Custom risk table — all three rows (title, numbers, x-axis) are geom/annotate
# inside one panel so spacing is fully explicit with no plot.title margin issues
make_risk_table <- function(fit) {
  s      <- summary(fit, times = XBREAKS, extend = TRUE)
  n_risk <- s$n.risk
  if (length(n_risk) < length(XBREAKS))
    n_risk <- c(n_risk, rep(0, length(XBREAKS) - length(n_risk)))

  df_risk <- data.frame(time = XBREAKS, n = n_risk)

  ggplot(df_risk, aes(x = time, y = 0.5, label = n)) +
    geom_text(colour = PAL, size = 3.0) +
    shared_x +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(title = "Number at risk",
         x     = "Time from surgery (months)",
         y     = NULL) +
    theme_classic(base_size = 10) +
    theme(
      plot.title   = element_text(size = 8.5, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, b = 2)),
      axis.text.x  = element_text(size = 8, colour = "black"),
      axis.title.x = element_text(size = 9, margin = margin(t = 2)),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x  = element_blank(),
      panel.grid   = element_blank(),
      plot.margin  = margin(10, 14, 2, 6)
    )
}

print_2yr <- function(fit, label) {
  s <- summary(fit, times = 24, extend = TRUE)
  cat("\n---", label, "---\n")
  cat("  2-year estimate:", round(s$surv * 100, 1), "%\n")
  cat("  95% CI:        ", round(s$lower * 100, 1), "% -",
      round(s$upper * 100, 1), "%\n")
}

make_km <- function(fit, ylab, panel_tag) {
  gsp <- ggsurvplot(
    fit,
    data             = df,
    palette          = PAL,
    xlim             = c(0, XLIM),
    break.time.by    = 12,
    ylim             = c(0, 1.05),
    surv.scale       = "percent",
    xlab             = "Time from surgery (months)",
    ylab             = ylab,
    censor           = TRUE,
    censor.shape     = "|",
    censor.size      = 3,
    conf.int         = FALSE,
    size             = 0.75,
    legend           = "none",
    risk.table       = FALSE,
    surv.median.line = "none",
    axes.offset      = FALSE,
    ggtheme          = prism_theme
  )

  gsp$plot <- gsp$plot + labs(tag = panel_tag) + shared_x
  gsp$plot
}

km_dfs <- make_km(surv_dfs, ylab = "Disease-Free Survival (%)", panel_tag = "A")
km_os  <- make_km(surv_os,  ylab = "Overall Survival (%)",      panel_tag = "B")

rt_dfs <- make_risk_table(surv_dfs)
rt_os  <- make_risk_table(surv_os)

print_2yr(surv_dfs, "Disease-Free Survival")
print_2yr(surv_os,  "Overall Survival")

# Sync grob widths so panels share identical left/right margins
sync_widths <- function(main_gg, table_gg) {
  g_main        <- ggplotGrob(main_gg)
  g_table       <- ggplotGrob(table_gg)
  g_table$widths <- g_main$widths
  g_table
}

gt_dfs <- sync_widths(km_dfs, rt_dfs)
gt_os  <- sync_widths(km_os,  rt_os)

col_dfs <- plot_grid(km_dfs, gt_dfs, ncol = 1, rel_heights = c(5, 1))
col_os  <- plot_grid(km_os,  gt_os,  ncol = 1, rel_heights = c(5, 1))

combined <- plot_grid(col_dfs, col_os, ncol = 2)

ggsave("F1.tiff",
       plot        = combined,
       width       = 8,
       height      = 5,
       dpi         = 600,
       compression = "lzw")

ggsave("F1.pdf",
       plot        = combined,
       width       = 8,
       height      = 5,
       useDingbats = FALSE)
