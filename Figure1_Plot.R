library(ggplot2)
library(ggpubr)
library(dplyr)
library(patchwork)
library(readr)

setwd("D:/Work/pwords")

plot_theme <- theme_bw(base_size = 7, base_family = "sans") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),

    axis.text = element_text(size = 6, color = "black"),
    axis.title = element_text(size = 6),
    
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt"), 
    
    axis.title.x = element_text(margin = margin(t = 2)),
    axis.title.y = element_text(margin = margin(r = 2)),
    
    aspect.ratio = 1
  )
# ==============================================================================
# 2.Data Preparation
# ==============================================================================

# --- Row 1 Data ---
# --- Row 2 Data ---
df_patent_m <- read_csv("patent_margins.csv") %>% 
  rename(frac_hype_words = `_at1`, AME = `_margin`, conf.low = `_ci_lb`, conf.high = `_ci_ub`)

df_transfer_m <- read_csv("margins_transfered.csv") %>% 
  rename(frac_hype_words = `_at1`, AME = `_margin`, conf.low = `_ci_lb`, conf.high = `_ci_ub`)

df_appeal_m <- read_csv("margins_appeal.csv") %>% 
  rename(frac_hype_words = `_at1`, AME = `_margin`, conf.low = `_ci_lb`, conf.high = `_ci_ub`)

# --- Row 3 Data (Manually Created) ---
patent_data_r3 <- data.frame(
  comparison = c("Q1", "Q2", "Q3", "Q4"),
  estimate_raw = c(-0.05564539, -0.05847013, -0.05073620, -0.03463536),
  conf.low_raw = c(-0.05739582, -0.06021828, -0.05249052, -0.03640169),
  conf.high_raw = c(-0.05389496, -0.05672198, -0.04898188, -0.03286903)
) %>%
  mutate(
    estimate = estimate_raw * -1,
    conf.low = conf.high_raw * -1,
    conf.high = conf.low_raw * -1,
    comparison = factor(comparison, levels = c("Q1", "Q2", "Q3", "Q4"))
  )

transfer_data_r3 <- data.frame(
  comparison = c("Q1", "Q2", "Q3", "Q4"),
  estimate = c(0.05930136, 0.04635267, 0.04049947, 0.03083205),
  conf.low = c(0.05804454, 0.04506792, 0.03920260, 0.02951580),
  conf.high = c(0.06055819, 0.04763741, 0.04179633, 0.03214830)
) %>% mutate(comparison = factor(comparison, levels = c("Q1", "Q2", "Q3", "Q4")))

appeal_data_r3 <- data.frame(
  comparison = c("Q1", "Q2", "Q3", "Q4"),
  estimate = c(0.05333941, 0.04422156, 0.02165489, 0.00661044),
  conf.low = c(0.033458390, 0.024393597, 0.001974756, -0.012957857),
  conf.high = c(0.07322043, 0.06404953, 0.04133502, 0.02617874)
) %>% mutate(comparison = factor(comparison, levels = c("Q1", "Q2", "Q3", "Q4")))



sa_theme_6pt <- theme_bw(base_size = 7, base_family = "sans") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.text = element_text(size = 6, color = "black"),
    axis.title = element_text(size = 6),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.title.y = element_text(margin = margin(r = 3)),
    aspect.ratio = 1
  )

df_grant <- read_csv("plot_patent_EDA.csv", show_col_types = FALSE)
df_grant$section <- substr(df_grant$cpc_class, 1, 1)
df_grant$mean_hype <- df_grant$mean_hype * 100

df_trans <- read_csv("plot_transfer_EDA.csv", show_col_types = FALSE)
df_trans$section <- substr(df_trans$cpc_class, 1, 1)
df_trans$mean_hype <- df_trans$mean_hype * 100

df_appeal <- read_csv("plot_appeal_EDA.csv", show_col_types = FALSE)
df_appeal$section <- substr(df_appeal$cpc_class, 1, 1)
df_appeal$mean_hype <- df_appeal$mean_hype * 100


get_weighted_label <- function(df, x_var, y_var, weight_var) {
  cov_res <- cov.wt(df[, c(x_var, y_var)], wt = df[[weight_var]], cor = TRUE)
  r_val <- round(cov_res$cor[1, 2], 2)
  n_eff <- sum(df[[weight_var]]) 
  t_stat <- r_val * sqrt((n_eff - 2) / (1 - r_val^2))
  p_val <- 2 * pt(abs(t_stat), df = n_eff - 2, lower.tail = FALSE)
  
  p_text <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3)))
  return(paste0("r = ", r_val, ", ", p_text))
}

label_grant <- get_weighted_label(df_grant, "mean_hype", "grant_rate", "n_patents")
label_trans <- get_weighted_label(df_trans, "mean_hype", "grant_rate", "n_patents")
label_appeal <- get_weighted_label(df_appeal, "mean_hype", "grant_rate", "n_patents")

common_x_limits <- c(0.091, 0.259) 
common_x_breaks <- seq(0.10, 0.25, 0.05)
no_margin <- c(0, 0) 

# --- Panel A (P1) ---
p1 <- ggplot(df_grant, aes(x = mean_hype, y = grant_rate)) + 
  geom_smooth(aes(weight = n_patents), method = "lm", color = "grey40", fill = "grey90", size = 0.6, alpha = 1) +
  geom_point(aes(color = section, size = n_patents), alpha = 1) + 
 
  annotate("text", x = 0.1, y = 0.40, label = label_grant, size = 2.4, hjust = 0, color = "black") +
  annotate("text", x = -Inf, y = Inf, label = "A", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(breaks = common_x_breaks, expand = no_margin) +

  scale_y_continuous(breaks = seq(0.4, 0.9, 0.1), expand = no_margin) +
  coord_cartesian(xlim = common_x_limits, ylim = c(0.35, 0.92)) +
  
  labs(x = "Average percentage of promotional words", y = "Average prob. a patent app. is granted") +
  scale_color_brewer(palette = "Set1") +
  scale_size_continuous(range = c(0.5, 5), guide = "none") + 
  sa_theme_6pt +
  theme(legend.position = "none")


# --- Panel B ---
p2 <- ggplot(df_trans, aes(x = mean_hype, y = grant_rate)) + 
  geom_smooth(aes(weight = n_patents), method = "lm", color = "grey40", fill = "grey90", size = 0.6, alpha = 1) +
  geom_point(aes(color = section, size = n_patents), alpha = 1) +
  
  annotate("text", x = 0.1, y = 0.60, label = label_trans, size = 2.4, hjust = 0, color = "black") + 
  annotate("text", x = -Inf, y = Inf, label = "B", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(breaks = common_x_breaks, expand = no_margin) +
  scale_y_continuous(breaks = seq(0.6, 1.0, 0.1), expand = no_margin) +
  coord_cartesian(xlim = common_x_limits, ylim = c(0.56, 1.02)) +
  
  labs(x = "Average percentage of promotional words", y = "Average prob. of ownership transfer") +
  scale_color_brewer(palette = "Set1") +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  sa_theme_6pt +
  theme(legend.position = "none")


# --- Panel C ---
p3 <- ggplot(df_appeal, aes(x = mean_hype, y = grant_rate)) + 
  geom_smooth(aes(weight = n_patents), method = "lm", color = "grey40", fill = "grey90", size = 0.6, alpha = 1) +
  geom_point(aes(color = section, size = n_patents), alpha = 1) +
  
  annotate("text", x = 0.1, y = 0.10, label = label_appeal, size = 2.4, hjust = 0, color = "black") + 
  annotate("text", x = -Inf, y = Inf, label = "C", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(breaks = common_x_breaks, expand = no_margin) +
  scale_y_continuous(breaks = seq(0.1, 0.7, 0.1), expand = no_margin) +
  coord_cartesian(xlim = common_x_limits, ylim = c(0.04, 0.72)) +
  labs( x = "Average percentage of promotional words", y = "Average prob. of appeal success", color = NULL) +
  scale_color_brewer(palette = "Set1", labels = function(x) paste("CPC Sec.", x)) + 
  scale_size_continuous(range = c(0.5, 5), guide = "none") + 
  sa_theme_6pt + 
  
  theme(
    legend.position = c(0.97, 0.97), 
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.2),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(2, "mm"), 
    legend.text = element_text(size = 5), 
    legend.spacing.y = unit(0.5, "mm")
  ) +
  guides(color = guide_legend(ncol = 2))


# Panel D
p4 <- ggplot(df_patent_m, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.25) +
  geom_line(color = "#1f77b4", size = 0.6) + 
  geom_point(size = 1, color = "#1f77b4") +
  annotate("text", x = -Inf, y = Inf, label = "D", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(labels = function(x) x*100) +
  labs( x = "Percentage of promotional words", y = "Predicate prob. a patent app. is granted") +
  coord_cartesian(ylim = c(0.587, 0.615)) +
  plot_theme

# Panel E
p5 <- ggplot(df_transfer_m, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.25) +
  geom_line(color = "#1f77b4", size = 0.6) + 
  geom_point(size = 1, color = "#1f77b4") +
  annotate("text", x = -Inf, y = Inf, label = "E", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(labels = function(x) x*100) +
  labs(x = "Percentage of promotional words", y = "Predicate prob. of ownership transfer") +
  coord_cartesian(ylim = c(0.687, 0.721)) +
  plot_theme

# Panel F
p6 <- ggplot(df_appeal_m, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.25) +
  geom_line(color = "#1f77b4", size = 0.6) + 
  geom_point(size = 1, color = "#1f77b4") +
  annotate("text", x = -Inf, y = Inf, label = "F", size = 4, hjust = -1, vjust = 1.5) +
  scale_x_continuous(labels = function(x) x*100) +
  labs(x = "Percentage of promotional words", y = "Predicate prob. of appeal success") +
  coord_cartesian(ylim = c(0.305, 0.371)) +
  plot_theme


# Panel G
p7 <- ggplot(patent_data_r3, aes(x = comparison, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 0.4, color = "darkgreen", alpha = 0.8) +
  geom_line(aes(group = 1), color = "darkgreen", linetype = "solid", size = 0.4) +
  geom_point(size = 1.2, color = "darkgreen") +
  labs(
    y = "Diff. in prob. a patent app. is granted (vs. Q5)", 
    x = "Percentage of promotional words (Quintile)"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "G", size = 4, hjust = -1, vjust = 1.5) +
  coord_cartesian(ylim = c(0.03, 0.062)) +
  scale_y_continuous(breaks = seq(0.03, 0.06, 0.01)) +
  plot_theme

# Panel H
p8 <- ggplot(transfer_data_r3, aes(x = comparison, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 0.4, color = "darkgreen", alpha = 0.8) +
  geom_line(aes(group = 1), color = "darkgreen", linetype = "solid", size = 0.4) +
  geom_point(size = 1.2, color = "darkgreen") +
  labs(
    y = "Diff. in prob. of ownership transfer (vs. Q5)", 
    x = "Percentage of promotional words (Quintile)"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "H", size = 4, hjust = -1, vjust = 1.5) +
  coord_cartesian(ylim = c(0.025, 0.066)) +
  scale_y_continuous(breaks = seq(0.02, 0.06, 0.01)) +
  plot_theme

# Panel I
p9 <- ggplot(appeal_data_r3, aes(x = comparison, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 0.4, color = "darkgreen", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.3) +
  geom_line(aes(group = 1), color = "darkgreen", linetype = "solid", size = 0.4) +
  geom_point(size = 1.2, color = "darkgreen") +
  labs(
    y = "Diff. in prob. of appeal success (vs. Q5)", 
    x = "Percentage of promotional words (Quintile)"
  ) +
  annotate("text", x = -Inf, y = Inf, label = "I", size = 4, hjust = -2, vjust = 1.5) +
  coord_cartesian(ylim = c(-0.02, 0.083)) +
  scale_y_continuous(breaks = seq(-0.02, 0.08, 0.02)) +
  plot_theme

final_plot <- (p1 + p2 + p3) / 
  (p4 + p5 + p6) / 
  (p7 + p8 + p9) +
  plot_layout(heights = c(1, 1, 1)) & 
  theme(
    plot.margin = margin(2, 2, 2, 2) 
  )

ggsave(
  filename = "figure1.pdf", 
  plot = final_plot, 
  width = 16, 
  height = 16, 
  units = "cm", 
  dpi = 300,
  device = cairo_pdf
)


ggsave(
  filename = "Figure1.png", 
  plot = final_plot, 
  width = 16, 
  height = 16, 
  units = "cm", 
  dpi = 300,
  bg = "white"
)