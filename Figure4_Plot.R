

library(tidyverse)
library(patchwork)
library(scales)

if (Sys.info()[['sysname']] == "Windows") {
  windowsFonts(Arial = windowsFont("Times New Roman"))
  my_font_family <- "Times"
} else {
  my_font_family <- "Times" 
}


df_patent_gender <- read_csv("patent_gender.csv", show_col_types = FALSE) %>%
  rename(
    frac_hype_words = `_at1`,
    group_var = `_m1`,
    AME = `_margin`,
    conf.low = `_ci_lb`,
    conf.high = `_ci_ub`
  ) %>%
  mutate(
    gender_group = factor(group_var, levels = c(2), labels = c("Female"))
  ) %>%
  filter(group_var == 2) 

plot_patent_gender <- ggplot(df_patent_gender, aes(x = frac_hype_words, y = -AME)) +
  geom_ribbon(aes(ymin = -conf.low, ymax = -conf.high, fill = gender_group), alpha = 0.4) +
  geom_line(aes(color = gender_group), linewidth = 1.2) +
  geom_point(aes(color = gender_group), size = 3.0, shape = 16) +
  labs(
    x = "Percentage of promotional words",
    y = "Pred. prob. of patentability (Woman-Man)"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  annotate("text", x = -Inf, y = Inf, label = "A", size = 9, hjust = -1, vjust = 1.5) +
  coord_cartesian(ylim = c(-0.0279, -0.0173)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_color_brewer(palette = "Set2", guide = "none") +   
  scale_fill_brewer(palette = "Set2", guide = "none") +    
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = my_font_family),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    aspect.ratio = 1
  )

df_patent_prior <- read_csv("patent_prior_V2.csv", show_col_types = FALSE) %>%
  rename(
    frac_hype_words = `_at1`,
    AME = `_margin`,
    conf.low = `_ci_lb`,    
    conf.high = `_ci_ub`,   
    group_var = `_m1`
  ) %>%
  mutate(
    group_var = factor(group_var, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Q1",
                                  "Num. of Apps Reviewed by Examiner: Q2",
                                  "Num. of Apps Reviewed by Examiner: Q3",
                                  "Num. of Apps Reviewed by Examiner: Q4",
                                  "Num. of Apps Reviewed by Examiner: Q5"))
  )
plot_patent_prior <- ggplot(df_patent_prior, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group_var, group = group_var), alpha = 0.25) +
  geom_line(aes(color = group_var, group = group_var), linewidth = 1.1) +
  geom_point(aes(color = group_var), size = 5, shape = 16) +
  labs(
    x = "Percentage of promotional words",
    y = "Pred. prob. of patentability (vs. Examiner Q1)"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5), 
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  annotate("text", x = -Inf, y = Inf, label = "B", size = 9, hjust = -1, vjust = 1.5) +
  coord_cartesian(ylim = c(0.15, 0.49)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = my_font_family), 
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    
    # 图例设置 (保留您原来的精细调整)
    legend.position = c(0.27, 0.98), 
    legend.justification = c(0, 1),    
    legend.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = "solid"),
    legend.margin = margin(5, 5, 5, 5), 
    legend.key = element_rect(fill = "white", color = NA), 
    legend.key.size = unit(0.8, "lines"), 
    legend.spacing.y = unit(0.3, "cm"), 
    legend.text = element_text(size = 12, margin = margin(r = 5)), 
    legend.title = element_blank(),
    aspect.ratio = 1
  )


combined_plot <- (plot_patent_gender + plot_patent_prior) +
  plot_layout(ncol = 2) 

ggsave(
  filename = "figure4.pdf",
  plot = combined_plot,
  width = 12,    # 12英寸宽，适合双栏全宽图
  height = 6,    # 2:1 比例
  device = cairo_pdf, 
  family = my_font_family,
  units = "in"
)

ggsave(
  filename = "figure4.png",
  plot = combined_plot,
  width = 12, 
  height = 6, 
  dpi = 300, 
  bg = "white"
)


