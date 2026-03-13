
library(tidyverse)
library(patchwork)
library(scales)


if (Sys.info()[['sysname']] == "Windows") {
  windowsFonts(Arial = windowsFont("Arial"))
  my_font_family <- "Arial"
} else {
  my_font_family <- "sans" 
}

# --- Plot A: Novelty ---
df_novelty <- read_csv("mar_novelty2.csv", show_col_types = FALSE)
plot_data_novelty <- df_novelty %>%
  rename(frac_hype_words = `_at1`, AME = `_margin`, conf.low = `_ci_lb`, conf.high = `_ci_ub`)

plot_novelty <- ggplot(plot_data_novelty, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.25) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3.0, shape = 16, color = "#1f77b4") +
  labs(
    x = "Percentage of promotional words",
    y = "Predicted percentile of novelty score"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = function(x) { paste0(scales::number(x * 100, accuracy = 0.1), "%") }
  ) +
  annotate("text", x = -Inf, y = Inf, label = "A", size = 9, hjust = -1, vjust = 1.5) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = my_font_family),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    aspect.ratio = 1
  )

# --- Plot B: Citations ---
df_citation_3 <- read_csv("citation_margins.csv", show_col_types = FALSE)
plot_data_citation_3 <- df_citation_3 %>%
  rename(frac_hype_words = `_at1`, AME = `_margin`, conf.low = `_ci_lb`, conf.high = `_ci_ub`)

plot_citation_3_scaled <- ggplot(plot_data_citation_3, aes(x = frac_hype_words, y = AME)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.25) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +
  geom_point(size = 3.0, shape = 16, color = "#1f77b4") +
  labs(
    x = "Percentage of promotional words",
    y = "Predicted percentile of citation in 3 years"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = function(x) { paste0(scales::number(x, accuracy = 0.1), "%") }
  ) +
  annotate("text", x = -Inf, y = Inf, label = "B", size = 9, hjust = -1, vjust = 1.5) +
  theme_bw(base_size = 16) +
  theme(
    text = element_text(family = my_font_family),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    aspect.ratio = 1
  )
  

combined_plot <- (plot_novelty + plot_citation_3_scaled) +
  plot_layout(ncol = 2)

ggsave(
  filename = "Figure3.pdf",
  plot = combined_plot,
  width = 12,
  height = 6,
  device = cairo_pdf,
  units = "in"
)


ggsave(
  filename = "Figure3.png",
  plot = combined_plot,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)
