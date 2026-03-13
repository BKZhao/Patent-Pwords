library(tidyverse)
library(scales) 
library(patchwork)

patent_path <- "D:/Work/pwords/margins_patent_cpc.csv"
patent_data <- read_csv(patent_path, show_col_types = FALSE)
plot_patent <- patent_data %>%
  rename(
    frac_hype_words = `_at1`,    
    predicted_value = `_margin`, 
    conf.low = `_ci_lb`,
    conf.high = `_ci_ub`,
    cpc_section = `_m1`       
  ) %>%
  mutate(
    cpc_section = factor(cpc_section,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                         labels = c("CPC Section A", "CPC Section B", "CPC Section C",
                                    "CPC Section D", "CPC Section E", "CPC Section F",
                                    "CPC Section G", "CPC Section H")) 
  )

transfered_path <- "D:/Work/pwords/transfered_cpc.csv"
transfered_data <- read_csv(transfered_path, show_col_types = FALSE)
plot_transfered <- transfered_data %>%
  rename(
    frac_hype_words = `_at1`,   
    predicted_value = `_margin`, 
    conf.low = `_ci_lb`,
    conf.high = `_ci_ub`,
    cpc_section = `_m1`         
  ) %>%
  mutate(
    cpc_section = factor(cpc_section,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                         labels = c("CPC Section A", "CPC Section B", "CPC Section C",
                                    "CPC Section D", "CPC Section E", "CPC Section F",
                                    "CPC Section G", "CPC Section H")) 
  )


plot_patent_cpc <- ggplot(plot_patent, aes(x = frac_hype_words, y = predicted_value)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cpc_section, group = cpc_section), alpha = 0.2) +
  geom_line(aes(color = cpc_section, group = cpc_section), linewidth = 1.1) +
  geom_point(aes(color = cpc_section), size = 2.5, shape = 16) +
  
  labs(
    x = "Percentage of promotional words",
    y = "Pred. prob. a patent app. is granted",
    color = NULL, 
    fill = NULL    
  ) +
  
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5), 
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  coord_cartesian(ylim = c(0.53, 0.83)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),

    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),   
 
    legend.background = element_rect(
      fill = "white",     
      color = "black",  
      size = 0.3,         
      linetype = "solid"  
    ),
    legend.margin = margin(2, 2, 2, 2), 
    legend.key = element_rect(fill = "white", color = NA),  
    legend.text = element_text(size = 9),  
    legend.key.size = unit(0.4, "cm"),   
    legend.direction = "vertical",
    aspect.ratio = 1
  ) +
  guides(
    color = guide_legend(
      ncol = 2,         
      byrow = TRUE,      
      title.position = "top"  
    ),
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    )
  )

print(plot_patent_cpc)


plot_transfer_cpc <- ggplot(plot_transfered, aes(x = frac_hype_words, y = predicted_value)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cpc_section, group = cpc_section), alpha = 0.2) +
  geom_line(aes(color = cpc_section, group = cpc_section), linewidth = 1.1) +
  geom_point(aes(color = cpc_section), size = 2.5, shape = 16) +
  
  labs(
    x = "Percentage of promotional words",
    y = "Pred. prob. of ownership transfer",
    color = NULL, 
    fill = NULL   
  ) +
  
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5), 
    labels = function(x) { scales::number(x * 100, accuracy = 0.1) }
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  coord_cartesian(ylim = c(0.63, 0.83)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
   
    legend.position = c(0.02, 0.98), 
    legend.justification = c(0, 1),  

    legend.background = element_rect(
      fill = "white",     
      color = "black",   
      size = 0.3,         
      linetype = "solid" 
    ),
    legend.margin = margin(2, 2, 2, 2),  
    legend.key = element_rect(fill = "white", color = NA),  
    legend.text = element_text(size = 9), 
    legend.key.size = unit(0.4, "cm"),     
    legend.direction = "vertical",
    aspect.ratio = 1
  ) +

  guides(
    color = guide_legend(
      ncol = 2,           
      byrow = TRUE,      
      title.position = "top"  
    ),
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    )
  )

print(plot_transfer_cpc)


p1 <- plot_patent_cpc + ggtitle("A")
p2 <- plot_transfer_cpc + ggtitle("B")


combined <- (p1 + p2) +
  plot_layout(ncol = 2) &
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 20, hjust = -0.05),

    plot.margin = margin(10, 15, 10, 10) 
  )

ggsave(
  filename = "figure2.pdf", 
  plot = combined,
  width = 12,   
  height = 6,   
  device = cairo_pdf, 
  units = "in"
)


ggsave(
  filename = "figure2.png", 
  plot = combined,
  width = 12, 
  height = 6, 
  dpi = 300,    
  bg = "white"
)