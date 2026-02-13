# 04_figures.R
# Project: Nepal Typhoid TND
# Description: This script generates Figure 2 (Forest Plot of Vaccine Effectiveness)
#              using summarized results.
# Author: [Nepal Typhoid TND Team]

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, here)

# 1. Data Preparation -----------------------------------------------------
# Values derived from VE analysis (Table 3 in manuscript)
data_plot <- data.frame(
    group = c("All Vaccinated", "Confirmed Vaccination", "15m to <5y", ">=5y"),
    aOR = c(0.11, 0.12, 0.28, 0.02),
    aOR_lower = c(0.03, 0.03, 0.03, 0.005), # Adjusted 0.00 to 0.005 for log scale
    aOR_upper = c(0.35, 0.47, 3.03, 0.20),
    stringsAsFactors = FALSE
)

# Reverse order for plotting top-to-bottom
data_plot$group <- factor(data_plot$group,
    levels = rev(c(
        "All Vaccinated", "Confirmed Vaccination",
        "15m to <5y", ">=5y"
    ))
)

# 2. Create Forest Plot ---------------------------------------------------
forest_plot <- ggplot(data_plot, aes(y = group)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = aOR_lower, xmax = aOR_upper),
        height = 0.2, linewidth = 0.7
    ) +
    geom_point(aes(x = aOR), size = 4, shape = 18) +
    scale_x_continuous(
        name = "Adjusted Odds Ratio",
        trans = "log10",
        breaks = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10),
        limits = c(0.005, 10)
    ) +
    labs(y = NULL) +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 11),
        plot.margin = margin(10, 15, 10, 5)
    ) +
    annotate("text",
        x = 0.006, y = 0.5,
        label = "Favors vaccination", hjust = 0, size = 3
    ) +
    annotate("text",
        x = 9, y = 0.5,
        label = "Favors no vaccination", hjust = 1, size = 3
    )

# 3. Save Output ----------------------------------------------------------
dir.create(here("figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here("figures", "Figure2_ForestPlot.png"), forest_plot,
    width = 6, height = 4, units = "in", dpi = 300
)

message("Figure 2 generated and saved to 'figures/Figure2_ForestPlot.png'.")
