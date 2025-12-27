# GRAPH 1: AI AWARENESS - LIKERT CHART

library(tidyverse)

df_awareness <- data.frame(
  Country = c("U.S.", "Canada", "France", "Germany", "Sweden", "Netherlands",
              "Italy", "UK", "Hungary", "Poland", "Greece", "Spain", "Japan",
              "Australia", "South Korea", "Indonesia", "India", "Israel",
              "Turkey", "South Africa", "Nigeria", "Kenya", "Argentina",
              "Brazil", "Mexico"),
  A_lot_pct = c(47, 41, 52, 51, 46, 46, 45, 41, 38, 34, 34, 30, 53,
                44, 21, 18, 14, 36, 19, 30, 17, 12, 24, 22, 19),
  A_little_pct = c(48, 51, 40, 45, 45, 44, 46, 49, 53, 53, 49, 56, 36,
                   53, 57, 36, 32, 44, 50, 31, 44, 36, 48, 47, 53),
  Nothing_at_all_pct = c(5, 8, 8, 4, 8, 10, 9, 10, 8, 13, 17, 14, 11,
                         3, 21, 43, 35, 18, 25, 34, 32, 49, 28, 30, 27),
  stringsAsFactors = FALSE
)


df_likert <- df_awareness %>%
  arrange(desc(A_lot_pct)) %>%
  mutate(Country = factor(Country, levels = rev(Country))) %>%
  mutate(
    half_little = A_little_pct / 2
  )


df_plot <- df_likert %>%
  transmute(
    Country,
    A_lot_pct,
    Nothing_at_all_pct,
    nothing_xmin = -(Nothing_at_all_pct + half_little),
    nothing_xmax = -half_little,
    alittle_left_xmin = -half_little,
    alittle_left_xmax = 0,
    
    alittle_right_xmin = 0,
    alittle_right_xmax = half_little,
    alot_xmin = half_little,
    alot_xmax = half_little + A_lot_pct
  )

col_alot <- "#1a5276"
col_alittle <- "#85c1e9"
col_nothing <- "#c0392b"

ggplot(df_plot) +
  geom_segment(aes(x = 0, xend = 0, y = 0.5, yend = 25.5), 
               color = "#2c3e50", linewidth = 1.5) +
  
  geom_rect(aes(xmin = nothing_xmin, xmax = nothing_xmax, 
                ymin = as.numeric(Country) - 0.4, ymax = as.numeric(Country) + 0.4),
            fill = col_nothing) +
  geom_rect(aes(xmin = alittle_left_xmin, xmax = alittle_left_xmax,
                ymin = as.numeric(Country) - 0.4, ymax = as.numeric(Country) + 0.4),
            fill = col_alittle) +
  geom_rect(aes(xmin = alittle_right_xmin, xmax = alittle_right_xmax,
                ymin = as.numeric(Country) - 0.4, ymax = as.numeric(Country) + 0.4),
            fill = col_alittle) +
  geom_rect(aes(xmin = alot_xmin, xmax = alot_xmax,
                ymin = as.numeric(Country) - 0.4, ymax = as.numeric(Country) + 0.4),
            fill = col_alot) +
  geom_text(aes(x = alot_xmax + 3, y = as.numeric(Country), 
                label = paste0(A_lot_pct, "%")),
            hjust = 0, size = 7, fontface = "bold", color = col_alot) +
  
  geom_text(aes(x = nothing_xmin - 3, y = as.numeric(Country),
                label = paste0(Nothing_at_all_pct, "%")),
            hjust = 1, size = 7, fontface = "bold", color = col_nothing) +

  scale_y_continuous(
    breaks = 1:25,
    labels = levels(df_plot$Country),
    expand = c(0.02, 0.02)
  ) +
  
  scale_x_continuous(
    limits = c(-85, 90),
    breaks = seq(-60, 60, 20),
    labels = function(x) paste0(abs(x), "%")
  ) +
  
  # Legend için dummy noktalar
  annotate("rect", xmin = -100, xmax = -99, ymin = 0, ymax = 0.1, 
           fill = col_alot, alpha = 0) +
  
  labs(
    title = "THE GLOBAL AI AWARENESS DIVIDE",
    subtitle = "How much have people heard about Artificial Intelligence?",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(face = "bold", size = 36, color = "#1a5276",
                              hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 24, color = "#5d6d7e", hjust = 0.5,
                                 margin = margin(b = 30)),
    axis.title.x = element_text(face = "bold", size = 20, color = "#2c3e50",
                                margin = margin(t = 25)),
    axis.text.y = element_text(size = 18, face = "bold", color = "#2c3e50"),
    axis.text.x = element_text(size = 18, color = "#5d6d7e"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(30, 40, 30, 30)
  ) +
  annotate("rect", xmin = -55, xmax = -45, ymin = 26.5, ymax = 27.3, fill = col_nothing) +
  annotate("text", x = -43, y = 26.9, label = "Heard 'Nothing at all'", 
           hjust = 0, size = 6, fontface = "bold", color = "#2c3e50") +
  annotate("rect", xmin = -5, xmax = 5, ymin = 26.5, ymax = 27.3, fill = col_alittle) +
  annotate("text", x = 7, y = 26.9, label = "Heard 'A little'", 
           hjust = 0, size = 6, fontface = "bold", color = "#2c3e50") +
  annotate("rect", xmin = 45, xmax = 55, ymin = 26.5, ymax = 27.3, fill = col_alot) +
  annotate("text", x = 57, y = 26.9, label = "Heard 'A lot'", 
           hjust = 0, size = 6, fontface = "bold", color = "#2c3e50") +
  
  coord_cartesian(ylim = c(0.5, 27.5), clip = "off")





# GRAPH 2: THE SENTIMENT SPECTRUM

library(tidyverse)
library(ggrepel)

df_sentiment <- data.frame(
  Country = c("U.S.", "Italy", "Australia", "Brazil", "Greece", "Canada",
              "UK", "Argentina", "Spain", "Poland", "Mexico", "France",
              "Netherlands", "Hungary", "Indonesia", "Kenya", "Sweden",
              "South Africa", "Germany", "Japan", "Turkey", "Nigeria",
              "Israel", "India", "South Korea"),
  More_concerned_than_excited_pct = c(50, 50, 49, 48, 47, 45, 39, 39, 39, 37,
                                      35, 35, 34, 33, 32, 31, 31, 30, 29, 28,
                                      26, 24, 21, 19, 16),
  More_excited_than_concerned_pct = c(10, 12, 13, 10, 10, 9, 13, 13, 19, 15,
                                      13, 15, 16, 18, 14, 17, 22, 18, 17, 16,
                                      19, 20, 29, 16, 22),
  stringsAsFactors = FALSE
)

df_sentiment_plot <- df_sentiment %>%
  mutate(
    Net_Sentiment = More_concerned_than_excited_pct - More_excited_than_concerned_pct
  )

axis_min <- 5
axis_max <- 55

ggplot(df_sentiment_plot, aes(x = More_excited_than_concerned_pct, 
                              y = More_concerned_than_excited_pct)) +
  
  annotate("polygon",
           x = c(axis_min, axis_min, axis_max, axis_max),
           y = c(axis_min, axis_max, axis_max, axis_max),
           fill = "#e74c3c", alpha = 0.12) +

  annotate("polygon",
           x = c(axis_min, axis_max, axis_max),
           y = c(axis_min, axis_max, axis_min),
           fill = "#27ae60", alpha = 0.12) +
  

  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "#bdc3c7", linewidth = 1) +
  
  annotate("label",
           x = 50, y = 53,
           label = "Equal Concern\n& Excitement",
           size = 5,
           color = "#7f8c8d",
           fill = "white",
           alpha = 0.95,
           label.padding = unit(0.6, "lines"),
           label.size = 0.3,
           fontface = "bold.italic") +
  
  geom_point(aes(fill = Net_Sentiment), 
             shape = 21, size = 8, color = "white", stroke = 1.5,
             show.legend = FALSE) +
  
  geom_text_repel(
    aes(label = Country),
    size = 5,
    fontface = "bold",
    color = "#2c3e50",
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "#5d6d7e",
    segment.size = 0.6,
    segment.alpha = 1,
    min.segment.length = 0,
    force = 2,
    force_pull = 0.5
  ) +
  
  scale_fill_gradient2(
    low = "#27ae60",
    mid = "#f5f5f5", 
    high = "#e74c3c",
    midpoint = 0,
    limits = c(-10, 45)
  ) +
  
  scale_x_continuous(
    limits = c(axis_min, axis_max), 
    breaks = seq(10, 50, 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(axis_min, axis_max), 
    breaks = seq(10, 50, 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  coord_fixed(ratio = 1) +
  
  labs(
    title = "THE SENTIMENT SPECTRUM: FEAR VS. HOPE",
    x = "More Excited than Concerned",
    y = "More Concerned than Excited"
  ) +
  
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 32, color = "#2c3e50",
                              hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 18, color = "#7f8c8d", hjust = 0.5,
                                 margin = margin(b = 20)),
    axis.title.x = element_text(face = "bold", size = 18, color = "#2c3e50",
                                margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 18, color = "#2c3e50",
                                margin = margin(r = 15)),
    axis.text.x = element_text(size = 16, color = "#34495e", margin = margin(t = 10)),
    axis.text.y = element_text(size = 16, color = "#34495e", margin = margin(r = 10)),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 25, 25, 25)
  )




# GRAPH 3: TRUST HEATMAP - WITH REGIONS

library(tidyverse)

df_trust <- data.frame(
  Country = c("U.S.", "Canada", "France", "Germany", "Greece", "Hungary",
              "Italy", "Netherlands", "Poland", "Spain", "Sweden", "UK",
              "Australia", "India", "Indonesia", "Japan", "South Korea",
              "Israel", "Turkey", "Kenya", "Nigeria", "South Africa",
              "Argentina", "Brazil", "Mexico"),
  Trust_Own_Country_pct = c(44, 64, 47, 70, 22, 56, 37, 68, 53, 55, 55, 57,
                            65, 89, 74, 41, 55, 72, 60, 54, 46, 64, 33, 36, 50),
  Trust_EU_pct = c(43, 57, 47, 71, 38, 56, 42, 68, 44, 61, 54, 56,
                   59, 44, 58, 43, 53, 54, 36, 58, 72, 42, 31, 26, 35),
  Trust_US_pct = c(44, 33, 21, 33, 37, 56, 32, 35, 37, 34, 25, 37,
                   30, 64, 54, 41, 58, 70, 23, 62, 79, 50, 35, 35, 24),
  Trust_China_pct = c(13, 17, 17, 23, 34, 43, 33, 25, 13, 31, 15, 24,
                      15, 27, 64, 17, 21, 22, 32, 61, 79, 57, 39, 32, 38),
  stringsAsFactors = FALSE
)

# Region mapping
region_mapping <- data.frame(
  Country = c("U.S.", "Canada", "Mexico",
              "France", "Germany", "Sweden", "Netherlands", "Italy", "UK", "Spain",
              "Hungary", "Poland", "Greece",
              "Japan", "Australia", "South Korea", "Indonesia", "India",
              "Israel", "Turkey",
              "South Africa", "Nigeria", "Kenya",
              "Argentina", "Brazil"),
  Region = c("North America", "North America", "Latin America",
             "Western Europe", "Western Europe", "Western Europe", "Western Europe", 
             "Western Europe", "Western Europe", "Western Europe",
             "Eastern Europe", "Eastern Europe", "Eastern Europe",
             "Asia-Pacific", "Asia-Pacific", "Asia-Pacific", "Asia-Pacific", "Asia-Pacific",
             "Middle East", "Middle East",
             "Africa", "Africa", "Africa",
             "Latin America", "Latin America"),
  stringsAsFactors = FALSE
)

df_trust_clean <- df_trust %>%
  left_join(region_mapping, by = "Country")

# Long format
df_trust_long <- df_trust_clean %>%
  pivot_longer(
    cols = c("Trust_Own_Country_pct", "Trust_EU_pct", "Trust_US_pct", "Trust_China_pct"),
    names_to = "Entity",
    values_to = "Trust_Score"
  ) %>%
  mutate(
    Entity = factor(Entity,
                    levels = c("Trust_Own_Country_pct", "Trust_EU_pct", 
                               "Trust_US_pct", "Trust_China_pct"),
                    labels = c("Own Government", "EU", "USA", "China")),
    Region = factor(Region, levels = c("Western Europe", "Eastern Europe", 
                                       "North America", "Latin America",
                                       "Asia-Pacific", "Middle East", "Africa"))
  )

country_order <- df_trust_clean %>%
  arrange(Region, desc(Trust_Own_Country_pct)) %>%
  pull(Country)

df_trust_long$Country <- factor(df_trust_long$Country, levels = rev(country_order))

ggplot(df_trust_long, aes(x = Entity, y = Country, fill = Trust_Score)) +
  
  geom_tile(color = "white", linewidth = 0.8) +
  
  scale_fill_gradient2(
    low = "#b2182b",
    mid = "#f7f7f7",
    high = "#2166ac",
    midpoint = 50,
    limits = c(13, 90),
    name = "Trust Level",
    breaks = c(20, 35, 50, 65, 80),
    labels = c("20%", "35%", "50%", "65%", "80%")
  ) +
  
  # FACET
  facet_grid(Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  
  labs(
    title = "THE GEOPOLITICS OF AI TRUST",
    subtitle = "Who do people trust to regulate AI responsibly?",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 32, color = "#2c3e50",
                              hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 20, color = "#7f8c8d", hjust = 0.5,
                                 margin = margin(b = 20)),
    
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 14, face = "bold", color = "#2c3e50"),

    strip.text.y.left = element_text(face = "bold", size = 12, color = "white",
                                     angle = 0, hjust = 1),
    strip.background = element_rect(fill = "#34495e", color = NA),
    strip.placement = "outside",
  
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    
    # Panel
    panel.grid = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  guides(fill = guide_colorbar(
    title.position = "left",
    title.vjust = 0.9,
    barwidth = 15,
    barheight = 1
  ))





# GRAPH 4: REGIONAL COMPARISON - GROUPED BAR + ERROR BARS


library(tidyverse)

df_awareness <- data.frame(
  Country = c("U.S.", "Canada", "France", "Germany", "Sweden", "Netherlands",
              "Italy", "UK", "Hungary", "Poland", "Greece", "Spain", "Japan",
              "Australia", "South Korea", "Indonesia", "India", "Israel",
              "Turkey", "South Africa", "Nigeria", "Kenya", "Argentina",
              "Brazil", "Mexico"),
  A_lot_pct = c(47, 41, 52, 51, 46, 46, 45, 41, 38, 34, 34, 30, 53,
                44, 21, 18, 14, 36, 19, 30, 17, 12, 24, 22, 19),
  stringsAsFactors = FALSE
)

df_sentiment <- data.frame(
  Country = c("U.S.", "Italy", "Australia", "Brazil", "Greece", "Canada",
              "UK", "Argentina", "Spain", "Poland", "Mexico", "France",
              "Netherlands", "Hungary", "Indonesia", "Kenya", "Sweden",
              "South Africa", "Germany", "Japan", "Turkey", "Nigeria",
              "Israel", "India", "South Korea"),
  More_concerned_pct = c(50, 50, 49, 48, 47, 45, 39, 39, 39, 37,
                         35, 35, 34, 33, 32, 31, 31, 30, 29, 28,
                         26, 24, 21, 19, 16),
  stringsAsFactors = FALSE
)

df_trust <- data.frame(
  Country = c("U.S.", "Canada", "France", "Germany", "Greece", "Hungary",
              "Italy", "Netherlands", "Poland", "Spain", "Sweden", "UK",
              "Australia", "India", "Indonesia", "Japan", "South Korea",
              "Israel", "Turkey", "Kenya", "Nigeria", "South Africa",
              "Argentina", "Brazil", "Mexico"),
  Trust_Own_pct = c(44, 64, 47, 70, 22, 56, 37, 68, 53, 55, 55, 57,
                    65, 89, 74, 41, 55, 72, 60, 54, 46, 64, 33, 36, 50),
  stringsAsFactors = FALSE
)

region_mapping <- data.frame(
  Country = c("U.S.", "Canada", "Mexico",
              "France", "Germany", "Sweden", "Netherlands", "Italy", "UK", "Spain",
              "Hungary", "Poland", "Greece",
              "Japan", "Australia", "South Korea", "Indonesia", "India",
              "Israel", "Turkey",
              "South Africa", "Nigeria", "Kenya",
              "Argentina", "Brazil"),
  Region = c("North America", "North America", "Latin America",
             "Western Europe", "Western Europe", "Western Europe", "Western Europe", 
             "Western Europe", "Western Europe", "Western Europe",
             "Eastern Europe", "Eastern Europe", "Eastern Europe",
             "Asia-Pacific", "Asia-Pacific", "Asia-Pacific", "Asia-Pacific", "Asia-Pacific",
             "Middle East", "Middle East",
             "Africa", "Africa", "Africa",
             "Latin America", "Latin America"),
  stringsAsFactors = FALSE
)

# Combine data
df_combined <- df_awareness %>%
  left_join(df_sentiment, by = "Country") %>%
  left_join(df_trust, by = "Country") %>%
  left_join(region_mapping, by = "Country")

df_regional_stats <- df_combined %>%
  group_by(Region) %>%
  summarise(
    Awareness_mean = mean(A_lot_pct, na.rm = TRUE),
    Awareness_se = sd(A_lot_pct, na.rm = TRUE) / sqrt(n()),
    Concern_mean = mean(More_concerned_pct, na.rm = TRUE),
    Concern_se = sd(More_concerned_pct, na.rm = TRUE) / sqrt(n()),
    Trust_mean = mean(Trust_Own_pct, na.rm = TRUE),
    Trust_se = sd(Trust_Own_pct, na.rm = TRUE) / sqrt(n()),
    n_countries = n(),
    .groups = "drop"
  )

# Long format
df_plot <- df_regional_stats %>%
  pivot_longer(
    cols = c(Awareness_mean, Concern_mean, Trust_mean),
    names_to = "Metric",
    values_to = "Mean"
  ) %>%
  mutate(
    SE = case_when(
      Metric == "Awareness_mean" ~ Awareness_se,
      Metric == "Concern_mean" ~ Concern_se,
      Metric == "Trust_mean" ~ Trust_se
    ),
    Metric = factor(Metric,
                    levels = c("Awareness_mean", "Concern_mean", "Trust_mean"),
                    labels = c("AI Awareness", "AI Concern", "Trust Own Government"))
  ) %>%
  select(Region, Metric, Mean, SE, n_countries)
region_order <- df_regional_stats %>%
  arrange(desc(Awareness_mean)) %>%
  pull(Region)

df_plot$Region <- factor(df_plot$Region, levels = region_order)

metric_colors <- c(
  "AI Awareness" = "#27ae60",
  "AI Concern" = "#e74c3c",
  "Trust Own Government" = "#3498db"
)

dodge_width <- 0.75

ggplot(df_plot, aes(x = Region, y = Mean, fill = Metric)) +
  
  geom_col(position = position_dodge(width = dodge_width), 
           width = 0.7, alpha = 0.9) +
  
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = dodge_width),
                width = 0.25, linewidth = 0.6, color = "#2c3e50") +
  
  geom_text(aes(y = Mean + SE + 3, label = round(Mean, 0)),
            position = position_dodge(width = dodge_width),
            size = 5, fontface = "bold", color = "#2c3e50") +
  
  scale_fill_manual(values = metric_colors, name = NULL) +
  
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "REGIONAL AI PERCEPTION AT A GLANCE",
    subtitle = "Mean scores with standard error bars | Regions sorted by AI Awareness (highest to lowest)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 28, color = "#2c3e50",
                              hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 16, color = "#7f8c8d", hjust = 0.5,
                                 margin = margin(b = 20)),
    axis.title.y = element_text(face = "bold", size = 16, color = "#2c3e50"),
    axis.text.x = element_text(size = 14, face = "bold", color = "#2c3e50",
                               angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 14, color = "#5d6d7e"),
    legend.position = "top",
    legend.text = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 25, 25, 25)
  ) +
  
  guides(fill = guide_legend(nrow = 1))