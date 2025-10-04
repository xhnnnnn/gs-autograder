# Author:  Jarad Niemi
# Date:    2025-10-04
# Purpose: HW07_TidyPart03 solutions
# ------------------------------------------------------------------------------
library("tidyverse")



shuttle <- readRDS("shuttle.RDS") |>
  mutate(TempC = 5 / 9 * (Temp - 32))

shuttle_plot <- ggplot(shuttle, 
       aes(x = TempC, y = Incidents)) +
  geom_point() +
  geom_smooth() +
  labs(
    x        = "Temperature (C)",
    y        = "Number of O-ring Incidents",
    title    = "Shuttle O-ring Incidents by Temperature",
    subtitle = "Prior to Challenger Disaster"
  ) +
  theme_bw()





rex <- read_csv("rex.csv") |>
  mutate(Bone = factor(Bone, levels = paste0("Bone", 1:12)))

rex_summary <- rex |>
  group_by(Bone) |>
  summarize(
    n = n(),
    mean = mean(Oxygen),
    sd   = sd(Oxygen),
    lcl = mean - 2 * sd,
    ucl = mean + 2 * sd
  )

rex_plot <- ggplot(rex, aes(x = Bone)) +
  geom_jitter(mapping = aes(y = Oxygen)) + 
  geom_pointrange(data = rex_summary,
                  aes(y = mean, ymin = lcl, ymax = ucl),
                  color = 'red') +
  labs(
    x = 'Bone ID',
    y = 'Oxygen Isotopic Composition',
    title = 'Oxygen Isotopic Composition of Vertebrate Bone Phosphate',
    subtitle = 'in 12 bones of a single T. rex specimen'
  ) + 
  theme_bw()





load('blocks.RData')

grazer <- bind_rows(
  B1 |> mutate(Block = "B1"),
  B2 |> mutate(Block = "B2"),
  B3 |> mutate(Block = "B3"),
  B4 |> mutate(Block = "B4")
) |>
  mutate(
    Treat = factor(Treat, levels = c("C","f","fF","L","Lf","LfF"))
  )

grazer_plot <- ggplot(grazer,
                      aes(x = Treat, 
                          y = Cover,
                          color = Treat,
                          shape = Treat)) +
  geom_point() +
  facet_wrap(~Block, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom")



grazer_summary <- grazer |>
  group_by(Block, Treat) |>
  summarize(
    mean = mean(Cover),
    
    .groups = "drop"
  ) |>
  mutate(
    Treat = factor(Treat, levels = c("C", "f", "fF", "L", "Lf", "LfF"))
  ) |>
  arrange(Block, Treat)

grazer_means <- grazer_summary |>
  pivot_wider(
    names_from = "Treat",
    values_from = "mean"
  ) 

grazer_summary_plot <- ggplot(grazer_summary,
       aes(
         x     = Block,
         y     = mean,
         color = Treat,
         shape = Treat,
         group = Treat
       )) +
  geom_point() +
  geom_line() +
  labs(
    y = "Mean Cover",
  ) +
  theme_bw()



WorldPhones_df <- bind_cols(
  data.frame(Year = as.numeric(rownames(WorldPhones))),
  as.data.frame(WorldPhones)
)

WorldPhones_transformed <- WorldPhones_df |>
  pivot_longer(
    -Year,
    names_to  = "Region",
    values_to = "Count"
  )

WorldPhones_plot <- ggplot(WorldPhones_transformed,
                           aes(
                             x = Year,
                             y = Count,
                             color = Region,
                             linetype = Region,
                             group    = Region)) +
  geom_line() +
  scale_y_log10() +
  theme_bw()
