# Author: Qingrou Lin
# Date: 2025-10-10
# Purpose: HW07_TidyPart03 assignment
# ------------------------------------------------------------------------------

# Load tidyverse
library(tidyverse)

# 1
# shuttle_file <- file.choose()
shuttle <- readRDS("C:\\ISU\\Courses\\STAT 5279\\HW7\\shuttle.RDS") |>
  mutate("TempC" = (Temp - 32) / 1.8)
shuttle_plot <- ggplot(data = shuttle,
                       mapping = aes(x = TempC, y = Incidents)) +
  geom_point() +
  labs(title = "Shuttle O−ring Incidents by Temperature",
       subtitle = "Prior to Challenger Disaster",
       x = "Temperature (C)",
       y = "Number of O−ring Incidents") +
  geom_smooth(method = 'loess', formula = 'y ~ x')
shuttle_plot

# 2
rex <- read.csv("C:\\ISU\\Courses\\STAT 5279\\HW7\\rex.csv") |> mutate(
  Bone = as.factor(Bone))
rex$Bone <- factor(rex$Bone, ordered = TRUE, levels = c("Bone1",
                                                        "Bone2",
                                                        "Bone3",
                                                        "Bone4",
                                                        "Bone5",
                                                        "Bone6",
                                                        "Bone7",
                                                        "Bone8",
                                                        "Bone9",
                                                        "Bone10",
                                                        "Bone11",
                                                        "Bone12"))
rex_summary <- rex |> group_by(Bone) |> summarize(n = n(),
                                                  mean = mean(Oxygen),
                                                  sd = sd(Oxygen),
                                                  lcl = mean - 2 * sd,
                                                  ucl = mean + 2 * sd)
rex_plot <- ggplot(data = rex_summary,
                   mapping = aes(x = Bone, y = mean)) +
  geom_pointrange(aes(ymin = rex_summary$lcl, ymax = rex_summary$ucl), color = "red") +
  geom_jitter(data = rex,
              mapping = aes(x = Bone, y = Oxygen)) +
  labs(title = "Oxygen Isotopic Composition of Vertebrate Bone Phosphate",
       subtitle = "in 12 bones of a single T. rex specimen",
       x = "Bone ID",
       y = "Oxygen Isotopic Composition")
rex_plot

# 3
load("C:\\ISU\\Courses\\STAT 5279\\HW7\\blocks.RData")
B1 <- B1 |> mutate(Block = "B1")
B2 <- B2 |> mutate(Block = "B2")
B3 <- B3 |> mutate(Block = "B3")
B4 <- B4 |> mutate(Block = "B4")
grazer <- bind_rows(B1,B2,B3,B4)
grazer$Treat <- factor(grazer$Treat, ordered = TRUE, levels = c("C",
                                                                "f",
                                                                "fF",
                                                                "L",
                                                                "Lf",
                                                                "LfF"))
grazer_plot <- ggplot(data = grazer) +
  geom_point(mapping = aes(x = Treat, y = Cover, color = Treat, shape = Treat)) +
  facet_wrap(~Block, nrow = 2, scales = "free") +
  scale_color_manual(values = c("#F8766D",
                                "#B79F00",
                                "#00BA38",
                                "#00BFC4",
                                "#619CFF",
                                "#F564E3")) +
  theme(legend.position = "bottom")
grazer_plot
grazer_summary <- grazer |> group_by(Block, Treat) |> 
  summarize(`Mean Cover` = mean(Cover))
grazer_means <- grazer_summary |> pivot_wider(names_from = "Treat",
                                              values_from = "Mean Cover")
grazer_summary_plot <- ggplot(data = grazer_summary,
                              mapping = aes(x = Block,
                                            y = `Mean Cover`,
                                            group = Treat,
                                            color = Treat,
                                            shape = Treat)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#F8766D",
                                "#B79F00",
                                "#00BA38",
                                "#00BFC4",
                                "#619CFF",
                                "#F564E3"))
grazer_summary_plot

# 4
WorldPhones_df <- WorldPhones |> as_tibble(rownames = "Year")
WorldPhones_transformed <- WorldPhones_df |> pivot_longer(-Year,
                                                          names_to = "Region",
                                                          values_to = "Count")
WorldPhones_plot <- ggplot(data = WorldPhones_transformed) +
  geom_line(mapping = aes(x = Year, y = Count, 
                          group = Region, color = Region,
                          linetype = Region)) +
  scale_y_log10()
WorldPhones_plot