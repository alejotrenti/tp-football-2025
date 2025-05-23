library(tidyverse)

df <- read_csv("players_data-2024_2025.csv") 

df_bota_oro_5 <- df %>%
  select(-contains("keeper")) %>%
  select(-all_of(c("GA","GA90","Saves", "Save%", "SoTA", "W", "D", "L","CS","CS%","PKA","PKsv","PKm","PSxG", "PSxG/SoT", "PSxG+/-", "/90", "Att (GK)", "Thr", "Launch%", "AvgLen", "Opp", "Stp","Stp%", "#OPA", "#OPA/90", "AvgDist"))) %>%
  select(-contains("defense")) %>%
  select(-contains("Crd")) %>%
  select(all_of(c(
    "Player", "Nation", "Pos", "Squad", "Comp",
    "Gls", "Ast", "G+A", "G-PK", "PK", "PKatt",
    "MP", "Starts", "Min", "90s",
    "xG", "npxG", "xAG", "npxG+xAG",
    "Sh", "SoT", "SoT%", "Sh/90", "SoT/90",
    "G/Sh", "G/SoT", "Dist", "FK",
    "PrgC", "PrgP", "PrgR",
    "KP", "1/3", "PPA", "CrsPA",
    "xA", "A-xAG"
  ))) %>%
  filter(Gls > 15) %>%
  slice_max(order_by = Gls, n = 8, with_ties = FALSE)

df_bota_oro_5 %>%
  ggplot(aes(x = reorder(Player, Gls), y = Gls, fill = Comp)) +
  geom_col(width = 0.7, color = "black") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 5 Goal Scorers - Season 2024/2025",
    x = "Player",
    y = "Goals",
    fill = "Team"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom"
  )


modelo2 <- lm(Gls ~ xG + xAG + Sh, data = df_bota_oro)
summary(modelo2)

df_bota_oro %>%
  mutate(pred = predict(modelo2)) %>%
  ggplot(aes(x = pred, y = Gls)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Gls", x = "Predicted", y = "Actual")

anova_eq <- aov(xG ~ Squad, data = df_bota_oro)
summary(anova_eq)


