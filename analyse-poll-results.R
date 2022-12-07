library(tidyverse)

polls <- read_excel("data/Polls-per-user-CEU_R_Python_Stata_2022.xlsx") |>
  slice(-1) |>
  rename(
    id = `User ID`,
    after = starts_with("[AFTER"),
    before = starts_with("[Start]"),
    convincing = starts_with("Which was")
  ) |>
  select(id, after, before, convincing) |>
  mutate(convincing = str_match(convincing, "Team ([A-Za-z]+)")[,2]) |>
  # Limit to people who answer before and after
  filter(if_all(c(after, before), ~ !is.na(.)))

flow_plot <- polls |>
  count(before, after) |>
  ggplot() + 
  geom_col(aes(x = before, y = n, fill = before)) + 
  facet_wrap(~after) + 
  labs(caption = "Final votes for each lang, broken down by initial (pre-debate) vote.")

ggsave("img/flow_plot_ceu_debate.png", flow_plot)