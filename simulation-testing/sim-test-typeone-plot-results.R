library(here)
library(tidyverse)

distr <- "quad"
# distr <- "fgld"

load(here("output", str_c("sim-test-typeone-", distr, ".RData")))

map(typeones, median)
typeones <- set_names(typeones,nm = n)
as_tibble(typeones) |> 
  pivot_longer(cols = everything(), names_to = "n",values_to = "error") |> 
  mutate(n = parse_number(n)) |> 
  ggplot(aes(x = factor(n), y = error))+
  geom_hline(yintercept = 0.05, lty = 3)+
  geom_boxplot(fill = NA, outlier.alpha = 0.5)+
  scale_y_continuous(limits = c(0, 0.1), n.breaks = 6)+
  theme_bw()+
  labs(x = "sample size", y = "type I error")

ggsave(here("output", str_c("typeones-", distr, ".pdf")), width = 4, height = 4)
