library(tidyverse)
library(here)
library(gt)

load(here("output", "naivebayes-simulation.Rdata"))

# array -> long (tidy?) tibble

names <- c("error", "time")

out2 <- transpose(out) |> 
  set_names(names) |> 
  map2(.y = names, \(x, y) x %>%
  purrr::map(as.data.frame) %>%
  purrr::map(tibble) %>%
  purrr::map(., \(d) rownames_to_column(.data = d, var = "rep")) %>%
  tibble(error = .) %>%
  bind_cols(., cases) %>%
  group_by(scenario, nvars, cor, sample_size) %>%
  unnest(error) %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = -c(scenario, nvars, cor, sample_size, rep),
    names_to = "method",
    values_to = y
  ) %>%
  rename(n = sample_size, p = nvars))

out2$error %>% 
  group_by(method) %>% 
  summarise(mean = mean(error), median(error)) %>% 
  arrange(mean)






##--------------
##  Error plot  
##--------------


# Paired scaled differences from method of interest
# Within each scenario

# First averaged within each p n cor method

reference_method <- quote(fgld)

diff <- out2$error %>%
  group_by(scenario, p, n, cor, method) %>%
  summarise(mean_error = mean(error), .groups = "drop_last") %>%
  mutate(mean_setting_error = mean(mean_error)) %>%
  ungroup() %>% 
  pivot_wider(names_from = method, values_from = mean_error) %>%
  mutate(across(
    .cols = -c(scenario, p, n, cor, mean_setting_error, {{reference_method}}),
    .fns = \(x) (x - {{reference_method}}) / mean_setting_error
  )) %>%
  select(-c(p, n, cor, mean_setting_error, {{reference_method}})) %>%
  pivot_longer(
    cols = - scenario,
    names_to = "method",
    values_to = "error"
  )

diff$method %>% table
diff$error %>% is.na %>% sum
nas <- which(is.na(diff$error))
diff$method[nas]

diff_plot<- diff %>%
  mutate(
    scenario = fct_relevel(scenario,
                           c("norm", "t", "logabst", "ilogabst", "exp")),
    scenario = fct_recode(scenario,
                          "normal" = "norm",
                          "t[nu==3]" = "t",
                          "log(abs(t[nu==3]))" = "logabst",
                          "log(abs(t[nu==3]))~scaled" = "ilogabst",
                          "exponential" = "exp"),
    method = fct_relevel(method,
                         c("normal","kde","discrete"))
  )

diff_plot |> 
  ggplot(aes(x = method, y = error, col = method))+
  geom_boxplot(fill=NA)+
  geom_hline(yintercept = 0,lty = 1,alpha=0.5)+
  theme_bw()+
  facet_wrap(facets = vars(scenario), nrow = 1, labeller = label_parsed)+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill =  NA),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(size = 12,face = "bold"),
        plot.subtitle = element_text(size = 11))+
  # coord_cartesian(ylim = c(-0.5, 2.5))+
  labs(y = "scaled error difference")

ggsave(filename = here("output", "naive-bayes-simulation-results.pdf"), width = 10, height = 3)




##--------------
##  Time table  
##--------------

out2$time |> 
  group_by(n, p, method) |> 
  summarise(mean = mean(time),
            sd = sd(time),
            .groups = "drop_last") |>
  mutate(n = str_c("n = ", n)) |> 
  pivot_wider(values_from = c(mean, sd), names_from = p) |> 
  gt() |> 
  fmt_number(columns = -c(n, method), decimals = 2) |>
  cols_merge(columns = c(mean_10, sd_10),
             pattern = "{1} ({2})") |> 
  cols_merge(columns = c(mean_50, sd_50),
             pattern = "{1} ({2})") |> 
  cols_merge(columns = c(mean_100, sd_100),
             pattern = "{1} ({2})") |> 
  cols_label(
    mean_10 = "p = 10",
    mean_50 = "p = 50",
    mean_100 = "p = 100",
  ) |> 
  tab_options(row_group.as_column = TRUE) |> 
  # tab_style(style = "vertical-align:center",
  #           locations = cells_body(columns = c(n)))
  as_latex() |> 
  as.character() |> 
  writeLines()

