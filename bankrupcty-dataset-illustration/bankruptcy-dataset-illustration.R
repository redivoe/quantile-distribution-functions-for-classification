library(here)

# loading packages and functions

pkgs <- c("tidyverse", "tidymodels", "quadprog", "foreach", "doParallel", "doRNG")
mapply(require, pkgs, MoreArgs = list(character.only = TRUE))

foos <- list.files(path = here("functions"), full.names = TRUE)
purrr::walk(foos, source)


# loading the dataset

# install.packages("MixGHD")
data(bankruptcy, package = "MixGHD")

bankruptcy <- bankruptcy |> 
  transmute(status = factor(Y, levels = c(0, 1), labels = c("bankrupcty", "financially sound")),
            RE,
            EBIT)

# plot

basic_plot <- bankruptcy |>
  ggplot(aes(x = RE, y = EBIT, col = status))+
  geom_point(alpha = 0.9)+
  geom_rug(alpha = 0.7, sides = "tr")+
  theme_bw()

ggsave(filename = here("output", "bankruptcy-dataset.pdf"), plot = basic_plot, width = 6, height = 4)


# add noise variables
n <- nrow(bankruptcy)
p <- 200
p_noise <- p - (ncol(bankruptcy) - 1)

set.seed(1021)
noise <- matrix(rnorm(n * p_noise), nrow = n, ncol = p_noise) |> 
  as.data.frame()

bank_wnoise <- bind_cols(bankruptcy, noise) |> 
  mutate(across(.cols = -status, scale))

fgld_test_ordering <- test_fun(y = bank_wnoise$status, X = bank_wnoise[,-1])
fgld_test_ordering

fgld_test_ordering |> 
  filter(pvalue < 0.05/p)

bhp <- function(p, ahat = 0, alpha = 0.05) {
  m <- length(p)
  ps <- sort(p)
  u <- ps * (1 - ahat) < (1:m) * alpha / m
  Ts <- 0
  if (any(u)) {
    Ts <- ps[max(which(u))]
  }
  return(Ts)
}

fgld_test_ordering |> 
  filter(pvalue <= bhp(fgld_test_ordering$pvalue))


# loocv for all methods
rs_obj <- get_splits_loo_cv(n = n)

models <- c("nb_normal", "nb_kde", "nb_discrete", "nb_fgld", "lda", "logistic", "knn")
models_labels <- c("Naive Bayes normal",
                   "Naive Bayes KDE",
                   "Naive Bayes discrete",
                   "Naive Bayes fgld",
                   "LDA",
                   "Logistic regression",
                   "KNN k = 3")

cores_used <- 6
cl <- makeCluster(spec = cores_used, type = "PSOCK")
registerDoParallel(cl = cl)
registerDoRNG(20)

seq_p <- c(2, 50, 100, 150, 200)

bank_selection <- foreach(
  current_p = seq_p,
  .packages = pkgs,
  .export = ls()
) %dopar% {
  
  # selecting the current_p most informative vars
  X <- bank_wnoise[,fgld_test_ordering$x[1:current_p], drop = FALSE]
  
  kfold_results_prefilled <- partial(kfold_results,
                                     y = bank_wnoise$status,
                                     X = X,
                                     models = models)
  
  purrr::map(rs_obj$splits, kfold_results_prefilled) |> 
    purrr::transpose() |> 
    purrr::map(~ .x |> 
                 purrr::reduce(bind_rows) |> 
                 tidyr::pivot_longer(cols = dplyr::everything(),
                                     names_to = "model",
                                     values_to = "accuracy") |> 
                 dplyr::group_by(model) |> 
                 dplyr::summarise(mean = mean(accuracy), std_err = sd(accuracy)/sqrt(dplyr::n())))
}

bank_out_train <- map(bank_selection, "train")
bank_out_test <- map(bank_selection, "test")

bank_out_test <- bank_out_test |> 
  imap(~ mutate(.x, p = seq_p[.y])) |> 
  reduce(bind_rows) |>
  mutate(model = factor(model, levels = models, labels = models_labels))

tibble_table <- bank_out_test |> 
  mutate(mean = mean*100) |> 
  select(-std_err) |> 
  pivot_wider(names_from = p, values_from = mean, names_glue = "p = {p}")

tibble_table |> 
  xtable::xtable() |> 
  print(include.rownames=FALSE)


out_for_visual <- naive_bayes(y = bank_wnoise$status,
                              X = bank_wnoise[,fgld_test_ordering$x],
                              distr = "fgld",
                              fitted_values = FALSE)

naive_visual(out = out_for_visual, max_var = 7, n_bins = 20)
ggsave(here("output", "bankruptcy-visual-ordered.pdf"), width = 9, height = 3)


save.image(here("output", "bankruptcy-out.RData"))
