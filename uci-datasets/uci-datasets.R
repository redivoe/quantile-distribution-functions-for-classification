library(here)
library(gt)

pkgs <- c("tidyverse", "tidymodels", "quadprog", "foreach", "doParallel", "doRNG")
mapply(require, pkgs, MoreArgs = list(character.only = TRUE))

foos <- list.files(path = here("..", "functions"), full.names = TRUE)
purrr::walk(foos, source)

get_test_out <- function(out){
  map(out, "test") |> 
    reduce(bind_rows) |> 
    colMeans()  
}


load(here("data", "uci-datasets.RData"))

datas <- tribble(
  ~name, ~class,
  "cleveland", "diagnosis_heart_disease",
  "credit", "V16",
  "diabetes", "diabetes",
  "glass", "Type",
  "heart", "y",
  "ionosphere", "Class",
  "letter", "lettr",
  "sonar", "Class",
  "thyroid", "thryroid_class",
  "vehicle", "y",
  "waveform", "class",
  "wbcd", "outcome",
)

models <- c("nb_normal", "nb_kde", "nb_discrete", "nb_fgld")
# number of cross-validation folds
v <- 10

cores_used <- 8
cl <- makeCluster(spec = cores_used, type = "PSOCK")
registerDoParallel(cl = cl)
registerDoRNG(20)

out <- foreach(
  i = 1:nrow(datas),
  .packages = pkgs,
  .export = ls()) %dopar% {
  
  y <- get(datas$name[i]) |> 
    pull(datas$class[i])
  
  X <- get(datas$name[i]) |> 
    select(-datas$class[i])
  n <- nrow(X)
  kfold_splits <- get_splits_vfold_cv(n = n, v = v, strata = y)

  out_temp <- purrr::map(kfold_splits$splits, kfold_results, y = y, X = X, models = models)
  get_test_out(out_temp)
}

out <- out |> 
  reduce(bind_rows)
out <- bind_cols("dataset" = datas$name, out)


out |> 
  mutate(across(-dataset, \(x) x*100)) |> 
  gt(rowname_col = "dataset") |> 
  fmt_number(columns = everything(), decimals = 2) |>
  cols_label(nb_fgld = "fgld",
             nb_normal = "normal",
             nb_kde = "kde",
             nb_discrete = "discrete") |> 
  as_latex() |> 
  as.character() |> 
  writeLines()


ranks <- out |> 
  pivot_longer(cols = -dataset, names_to = "method", values_to = "accuracy") |> 
  group_by(dataset) |> 
  mutate(rank = n() - rank(accuracy) + 1) |> 
  select(-accuracy) |> 
  pivot_wider(names_from = method, values_from = rank) |> 
  ungroup()

ranks |> 
  select(-dataset) |> 
  colMeans()



# n, p_numeric, p_categorical, K

sizes <- c("n", "p_num", "p_cat", "p", "K")
datas_size <- matrix(nrow = nrow(datas), ncol = length(sizes))
colnames(datas_size) <- sizes

for(i in 1:nrow(datas)){
  current <- get(datas$name[i])
  datas_size[i, "n"] <- nrow(current)
  types <- sapply(current, class)
  datas_size[i, "p_num"] <- sum(types %in% c("integer", "numeric"))
  datas_size[i, "p_cat"] <- sum(types == "factor") - 1
  datas_size[i, "p"] <- ncol(current) - 1
  datas_size[i, "K"] <- length(levels(current |> pull(datas$class[i])))
  rm(types, current)
}

tibble("dataset" = datas$name, as_tibble(datas_size)) |> 
  select(-p) |> 
  gt(rowname_col = "dataset") |> 
  cols_label(n = "sample size",
             p_num = "numerical variables",
             p_cat = "categorical variables",
             K = "response classes") |> 
  as_latex() |> 
  as.character() |> 
  writeLines()


rm(list = setdiff(ls(), c("out", "datas_size", "datas")))
save.image(file = here("output.RData"))
