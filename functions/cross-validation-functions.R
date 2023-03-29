
get_splits_vfold_cv <- function(n, v = 10, strata){
  dummy_data <- tibble::tibble("id" = 1:n,
                               "strata" = strata)
  
  rsample::vfold_cv(dummy_data,
                    v = v,
                    strata = strata)  
}

get_splits_loo_cv <- function(n){
  dummy_data <- tibble::tibble("id" = 1:n)
  
  rsample::loo_cv(dummy_data)  
}



get_test_ids <- function(split){
  rsample::assessment(split)$id
}

kfold_results <- function(split,
                          y,
                          X,
                          models = c("nb_normal", "nb_kde", "nb_discrete", "nb_fgld", "lda", "logistic", "knn", "svm")) {
  
  test_ids <- get_test_ids(split)
  
  y_train <- y[-test_ids]
  y_test <-  y[test_ids]
  X_train <- as.data.frame(X[-test_ids, , drop = FALSE])
  X_test <-  as.data.frame(X[test_ids, , drop = FALSE])
  
  out_train <- list()
  out_test <- list()
  
  if("lda" %in% models){
    out_lda <- MASS::lda(grouping = y_train, x = X_train)
    out_train[["lda"]] <- MASS:::predict.lda(object = out_lda)
    out_test[["lda"]] <- predict(object = out_lda, newdata = X_test)$class
  }
  
  if("knn" %in% models){
    out_train[["knn"]] <- class::knn(train = X_train, test = X_train, cl = y_train, k = 3)
    out_test[["knn"]] <- class::knn(train = X_train, test = X_test, cl = y_train, k = 3)
  }
  
  if("nb_fgld" %in% models){
    out_nb_fgld <- naive_bayes(y = y_train, X = X_train, distr = "fgld")
    out_train[["nb_fgld"]] <- out_nb_fgld$class
    out_test[["nb_fgld"]] <- predict_naive_bayes(out = out_nb_fgld, X = X_test)$class
  }
  
  if("nb_normal" %in% models){
    out_nb_normal <- naive_bayes(y = y_train, X = X_train, distr = "normal")
    out_train[["nb_normal"]] <- out_nb_normal$class
    out_test[["nb_normal"]] <- predict_naive_bayes(out_nb_normal, X = X_test)$class
  }
  
  if("nb_kde" %in% models){
    out_nb_normal <- naive_bayes(y = y_train, X = X_train, distr = "kde")
    out_train[["nb_kde"]] <- out_nb_normal$class
    out_test[["nb_kde"]] <- predict_naive_bayes(out_nb_normal, X = X_test)$class
  }
  
  if("nb_discrete" %in% models){
    out_nb_normal <- naive_bayes(y = y_train, X = X_train, distr = "discrete")
    out_train[["nb_discrete"]] <- out_nb_normal$class
    out_test[["nb_discrete"]] <- predict_naive_bayes(out_nb_normal, X = X_test)$class
  }
  
  if("logistic" %in% models){
    get_class <- function(predicted_probs, levels){
      ifelse(predicted_probs > 0.5, 2, 1) |> 
        factor(levels = 1:2, labels = levels)
    }
    out_logistic <- glm(formula = y ~ .,
                        data = as.data.frame(cbind("y" = y_train, X_train)), family = "binomial")
    out_train[["logistic"]] <- predict(object = out_logistic, type = "response") |> 
      get_class(levels = levels(y))
    out_test[["logistic"]] <- predict(object = out_logistic, newdata = X_test, type = "response") |> 
      get_class(levels = levels(y))
    
  }
  
  if("svm" %in% models){
    out_svm <- e1071::svm(x = X_train, y = y_train)
    out_train[["svm"]] <- predict(out_svm)
    out_test[["svm"]] <- predict(out_svm, newdata = X_test)
  }
  
  return(list("train" = map_dbl(out_train, ~ yardstick::accuracy_vec(truth = y_train, estimate = .x)),
              "test" = map_dbl(out_test, ~ yardstick::accuracy_vec(truth = y_test, estimate = .x))))
}
