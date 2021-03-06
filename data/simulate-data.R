library(magrittr)
simulate_data <- function(n){
  X <- data.frame(matrix(runif(n*10), ncol = 10))
  Y <- data.frame(Y = rbinom(n, size = 1, prob = apply(X, 1, sum) %>%
                               pnorm(mean = 5)
  ) %>%
    as.factor()

  )
  dplyr::bind_cols(X, Y)
}
set.seed(1)
df <- simulate_data(10000)
saveRDS(df, "data/df.rds")


df_classif_task <- mlr::makeClassifTask(id = "df.classif.task", data = df, target = "Y",
                                   check.data = TRUE)
saveRDS(df_classif_task, file = "data/df_classif_task.rds")
