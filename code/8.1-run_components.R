library(dbarts)
library(MASS)
library(purrr)
library(optparse)
library(here)
library(onehot)

option_list = list(
  make_option(c("-d", "--dgp"), type="character", default="sum",
              help="data generating process", metavar="character"),
  make_option(c("-r", "--runs"), type="integer", default=8,
              help="number of runs", metavar="integer")
);

get.labels <- function(data){
  cols <- ncol(data)
  return(data[, cols])
  # return(data %>% dplyr::select(last_col()) %>% .[, 1])
}

get.features <- function(data){
  feature_cols <- ncol(data) - 1
  return(data[, 1:feature_cols])
  # return(data %>% dplyr::select(-last_col()))
}

.generate_data <- function(dgp, n, q, rho){
  if (dgp == "sum"){
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- rowSums(X[, 1:q])
  } else if (dgp == "highint") {
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- X[,1]*X[,2] + X[,3]*X[,4] + X[,5]*X[,6]
  } else if (dgp == "lowint") {
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- X[,1]*X[,2]
  } else if (dgp == "linear_add") {
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- .2*X[,1]-X[,2]+.6*X[,3]-.9*X[,4]+.85*X[,5]
  } else if (dgp == "smooth_add") {
    cov_q <- diag(1, q)
    cov_q_q <- diag(rho, q)
    cov_matrix <- cbind(rbind(cov_q, cov_q_q), rbind(cov_q_q, cov_q))
    X <- mvrnorm(n = n, mu = rep(0, 2*q), Sigma = cov_matrix)
    y <- .2*(X[,1])^(2)-(X[,2])+.6*cos(X[,3])-.9*sqrt(abs(X[,4]))+.85*sin(X[,5])
  } else if (dgp == "tree_add") {
    X <- data.frame(matrix(rnorm(1000*10), ncol = 10))
    
    tree_1 <- loadForestry("code/saved_tree1.RDS")
    p_1 <- predict(tree_1, newdata = data.frame(V1 = X[,1]))
    tree_2 <- loadForestry("code/saved_tree2.RDS")
    p_2 <- predict(tree_1, newdata = data.frame(V1 = X[,2]))
    tree_3 <- loadForestry("code/saved_tree3.RDS")
    p_3 <- predict(tree_1, newdata = data.frame(V1 = X[,3]))
    tree_4 <- loadForestry("code/saved_tree4.RDS")
    p_4 <- predict(tree_1, newdata = data.frame(V1 = X[,4]))
    tree_5 <- loadForestry("code/saved_tree5.RDS")
    p_5 <- predict(tree_1, newdata = data.frame(V1 = X[,5]))
    
    y <- p_1 + p_2 + p_3 + p_4 + p_5
  }

  return(list(X=X, y=y))

}

get_data <- function(dgp,n, q=10, rho=0.05, seed=0){

    train_n <- n
    test_n <- 200
    set.seed(seed)

    if ((dgp %in% c("2016","2017","2018","2019"))) {
      data <- read.csv(paste0("data/ACIC",dgp,".csv"))

      # Shuffle the data set, so the train and test set are disjoint
      # but we break any unintended clustering of observations
      data <- data[sample(1:nrow(data), size = train_n+test_n, replace = FALSE),]
      encoding <- onehot::onehot(data, stringsAsFactors = TRUE, max_levels = 100)
      data <- predict(encoding, data)

      if (nrow(data) < train_n+test_n) {
        stop("N is too large for the current dgp")
      }

      data_train <- data[1:train_n,-which(colnames(data) == "Ytrue")]
      data_test <- data[(train_n+1):(train_n+200),-which(colnames(data) == "Y")]

      colnames(data_train)[which(colnames(data_train) == "Y")] <- "y"
      colnames(data_test)[which(colnames(data_test) == "Ytrue")] <- "y"

      data_train <- cbind(data_train[,-which(colnames(data_train) == "y")],
                          data_train[,which(colnames(data_train) == "y")])

      data_test <- cbind(data_test[,-which(colnames(data_test) == "y")],
                         data_test[,which(colnames(data_test) == "y")])


      data_all <- list(train=as.matrix(data_train),
                       test=as.matrix(data_test))
    } else {
      data_train <- .generate_data(dgp, train_n, q, rho)
      data_train$y <- data_train$y + 2 * rnorm(train_n)
      data_test <- .generate_data(dgp, test_n, q, rho)

      data_all <- list(train=cbind(data_train$X, data_train$y),
                       test=cbind(data_test$X, data_test$y))
    }
    return(data_all)
}

bart_sim <- function(dgp, nchain, n, total_ndpost, seed, num_trees_fit){
  nskip <- 1000
  ndpost <- ceiling(total_ndpost / nchain)
  n_tree <- 200
  probs <-c(0.5, 0.1, 0.4,0.5)
  q <- 10
  data_all <- get_data(dgp=dgp, n = n, q = q, rho = 0.01, seed = seed)
  data_train <- data_all$train
  data_test <- data_all$test
  x.train <- get.features(data_train)
  y.train <- get.labels(data_train)
  x.test <- get.features(data_test)
  y.test <- get.labels(data_test)
  names(probs) <- c("birth_death", "change","swap","birth")
  bart_fit <- bart(x.train = x.train,
                   y.train = y.train,
                   x.test = x.test,
                   keeptrees = F,
                   verbose = F,
                   nskip = nskip,
                   keepevery = 1,
                   ntree = num_trees_fit,
                   ndpost = ndpost,
                   nchain = nchain,
                   seed = seed,
                   proposalprobs=probs)

  point_preds_bart <-  bart_fit$yhat.test.mean
  rmse <- sqrt(mean((point_preds_bart-y.test)^2))
  posterior_bart <- bart_fit$yhat.test
  quantiles <- apply(posterior_bart, 2, quantile, probs = c(0.025, 0.975))
  average_length <- mean(quantiles[2, ] - quantiles[1, ])
  average_coverage <- mean(y.test > quantiles[1, ] & y.test < quantiles[2, ])
  return(list(rmse=rmse, coverage=average_coverage, interval_length=average_length))


}

main <- function(args){
  runs <- args$runs
  dgp <- args$dgp
  total_ndpost <- 1000
  column_names <- c("RMSE", "Coverage", "Interval length", "n", "run", "Chains")
  results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(results) <- column_names
  for (n in c(100,1000,10e3,20e3,50e3,100e3)){
        for (run in 1:runs){

    bart_sim_partial <- partial(bart_sim, dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n)

    cur_res = list()
    for (trees in 1:10) {
      bart_1 <- bart_sim(nchain=5,dgp = dgp, total_ndpost=total_ndpost, seed=run, n=n, num_trees_fit=trees)
      cur_res[[trees]] <- c(bart_1$rmse, bart_1$coverage, bart_1$interval_length, n, run, trees)
    }

    combined_results <- rbind(cur_res[[1]], cur_res[[2]], cur_res[[3]],
                              cur_res[[4]], cur_res[[5]], cur_res[[6]],
                              cur_res[[7]], cur_res[[8]], cur_res[[9]],cur_res[[10]])
    results <- rbind(results, combined_results, make.row.names=FALSE)



    }}

  colnames(results) <- column_names
  file_name <- here(file.path("theory_results2", paste("dgp", dgp,"components_analysis.csv", sep = '_')))
  write.csv(results, file_name, row.names = FALSE, sep = ",")
  return(results)



}


if (getOption('run.main', default=TRUE)) {
  parser <- OptionParser(option_list=option_list);
  args <- parse_args(parser);

  print(main(args))
}
