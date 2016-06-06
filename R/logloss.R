#' Function to calculate the Log Loss Measure
#'
#' @param task task
#' @param model model
#' @param pred pred
#' @param feats feats
#' @param extra.args extra.args
#'
#' @importFrom Metrics logLoss
#'
logLoss <- function(task, model, pred, feats, extra.args) {
  truth <- as.numeric(as.character(getPredictionTruth(pred)))
  probs <- getPredictionProbabilities(pred)
  eps <- 1e-15
  probs <- ifelse(probs < eps, eps, probs)
  probs <- ifelse(probs > 1 - eps, 1 - eps, probs)
  Metrics::logLoss(truth, probs)
}

#' Log Loss mlr custom measure
#'
#' @examples
#'
#' ## Define tasks
#' iris$Species <- as.integer(iris$Species == "virginica")
#' lrn = makeLearner("classif.nnet", predict.type = "prob")
#' iris.task <- makeClassifTask(data = iris, target = "Species")
#' ## Train a learner and make predictions
#' mod = train(learner = lrn, iris.task)
#' pred = predict(mod, task = iris.task)
#' ## Calculate the performance using the new measure
#' performance(pred, measures = log.loss)
#'
#' @importFrom mlr makeMeasure
#'
#' @export
log.loss <- mlr::makeMeasure(
  id = "log.loss", name = "Mean Logloss",
  properties = c("classif", "req.pred", "req.truth"),
  minimize = TRUE, best = 0, worst = Inf,
  fun = logLoss
)
