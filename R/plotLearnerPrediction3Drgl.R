#' @title Visualizes a learning algorithm on a 3D data set.
#'
#' @description
#' Trains the model for 1 or 2 selected features, then displays it via \code{\link[ggplot2]{ggplot}}.
#' Good for teaching or exploring models.
#'
#' For classification and clustering, only 2D plots are supported. The data points, the classification and
#' potentially through color alpha blending the posterior probabilities are shown.
#'
#' For regression, 1D and 2D plots are supported. 1D shows the data, the estimated mean and potentially
#' the estimated standard error. 2D does not show estimated standard error,
#' but only the estimated mean via background color.
#'
#' The plot title displays the model id, its parameters, the training performance
#' and the cross-validation performance.
#'
#' @template arg_learner
#' @template arg_task
#' @param features [\code{character}]\cr
#'   Selected features for model.
#'   By default the first 2 features are used.
#' @template arg_measures
#' @param cv [\code{integer(1)}]\cr
#'   Do cross-validation and display in plot title?
#'   Number of folds. 0 means no CV.
#'   Default is 10.
#' @param ... [any]\cr
#'   Parameters for \code{learner}.
#' @param gridsize [\code{integer(1)}]\cr
#'   Grid resolution per axis for background predictions.
#'   Default is 500 for 1D and 100 for 2D.
#' @param pointsize [\code{numeric(1)}]\cr
#'   Pointsize for ggplot2 \code{\link[ggplot2]{geom_point}} for data points.
#'   Default is 2.
#' @param prob.alpha [\code{logical(1)}]\cr
#'   For classification: Set alpha value of background to probability for
#'   predicted class? Allows visualization of \dQuote{confidence} for prediction.
#'   If not, only a constant color is displayed in the background for the predicted label.
#'   Default is \code{TRUE}.
#' @param se.band [\code{logical(1)}]\cr
#'   For regression in 1D: Show band for standard error estimation?
#'   Default is \code{TRUE}.
#' @param err.mark [\code{character(1)}]:
#'   For classification: Either mark error of the model on the training data (\dQuote{train}) or
#'   during cross-validation (\dQuote{cv}) or not at all with \dQuote{none}.
#'   Default is \dQuote{train}.
#' @param bg.cols [\code{character(3)}]\cr
#'   Background colors for classification and regression.
#'   Sorted from low, medium to high.
#'   Default is \code{TRUE}.
#' @param err.col [\code{character(1)}]\cr
#'   For classification: Color of misclassified data points.
#'   Default is \dQuote{white}
#' @param err.size [\code{integer(1)}]\cr
#'   For classification: Size of misclassified data points.
#'   Default is \code{pointsize}.
#' @param greyscale [\code{logical(1)}]\cr
#'   Should the plot be greyscale completely?
#'   Default is \code{FALSE}.
#' @template arg_prettynames
#' @return The plot3Drgl object.
#' @importFrom plot3Drgl scatter3Drgl persp3Drgl
#' @export
plotLearnerPrediction3Drgl = function(learner, task, features = NULL, measures, cv = 10L,  ...,
                                      gridsize, pointsize = 2,
                                      prob.alpha = TRUE, se.band = TRUE,
                                      err.mark = "train",
                                      posClass = NULL) {

  learner = checkLearner(learner)
  assert(
    checkClass(task, "ClassifTask"),
    checkClass(task, "RegrTask")
  )
  td = getTaskDesc(task)

  # features and dimensionality
  fns = getTaskFeatureNames(task)
  if (is.null(features) && td$type == "regr") {
    # take first 2 features for regr.task as default
    features = if (length(fns) == 1L) stop("Min. 2 features") else fns[1:2]
  } else if (is.null(features) && td$type == "classif" && length(td$class.levels) == 2L) {
    # take first 2 features for binary classif.task as default
    features = if (length(fns) == 1L) stop("Min. 2 features") else fns[1:2]
  } else if (is.null(features) && td$type == "classif") {
    # take first 3 features for classif.task as default
    features = if (length(fns) == 1L) stop("Min. 2 features") else fns[1:3]
  } else {
    assertCharacter(features, max.len = 3L)
    assertSubset(features, choices = fns)
  }
  taskdim = length(features)
  if (td$type == "classif" && taskdim %nin% c(2L, 3L))
    stopf("Classification and clustering: currently only 3D plots supported, not: %i", taskdim)
  if (td$type == "regr" && taskdim != 2L)
    stopf("Regression: currently only 2D plots supported, not: %i", taskdim)

  measures = checkMeasures(measures, task)
  cv = asCount(cv)

  if (missing(gridsize)) {
    gridsize = ifelse(taskdim == 2L, 100, 50)
  } else {
    gridsize = asCount(gridsize)
  }
  assertNumber(pointsize, lower = 0)
  assertFlag(prob.alpha)
  assertFlag(se.band)
  assertChoice(err.mark, choices = c("train", "cv", "none"))

  if (td$type == "classif" && err.mark == "cv" && cv == 0L)
    stopf("Classification: CV must be switched on, with 'cv' > 0, for err.type = 'cv'!")

  # subset to features, set hyperpars
  task = subsetTask(task, features = features)
  learner = setHyperPars(learner, ...)

  # some shortcut names
  target = td$target
  data = getTaskData(task)
  y = getTaskTargets(task)
  x1n = features[1L]
  x1 = data[, x1n]
  x2n = features[2L]
  x2 = data[, x2n]
  if (td$type == "classif" && taskdim == 3L) {
    x3n = features[3L]
    x3 = data[, x3n]
  }

  # predictions
  if (td$type == "classif" && hasLearnerProperties(learner, "prob"))
    learner = setPredictType(learner, "prob")
  mod = train(learner, task)
  pred.train = predict(mod, task)
  yhat = pred.train$data$response
  perf.train = performance(pred.train, task = task, measures = measures)
  if (cv > 0L) {
    cv = crossval(learner, task, iters = 10L, measures = measures, show.info = FALSE)
    perf.cv = cv$aggr
    pred.cv = cv$pred
  } else {
    perf.cv = NA_real_
  }

  # grid for predictions
  if (taskdim == 2L) {
    # setup data frames for ggplot. grid = background, data = points
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
  } else if (taskdim == 3L) {
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize),
      seq(min(x3), max(x3), length.out = gridsize)
    )
  }
  colnames(grid) = features
  pred.grid = predict(mod, newdata = grid)
  grid[, target] = pred.grid$data$response

  if (td$type == "classif") {
    data$.err = if (err.mark == "train")
      y != yhat
    else if (err.mark == "cv")
      y != pred.cv$data[order(pred.cv$data$id), "response"]
    if (taskdim == 2L) {
      cdata = cbind(pred.grid, grid)

      if (is.null(posClass)) {
        if (is.na(task$task.desc$positive))
          posClass = levels(data[, target])[1]
        else
          posClass = task$task.desc$positive
      } else
        posClass = posClass

      grid.dcast = data.table::dcast(cdata, as.formula(stri_paste(x1n, x2n, sep = "~")),
                                     value.var = stri_paste("prob.", posClass))
      grid.3d = list(x = grid.dcast[,1],
                     y = as.numeric(colnames(grid.dcast)[-1]),
                     z = t(as.matrix(grid.dcast[,-1])))

      data$.z = 0

      scatter3Drgl(x = data[, 1], y = data[, 2], z = data$.z,
                   zlim = c(0, 1),
                   xlab = x1n, ylab = x2n, zlab = "f",
                   ticktype = "detailed",
                   colvar = as.integer(data[, 3]),
                   col = palette()[2:3],
                   colkey = list(at = c(1, 2),
                                 addlines = TRUE,
                                 labels = td$class.levels))
      persp3Drgl(x = grid.3d$x,
                 y = grid.3d$y,
                 z = grid.3d$z,
                 add = TRUE)
    } else if (taskdim == 3L) {
      # reshape grid data
      grid.dcast = data.table::dcast(grid,
                                     as.formula(stri_paste(stri_paste(x3n, x1n, sep = "+"),
                                                           x2n, sep = "~")),
                                     value.var = target)
      # get boundary points
      x1seq = unique(grid[, 1])
      x3seq = unique(grid[, 3])
      boundary = NULL
      for (i in 1L:(length(x3seq) - 1)){
        boundaryMat = grid.dcast[grid.dcast[, 1] == x3seq[i],] !=
          grid.dcast[grid.dcast[, 1] == x3seq[i + 1],]
        boundaryMat[, 1] = x3seq[i]
        boundaryMat[, 2] = x1seq
        boundaryDf = as.data.frame(boundaryMat)
        boundaryMelt = reshape2::melt(boundaryDf, id = colnames(boundaryDf)[1:2])

        boundary = rbind(boundary, boundaryMelt[boundaryMelt$value == 1, ])
      }
      boundary$variable = as.numeric(as.character(boundary$variable))
      # shift the z axis of boundary above
      boundary[, 1] = boundary[, 1] + diff(x3seq)[1]/2

      # plot data points without error
      scatter3Drgl(x = data[!data$.err, 1],
                   y = data[!data$.err, 2],
                   z = data[!data$.err, 3],
                   xlab = x1n, ylab = x2n, zlab = target,
                   colvar = as.integer(data[!data$.err, 4]),
                   col = palette()[2L:(length(td$class.levels) + 1)],
                   ticktype = "detailed",
                   colkey = list(at = 1L:length(td$class.levels),
                                 addlines = TRUE,
                                 labels = td$class.levels))
      # plot error points
      scatter3Drgl(x = data[data$.err, 1],
                   y = data[data$.err, 2],
                   z = data[data$.err, 3],
                   col = "black",
                   add = TRUE)
      # plot boundary points
      scatter3Drgl(x = boundary$Sepal.Length,
                   y = boundary$variable,
                   z = boundary$Petal.Length,
                   add = TRUE)
    }
  } else if (td$type == "regr") {
    # reform grid data
    grid.dcast = data.table::dcast(grid, as.formula(stri_paste(x1n, x2n, sep = "~")), value.var = target)
    # generate 3D plots data list
    grid.3d = list(x = grid.dcast[,1],
                   y = as.numeric(colnames(grid.dcast)[-1]),
                   z = as.matrix(grid.dcast[,-1]))
    # generate scatter plot
    scatter3Drgl(x = data[, 1], y = data[, 2], z = data[, 3],
                 xlab = x1n, ylab = x2n, zlab = target,
                 ticktype = "detailed", clab = target)

    persp3Drgl(x = grid.3d$x, y = grid.3d$y, z = grid.3d$z, add = T)
  }

  # # set title
  # if (pretty.names) {
  #   lrn.str = getLearnerShortName(learner)
  # } else {
  #   lrn.str = getLearnerId(learner)
  # }
  # title = sprintf("%s: %s", lrn.str, paramValueToString(learner$par.set, learner$par.vals))
  # title = sprintf("%s\nTrain: %s; CV: %s", title, perfsToString(perf.train), perfsToString(perf.cv))
  # p = p + ggtitle(title)
  #
  # # deal with greyscale
  # if (greyscale) {
  #   p = p + scale_fill_grey()
  # }
  # return(p)
}
