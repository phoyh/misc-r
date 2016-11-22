aovAll <- function(df, alpha=NULL) {
  cols <- colnames(df)
  factors <- c()
  metrics <- c()
  for (i in 1:length(cols)) {
    colName <- cols[i]
    col <- df[,i]
    if (is.factor(col)) {
      factors <- c(factors, colName)
    }
    if (is.numeric(col) | is.integer(col)) {
      metrics <- c(metrics, colName)
    }
  }
  result <- data.frame(metrics=metrics)
  for (f in 1:length(factors)) {
    pLevels <- c()
    fName <- factors[f]
    for (m in 1:length(metrics)) {
      mName <- metrics[m]
      form <- paste(mName, "~", fName)
      aovResult <- aov(as.formula(form), data=df)
      pLevel <- unlist(summary(aovResult))[["Pr(>F)1"]]
      pLevel <- convertSignificanceLevel(pLevel, alpha)
      pLevels <- c(pLevels, pLevel)
    }
    result[[fName]] <- pLevels
  }
  return (result)
}

tAll <- function(df, alpha=NULL, restrictToMetrics=NULL) {
  cols <- colnames(df)
  if (!is.null(restrictToMetrics)) {
    cols <- restrictToMetrics
  }
  metrics <- c()
  for (i in 1:length(cols)) {
    colName <- cols[i]
    col <- df[[colName]]
    if (is.numeric(col) | is.integer(col)) {
      metrics <- c(metrics, colName)
    }
  }
  pValues <- c()
  for (smaller in 1:length(metrics)) {
    for (larger in 1:length(metrics)) {
      x <- df[[metrics[smaller]]]
      y <- df[[metrics[larger]]]
      n <- length(x)
      m <- length(y)
      del <- mean(x) - mean(y)
      variance <- (sum((x - mean(x))^2) + sum((y - mean(y))^2)) *
                  (1 / n + 1 / m) / 
                  (n + m - 2)
      t <- del / sqrt(variance)
      p <- pt(t, n + m - 2)
      p <- convertSignificanceLevel(p, alpha)
      pValues <- c(pValues, p)
    }
  }
  matr <- matrix(pValues, ncol=length(metrics))
  colnames(matr) <- metrics
  rownames(matr) <- metrics
  result <- as.table(matr)
  return (result)
}

convertSignificanceLevel <- function(p, alpha) {
  if (!is.null(alpha)) {
    if (p < alpha) {
      p <- "*"
    } else {
      p <- "."
    }
  }
  return (p)
}

stats <- list()
stats$aovAll <- aovAll
stats$tAll <- tAll