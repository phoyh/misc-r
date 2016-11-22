dx <- 0.01
nx <- seq(0,1,by=dx)
np <- rep(1,1+(1/dx))
update <- function(isCoop) {
  result <- np
  area <- 0
  for (i in 1:length(nx)) {
    if (isCoop) {
      result[i] <- np[i] * nx[i]
    } else {
      result[i] <- np[i] * (1 - nx[i])
    }
    area <- area + result[i] * dx
  }
  for (i in 1:length(nx)) {
    result[i] <- result[i] / area
  }
  return (result)
}