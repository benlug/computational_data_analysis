shadeNV <- function(M=0, SD=1, Z=1.96, 
                    lb=M-Z*SD, ub=M+Z*SD, 
                    shade = c("inner", "outer", "lower", "upper"),
                    C="skyblue", ...) {
  mymin = M-4*SD; mymax = M+4*SD
  xvals <- seq(mymin, mymax, length=100000)
  dens <- dnorm(x = xvals, M, SD)
  curve(dnorm(x, M, SD), xlim = c(M-4*SD, M+4*SD), type="l", 
        xaxs="i", yaxs="i", 
        bty="n", yaxt="n", 
        ylab="", ... )
  if (shade == "lower") {
    area <- xvals >= mymin & xvals <= ub
    polygon(x = c(mymin, xvals[area], ub), 
            y = c(0, dens[area], 0), col = C)
    pro <- pnorm(ub, M, SD)
    return(pro)
  }
  if (shade == "upper") {
    area <- xvals <= mymax & xvals >= lb
    polygon(x = c(lb, xvals[area], mymax), 
            y = c(0, dens[area], 0), col = C)
    pro <- pnorm(lb, M, SD, lower.tail = F)
    return(pro)
  }
  if (shade == "inner") {
    area <- xvals >= lb & xvals <= ub
    polygon(c(lb, xvals[area], ub), 
            c(0, dens[area], 0), col = C)
    pro <- pnorm(ub, M, SD) - pnorm(lb, M, SD)
    return(pro)
    }
  if (shade == "outer") {
    area1 <- xvals <= lb 
    area2 <- xvals >= ub
    polygon(c(xvals[area1], lb), 
            c(dens[area1], 0), col = C)
    polygon(c(ub, xvals[area2]), 
            c(0, dens[area2]), col = C)
    pro1 <- pnorm(lb, M, SD)
    pro2 <- pnorm(ub, M, SD, lower=FALSE)
    return(c(pro1, pro2))
  }
  }