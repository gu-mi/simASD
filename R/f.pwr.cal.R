
##' @title Power Calculations based on Different Parameter Combinations
##'
##' @description This function takes on a mega dataset, candidate learn/confirm allocations, biomarker cutoff value,
##' candidate alpha levels for all-comer analysis and sensitive group analysis. Please note that this function takes
##' time to run as different combinations are considered. Suggest to run this function separately and store the results
##' locally. Plotting (by function \code{\link{plot_simASD}} will directly utilize the result returned by this function.
##'
##' @param data the mega dataset consisting of many individual datasets of the same number of subjects and variables
##' @param learn.allocation.v a vector of the percentages for subjects allocated to the learn stage
##' @param cutoff biomarker cutoff value for learn-stage dataset: a numerical value
##' @param ac.alpha.v a vector of alpha levels assigned to all-comer analysis
##' @param sg.alpha.v a vector of alpha levels assigned to sensitive group analysis
##'
##' @return a list of objects containing key information for plotting by function \code{\link{plot_simASD}}
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
##' @seealso
##' \code{\link{f.learn.1}} for learn stage analysis for a single dataset
##' \code{\link{f.confirm.1}} for confirm stage analysis for a single dataset
##' \code{\link{f.learn.confirm}} for the main function of learn/confirm analysis (taking on a mega dataset)
##' \code{\link{plot_simASD}} for plotting the final results
##'
##' @export
##'
f.pwr.cal = function(data, learn.allocation.v, cutoff, ac.alpha.v, sg.alpha.v) {

  l.allocation.v = length(learn.allocation.v)
  l.ac.alpha.v = length(ac.alpha.v)
  pwr.v = numeric(l.allocation.v*l.ac.alpha.v)
  m = 1

  for (i in 1:l.allocation.v) {
    cat("** Learn Allocation =", learn.allocation.v[i], "** \n")
    for (j in 1:l.ac.alpha.v) {
      cat("* All-comer Alpha =", ac.alpha.v[j], "\n")
      res = f.learn.confirm(data = data,
                            learn.allocation = learn.allocation.v[i],
                            cutoff = cutoff,
                            ac.alpha = ac.alpha.v[j],
                            sg.alpha = 0.05-ac.alpha.v[j])
      pwr.v[m] = pwr.cal(res)
      m = m+1
    }
  }

  # One-stage All-Comer Power calculation (for plotting purpose)
  n.dat = max(data$dataset)
  flco = f.learn.confirm(data = data,
                         learn.allocation = learn.allocation.v[1],
                         cutoff = cutoff,
                         ac.alpha = ac.alpha.v[1] + sg.alpha.v[1],
                         sg.alpha = 0.0)
  power.ord = (sum(flco$aconly[flco$aconly != -1])) / n.dat

  res = list(data = data,
             learn.allocation.v = learn.allocation.v,
             cutoff = cutoff,
             ac.alpha.v = ac.alpha.v,
             pwr.v = pwr.v,
             one.stage.power = power.ord)

  return(res)
}

# test
# fpc.obj = f.pwr.cal(data = data.simASD,
#                     learn.allocation.v = c(30, 40, 50, 60, 70),
#                     cutoff = 0.4,
#                     ac.alpha.v = c(0.025, 0.03, 0.035, 0.04),
#                     sg.alpha.v = 0.05-c(0.025, 0.03, 0.035, 0.04))
# fpc.obj
