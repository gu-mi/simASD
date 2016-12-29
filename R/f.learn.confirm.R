
##' @title Main Function for Learn-Confirm Analysis
##'
##' @description This function takes a mega dataset and returns test significances for one-stage design, two-stage design
##' (all-comer and sensitive group). This is in preparation for function \code{\link{f.pwr.cal}} where multiple combinations
##' are considered.
##'
##' @param data pass a mega dataset (including multiple individual datasets)
##' @param learn.allocation percentage of all subjects allocated to the learn stage
##' @param ac.alpha alpha level assigned to (two-stage) all-comer analysis
##' @param sg.alpha alpha level assigned to (two-stage) sensitive group analysis
##' @param verbose (logical) whether to print out results for each trial (default: FALSE)
##'
##' @return a list of three vectors for indicating trial results, and other quantities for the integrated power analyses
##' performed by the function \code{\link{f.pwr.cal}}
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
##' @seealso
##' \code{\link{f.learn.1}} for learn stage analysis for a single dataset
##' \code{\link{f.confirm.1}} for confirm stage analysis for a single dataset
##'
##' @export
##'
f.learn.confirm = function(data,
                           learn.allocation=30, cutoff=0.4,
                           ac.alpha = 0.04, sg.alpha = 0.01, verbose = FALSE) {

  # total number of datasets
  total.n.dat = max(data$dataset)

  # vectors for results (trial significant or not indicator)
  sig.res.aco.vec = sig.res.ac.vec = sig.res.sg.vec = rep(-1, total.n.dat)

  # loop by each individual dataset
  for (i in seq_len(total.n.dat)) {

    # extract each single dataset (including both learn/confirm)
    data.1 = data[data$dataset == i, ]

    # call f.learn.1() to perform analyses on a single dataset (learning stage)
    learn.obj = f.learn.1(data.1 = data.1,
                          learn.allocation = learn.allocation,
                          cutoff = cutoff)

    # call f.confirm.1() to perform analyses on a single dataset (confirm stage)
    confirm.obj = f.confirm.1(learn.obj = learn.obj, ac.alpha = ac.alpha, sg.alpha = sg.alpha,
                              verbose = verbose)

    # results
    sig.res.aco.vec[i] = confirm.obj$sig.res.aco
    sig.res.ac.vec[i] = confirm.obj$sig.res.ac
    sig.res.sg.vec[i] = confirm.obj$sig.res.sg
  }

  res = list(aconly = sig.res.aco.vec,
             two.stage.ac = sig.res.ac.vec,
             two.stage.sg = sig.res.sg.vec,
             data = data,
             learn.allocation = learn.allocation,
             cutoff = cutoff,
             ac.alpha = ac.alpha,
             sg.alpha = sg.alpha)
  return(res)
}

# test
# flc.obj = f.learn.confirm(data.simASD, learn.allocation=30, cutoff=0.4, ac.alpha = 0.04, sg.alpha = 0.01, verbose = FALSE)
# flc.obj
