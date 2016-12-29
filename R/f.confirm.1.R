
##' @title Confirm Stage Analyses on a Single Dataset
##'
##' @description This function analyzes a single dataset based on the results from the learn stage, and prepares quantities for
##' the subsequent learn/confirm analysis.
##'
##' @param learn.obj an R object from the function output of \code{\link{f.learn.1}}
##' @param ac.alpha alpha level assigned to (two-stage) all-comer analysis
##' @param sg.alpha alpha level assigned to (two-stage) sensitive group analysis
##' @param verbose (logical) whether to print out results for each trial (default: FALSE)
##'
##' @return a list of three numericals showing all-comer-only (one-stage) analysis, two-stage all-comer analysis and
##' two-stage subgroup analysis significance (1) or non-significance (0)
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
##' @seealso
##' \code{\link{f.learn.1}} for learn stage analysis for a single dataset
##' \code{\link{f.learn.confirm}} for the main function of learn/confirm analysis (taking on a mega dataset)
##'
##' @export
##'
f.confirm.1 = function(learn.obj, ac.alpha=0.04, sg.alpha=0.01, verbose=FALSE) {

  confirm.alpha = ac.alpha + sg.alpha
  # all-comer dataset (for a given allocation)
  ac.data = rbind(learn.obj$data.learn.cont, learn.obj$data.confirm.cont)
  cf.data = learn.obj$data.confirm.cont

  # extract key quantities in learn.obj
  x.term = learn.obj$x.ms
  learn.cutoff = learn.obj$learn.cutoff

  # different dataset column names for marker predictiveness
  mk.pred = colnames(ac.data[ , grepl(glob2rx("y*") , colnames(ac.data))])

  # pre-allocate result vectors
  sig.res.aco = sig.res.ac = sig.res.sg = -1

  # all-comer-only (aco) ONE-STAGE analysis
  if (verbose) {
    message(paste0("No marker identified at learning stage: will use alpha level of ",
                   confirm.alpha, " for all subjects in the confirm stage."))
  }
  fit.cox.aco = coxph(as.formula(paste0("Surv(", mk.pred[1], ", ", mk.pred[2], ") ~ trt")),
                      data=ac.data, method="breslow")
  p.aco = summary(fit.cox.aco)$coefficients[1, "Pr(>|z|)"]
  sig.res.aco = ifelse(p.aco < confirm.alpha, 1, 0)

  # TWO-STAGE: both all-comer and sensitive group analyses
  if (verbose) {
    message(paste0("Marker ", x.term,
                   " is identified at learning stage: will use alpha level of ",
                   sg.alpha, " for sensitive subjects, and use alpha level of ",
                   ac.alpha, " for all subjects in the confirm stage."))
  }
  # all-comer analysis
  fit.cox.ac = coxph(as.formula(paste0("Surv(", mk.pred[1], ", ", mk.pred[2], ") ~ trt")),
                     data=ac.data, method="breslow")
  p.ac = summary(fit.cox.ac)$coefficients[1, "Pr(>|z|)"]
  sig.res.ac = ifelse(p.ac < ac.alpha, 1, 0)

  # identify sensitive subjects among confirm patients (NOT ALL COMERS!!!)
  sg.data = cf.data[cf.data[ ,x.term] < learn.cutoff, ]
  fit.cox.sg = coxph(as.formula(paste0("Surv(", mk.pred[1], ", ", mk.pred[2], ") ~ trt")),
                     data=sg.data, method="breslow")
  p.sg = summary(fit.cox.sg)$coefficients[1, "Pr(>|z|)"]
  sig.res.sg = ifelse(p.sg < sg.alpha, 1, 0)

  res = list(sig.res.aco = sig.res.aco,
             sig.res.ac = sig.res.ac,
             sig.res.sg = sig.res.sg)
  return(res)

}

# test
# fc.obj = f.confirm.1(learn.obj = fl.obj, ac.alpha=0.04, sg.alpha=0.01, verbose=FALSE)
# fc.obj

