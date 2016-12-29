
##' @title Learning Stage Analyses on a Single Dataset
##'
##' @description This function analyzes a single dataset and prepares quantities for the subsequent confirm stage analysis.
##'
##' @param data.1 an individual dataset consisting of all variables needed in an ASD
##' @param learn.allocation percentage of all subjects allocated to the learn stage: specify 30 means
##' 30\% of subjects are allocated to the learn stage, while 70\% of subjects to the confirm stage
##' @param cutoff biomarker cutoff value for learn-stage dataset: a numerical value
##' @param side how to define sensitive subgroup using the cutoff value. Default is "LT" (less than),
##' indicating subjects in the subgroup all have biomarker value < cutoff. Alternatively, "GT" (greater than),
##' "LE" (less or equal to) or "GE" (greater or equal to) can be specified.
##'
##' @return a list of intermediate data results to be used for subsequent confirm stage analysis
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
##' @seealso
##' \code{\link{f.confirm.1}} for confirm stage analysis for a single dataset
##' \code{\link{f.learn.confirm}} for the main function of learn/confirm analysis
##'
##' @export
##'
f.learn.1 = function(data.1, learn.allocation=30, cutoff=0.4, side="LT") {

  # marker data frame (both learn and confirm, continuous marker)
  X.mat = data.1[ , grepl(glob2rx("x*") , colnames(data.1))]
  x.terms = colnames(X.mat)     # x1, x2, x3, etc.
  # number of markers
  n.X = dim(X.mat)[2]

  # different dataset column names for marker predictiveness
  mk.pred = colnames(data.1[ , grepl(glob2rx("y*") , colnames(data.1))])

  # construct two subsets based on learn/confirm allocations (continuous marker)
  allo.col = paste0("ME_", learn.allocation, "_flag")
  new.data = tbl_df(cbind(select_(data.1, quote(dataset), quote(id), quote(trt),
                                  mk.pred[1],
                                  mk.pred[2],
                                  allo.col), X.mat))  # including both learn/confirm subjects
  data.learn.cont = new.data[new.data[ ,allo.col] == 1, ]
  data.confirm.cont = new.data[new.data[ ,allo.col] == 0, ]

  # convert X matrix from continuous covariates (X.mat) to indicators by fixed quantiles
  # FOR LEARN POPULATION ONLY -- result: X.learn.cont
  # X.ind.learn: a matrix of 0/1 for LEARN POPULATION ONLY (by fixed quantiles)
  X.learn.cont = data.learn.cont[data.learn.cont[allo.col] == 1, x.terms]
  # convert to indicators for X (learn pop ONLY)
  X.ind.learn = switch(side,
                       LT = ifelse(X.learn.cont < cutoff, 1, 0),
                       GT = ifelse(X.learn.cont > cutoff, 1, 0),
                       LE = ifelse(X.learn.cont <= cutoff, 1, 0),
                       GE = ifelse(X.learn.cont >= cutoff, 1, 0))
  X.ind.learn = tbl_df(as.data.frame(X.ind.learn))

  # full learning dataset with 0/1 for markers (to be used in Cox model)
  data.learn.ind = tbl_df(cbind(data.learn.cont[ ,!colnames(data.learn.cont) %in% x.terms],
                                X.ind.learn))

  # for data.learn.ind ONLY:
  # fit Cox regression model with trt, x_i, and trt*x_i (for i=1,2,3, separately)
  interaction.p = numeric(n.X)  # store trt:x_i interaction p-value
  for (i in 1:n.X) {
    # fix Cox model for each x_i
    fit.cox = coxph(as.formula(paste0("Surv(", mk.pred[1], ", ", mk.pred[2], ") ~ trt*",
                                      x.terms[i])), data=data.learn.ind, method="breslow")
    # extract interaction p-values
    interaction.p[i] = summary(fit.cox)$coefficients[3, "Pr(>|z|)"]
  }
  # smallest p-value index (which x_i has the most significant interaction w/ trt?)
  smallest.p.idx = which(interaction.p == min(interaction.p))
  x.ms = x.terms[smallest.p.idx]

  learn.res = list(x.ms = x.ms,
                   learn.cutoff = cutoff,
                   data.learn.cont = data.learn.cont,
                   data.learn.ind = data.learn.ind,
                   data.confirm.cont = data.confirm.cont)
  return(learn.res)

}

# test
# data.1 = data.simASD[data.simASD$dataset==4, ]
# fl.obj = f.learn.1(data.1, learn.allocation=30, cutoff=0.4, side="LT")
# fl.obj
