
##' @title Plot Empirical Powers vs. Learn/Confirm Allocations under Different Combinations of ASD Parameters
##'
##' @description This function takes an object from the return result of function \code{\link{f.pwr.cal}}, and makes
##' a ggplot2 object showing the empirical powers vs. learn/confirm allocations under different alpha-splitting scenarios.
##' The one-stage design power is also plotted as a horizontal dashed line for comparison.
##'
##' @param fpc.obj an object from the returned result of function \code{\link{f.pwr.cal}}
##'
##' @return a ggplot2 object with empirical power vs. learn/confirm allocation plot, with one-stage design power as a
##' horizontal dashed line for comparison
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
##' @seealso
##' \code{\link{f.learn.confirm}} for the main function of learn/confirm analysis (taking on a mega dataset)
##' \code{\link{f.pwr.cal}} for calculating powers based on a mega dataset
##'
##' @export
##'
plot_simASD = function(fpc.obj) {

  learn.allocation.v = fpc.obj$learn.allocation.v
  cutoff = fpc.obj$cutoff
  ac.alpha.v = fpc.obj$ac.alpha.v
  pwr.v = fpc.obj$pwr.v
  one.stage.power = fpc.obj$one.stage.power

  l.alpha = length(ac.alpha.v)
  l.allocation = length(learn.allocation.v)

  allo = rep(paste0(learn.allocation.v, "/", 100-learn.allocation.v), each = l.alpha)
  alpha = rep(ac.alpha.v, l.allocation)

  d1 = tbl_df(data.frame(allo, as.factor(alpha), pwr.v))
  colnames(d1) = c("Allocation", "alpha", "Power")

  gp.obj = ggplot(data = d1, aes(x=Allocation, y=Power,
                                 group=alpha,
                                 linetype=alpha)) +
    scale_linetype_manual(values=seq(1:l.alpha),
                          breaks=unique(as.character(alpha)),
                          labels=unique(as.character(alpha)),
                          name=expression(paste("All-Comer ", alpha))) +
    xlab("Learn/Confirm Sample Size Allocation (%)") +
    ylab("Empirical Power") +
    ylim(c(0,1)) +
    geom_line() + geom_point() + theme_bw() +
    geom_hline(aes(yintercept=one.stage.power), color="blue", linetype="dashed", size=1) +
    annotate("text", label = paste0("One-stage Power = ", one.stage.power), x=-Inf, y=one.stage.power-0.05,
             hjust=0, vjust=0, size = 4, colour = "blue")

  return(gp.obj)

}

# test
# plot_simASD(fpc.obj)
