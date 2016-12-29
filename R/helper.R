
##' @title helper functions for getting powers, given a particular learn and confirm object
##'
##' @param f.learn.confirm.obj an object from the function output \code{\link{f.learn.confirm}}
##'
##' @keywords internal
##'
##' @author Gu Mi <neo.migu@gmail.com>
##'
pwr.cal = function(f.learn.confirm.obj) {

  flco = f.learn.confirm.obj
  n.dat = max(flco$data$dataset)

  # ordinary all-comer one stage analysis (trick: set sg.alpha = 0)
  ex.ord = f.learn.confirm(data = flco$data,
                           learn.allocation = flco$learn.allocation,
                           cutoff = flco$cutoff,
                           ac.alpha = flco$ac.alpha + flco$sg.alpha,
                           sg.alpha = 0.0)
  power.ord = (sum(ex.ord$aconly[ex.ord$aconly != -1])) / n.dat

  # ASD analysis
  v.aconly = flco$aconly
  v.two.stage.ac = flco$two.stage.ac
  v.two.stage.sg = flco$two.stage.sg
  v.sig.trial.in.common = v.two.stage.ac + v.two.stage.sg
  n.sig.asd = sum(v.two.stage.ac)+sum(v.two.stage.sg)-sum(v.sig.trial.in.common==2)
  power.asd = n.sig.asd / n.dat

  # AC significant: power
  two.stage.acS.power = sum(v.two.stage.ac) / n.dat
  # AC not significant, but SG significant: power
  two.stage.acNS.sgS.power = sum(v.two.stage.sg[v.two.stage.sg == 1 & v.two.stage.ac == 0]) / n.dat

  res = power.asd

  return(res)
}


