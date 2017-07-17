# Vignette for R Package 'simASD'

In this vignette, we describe the use of the R package `simASD` for performing a simple two-stage adaptive signature design (ASD) based on simulated datasets. For the methodology discussions and more simulations, please refer to the manuscript by Gu Mi, titled **Enhancement of the Adaptive Signature Design for Learning and Confirming in a Single Pivotal Trial**.

## Introduction

We use a simplified simulated "mega" dataset `data.simASD` with step-by-step instructions to illustrate the workflow of evaluating key parameters in an ASD. At the planning stage of any phase III trial, it is recommended that similar simulations be conducted to determine the best parameter specifications which will be prospectively documented in the protocol and/or statistical analysis plan. 

## Simulated Mega Dataset Structure

The mega dataset included in the `simASD` package is a subset of the mega dataset we used in the manuscript. We restrict to one scenario (as an example) where each dataset contains 700 subjects, three biomarkers, and the biomarker predictive effect is the strongest. The first several rows of the dataset is shown below:

```{r, echo=TRUE, eval=TRUE, warning = FALSE, message = FALSE}
library(simASD)
data("data.simASD")
data.simASD
```

As a "mega" dataset, `data.simASD` contains a total of 100 "individual" dataset (for power evaluation), each having 700 subjects (so the total number of rows is 70,000). It has 13 variables explained in more details below:

* **dataset**: an index for the 100 "individual" datasets, each having 700 subjects;
* **id**: unique subject IDs in each dataset;
* **trt**: treatment arm indicator (0 = control; 1 = treatment);
* **y_duration**: survival time (in days);
* **y_censor**: censoring status (0 = censored; 1 = event);
* **x1-x3**: continuous biomarkers generated from Uniform(0,1) distribution;
* **ME_30_flag-ME_70_flag**: indicator (0/1) for learn/confirm allocations (1 means the subject is allocated to the learn stage): column name "30" means 30\% of subjects are in the learn stage, while 70\% are in the confirm stage.

This mega dataset contains the minimal sufficient information to perform power evaluations for a two-stage ASD. Any new simulated mega dataset with identical column names and attributes is directly applicable using the functions in the `simASD` package.


## Power Evaluations

The functions in the `simASD` package are highly modular and can be extended easily for alternative scenarios and specifications. In general, after only two function calls (`f.pwr.cal` and `plot_simASD`) the power evaluation plot of empirical power vs. learn/confirm allocation by different $\alpha$ splitting will be displayed (one-stage power is also shown as a horizontal dashed line):

```{r, echo=TRUE, eval=TRUE, warning = FALSE, message = FALSE, fig.show='asis', fig.width=6, fig.height=4}
ac.alpha = c(0.025, 0.04)  # alpha allocated to all-comer analysis at confirm stage
learn.allo = c(30, 70)     # percentage of subjects allocated to the learn stage
fpc.obj = f.pwr.cal(data = data.simASD,
                    learn.allocation.v = learn.allo,
                    cutoff = 0.4,
                    ac.alpha.v = ac.alpha,
                    sg.alpha.v = 0.05-ac.alpha)
plot_simASD(fpc.obj)       # return a ggplot2 object
```

For simplicity, in this vignette we only included two $\alpha$ splits (0.025/0.025 and 0.04/0.01 for all-comer/sensitive group at confirm stage) and two learn/confirm allocations (30\%/70\% and 70\%/30\%). Multiple values can be specified in a vector to show more combinations, e.g., `ac.alpha = c(0.025, 0.030, 0.035, 0.040)` and `learn.allo = seq(30, 70, by=10)` as we did in the manuscript.


## Resources

* **Mi, G**. (2017). [Enhancement of the Adaptive Signature Design for Learning and Confirming in a Single Pivotal Trial](http://onlinelibrary.wiley.com/doi/10.1002/pst.1811/full). Pharmaceutical Statistics.
* Package development page including all source codes can be found at: https://github.com/gu-mi/simASD.
* Questions? Comments? Please contact me at: neo.migu@gmail.com, or send a pull request at my GitHub page.

## Disclaimer

The content in the manuscript and this vignette is solely the responsibility of the authors and does not necessarily represent the official views of Eli Lilly and Company. The R package `simASD` is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
