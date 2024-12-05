OpenClustered
================

OpenClustered is an R package data repository for clustered and
longitudinal datasets. The goal of this package is to coalesce clustered
datasets in harmonized fashioned for developing, testing, and
benchmarking existing and new methods for clustered data analysis and
prediction. Currently, there are 19 datasets in this repository loaded
in with the list “data_list”. Each dataset has a unique set of predictor
variables/features, with the outcome commonly renamed to “taget” and the
cluster variable to “cluster_id”. The dataset “meta_data” contains
information on each of these data sets. Current functionality of this
package is basic - limited to reading in, summarizing, and subsetting
datasets based on user defined filtering criteria. The development of
this R package is ongoing, as we will continue to add more clustered
data sets they become available and can be harmonized. We will continue
to add more functionality to this package based on user
feedback/requests.

Here is a simple tutorial for using this package:

## Install package from github

``` r
# Install package from github
devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

# load package
library(OpenClustered)
```

Currently, the package has the following packages as dependecies:
tidyverse, gridExtra, and table1.

## View Meta Data and Info of available datasets

We can view info and meta data on the availble datasets within the list
‘data_list’ by viewing ‘meta_data’:

``` r
# View Meta Data files
# exclude the 6th column 'origin' for cleaner output
head(OpenClustered::meta_data)[,-6]
```

    ##   dataset dataset_name   outcome      domain sim_or_real n_obs n_features
    ## 1    dat1     hospital remission    medicine   simulated  8469         20
    ## 2   dat10     prenatal      care    medicine        real  2449         12
    ## 3   dat11       memory       acc   education        real  1602          5
    ## 4   dat12       dative      Verb linguistics        real  3263         13
    ## 5   dat13      culcita predation       ocean        real    80          1
    ## 6   dat14      toenail   outcome    medicine        real  1908          2
    ##   n_clusters n_classes imbalance           task missing_obs
    ## 1        381         2      0.30 Classification        8469
    ## 2        161         2      0.45 Classification        2449
    ## 3         34         2      0.31 Classification         462
    ## 4         75         2      0.26 Classification        2360
    ## 5         10         2      0.38 Classification          80
    ## 6        294         2      0.21 Classification        1908

### Plot Meta Data

We can further visually assess characteristics of the datasets with the
‘plot_meta_data()’ function. This function by default returns
characteristic plots for *all* included datasets in ‘data_list’. The
parameter ‘allplots’ is logical; if T, it returns a 2x2 grid of plots
characterizing the number of observations, features, clustering units,
and imbalance across datasets. If ‘allplots=F’, it returns a list with 4
elements containing each of these plots.

``` r
# View meta data characteristics of all datasets in `data_list`
plot_meta_data(allplots=T)
```

![](README_files/figure-gfm/plot_meta-1.png)<!-- -->

### Tabulate Meta Data

We can further tabulate meta data summary statistics via the function
‘tab_meta_data’. At it’s core, this function is simply a wrapper for the
‘table1::table1” function, but further allows for simple specification
of a subset of datasets from data_list for summarization (to be shown in
an upcoming section). By defualt, this function summarizes all datasets
from “data_list”, with the primary parameter ’formula’ being a formula
specification following the notation, ’~ x + y + z\`, where x, y, and z
are variables to summarize.

``` r
# Summarize Meta Data (using r package "table1")
tab_meta_data(formula = ~n_obs + n_features + n_clusters + imbalance + missing_obs + 
                domain + sim_or_real)
```

<div class="Rtable1"><table class="Rtable1">
<thead>
<tr>
<th class='rowlabel firstrow lastrow'></th>
<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=19)</span></span></th>
</tr>
</thead>
<tbody>
<tr>
<td class='rowlabel firstrow'>n_obs</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>2060 (2110)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>1910 [80.0, 8470]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>n_features</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>6.11 (5.08)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>4.00 [1.00, 20.0]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>n_clusters</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>149 (168)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>60.0 [10.0, 537]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>imbalance</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.309 (0.118)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>0.330 [0.120, 0.480]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>missing_obs</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>1940 (2130)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>1910 [80.0, 8470]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>domain</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>animal</td>
<td>2 (10.5%)</td>
</tr>
<tr>
<td class='rowlabel'>education</td>
<td>1 (5.3%)</td>
</tr>
<tr>
<td class='rowlabel'>linguistics</td>
<td>6 (31.6%)</td>
</tr>
<tr>
<td class='rowlabel'>medicine</td>
<td>6 (31.6%)</td>
</tr>
<tr>
<td class='rowlabel'>ocean</td>
<td>1 (5.3%)</td>
</tr>
<tr>
<td class='rowlabel'>politics</td>
<td>1 (5.3%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>2 (10.5%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>sim_or_real</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>real</td>
<td>16 (84.2%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>simulated</td>
<td class='lastrow'>3 (15.8%)</td>
</tr>
</tbody>
</table>
</div>

## Subsetting data_list

We provide wrapper functons for easily subsetting the ‘data_list’ based
on meta data criteria through the function ‘filter_data()’. The primary
inputs follow ‘dplyr::filter()’ syntax. The ‘subset’ parameter is
logical. If “TRUE”, it returns a list containing each dataset meeting
the specified criteria as an element of that list. If “FALSE”, the
function returns a vector of the dataset names matching the filtered
criteria.

Here’s an example of us subsetting our ‘data_list’ to only those with
\>= 1000 observations within the domain “linguistics”:

``` r
# Subset data_list to datasets with >5000 observations and in the domain of 'linguistics'
ling_data = filter_data(n_obs >=1000, domain=="linguistics", subset=T)
```

### Summary Plots of Linguistics data

We can then plot the meta data characteristics as we did before, but
specifically for this subset data using the “plot_meta_data()” function
and specifying the ‘df’ parameter to be the new subset list. Note, the
‘df’ parameter in ‘plot_meta_data()’ can be either a list or the vector
of dataset names given by ‘filter_data()’:

``` r
# view characteristics of new data
plot_meta_data(allplots=T, df = ling_data)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/filter_summary-1.png)<!-- -->

### Tabulate Sumamry Statistics for Linguistics Data

Similarly, we can tabulate these chacarteristics in the
“tab_meta_data()” functuon through the ‘df’ parameter in the same way:

``` r
# Summarize the list of linguistic datasets with >=1000 observations
tab_meta_data(~n_obs + n_features + n_clusters + imbalance + missing_obs + 
                domain + sim_or_real, df= ling_data)
```

<div class="Rtable1"><table class="Rtable1">
<thead>
<tr>
<th class='rowlabel firstrow lastrow'></th>
<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=3)</span></span></th>
</tr>
</thead>
<tbody>
<tr>
<td class='rowlabel firstrow'>n_obs</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>2130 (1080)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>2000 [1110, 3260]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>n_features</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>7.00 (5.20)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>4.00 [4.00, 13.0]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>n_clusters</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>118 (78.0)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>75.0 [71.0, 208]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>imbalance</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>0.287 (0.132)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>0.260 [0.170, 0.430]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>missing_obs</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>1820 (642)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>2000 [1110, 2360]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>domain</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>linguistics</td>
<td class='lastrow'>3 (100%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>sim_or_real</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel lastrow'>real</td>
<td class='lastrow'>3 (100%)</td>
</tr>
</tbody>
</table>
</div>

# Example

In this example, we will take one of the linguistics datasets above
(“dat12” which can be called through the subset data list
“ling_dat\$dat12”) and create a assess AUC performance of a predicton
model using training and testing data and a GLMM with the package
‘lme4’. Note, we exclude one variable ‘Speaker’ due to sparsity

``` r
### Develop a Logistic Prediction Model on one of the datasets (dat12) in Linguistics 
### Load in necessary packages
library(lme4)
library(pROC)

#Summarize ling_dat$dat12 using table1 package
# Summarize dat12 using Table 1
table1::table1(~Modality + SemanticClass + LengthOfRecipient + 
                 AnimacyOfRec+ + DefinOfRec + PronomOfRec+LengthOfTheme+ AnimacyOfTheme+
                 DefinOfTheme+PronomOfTheme+AccessOfRec+AccessOfTheme, data=ling_data$dat12)
```

<div class="Rtable1"><table class="Rtable1">
<thead>
<tr>
<th class='rowlabel firstrow lastrow'></th>
<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=3263)</span></span></th>
</tr>
</thead>
<tbody>
<tr>
<td class='rowlabel firstrow'>Modality</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>spoken</td>
<td>2360 (72.3%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>written</td>
<td class='lastrow'>903 (27.7%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>SemanticClass</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>a</td>
<td>1433 (43.9%)</td>
</tr>
<tr>
<td class='rowlabel'>c</td>
<td>405 (12.4%)</td>
</tr>
<tr>
<td class='rowlabel'>f</td>
<td>59 (1.8%)</td>
</tr>
<tr>
<td class='rowlabel'>p</td>
<td>228 (7.0%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>t</td>
<td class='lastrow'>1138 (34.9%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>LengthOfRecipient</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>1.84 (2.07)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>1.00 [1.00, 31.0]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>AnimacyOfRec</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>animate</td>
<td>3024 (92.7%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>inanimate</td>
<td class='lastrow'>239 (7.3%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>DefinOfRec</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>definite</td>
<td>2775 (85.0%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>indefinite</td>
<td class='lastrow'>488 (15.0%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>PronomOfRec</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>nonpronominal</td>
<td>1229 (37.7%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>pronominal</td>
<td class='lastrow'>2034 (62.3%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>LengthOfTheme</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>4.27 (4.36)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>3.00 [1.00, 46.0]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>AnimacyOfTheme</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>animate</td>
<td>74 (2.3%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>inanimate</td>
<td class='lastrow'>3189 (97.7%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>DefinOfTheme</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>definite</td>
<td>929 (28.5%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>indefinite</td>
<td class='lastrow'>2334 (71.5%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>PronomOfTheme</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>nonpronominal</td>
<td>2842 (87.1%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>pronominal</td>
<td class='lastrow'>421 (12.9%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>AccessOfRec</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>accessible</td>
<td>615 (18.8%)</td>
</tr>
<tr>
<td class='rowlabel'>given</td>
<td>2302 (70.5%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>new</td>
<td class='lastrow'>346 (10.6%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>AccessOfTheme</td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>accessible</td>
<td>1742 (53.4%)</td>
</tr>
<tr>
<td class='rowlabel'>given</td>
<td>502 (15.4%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>new</td>
<td class='lastrow'>1019 (31.2%)</td>
</tr>
</tbody>
</table>
</div>

### Assessment of cluster ID variable

``` r
# Assess the cluster Variable
table(ling_data$dat12$cluster_id)
```

    ## 
    ##    accord    afford  allocate     allot     allow    assess    assign    assure 
    ##         1         1         3         3        13         1         4         2 
    ##     award  bequeath       bet     bring     carry     cause      cede    charge 
    ##        19         1         2        55         1        10         2        43 
    ##      cost      deal   deliver      deny        do    extend      feed      fine 
    ##       169         2         3        12        31         4        17         2 
    ##      flip     float    funnel       get      give     grant guarantee      hand 
    ##         1         1         1         1      1666        13         2        15 
    ## hand_over     issue     lease     leave      lend      loan      mail      make 
    ##         1         6         4        11        20        21        14         6 
    ##       net     offer       owe       pay  pay_back    permit    prepay   present 
    ##         1        79        31       207         1         2         1         1 
    ##   promise     quote      read    refuse reimburse     repay    resell       run 
    ##        10         2         8         1         1         1         3         1 
    ##      sell sell_back  sell_off      send     serve      show      slip    submit 
    ##       206         1         1       172         7        58         1         1 
    ##    supply      swap      take     teach      tell    tender     trade      vote 
    ##         1         1        58        64       128         1         1         1 
    ##      will      wish     write 
    ##         1         9        17

We see numerous clusters with small counts. This will give us issues
when fitting training and testing datasets using a GLMM. Next we write a
simple function to convert all clusters with fewer than 30 counts into
an “other” category:

``` r
# relevel all clusters levels with less than `threshold' observations into their own category
relevel_to_other <- function(factor_var, threshold) {
  # Get frequency counts for each level
  level_counts <- table(factor_var)
  
  # Identify levels to be grouped into "other"
  levels_to_other <- names(level_counts[level_counts < threshold])
  
  # rename variables in levels_to_other to "other"
  factor_var = ifelse(factor_var %in% levels_to_other, "other", as.character(factor_var))
  return(factor_var)
}

ling_data$dat12$cluster_id <- relevel_to_other(as.factor(ling_data$dat12$cluster_id), threshold = 30)
```

We then split the dataset into training and testing datasets with a
70:30 split:

``` r
set.seed(123)

# Split Dataset by single split into training and testing datasets
train_ids <- sample(1:nrow(ling_data$dat12), size = round(.7 * nrow(ling_data$dat12)))
train_data <- ling_data$dat12[train_ids, ]  # Training set
test_data <- ling_data$dat12[-train_ids, ]  # Testing set
```

We then fit our mixed model:

``` r
#Fit Mixed Model
fit = glmer(target == "PP" ~ Modality + SemanticClass + LengthOfRecipient + 
             AnimacyOfRec+ + DefinOfRec + PronomOfRec+LengthOfTheme+ AnimacyOfTheme+
             DefinOfTheme+PronomOfTheme+AccessOfRec+AccessOfTheme+(1|cluster_id), data=train_data,
            family=binomial(link="logit"))
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00700836 (tol = 0.002, component 1)

``` r
# Summarize Mixed Model
summary(fit)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: target == "PP" ~ Modality + SemanticClass + LengthOfRecipient +  
    ##     AnimacyOfRec + +DefinOfRec + PronomOfRec + LengthOfTheme +  
    ##     AnimacyOfTheme + DefinOfTheme + PronomOfTheme + AccessOfRec +  
    ##     AccessOfTheme + (1 | cluster_id)
    ##    Data: train_data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1124.0   1232.9   -543.0   1086.0     2265 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6152 -0.2191 -0.0921  0.0261 14.3102 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance Std.Dev.
    ##  cluster_id (Intercept) 3.973    1.993   
    ## Number of obs: 2284, groups:  cluster_id, 15
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              1.28214    0.87405   1.467 0.142406    
    ## Modalitywritten         -0.01504    0.23272  -0.065 0.948474    
    ## SemanticClassc           0.39657    0.39548   1.003 0.315973    
    ## SemanticClassf          -0.28730    0.61095  -0.470 0.638176    
    ## SemanticClassp          -2.58196    1.14129  -2.262 0.023678 *  
    ## SemanticClasst           0.21718    0.23464   0.926 0.354649    
    ## LengthOfRecipient        0.27988    0.05180   5.404 6.53e-08 ***
    ## AnimacyOfRecinanimate    2.06127    0.29811   6.914 4.70e-12 ***
    ## DefinOfRecindefinite     0.79465    0.23535   3.376 0.000734 ***
    ## PronomOfRecpronominal   -1.92949    0.26347  -7.323 2.42e-13 ***
    ## LengthOfTheme           -0.22020    0.03034  -7.258 3.92e-13 ***
    ## AnimacyOfThemeinanimate -1.26939    0.57454  -2.209 0.027147 *  
    ## DefinOfThemeindefinite  -1.00578    0.21648  -4.646 3.38e-06 ***
    ## PronomOfThemepronominal  2.07952    0.29192   7.124 1.05e-12 ***
    ## AccessOfRecgiven        -0.89184    0.25057  -3.559 0.000372 ***
    ## AccessOfRecnew           0.51453    0.27542   1.868 0.061743 .  
    ## AccessOfThemegiven       1.49764    0.30475   4.914 8.91e-07 ***
    ## AccessOfThemenew        -0.29133    0.21875  -1.332 0.182922    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 18 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00700836 (tol = 0.002, component 1)

And lastly we predict over the test dataset and assess the AUC using the
\`pROC’ package:

``` r
# Predict 
test_data$predicted_prob <- predict(fit, newdata = test_data, type = "response")

# Compute AUC
auc_result <- pROC::roc(response =test_data$target, predictor = test_data$predicted_prob)
auc_result
```

    ## 
    ## Call:
    ## roc.default(response = test_data$target, predictor = test_data$predicted_prob)
    ## 
    ## Data: test_data$predicted_prob in 739 controls (test_data$target NP) < 240 cases (test_data$target PP).
    ## Area under the curve: 0.9657
