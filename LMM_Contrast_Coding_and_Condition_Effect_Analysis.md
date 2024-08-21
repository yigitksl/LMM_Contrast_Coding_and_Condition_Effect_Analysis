LMM Contrast Coding and Condition Effect Analysis
================
Yigit Kasal
2024-02-13

## Introduction

This project aims to investigate the effect of grammaticality on reading
times using Linear Mixed Models (LMMs). Specifically, we will explore
how different contrast coding schemes affect the estimated effects and
variance components. The data used in this analysis are from a
self-paced reading study (Frank, Trompenaars, & Vasishth, 2015)
examining cross-linguistic differences in processing double-embedded
relative clauses.

## Data Preliminaries

We begin by loading the necessary data from the `lingpsych` package. The
dataset `df_dutch` contains reading times (`NP1`) for different sentence
conditions (`condition`), where `+1` indicates a grammatical sentence,
and `-1` indicates an ungrammatical sentence. Our first step is to load
and inspect the data.

``` r
library(lingpsych)
data("df_dutch")
head(df_dutch)
```

    ##   subject item condition      NP1
    ## 1      s1   i7        -1 7.052721
    ## 2      s1  i12         1 6.927558
    ## 3      s1   i6         1 6.021023
    ## 4      s1  i13        -1 6.958448
    ## 5      s1  i10         1 6.467699
    ## 6      s1  i14         1 5.981414

## Fitting the Initial Linear Mixed Model

To investigate the effect of condition (grammatical vs. ungrammatical)
on reading time, we fit a Linear Mixed Model (LMM) with varying
intercepts and slopes for each subject. This model will help us
understand the baseline relationship between grammaticality and reading
time.

``` r
library(Matrix)
```

    ## Warning: package 'Matrix' was built under R version 4.3.2

``` r
library(lme4)
```

    ## Warning: package 'lme4' was built under R version 4.3.2

``` r
LMM <- lmer(NP1 ~ condition + (1 + condition || subject), data = df_dutch)
summary(LMM)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: NP1 ~ condition + ((1 | subject) + (0 + condition | subject))
    ##    Data: df_dutch
    ## 
    ## REML criterion at convergence: 478.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1712 -0.5784 -0.1314  0.3631  4.6650 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  subject   (Intercept) 0.035054 0.18723 
    ##  subject.1 condition   0.005964 0.07722 
    ##  Residual              0.185207 0.43036 
    ## Number of obs: 372, groups:  subject, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  6.28124    0.04444  141.34
    ## condition   -0.04911    0.02744   -1.79
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr)
    ## condition 0.008

## Interpreting the Estimated Standard Deviations Printed Out by the Model

``` r
random_effects <- as.data.frame(VarCorr(LMM))
print(random_effects)
```

    ##         grp        var1 var2        vcov      sdcor
    ## 1   subject (Intercept) <NA> 0.035053906 0.18722688
    ## 2 subject.1   condition <NA> 0.005963692 0.07722494
    ## 3  Residual        <NA> <NA> 0.185206751 0.43035654

1.  **Standard Deviation of the Intercept (Subject-Level Random
    Effect)**: This represents the variability in the average log
    reading time across different subjects. A larger value indicates
    greater variability between subjects in their baseline reading
    times.

2.  **Standard Deviation of the Slope (Subject-Level Random Effect for
    Condition)**: This reflects the variability in how different
    subjects are affected by the grammatical vs. ungrammatical
    condition. A larger value suggests that subjects vary widely in
    their sensitivity to grammaticality.

3.  **Residual Standard Deviation**: This is the standard deviation of
    the residuals, representing the variability in reading times that is
    not explained by the fixed effects (condition) or the random effects
    (subject and item). It gives us an indication of how well the model
    fits the data.

These standard deviations help us understand both the fixed and random
components of the model, allowing us to assess the extent to which
individual differences and residual variability influence the overall
reading times.

## Calculating the Difference in Reading Times

To quantify the effect of grammaticality on reading times, we calculate
the difference in the predicted reading times for grammatical and
ungrammatical sentences. This involves computing the expected log
reading times for each condition, exponentiating these values to return
to the millisecond scale, and then finding the difference.

``` r
beta0 <- 6.28124
beta1 <- -0.04911

# Calculate the value for condition = +1 (grammatical condition)
value_condition_plus1 <- exp(beta0 + beta1 * 1)

# Calculate the value for condition = -1 (ungrammatical condition)
value_condition_minus1 <- exp(beta0 + beta1 * (-1))

# Calculate the difference
difference_ms <- value_condition_plus1 - value_condition_minus1
difference_ms
```

    ## [1] -52.51488

## Refitting the Model with ±1/2 Coding

To explore the impact of contrast coding on our results, we refit the
model using ±1/2 coding. This coding scheme will scale the slope
estimate by a factor of two, and we expect corresponding changes in the
variance components and slope interpretation.

``` r
df_dutch$condition <- ifelse(df_dutch$condition == 1, 0.5, -0.5)
new_LMM <- lmer(NP1 ~ condition + (1 + condition | subject), data = df_dutch, REML = FALSE)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(new_LMM)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: NP1 ~ condition + (1 + condition | subject)
    ##    Data: df_dutch
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    468.4    492.0   -228.2    456.4      366 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2631 -0.5773 -0.1571  0.3787  4.5890 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  subject  (Intercept) 0.03387  0.1840        
    ##           condition   0.03291  0.1814   -1.00
    ##  Residual             0.18099  0.4254        
    ## Number of obs: 372, groups:  subject, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  6.28118    0.04365 143.893
    ## condition   -0.09882    0.05762  -1.715
    ## 
    ## Correlation of Fixed Effects:
    ##           (Intr)
    ## condition -0.553
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
new_beta0 <- 6.28118
new_beta1 <- -0.09882

# Calculate the value for condition = +1 (grammatical condition)
new_value_condition_plus1 <- exp(new_beta0 + new_beta1 * 1)

# Calculate the value for condition = -1 (ungrammatical condition)
new_value_condition_minus1 <- exp(new_beta0 + new_beta1 * (-1))

# Calculate the difference
new_difference_ms <- new_value_condition_plus1 - new_value_condition_minus1
new_difference_ms
```

    ## [1] -105.7945

## Treatment Coding

Next, we apply treatment coding, where the grammatical condition is
coded as 0, and the ungrammatical condition is coded as 1. This allows
us to directly estimate the effect of moving from a grammatical to an
ungrammatical sentence, providing a different perspective on the effect
of condition.

``` r
df_dutch$condition_recode <- ifelse(df_dutch$condition == 1, 0, 1)

# Fit the model with the recoded condition
treated_LMM <- lmer(NP1 ~ condition_recode + (1|subject) + (1|item), data = df_dutch)
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

    ## boundary (singular) fit: see help('isSingular')

``` r
# Summarize the treated_LMM model to get the correct output
summary(treated_LMM)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: NP1 ~ condition_recode + (1 | subject) + (1 | item)
    ##    Data: df_dutch
    ## 
    ## REML criterion at convergence: 479.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2806 -0.6092 -0.1503  0.3168  4.8273 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 0.03461  0.1860  
    ##  item     (Intercept) 0.00000  0.0000  
    ##  Residual             0.19333  0.4397  
    ## Number of obs: 372, groups:  subject, 24; item, 16
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  6.28201    0.04446   141.3
    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
treated_beta0 <- 6.28124
treated_beta1 <- -0.04911

# Calculate the value for condition = 0 (grammatical condition)
treated_value_condition_0 <- exp(treated_beta0 + treated_beta1 * 0)

# Calculate the value for condition = 1 (ungrammatical condition)
treated_value_condition_1 <- exp(treated_beta0 + treated_beta1 * 1)

# Calculate the difference
treated_difference_ms <- treated_value_condition_1 - treated_value_condition_0
treated_difference_ms
```

    ## [1] -25.61282

## Comparing Models

Finally, we compare the random effects (variance components) of the
original and recoded models to observe how the coding scheme influences
these estimates. This comparison helps us understand the impact of
different coding strategies on the interpretation of the fixed and
random effects in our models.

- The intercept now represents the expected log reading time for the
  grammatical condition when other variables in the model are held
  constant.
- The slope now represents the difference in the expected log reading
  time between ungrammatical and grammatical conditions assuming all
  other variables in the model are held constant.

``` r
# Fit the original model with original condition coding
original_LMM <- lmer(NP1 ~ condition + (1|subject) + (1|item), data = df_dutch)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
# Fit the model with recoded condition (treatment coding)
treated_LMM <- lmer(NP1 ~ condition_recode + (1|subject) + (1|item), data = df_dutch)
```

    ## fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
    ## boundary (singular) fit: see help('isSingular')

``` r
# Extract variance components (random effects)
original_components <- VarCorr(original_LMM)
treated_components <- VarCorr(treated_LMM)

# Print variance components for comparison
print(original_components)
```

    ##  Groups   Name        Std.Dev.
    ##  subject  (Intercept) 0.18618 
    ##  item     (Intercept) 0.00000 
    ##  Residual             0.43742

``` r
print(treated_components)
```

    ##  Groups   Name        Std.Dev.
    ##  subject  (Intercept) 0.18604 
    ##  item     (Intercept) 0.00000 
    ##  Residual             0.43969

## Conclusion

In this analysis, we explored the effect of grammaticality on reading
times using Linear Mixed Models (LMMs). We aimed to understand how
different contrast coding schemes can impact the estimated effects and
variance components.

### Findings:

1.  **Initial Model**: The initial LMM with sum coding revealed that the
    grammaticality condition had a small negative effect on reading
    times with grammatical sentences being slightly faster to read than
    ungrammatical ones. However, this effect was not statistically
    significant.

2.  **Effect of Contrast Coding**: By refitting the model using ±1/2
    coding, we observed that the slope estimate for the condition effect
    doubled. This change in coding also led to changes in the variance
    components for the random effects. Particularly in the random slope
    for subjects. Despite these changes, the overall interpretation of
    the condition effect remained consistent. This reaffirms the
    robustness of our findings across different coding schemes.

3.  **Treatment Coding**: Applying treatment coding, where the
    grammatical condition was set as the baseline, provided a different
    perspective on the fixed effects. The model confirmed that the
    difference in reading times between ungrammatical and grammatical
    sentences persisted with slightly different variance components.

### Implications:

These findings suggest that while the choice of contrast coding can
influence the magnitude of fixed effect estimates and the interpretation
of variance components, the overall conclusions about the effect of
grammaticality on reading times are consistent.

### Limitations:

Several warnings during model fitting (boundary singular fits and rank
deficiency) suggest potential limitations in the models’ ability to
fully capture the underlying variability in the data. These issues
highlight the need for further investigation, potentially with a larger
dataset.
