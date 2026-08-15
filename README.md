Cofad User Guide
================

# <img src='logo/cover.png' align='right' height='100px'/>

[![R-CMD-check](https://github.com/johannes-titz/cofad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johannes-titz/cofad/actions/workflows/R-CMD-check.yaml)
[![webR
app](https://github.com/johannes-titz/cofad/actions/workflows/deploy-shinylive.yaml/badge.svg)](https://johannes-titz.github.io/cofad/)
[![CRAN
status](https://www.r-pkg.org/badges/version/cofad)](https://CRAN.R-project.org/package=cofad)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03822/status.svg)](https://doi.org/10.21105/joss.03822)

<!-- [![DOI](https://joss.theoj.org/papers/10.21105/joss.02116/status.svg)](https://doi.org/10.21105/joss.02116) -->

## Citation

If you use `cofad`, please cite both the current tutorial and the
software paper:

Henninger, M., Malejka, S., & Titz, J. (2025). Contrast analysis for
competing hypotheses: A tutorial using the R package cofad. *Behavior
Research Methods, 57*, Article 326.
<https://doi.org/10.3758/s13428-025-02833-w>

Titz, J., & Burkhardt, M. (2021). cofad: An R package and Shiny app for
contrast analysis. *Journal of Open Source Software, 6*(67), 3822.
<https://doi.org/10.21105/joss.03822>

R returns both entries (and their BibTeX forms) with
`citation("cofad")`. The app also has buttons for copying both
references as APA 7 plain text, HTML, or BibTeX. On GitHub, use the
native copy button in the upper-right corner of any of the following
code blocks.

### Plain text

``` text
Henninger, M., Malejka, S., & Titz, J. (2025). Contrast analysis for competing hypotheses: A tutorial using the R package cofad. Behavior Research Methods, 57, Article 326. https://doi.org/10.3758/s13428-025-02833-w

Titz, J., & Burkhardt, M. (2021). cofad: An R package and Shiny app for contrast analysis. Journal of Open Source Software, 6(67), 3822. https://doi.org/10.21105/joss.03822
```

### HTML

``` html
<div class="csl-bib-body">
  <div class="csl-entry">Henninger, M., Malejka, S., &amp; Titz, J. (2025). Contrast analysis for competing hypotheses: A tutorial using the R package cofad. <i>Behavior Research Methods, 57</i>, Article 326. <a href="https://doi.org/10.3758/s13428-025-02833-w">https://doi.org/10.3758/s13428-025-02833-w</a></div>
  <div class="csl-entry">Titz, J., &amp; Burkhardt, M. (2021). cofad: An R package and Shiny app for contrast analysis. <i>Journal of Open Source Software, 6</i>(67), 3822. <a href="https://doi.org/10.21105/joss.03822">https://doi.org/10.21105/joss.03822</a></div>
</div>
```

### BibTeX

``` bibtex
@article{henninger2025cofad,
  author = {Henninger, Mirka and Malejka, Simone and Titz, Johannes},
  title = {Contrast analysis for competing hypotheses: A tutorial using the {R} package cofad},
  journal = {Behavior Research Methods},
  year = {2025},
  volume = {57},
  pages = {326},
  doi = {10.3758/s13428-025-02833-w}
}

@article{titz2021cofad,
  author = {Titz, Johannes and Burkhardt, Markus},
  title = {cofad: An {R} package and {Shiny} app for contrast analysis},
  journal = {Journal of Open Source Software},
  year = {2021},
  volume = {6},
  number = {67},
  pages = {3822},
  doi = {10.21105/joss.03822}
}
```

## Introduction

Cofad is an R package for conducting COntrast analysis in FActorial
Designs, such as ANOVAs. If contrast analysis were to win an award, it
might be for the most underestimated and underused statistical
technique. This is unfortunate, because contrast analysis is at least as
informative as ANOVA—and often more so. Rather than testing an
unspecific omnibus hypothesis like “there are differences somewhere”,
contrast analysis allows you to test a precise, numerically specified
hypothesis. It also shifts the focus from mere significance testing to
the evaluation of effects.

This focus on effects is reflected in two key ways:

1.  Contrast analysis offers three distinct effect size measures:
    ![r\_\mathrm{effectsize}](https://latex.codecogs.com/png.latex?r_%5Cmathrm%7Beffectsize%7D "r_\mathrm{effectsize}"),
    ![r\_\mathrm{contrast}](https://latex.codecogs.com/png.latex?r_%5Cmathrm%7Bcontrast%7D "r_\mathrm{contrast}"),
    and
    ![r\_\mathrm{alerting}](https://latex.codecogs.com/png.latex?r_%5Cmathrm%7Balerting%7D "r_\mathrm{alerting}").

2.  These effect sizes describe different aspects of how closely the
    observed data align with the specified contrast.

Cofad also makes it possible to compare two competing hypotheses
directly (experimentum crucis) by examining the effect sizes associated
with each.

Sounds interesting? Then take a look at some introductory literature,
such as Furr (2004), Rosenthal & Rosnow (1985), Rosenthal, Rosnow, &
Rubin (2000), or—for German-speaking readers— Sedlmeier & Renkewitz
(2018). Contrast analysis is relatively easy to grasp if you’re already
familiar with ANOVA and correlation.

In this vignette, we assume you have a basic understanding of contrast
analysis and are ready to apply it to a specific dataset. We begin by
showing how to install cofad and use its graphical user interface. Then,
we walk through several example analyses for between-subjects,
within-subjects, and mixed designs using R.

## Installation

Cofad has two components: the R package and a Shiny app that offers a
graphical user interface.

If you just want to use the cofad app, you do not need to install it. Go
to <https://cofad.titz.science> and use the categorized example chooser
or upload your own data.

If you prefer the command line interface or want to use the cofad-app
locally, install it from CRAN:

``` r
install.packages("cofad")
```

Alternatively, you can install the development version from GitHub (you
need the `remotes` package for this):

``` r
# install.packages("remotes") # uncomment if you do not have devtools installed
remotes::install_github("johannes-titz/cofad")
```

Now you can load cofad and use it in your R scripts.

You can also run the app:

``` r
cofad::run_app()
```

<!-- If you have any problems installing cofad, check that your R version is up to date (currently R version 4.6.1 (2026-06-24)). If you are using Windows, enable TLS 1.2 in the Internet Options Advanced tab (see https://github.com/r-lib/remotes/issues/130#issuecomment-423830669). Under Windows, you will also need Rtools to build the package: https://cran.r-project.org/bin/windows/Rtools/. -->

<!-- If it still does not work drop an e-mail at johannes at titz.science or at johannes.titz at gmail.com. -->

## Using cofad

Before you start, your data must be in long format (also referred to as
narrow or tidy format). If you do not know what this means, see the
short description in the Wikipedia article:
<https://en.wikipedia.org/wiki/Wide_and_narrow_data>

### Graphical user interface

Load a `.csv` or `.sav` (SPSS) file, or select a packaged example
grouped as between, within, or mixed. Cofad suggests the dependent
variable and any between-subjects factor, within-subjects factor, and
participant ID from the data’s replication and nesting structure. Check
these editable suggestions in the model table, then enter the contrast
weights in the corresponding lambda tables. The detector is
conservative: it does not infer scientific intent from correlations and
may leave ambiguous roles empty.

As an example go to `https://cofad.titz.science/example` which will load
a data set from Rosenthal et al. (2000) (Table 5.3). The cognitive
ability of nine children belonging to different age groups (between) was
measured four times (within).

There are two hypotheses:

1.  cognitive ability linearly increases over time (within)
    (![\lambda\_\mathrm{1} = -3, \lambda\_\mathrm{2} = -1, \lambda\_\mathrm{3} = 1, \lambda\_\mathrm{4} = 3](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7B1%7D%20%3D%20-3%2C%20%5Clambda_%5Cmathrm%7B2%7D%20%3D%20-1%2C%20%5Clambda_%5Cmathrm%7B3%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7B4%7D%20%3D%203 "\lambda_\mathrm{1} = -3, \lambda_\mathrm{2} = -1, \lambda_\mathrm{3} = 1, \lambda_\mathrm{4} = 3"))
2.  cognitive ability linearly increases over age groups (between)
    (![\lambda\_\mathrm{Age 8} = -1, \lambda\_\mathrm{Age 10} = 0, \lambda\_\mathrm{Age12} = 1](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7BAge%208%7D%20%3D%20-1%2C%20%5Clambda_%5Cmathrm%7BAge%2010%7D%20%3D%200%2C%20%5Clambda_%5Cmathrm%7BAge12%7D%20%3D%201 "\lambda_\mathrm{Age 8} = -1, \lambda_\mathrm{Age 10} = 0, \lambda_\mathrm{Age12} = 1"))

Select the variables in the compact model table and edit the default
contrast weights in the lambda tables. The app reports the contrast, a
detailed variance decomposition with ordinary
![\eta^2](https://latex.codecogs.com/png.latex?%5Ceta%5E2 "\eta^2") for
each tested component, the three between-subjects effect sizes and their
squared values, and a plot that partitions total variation. Small *p*
values are shown numerically in scientific notation rather than as
threshold statements. For mixed designs, this is explicitly a partition
of participants’ derived within-contrast scores, not of the raw repeated
outcome variance. The paper-ready report text, F table, and effect-size
table have copy buttons; numeric table columns are right-aligned. The Help
and citation panel beneath the model contains the package references,
README, and GitHub issue tracker.

A mixed design is ideal for trying the cofad app. You can construct a
separate within-subjects model by setting the between-subjects factor to
`NONE`. Likewise, set the within-subjects factor and participant ID to
`NONE` to obtain a between-subjects model.

You can also inspect the same suggestion from R with
`detect_design(your_data)`. It reports structural diagnostics and a
confidence score, but its result should always be verified against the
study design.

The graphical user interface will suffice for most users, but some will
prefer to use the scripting capabilities of R. In the next sections we
will look at several script examples for different designs.

### Between-Subjects Designs

Let us first load the package:

``` r
library(cofad)
```

Now we need some data and hypotheses. We can simply take the data from
Furr (2004), where we have different empathy ratings of students from
different majors. This data set is available in the cofad package:

``` r
data("furr_p4")
furr_p4
#>    empathy      major
#> 1       51 psychology
#> 2       56 psychology
#> 3       61 psychology
#> 4       58 psychology
#> 5       54 psychology
#> 6       62  education
#> 7       67  education
#> 8       57  education
#> 9       65  education
#> 10      59  education
#> 11      50   business
#> 12      49   business
#> 13      47   business
#> 14      45   business
#> 15      44   business
#> 16      50  chemistry
#> 17      45  chemistry
#> 18      40  chemistry
#> 19      49  chemistry
#> 20      41  chemistry
```

Furr states three hypotheses:

- Contrast A: Psychology majors have higher empathy scores than
  Education majors
  (![\lambda\_\mathrm{psych} = 1, \lambda\_\mathrm{edu} = -1](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7Bpsych%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7Bedu%7D%20%3D%20-1 "\lambda_\mathrm{psych} = 1, \lambda_\mathrm{edu} = -1")).
- Contrast B: Business majors have higher empathy scores than Chemistry
  majors
  (![\lambda\_\mathrm{bus} = 1, \lambda\_\mathrm{chem} = -1](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7Bbus%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7Bchem%7D%20%3D%20-1 "\lambda_\mathrm{bus} = 1, \lambda_\mathrm{chem} = -1")).
- Contrast C: On average, Psychology and Education majors have higher
  empathy scores than Business and Chemistry majors
  (![\lambda\_\mathrm{psych} = 1, \lambda\_\mathrm{edu} = 1, \lambda\_\mathrm{bus} = -1, \lambda\_\mathrm{chem} = -1](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7Bpsych%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7Bedu%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7Bbus%7D%20%3D%20-1%2C%20%5Clambda_%5Cmathrm%7Bchem%7D%20%3D%20-1 "\lambda_\mathrm{psych} = 1, \lambda_\mathrm{edu} = 1, \lambda_\mathrm{bus} = -1, \lambda_\mathrm{chem} = -1")).

These hypotheses are only mean comparisons, but this is a good way to
start. Let’s use cofad to conduct the contrast analysis:

``` r
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = -1,
                                       "business" = 0, "chemistry" = 0),
                    data = furr_p4)
ca
#> 
#> We ran a contrast analysis for the following between contrasts: business = 0; chemistry = 0; education = -1; psychology = 1. This resulted in statistics of t(16) = -2.481; p = 0.9877 and an effect magnitude of rₑₛ = -0.276. Attention: Contrast fits in the opposite direction!
```

The print method shows some basic information that can be directly used
in a publication. With the summary method some more details are shown:

``` r
summary(ca)
#> Contrast Analysis Between
#> 
#> Your contrast estimate is negative. This means that your data does not reflect the expected direction of your hypothesis specified by the contrast weights (lambdas).
#> 
#> $Lambdas
#>   business  chemistry  education psychology 
#>          0          0         -1          1 
#> 
#> $tTable
#>   L df      t p(t≥-2.481)①
#>  -6 16 -2.481        0.988
#> 
#> ①The p-value refers to a one-tailed test.
#> 
#> $FTable
#>            SS df     MS     F      p
#> contrast   90  1 90.000 6.154 0.0246
#> within    234 16 14.625             
#> total    1179 19                    
#> 
#> $Effects
#>              effects
#> r_effectsize  -0.276
#> r_contrast    -0.527
#> r_alerting    -0.309
```

From this table,
![r\_\mathrm{effectsize}](https://latex.codecogs.com/png.latex?r_%5Cmathrm%7Beffectsize%7D "r_\mathrm{effectsize}")
is probably the most useful statistic. It is just the correlation
between the lambdas and the dependent variable, which can also be
calculated by hand:

``` r
lambdas <- rep(c(1, -1, 0, 0), each = 5)
cor(furr_p4$empathy, lambdas)
#> [1] -0.2762895
```

As you can see, the effect is negative and `cofad` also warns the user
that the contrast fits in the opposite direction. This is a big failure
for the hypothesis and indicates substantial problems in theorizing.

The other two hypotheses can be tested accordingly:

``` r
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 0, "education" = 0,
                                       "business" = 1, "chemistry" = -1),
                    data = furr_p4)
ca
#> 
#> We ran a contrast analysis for the following between contrasts: business = 1; chemistry = -1; education = 0; psychology = 0. This resulted in statistics of t(16) = 0.827; p = 0.2102 and an effect magnitude of rₑₛ = 0.092.
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = 1,
                                       "business" = -1, "chemistry" = -1),
                    data = furr_p4)
ca
#> 
#> We ran a contrast analysis for the following between contrasts: business = -1; chemistry = -1; education = 1; psychology = 1. This resulted in statistics of t(16) = 7.601; p = 5.349e-07 and an effect magnitude of rₑₛ = 0.847.
```

When you compare the numbers with those presented in Furr (2004), you
will find the same result, except that Furr (2004) uses a *t* statistic
and a directional, one-sided *p* value. A one-sided test is appropriate
when the direction was specified in advance. The *F* test itself is
non-directional; for a single contrast, the corresponding directional
test uses the signed value of
![t = \sqrt{F}](https://latex.codecogs.com/png.latex?t%20%3D%20%5Csqrt%7BF%7D "t = \sqrt{F}").

Now, imagine we have a more fun hypothesis and not just mean
differences. From an elaborate theory we could derive that the means
should be 73, 61, 51 and 38. We can test this with cofad directly
because cofad will center the lambdas (the mean of the lambdas has to be
0):

``` r
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 73, "education" = 61,
                                       "business" = 51, "chemistry" = 38),
                    data = furr_p4)
#> lambdas are centered and rounded to 3 digits
ca
#> 
#> We ran a contrast analysis for the following between contrasts: business = -4.75; chemistry = -17.75; education = 5.25; psychology = 17.25. This resulted in statistics of t(16) = 6.121; p = 7.375e-06 and an effect magnitude of rₑₛ = 0.682.
```

The manual test gives the same effect size:

``` r
lambdas <- rep(c(73, 61, 51, 38), each = 5)
cor(furr_p4$empathy, lambdas)
#> [1] 0.6817294
```

Let us now run an analysis for within-subjects designs.

## Within-Subjects Designs

For within designs the calculations are quite different, but cofad takes
care of the details. We just have to use the within parameters *within*
and *lambda_within* instead of the between equivalents. As an example we
use Table 16.5 from Sedlmeier & Renkewitz (2018). Reading ability was
assessed for eight participants under four different conditions. The
hypothesis is that you can read best without music, white noise reduces
your reading ability and music (independently of type) reduces it even
further.

``` r
data("sedlmeier_p537")
head(sedlmeier_p537)
#>   reading_test participant         music
#> 1           27           1 without music
#> 2           25           2 without music
#> 3           30           3 without music
#> 4           29           4 without music
#> 5           30           5 without music
#> 6           33           6 without music
within <- calc_contrast(dv = reading_test, within = music,
                        lambda_within = c("without music" = 1.25, 
                                          "white noise" = 0.25,
                                          "classic" = -0.75,
                                          "jazz" = -0.75),
                        id = participant, data = sedlmeier_p537)
summary(within)
#> Contrast Analysis Within
#> 
#> $Lambdas
#>       classic          jazz   white noise without music 
#>         -0.75         -0.75          0.25          1.25 
#> 
#> $tTable
#>  mean of L    SE df     t p(t≥5.269)① 95%CI-lower 95%CI-upper
#>      5.875 1.115  7 5.269    0.000581       3.238       8.512
#> 
#> ①The p-value refers to a one-tailed test.
#> 
#> $Effects
#>                 
#> r-contrast 0.687
#> g-contrast 1.863
within
#> 
#> We ran a within-subjects contrast analysis using participant-level L scores (weighted sums retaining response magnitude) for the following contrast weights: classic = -0.75; jazz = -0.75; white noise = 0.25; without music = 1.25. This resulted in t(7) = 5.269; p = 5.810e-04 and an effect magnitude of g_effectsize = 1.863.
```

You can see that the significance test is just a
![t](https://latex.codecogs.com/png.latex?t "t")-test and the reported
effect size is referring to a mean comparison
(![g](https://latex.codecogs.com/png.latex?g "g")). (The
![t](https://latex.codecogs.com/png.latex?t "t")-test is one-tailed,
because contrast analysis has always a specific hypothesis.) When
conducting the analysis by hand, we can see why:

``` r
mtr <- matrix(sedlmeier_p537$reading_test, ncol = 4)
lambdas <- c(1.25, 0.25, -0.75, -0.75)
lc1 <- mtr %*% lambdas
t.test(lc1, alternative = "greater")
#> 
#>  One Sample t-test
#> 
#> data:  lc1
#> t = 5.2689, df = 7, p-value = 0.000581
#> alternative hypothesis: true mean is greater than 0
#> 95 percent confidence interval:
#>  3.762478      Inf
#> sample estimates:
#> mean of x 
#>     5.875
```

Only the linear combination of the dependent variable and the contrast
weights for each participant is needed. With these values a normal
![t](https://latex.codecogs.com/png.latex?t "t")-test against 0 is
conducted. While you can do this manually, using cofad is quicker and it
also gives you more information, such as the different effect sizes.

### Choosing participant L or r scores

For a within contrast, `within_score = "L"` (the default) calculates
each participant’s weighted sum. It retains the absolute magnitude of
the response, so choose L when the size of a predicted increase or
decrease is substantively meaningful. `within_score = "r"` instead
correlates each participant’s response profile with the contrast
weights. It measures pattern agreement and is unchanged by adding a
constant or multiplying a profile by a positive constant. Choose r when
adherence to the predicted shape matters more than amplitude, especially
when there are several repeated conditions.

The choice should follow the research question and be made before
comparing the results. With only two repeated levels, a defined r score
can only be -1 or 1 and discards magnitude; L is usually more
informative. An r score is undefined for a participant whose responses
are all equal, and cofad reports those IDs.

Rosenthal et al.’s Table 5.4 makes the distinction concrete. Child A has
the larger weighted sum but Child B follows the linear pattern
perfectly:

``` r
data("rosenthal_tbl54")
weights <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)

calc_contrast(dv, within = within, lambda_within = weights, id = id,
              data = rosenthal_tbl54, within_score = "L")$participant_scores
#>  A  B
#> 18 10
calc_contrast(dv, within = within, lambda_within = weights, id = id,
              data = rosenthal_tbl54, within_score = "r")$participant_scores
#>       A       B
#> 0.56921 1.00000
```

The app exposes the same choice as **Participant-level within score**,
with labels that distinguish magnitude (L) from pattern fit (r).

## Mixed Designs

A mixed design combines between and within factors. In this case cofad
first calculates the linear combination (*L*-Values) for the within
factor. This new variable serves as the dependent variable for a between
contrast analysis. We will again look at the example presented in
Rosenthal et al. (2000) (see the section graphical user interface). The
cognitive ability of nine children belonging to different age groups
(between) was measured four times (within).

There are two hypotheses:

1.  cognitive ability linearly increases over time (within)
    (![\lambda\_\mathrm{1} = -3, \lambda\_\mathrm{2} = -1, \lambda\_\mathrm{3} = 1, \lambda\_\mathrm{4} = 3](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7B1%7D%20%3D%20-3%2C%20%5Clambda_%5Cmathrm%7B2%7D%20%3D%20-1%2C%20%5Clambda_%5Cmathrm%7B3%7D%20%3D%201%2C%20%5Clambda_%5Cmathrm%7B4%7D%20%3D%203 "\lambda_\mathrm{1} = -3, \lambda_\mathrm{2} = -1, \lambda_\mathrm{3} = 1, \lambda_\mathrm{4} = 3"))
2.  cognitive ability linearly increases over age groups (between)
    (![\lambda\_\mathrm{Age 8} = -1, \lambda\_\mathrm{Age 10} = 0, \lambda\_\mathrm{Age12} = 1](https://latex.codecogs.com/png.latex?%5Clambda_%5Cmathrm%7BAge%208%7D%20%3D%20-1%2C%20%5Clambda_%5Cmathrm%7BAge%2010%7D%20%3D%200%2C%20%5Clambda_%5Cmathrm%7BAge12%7D%20%3D%201 "\lambda_\mathrm{Age 8} = -1, \lambda_\mathrm{Age 10} = 0, \lambda_\mathrm{Age12} = 1"))

Let’s have a look at the data and calculation:

``` r
data("rosenthal_tbl53")
head(rosenthal_tbl53)
#>   dv between id within
#> 1  3    age8  1      1
#> 2  1    age8  2      1
#> 3  4    age8  3      1
#> 4  4   age10  4      1
#> 5  5   age10  5      1
#> 6  5   age10  6      1
lambda_within <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)
lambda_between <-c("age8" = -1, "age10" = 0, "age12" = 1)

contr_mx <- calc_contrast(dv = dv, 
                          between = between,
                          lambda_between = lambda_between,
                          within = within,
                          lambda_within = lambda_within,
                          id = id, 
                          data = rosenthal_tbl53)
contr_mx
#> 
#> We ran a mixed contrast analysis using participant-level L scores (weighted sums retaining response magnitude) for the within-subjects contrast and the following between-subjects contrast weights: age10 = 0; age12 = 1; age8 = -1. This resulted in t(6) = 4.496; p = 0.002062 and an effect magnitude of rₑₛ = 0.871.
```

The results look like a contrast analysis for between-subject designs.
The summary gives some more details: The effect sizes, within group
means and standard errors of the *L*-values.

``` r
summary(contr_mx)
#> Contrast Analysis Mixed
#> 
#> $Lambdas
#> age10 age12  age8 
#>     0     1    -1 
#> 
#> $tTable
#>      L df     t p(t≥4.496)①
#>  5.333  6 4.496     0.00206
#> 
#> ①The p-value refers to a one-tailed test.
#> 
#> $FTable
#>              SS df     MS      F       p
#> contrast 42.667  1 42.667 20.211 0.00412
#> within   12.667  6  2.111               
#> total    56.222  8                      
#> 
#> $Effects
#>              effects
#> r_effectsize   0.871
#> r_contrast     0.878
#> r_alerting     0.990
```

To test pattern fit rather than response magnitude, use the same model
with `within_score = "r"`. The returned `participant_scores` retain the
unrounded correlations. Rosenthal et al. printed these values to two
decimals before their worked calculation, so calculations from the raw
data differ slightly from the book’s rounded F value.

``` r
contr_mx_r <- calc_contrast(
  dv = dv, between = between, lambda_between = lambda_between,
  within = within, lambda_within = lambda_within, id = id,
  data = rosenthal_tbl53, within_score = "r"
)
contr_mx_r$participant_scores
#>         1         2         3         4         5         6         7         8
#> 0.2581989 0.4472136 0.7745967 0.6741999 0.4472136 0.6741999 0.9438798 0.9233805
#>         9
#> 0.9486833
```

## Comparing two hypotheses

With `cofad` you can also compare two competing hypotheses. As an
example Sedlmeier & Renkewitz (2013) use a fictitious data set on
problem solving skills of boys:

``` r
sedlmeier_p525
#>    lsg between lambda
#> 1    1      KT     -2
#> 2    2      KT     -2
#> 3    2      KT     -2
#> 4    2      KT     -2
#> 5    3      KT     -2
#> 6    4      JT      3
#> 7    2      JT      3
#> 8    3      JT      3
#> 9    4      JT      3
#> 10   3      JT      3
#> 11   2      MT     -1
#> 12   3      MT     -1
#> 13   3      MT     -1
#> 14   1      MT     -1
#> 15   2      MT     -1
```

Here, `lsg` is the number of solved exercises, and the groups are KT =
no training, JT = boys-specific training, and MT = girls-specific
training. Two hypotheses are competing:

- -2, 3, -1 (boys benefit from boys-specific training)
- -2, 1, 1 (boys benefit from training, independently of the type of
  training)

First, we need to create the difference lambdas:

``` r
lambda1 <- c(-2, 3, -1)
lambda2 <- c(-2, 1, 1)
lambda <- lambda_diff(lambda1, lambda2, labels = c("KT", "JT", "MT"))
lambda
#>         JT         KT         MT 
#>  0.6816234  0.4883935 -1.1700168
```

Note that you cannot just subtract the lambdas because their variance
can differ, which has an effect on the test. Instead, you need to
standardize the lambdas first. `lambda_diff` takes care of this for you.

Now you can run a normal contrast analysis:

``` r
ca_competing <- calc_contrast(
  dv = lsg,
  between = between,
  lambda_between = round(lambda, 2),
  data = sedlmeier_p525
)
#> lambdas are centered and rounded to 3 digits
summary(ca_competing)
#> Contrast Analysis Between
#> 
#> $Lambdas
#>    JT    KT    MT 
#>  0.68  0.49 -1.17 
#> 
#> $tTable
#>      L df     t p(t≥1.136)①
#>  0.582 12 1.136       0.139
#> 
#> ①The p-value refers to a one-tailed test.
#> 
#> $FTable
#>              SS df    MS     F     p
#> contrast  0.818  1 0.818 1.291 0.278
#> within    7.600 12 0.633            
#> total    11.733 14                  
#> 
#> $Effects
#>              effects
#> r_effectsize   0.264
#> r_contrast     0.312
#> r_alerting     0.445
ca_competing
#> 
#> We ran a contrast analysis for the following between contrasts: JT = 0.68; KT = 0.49; MT = -1.17. This resulted in statistics of t(12) = 1.136; p = 0.139 and an effect magnitude of rₑₛ = 0.264.
```

Here, we rounded the lambdas so that the result is similar to the one in
Sedlmeier & Renkewitz (2013), who found *t* = 1.137 and
![r\_\mathrm{effectsize} = 0.26](https://latex.codecogs.com/png.latex?r_%5Cmathrm%7Beffectsize%7D%20%3D%200.26 "r_\mathrm{effectsize} = 0.26").
The effect size is the same. For the *t* value, we take the signed
square root of the *F* value, 1.291, which is 1.136. There is still a
slight difference from the original result of 1.137, which is likely due
to rounding errors.

The same also works for within-designs. The reading comprehension data
from above can serve as an example. Reading ability was assessed for
eight participants under four different conditions:

``` r
sedlmeier_p537
#>    reading_test participant         music
#> 1            27           1 without music
#> 2            25           2 without music
#> 3            30           3 without music
#> 4            29           4 without music
#> 5            30           5 without music
#> 6            33           6 without music
#> 7            31           7 without music
#> 8            35           8 without music
#> 9            25           1   white noise
#> 10           26           2   white noise
#> 11           32           3   white noise
#> 12           29           4   white noise
#> 13           28           5   white noise
#> 14           30           6   white noise
#> 15           32           7   white noise
#> 16           34           8   white noise
#> 17           21           1       classic
#> 18           25           2       classic
#> 19           23           3       classic
#> 20           26           4       classic
#> 21           27           5       classic
#> 22           26           6       classic
#> 23           29           7       classic
#> 24           31           8       classic
#> 25           23           1          jazz
#> 26           24           2          jazz
#> 27           24           3          jazz
#> 28           28           4          jazz
#> 29           24           5          jazz
#> 30           26           6          jazz
#> 31           27           7          jazz
#> 32           32           8          jazz
```

There are two hypotheses:

- 1.25, 0.25, -0.75, -0.75: You can read best without music, white noise
  reduces your reading ability and music (independently of type) reduces
  it even further.
- 3, -1, -1, -1: Noise of any kind reduces reading ability.

Again, we need to calculate the difference lambdas first:

``` r
lambda1 <- c(1.25, 0.25, -0.75, -0.75)
lambda2 <- c(3, -1, -1, -1)
lambda <- lambda_diff(lambda2, lambda1,
                      labels = c("without music", "white noise", "classic",
                                 "jazz"))
lambda
#>       classic          jazz   white noise without music 
#>     0.3271838     0.3271838    -0.8788616     0.2244941
```

Note that we use lambda2 as the first entry into `lambda_diff` because
this is how Sedlmeier & Renkewitz (2013) calculated it
(hypothesis2-hypothesis1).

And now the contrast analysis:

``` r
contr_wi <- calc_contrast(
  dv = reading_test,
  within = music,
  lambda_within = round(lambda, 2),
  id = participant,
  data = sedlmeier_p537
)
#> lambdas are centered and rounded to 3 digits
summary(contr_wi)
#> Contrast Analysis Within
#> 
#> Your contrast estimate is negative. This means that your data does not reflect the expected direction of your hypothesis specified by the contrast weights (lambdas).
#> 
#> $Lambdas
#>       classic          jazz   white noise without music 
#>          0.33          0.33         -0.88          0.22 
#> 
#> $tTable
#>  mean of L    SE df     t p(t≥-3.77)① 95%CI-lower 95%CI-upper
#>       -2.2 0.584  7 -3.77       0.997       -3.58       -0.82
#> 
#> ①The p-value refers to a one-tailed test.
#> 
#> $Effects
#>                  
#> r-contrast -0.561
#> g-contrast -1.333
contr_wi
#> 
#> We ran a within-subjects contrast analysis using participant-level L scores (weighted sums retaining response magnitude) for the following contrast weights: classic = 0.33; jazz = 0.33; white noise = -0.88; without music = 0.22. This resulted in t(7) = -3.77; p = 0.9965 and an effect magnitude of g_effectsize = -1.333. Attention: Contrast fits in the opposite direction!
```

Sedlmeier & Renkewitz (2013) found a *t* value of -3.75 and a
![g\_\mathrm{contrast}](https://latex.codecogs.com/png.latex?g_%5Cmathrm%7Bcontrast%7D "g_\mathrm{contrast}")
of -1.33. Again, there is a slight difference in the *t* value compared
with our calculation, likely due to rounding errors. The negative
statistic and effect indicate that hypothesis 1 fits better, given that
the difference contrast above was defined as hypothesis 2 minus
hypothesis 1.

## Aggregated Data

Sometimes you would like to run a contrast analysis on aggregated data
(e.g. when no raw data is available). If you have the means, standard
deviations and sample sizes for every condition, you can do this with
cofad. For instance, if we take our first example and aggregate it, we
can still run the contrast analysis:

``` r
furr_agg <- aggregate(
  empathy ~ major, furr_p4,
  function(x) c(mean = mean(x), sd = sd(x), n = length(x))
)
furr_agg <- data.frame(
  major = furr_agg$major,
  mean = furr_agg$empathy[, "mean"],
  sd = furr_agg$empathy[, "sd"],
  n = furr_agg$empathy[, "n"]
)
lambdas = c("psychology" = 1, "education" = -1, "business" = 0, "chemistry" = 0)
calc_contrast_aggregated(mean, sd, n, major, lambdas, furr_agg)
#> 
#> We ran a contrast analysis for the following between contrasts: business = 0; chemistry = 0; education = -1; psychology = 1. This resulted in statistics of t(16) = -2.481; p = 0.9877 and an effect magnitude of rₑₛ = -0.276. Attention: Contrast fits in the opposite direction!
```

And the result is indeed the same when compared to the analysis with the
raw data:

``` r
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = -1,
                                       "business" = 0, "chemistry" = 0),
                    data = furr_p4)
ca
#> 
#> We ran a contrast analysis for the following between contrasts: business = 0; chemistry = 0; education = -1; psychology = 1. This resulted in statistics of t(16) = -2.481; p = 0.9877 and an effect magnitude of rₑₛ = -0.276. Attention: Contrast fits in the opposite direction!
```

Note that this will only work for between-subjects designs.

## Testing

The test suite includes unit, regression, example, summary-method,
validation, in-process Shiny server, and browser integration tests. The
server tests use `shiny::testServer()`, so ordinary
`covr::package_coverage(type = "tests")` now measures the app logic
without starting a browser process. As of August 2026, line coverage is
92.0% overall and 89.8% for `server.R`. Browser tests remain as a
thinner end-to-end layer and are skipped on CRAN and continuous
integration because their timing has been unreliable on some runners.

## Browser-only webR version

A static Shinylive build can run cofad entirely in the browser through
webR, without a Shiny server. It is published to
<https://johannes-titz.github.io/cofad/> by the `deploy-shinylive`
GitHub Actions workflow on every push to the main branch. GitHub Pages
must be configured to use **GitHub Actions** as its source in the
repository settings.

For a local build, run `source("tools/build-shinylive.R")` from the
repository root; the generated `docs/` directory can be served by any
static web host. The current static artifact is approximately 65 MiB. In
a local Chromium benchmark on August 15, 2026, a fresh isolated browser
context showed the controls after 26.9 seconds and a selected example’s
model table after 32.9 seconds. A cached reload took 22.4 and 26.8
seconds, respectively. These are single-machine localhost measurements,
not network guarantees, but they show that webR initialization rather
than app-data transfer dominates startup. Cofad has no additional webR
package archive bundle and its packaged examples total only about 64
KiB, so Mimosa-style partial package loading is unlikely to improve
startup materially. The server-backed app remains preferable when the
lowest latency matters.

## Issues and Support

If you find any bugs, please use the issue tracker at:

<https://github.com/johannes-titz/cofad/issues>

If you need answers on how to use the package, drop an e-mail at
johannes at titz.science or johannes.titz at gmail.com

## Contributing

Comments and feedback of any kind are very welcome! We will thoroughly
consider every suggestion on how to improve the code, the documentation,
and the presented examples. Even minor things, such as suggestions for
better wording or improving grammar in any part of the package, are more
than welcome.

If you want to make a pull request, please check that you can still
build the package without any errors, warnings, or notes. Overall,
simply stick to the R packages book: <https://r-pkgs.org/> and follow
the code style described here: <https://style.tidyverse.org/>

## Acknowledgments

We want to thank Thomas Schäfer and Isabell Winkler for testing cofad
and giving helpful feedback.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-furr2004" class="csl-entry">

Furr, R. M. (2004). Interpreting effect sizes in contrast analysis.
*Understanding Statistics*, *3*, 1–25.
<https://doi.org/10.1207/s15328031us0301_1>

</div>

<div id="ref-rosenthal1985" class="csl-entry">

Rosenthal, R., & Rosnow, R. L. (1985). *Contrast analysis: Focused
comparisons in the analysis of variance*. Cambridge, England: Cambridge
University Press.

</div>

<div id="ref-rosenthal2000" class="csl-entry">

Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). *Contrasts and
Effect Sizes in Behavioral Research: A Correlational Approach*.
Cambridge University Press.

</div>

<div id="ref-sedlmeier2013" class="csl-entry">

Sedlmeier, P., & Renkewitz, F. (2013). *Forschungsmethoden und Statistik
für Psychologen und Sozialwissenschaftler* (2nd ed.). Hallbergmoos,
Germany: Pearson Studium.

</div>

<div id="ref-sedlmeier2018" class="csl-entry">

Sedlmeier, P., & Renkewitz, F. (2018). *Forschungsmethoden und Statistik
für Psychologen und Sozialwissenschaftler* (3rd ed.). Hallbergmoos,
Germany: Pearson Studium.

</div>

</div>
