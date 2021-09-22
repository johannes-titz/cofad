---
title: 'cofad: An R package and shiny app for contrast analysis'
tags:
  - contrast analysis
  - factorial design
  - shiny
  - R
authors:
  - name: Johannes Titz
    orcid: 0000-0002-1102-5719
    affiliation: 1
  - name: Markus Burkhardt
    orcid: 0000-0002-2498-2999
    affiliation: 1
affiliations:
 - name: Department of Psychology, TU Chemnitz, Germany
   index: 1
date: 15 September 2021
bibliography: library_paper.bib
---

# Summary

Contrast analysis offers a powerful method to test a hypothesis about means in factorial designs. Despite many advantages, the method is unknown to most researchers. For instance, even if researchers have a specific hypothesis for group means in an experimental design, they usually run an omnibus test or multiple post-hoc tests. The first approach reduces power while the second increases the type I error because of multiple testing. Contrast analysis is always the better alternative as it provides a single specific test with high power. The method is efficient, informative and shines with its simplicity [@furr2004]. To revive and popularize contrast analysis, a software is needed that is flexible but also intuitive to use.

# Statement of Need

`cofad` is an R package and shiny app for conducting **co**ntrast analysis in **fa**ctorial **d**esigns. Although it is possible to use contrasts in most statistical software tools, these are ad-hoc solutions that lack a consistent framework. In contrast, `cofad` provides a single function for between, within and mixed designs in the tradition and language of @rosenthal1985, @Rosenthal2000 and @sedlmeier2018. In addition, a graphical user interface in the form of a shiny app can either be used locally or online at https://cofad.titz.science. 

`cofad` is targeted at researchers looking for better ways to evaluate their theories. In particular contrast analysis has some tradition in psychology and the social sciences. But other fields using factorial designs will also benefit from higher test power. Practical research examples of using `cofad` can be found in a german textbook on data anlysis with R [@sedlmeier2021]. Furthermore, the package has been used in undergraduate courses at TU Chemnitz and is part of a free online course for R (https://rlernen.de).

# Alternatives

The R packages `lsmeans` [@lenth2016] and `multcomp` [@hothorn2008] provide contrast coding from a multiple comparison perspective. These tools are used to test typical (multiple) contrasts such as *all possible pairs* or *treatment versus control*. Although it is possible to use a more specific contrast it involves multiple steps and is less convenient than in `cofad`. For instance, the order of contrasts has to strictly adhere to the order of the independent variable, which can quickly lead to mistakes that are difficult to spot. Furthermore, typical effect sizes in contrast analysis such as $r_\mathrm{effectsize}$ are not reported. `cofad` is easier to use because the model and the contrast can be set in a single step. Furthermore, errors in specifying contrasts are unlikely because a named vector has to be provided with all conditions of the independent variable.

SPSS [@ibm2020] offers a function for planned contrasts in an ANOVA. But to our knowledge it cannot handle within designs or mixed designs. A minor annoyance is that adding contrasts involves many steps and the interface does not show which contrasts are mapped to which group. Again, `cofad` is more intuitive to use and prevents input errors.

# Acknowledgments

We want to thank Thomas Schäfer and Isabell Winkler for testing `cofad` and giving helpful feedback.

# References
