#' Empathy data set by Furr (2004)
#'
#' fictitious data set on empathy ratings of students from different majors
#'
#' @format a data frame with 20 rows and 2 columns
#' \describe{
#'   \item{empathy}{Empathy rating}
#'   \item{major}{major of student}
#' }
#'
#' @source Furr, R. M. (2004). Interpreting effect sizes in contrast analysis.
#'   Understanding Statistics, 3, 1–25.
#'   https://doi.org/10.1207/s15328031us0301_1
#'
#' @usage data(furr_p4)
"furr_p4"

#' Children data set by Rosenthal and Rosnow (2000)
#'
#' Table 5.3 in Rosenthal and Rosnow (2000) on p. 129.
#'
#' @format a data frame with 36 rows and 4 columns
#' \describe{
#'   \item{dv}{dependent variable}
#'   \item{between}{age group (8, 10, 12 years)}
#'   \item{id}{unique identifier for child}
#'   \item{within}{measurement (1, 2, 3, 4)}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_tbl53)
"rosenthal_tbl53"

#' Data set by Rosenthal and Rosnow (2000)
#'
#' Table 3.1 in Rosenthal and Rosnow (2000) on p. 38.
#'
#' @format a data frame with 20 rows and 2 columns
#' \describe{
#'   \item{dv}{dependent variable}
#'   \item{between}{group (A, B, C, D))}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_tbl31)
"rosenthal_tbl31"

#' Therapy data set by Rosenthal and Rosnow (2000)
#'
#' Table 5.9 in Rosenthal and Rosnow (2000)
#'
#' @format a data frame with 12 rows and 4 columns
#' \describe{
#'   \item{id}{unique identifier}
#'   \item{dv}{dependent variable}
#'   \item{med}{within variable: medication (treatment or placebo)}
#'   \item{pt}{between variable: psychotherapy (treatment or placebo)}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_tbl59)
"rosenthal_tbl59"

#' Complexity data set by Rosenthal and Rosnow (2000)
#'
#' Exercise 2 from Chapter 5 (table on p. 147) in Rosenthal and Rosnow (2000)
#'
#' @format a data frame with 12 rows and 4 columns
#' \describe{
#'   \item{dv}{dependent variable: rating of degree of complexity of social
#'   interaction from a series of clips}
#'   \item{id}{unique identifier of participant}
#'   \item{within}{within variable: complexity of interaction (low, medium
#'   high)}
#'   \item{between}{between variable: cognitive complexity of participant (high
#'   or low)}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_chap5_q2)
"rosenthal_chap5_q2"

#' Data set by Rosenthal and Rosnow (2000)
#'
#' Fictitious example of children ability, Table 6.8 in Rosenthal and Rosnow
#' (2000)
#'
#' @format a data frame with 8 rows and 4 columns
#' \describe{
#'   \item{id}{unique identifier of participant}
#'   \item{dv}{dependent variable}
#'   \item{within}{within variable}
#'   \item{between}{between variable}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_tbl68)
"rosenthal_tbl68"

#' Data set by Rosenthal and Rosnow (2000)
#'
#' Fictitious example corresponding to aggregated data set on p. 141 in
#' Rosenthal and Rosnow (2000)
#'
#' @format a data frame with 12 rows and 4 columns
#' \describe{
#'   \item{id}{unique identifier of participant}
#'   \item{dv}{dependent variable}
#'   \item{within}{within variable}
#'   \item{between}{between variable}
#' }
#'
#' @source Rosenthal, R., Rosnow, R. L., & Rubin, D. B. (2000). Contrasts and
#'   Effect Sizes in Behavioral Research: A Correlational Approach. Cambridge
#'   University Press.
#'
#' @usage data(rosenthal_p141)
"rosenthal_p141"

#' Music data set by Sedlmeier & Renkewitz (2018)
#'
#' Example 16.6, table 16.5 in Sedlmeier & Renkewitz (2018). Fictitious data set
#' with 8 participants that listened to no music, white noise, classical music,
#' and jazz music (within). The DV is a reading test.
#'
#' @format a data frame with 32 rows and 3 columns
#' \describe{
#'   \item{reading_test}{dependent variable}
#'   \item{participant}{unique id}
#'   \item{music}{within variable}
#' }
#'
#' @source Sedlmeier, P., & Renkewitz, F. (2018). Forschungsmethoden und
#'   Statistik für Psychologen und Sozialwissenschaftler (3rd ed.). Pearson
#'   Studium.
#'
#' @usage data(sedlmeier_p537)
"sedlmeier_p537"

#' Problem solving data set by Sedlmeier & Renkewitz (2018)
#'
#' Example 16.2, table 16.1 in Sedlmeier & Renkewitz (2018). Fictitious data set
#' with 15 boys divided into three groups (no training, boys-specific material,
#' girls-specific training material). The DV is the number of solved problem
#' (similar to the training).
#'
#' @format a data frame with 15 rows and 3 columns
#' \describe{
#'   \item{lsg}{dv, number of solved exercises}
#'   \item{between}{group, KT=no training, JT=boys-specific, MT=girls-specific}
#'   \item{lambda}{lambdas used for this example}
#' }
#'
#' @source Sedlmeier, P., & Renkewitz, F. (2018). Forschungsmethoden und
#'   Statistik für Psychologen und Sozialwissenschaftler (3rd ed.). Pearson
#'   Studium.
#'
#' @usage data(sedlmeier_p525)
"sedlmeier_p525"

#' Testing Effect data
#'
#' This dataset originates from a study conducted as part of a research seminar
#' in the Psychology B.Sc. program of the University of Cologne. The study
#' participants learned a list of 20 non-associated word pairs. Each half of the
#' word pair was associated with one of two sources (imaginating the word pair
#' in the sky or underwater). The final memory test (cued recall) was conducted
#' two days later. Cued recall means that one word of the word pair was
#' presented, and the participant had to recall the other word. The participants
#' were randomly assigned into one of three between-participant conditions:
#' restudy, source test, item test.
#'
#' @format a data frame with 60 rows and 3 variables:
#' \describe{
#'   \item{subject}{the participant's id}
#'   \item{condition}{the between-partipant condition}
#'   \item{recalled}{the number of words recalled in the cued-recall test}
#' }
#'
#' @usage data(testing_effect)
"testing_effect"

#' Data from Akan et al. (2018), experiment 2B
#'
#' Data contains information from a within-subjects experiment with N = 90
#' participants. The goal of the experiment was to investigate the benefits of
#' retrieval practice on memory performance. For the entire dataset and analysis
#' scripts see: \url{https://osf.io/bqr5f/}. The data was licensed under CC-BY
#' 4.0 Melisa Akan, Aaron Benjamin.
#'
#' @format a data frame with 270 rows and 3 variables:
#' \describe{
#'   \item{subject}{subject id}
#'   \item{condition}{experimental condition (test, restudy, control)}
#'   \item{recalled}{dependent variable}
#' }
#' @source Akan, M., Stanley, S. E., & Benjamin, A. S. (2018). Testing enhances
#'   memory for context. Journal of Memory and Language, 103, 19–27.
#'   \doi{10.1016/j.jml.2018.07.003}
#'
#' @usage data(akan)
"akan"

#' Data from Schwoebel et al. (2018)
#'
#' For the entire dataset and analysis scripts see:
#'
#' @format a data frame with 64 rows and 2 variables:
#' \describe{
#'   \item{condition}{experimental condition (massed-same, massed-different,
#'     spaced-same, spaced-different)}
#'   \item{percent_recalled}{dependent variable}
#' }
#' @source Schwoebel, J., Depperman, A. K., & Scott, J. L. (2018). Distinct
#'   episodic contexts enhance retrieval-based learning. Memory, 26(9),
#'   1291–1296. \doi{10.1080/09658211.2018.1464190}
#'
#' @usage data(schwoebel)
"schwoebel"

#' Haans within data example
#'
#' Fictitious data set from Haans, A. (2018). Contrast Analysis: A Tutorial.
#' https://doi.org/10.7275/7DEY-ZD62
#'
#' @format a data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{person}{person id}
#'   \item{name}{group name (sitting row 1 to 4)}
#'   \item{value}{dv, final exam grade}
#' }
#'
#' @usage data(haans_within1by4)
"haans_within1by4"
