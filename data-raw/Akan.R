###############################################################################
############# Data preparation (Experiment 2B, Akan et al., 2018) #############
###############################################################################

### Akan, M., Stanley, S.E., & Benjamin, A.S. (2018).
### Testing enhances memory for context.
### Journal of Memory and Language, 103, 19-27.
### https://doi.org/10.1016/j.jml.2018.07.003

### The authors investigate the effect of testing on item
### and context memory an item-then-context-memory task.

### Experiment 2B is 3x2 within-subject design with N = 90.
### Factor 1: practice phase (test, restudy, control)
### Factor 2: final test (item test, context test)

### Only Factor 1 (i.e., performance data of the final
### item-memory test) will be prepared.

### The resulting data file contains the following information:
### subject = subject number
### condition = experimental condition (test, restudy, control)
### recalled = number of items correctly recalled on the item-memory test

### Load packages
library(dplyr)
library(readxl)
library(ggplot2)

### Load data
raw.dat <- read_excel("data-raw/Exp2B.xlsx")

### Aggregate data
aggr.dat <- raw.dat %>%
  group_by(SubjectNo, `Condition (1=Test, 2=Restudy, 3=Control)`) %>%
  reframe(Condition=first(`Condition (1=Test, 2=Restudy, 3=Control)`),
          Recalled=sum(`Final Test Accuracy **CONTEXT RECALL** (1=correct, 0=incorrect)`),
          NewCondition=case_when(Condition==1 ~ "test",
                                 Condition==2 ~ "restudy",
                                 Condition==3 ~ "control"))

akan <- aggr.dat %>%
  transmute(subject=SubjectNo,
            condition=NewCondition,
            recalled=Recalled) # 16 seems to be the max according to Simone

akan$condition <- factor(akan$condition, levels = c("test", "restudy", "control"))

usethis::use_data(akan, overwrite = T)
