# packages need to install:
install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("devtools")
devtools::install_github("MathiasHarrer/dmetar")

# Import data:
madata <- read.csv("Meta_analysis_Data.csv")
# 对文件的变量类型等进行修改，使其符合分析的要求
# amendent the type of variables in the file to meet the need of meta analysis
madata$RoB <- factor(madata$RoB)
intervention.type.logical <- as.logical(madata$intervention.type == "mindfulness")

# Fix-ed-Effects-Model
m <- metagen(TE,
             seTE,
             data = madata,
             studlab = paste(Author),
             comb.fixed = TRUE,
             comb.random = FALSE,
             prediction = TRUE,
             sm = "SMD")
m
# the result including:
# the individual effect sizes for eah study, and their weight
# the total number of included studies(ie. k)
# the overall effect (ie. SMD) and its 95%CI and p-value
# measures of between-study heterogeneity, such as tau2 and I2 and a Q-test of heterogeneity

# Can use $ command to have a look at various outputs directly
m$lower.I2

# Save the result
sink("result.txt")
print(m)
sink()

# Raw Effect Size Data
metacont <- read.csv("metacont.csv")
m.raw <- metacont(Ne,
                  Me,
                  Se,
                  Nc,
                  Mc,
                  Sc,
                  data=metacont,
                  studlab=paste(Author),
                  comb.fixed = TRUE,
                  comb.random = FALSE,
                  prediction=TRUE,
                  sm="SMD")
m.raw
# Save the result:
sink("result_raw_effect_size.txt")
print(m.raw)
sink()

# random-effects-model meta-analysis:
m.hksj <- metagen(TE,
                  seTE,
                  data = madata,
                  studlab = paste(Author),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  sm = "SMD")
m.hksj

# the value of SMD in this result is 0.5935, it becomes clear that
# this effect is larger than the one found in the fixed-effect-model
# meta analysis since that SMD value is only 0.48

# using DerSimonianLaird estimator(dl)
m.dl <- metagen(TE,
                seTE,
                data = madata,
                studlab = paste(Author),
                comb.fixed = FALSE,
                comb.random = TRUE,
                hakn = FALSE,
                prediction = TRUE,
                sm = "SMD")
m.dl
# we can see that the overall size estimate is similar but the CI is 
# narrow because we didn't adjust it using the HKSJ method.

# Raw effect size data
m.hksj.raw <- metacont(Ne,
                       Me,
                       Se,
                       Nc,
                       Mc,
                       Sc,
                       data = metacont,
                       studlab = paste(Author),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "SJ",
                       hakn = TRUE,
                       prediction = TRUE,
                       sm = "SMD")
m.hksj.raw



