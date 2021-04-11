# packages need to install:
install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")
install.packages("devtools")
devtools::install_github("MathiasHarrer/dmetar")

library(tidyverse)
library(meta)
library(metafor)
library(dmetar)
library(dplyr)

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

# do meta analysis for Binary outcomes
# import data:
binarydata <- read.csv("binarydata.csv")

# Meta analysis with raw binary data, using random-effect model and 
# RR to be the summary measure
m.bin <-  metabin(Ec,
                  Ne,
                  Ec,
                  Nc,
                  data = binarydata,
                  studlab = paste(Author),
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE,
                  incr = 0.1,
                  sm = "RR")
m.bin

# L'AbbePlots
labbe.metabin(x = m.bin,
              bg = "red",
              studlab = TRUE,
              col.random = "blue")

# incidence rates
# load data
load("C:/Users/17524/Desktop/study/R/Meta-analysis/IRR.data.RData")

# metainc() in meta packages
args(metainc)

m.IRR <- metainc(event.e,
                 time.e,
                 event.c,
                 event.c,
                 studlab = paste(Author),
                 data = IRR.data,
                 sm = "IRR",
                 method.tau = "DL",
                 comb.random = TRUE,
                 comb.fixed = FALSE,
                 hakn = TRUE)
m.IRR

# Pre-caculated effect sizes
# first, log-transform all the effect size data
bin.metagen$RR <- log(bin.metagen$RR)
bin.metagen$lower <- log(bin.metagen$lower)
bin.metagen$upper <- log(bin.metagen$upper)
# calculate the standard error seTE
bin.metagen$seTE <- (bin.metagen$upper - bin.metagen$lower)/3.92
# reconvert effect sizes to their original scale, specificy sm = "RR"
metagen(RR,
        seTE,
        studlab = Author,
        method.tau = "SJ",
        sm = "RR",
        data = bin.metagen)

# When pooling correlations, it is advised to perform Fisher's z-
# transformation to obtain accurate weights for each study.
# function: metacont()
str(metacor)
# the data in metacor() should inclding study label, the correlation
# r(cor) reported for each study , and the sample size(n) for each study
load("cordata.R")
m.cor <- metacor(cor,
                 n,
                 data = cordata,
                 studlab = cordata$Author,
                 sm = "ZCOR",
                 method.tau = "SJ")
m.cor

# Generating a Forest Plot
# forest() function
str(forest)
forest(m.hksj.raw)
?meta::forest

forest(m.hksj.raw,
       sortvar = TE,
       xlim = c(-1.5, 0.5),
       rightlabs = c("g", "95% CI", "weight"),
       leftlabs = c("Author", "N", "Mean", "SD", "N", "Mean", "SD"),
       lab.e = "Intervention",
       pooled.totals = FALSE,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = FALSE,
       col.diamond = "blue",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2.ci = TRUE,
       digits.sd = 2
       )

# two layout types of forest plot: "RevMan5" and "JAMA"
# "RevMan5"
forest(m.hksj.raw,
       layout = "RevMan5",
       digits.sd = 2)
# "JAMA"
# "RevMan5"
forest(m.hksj.raw,
       layout = "JAMA",
       text.predict = "95% CI",
       col.predict = "black",
       colgap.forest.left = unit(15, "mm"))

# Saving the forest plots
pdf(file='forestplot with JAMA layout.pdf')
forest.jama <- forest(m.hksj.raw,
                      layout = "JAMA",
                      text.predict = "95% CI",
                      col.predict = "black",
                      colgap.forest.left = unit(15, "mm"))
dev.off()

png(file='forestplot with JAMA layout.png')
forest.jama <- forest(m.hksj.raw,
                      layout = "JAMA",
                      text.predict = "95% CI",
                      col.predict = "black",
                      colgap.forest.left = unit(15, "mm"))
dev.off()

svg(file='forestplot with JAMA layout.svg')
forest.jama <- forest(m.hksj.raw,
                      layout = "JAMA",
                      text.predict = "95% CI",
                      col.predict = "black",
                      colgap.forest.left = unit(15, "mm"))
dev.off()

# Hetergeneity Measures Q I^2 tau^2
m.hksj

# Detecting Outliers & Influential Cases
m.hksj$lower.random
m.hksj$upper.random

spot.outliers.random <- function(metagen_output){
        data <- metagen_output
        Author <- data$studlab
        lowerci <- data$lower
        upperci <- data$upper
        m.outliers <- data.frame(Author, lowerci, upperci)
        te.lower <- data$lower.random
        te.upper <- data$upper.random
        dplyr::filter(m.outliers, upperci < te.lower)
        dplyr::filter(m.outliers, lowerci > te.upper)
}

spot.outliers.random(metagen_output = m.hksj)

m.hksj.outliers <- update.meta(m.hksj,
                               subset = Author != c("DanitzOrsillo",
                                                    "Shapiro et al."))
m.hksj.outliers

# for fixed-effect model:
spot.outliers.fixed <- function(metagen_output){
        data <- metagen_output
        Author <- data$studlab
        lowerci <- data$lower
        upperci <- data$upper
        m.outliers <- data.frame(Author, lowerci, upperci)
        te.lower <- data$lower.fixed
        te.upper <- data$upper.fixed
        dplyr::filter(m.outliers, upperci < te.lower)
        dplyr::filter(m.outliers, lowerci > te.upper)
}

# Influence Analyses

InfluenceAnalysis(x = m.hksj,
                  random = TRUE,
                  return.separate.plots = TRUE)

# GOSH plot analysis

# 将用meta包生成的数据转换为metafor可以识别的数据：
m.rma <- rma(yi = m.hksj$TE,
             sei = m.hksj$seTE,
             method = m.hksj$method.tau,
             test = "knha")
# generate the GOAH plot:
dat.gosh <- gosh(m.rma)
plot(dat.gosh, alpha= 0.1, col = "blue")

gosh.diagnostics(dat.gosh)

metagen(TE,
        seTE,
        data = madata,
        studlab = paste(Author),
        comb.fixed = FALSE,
        comb.random = TRUE,
        method.tau = "SJ",
        hakn = TRUE,
        prediction = TRUE,
        sm = "SMD",
        exclude = c(3, 10, 16))

# subgroup analysis
# using the Mixed-Effects-Model

subgroup.analysis.mixed.effects(x = m.hksj,
                                subgroups = madata$Control)
# p值大于0.05， 说明亚组之间差距无统计学意义

# using the random-effects model

region.subgroup <- update.meta(m.hksj,
                               byvar = region,
                               comb.random = TRUE,
                               comb.fixed = FALSE)
region.subgroup

# Meta regression

metareg(m.hksj, Control)
# continuous variables
madata$pub_year<-c(2001,2002,2011,2013,2013,2014,1999,2018,2001,2002,
                   2011,2013,2013,2014,1999,2018,2003,2005)
madata$pub_year<-as.numeric(madata$pub_year)
m.pubyear<-metagen(TE,
                   seTE,
                   studlab = paste(Author),
                   comb.fixed = FALSE,
                   data=madata)
m.pubyear
output.metareg <- metareg(m.pubyear, pub_year)
output.metareg

# plotting regressions: bubble() function in meta
str(meta::bubble)
bubble(output.metareg,
       xlab = "Publication Year",
       col.line = "blue",
       studlab = TRUE)





