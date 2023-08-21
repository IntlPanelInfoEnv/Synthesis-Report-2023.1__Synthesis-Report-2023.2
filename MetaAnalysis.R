# this code is the supplement for the report 
# 
# International Panel on the Information Environment, 2023. Platform Responses to
# Misinformation: A Meta-Analysis of Data. SR2023.2. Zurich, Switzerland: IPIE. 
# 
# https://uploads-ssl.webflow.com/643ecb10be528d2c1da863cb/64be617bf46991cc7bdd21a6_SR2023.2.pdf

library(dplyr)
library(ggraph)
library(ggplot2)
library(readxl)
library(tidyr)
library(snakecase)
library(tokenizers)
library(tibble)
library(quanteda)
library(stringr)
library(purrr)
library(plotly)
library(scales)
library(meta)
library(metafor)
library(esc)

meta <-
  read.csv(
    "MetaAnalysis.csv", check.names = FALSE  )

#to analyse a specific subset, please refer to the relevant row number in the following function:
# LablbPer: 2:20 
# CamaignPer: 22:45
# LitPer: 47:51
# sharing: 52:53
metaG <- meta['=Please replace this line with the relevant row numbers for the variable of interest as directed above='
, ] %>%
  select(
    'Outcome',
    'AU',
    'Year',
    'nTotal',
    'StudyType',
    'Platform',
    'SampleType',
    'GeographicContext',
    'MisinforSource',
    'LabelOrCorrectionSourceClean',
    'LabelOrCorrectionFormat',
    'MisinformationFormat',
    'IssueType',
    'LabelOrCorrectionType',
    'd',
    '95%C.I.min',
    '95%C.I.max',
    'v'
  ) %>%
  rowwise() %>%
  mutate(g = hedges_g(as.numeric(d), as.numeric(nTotal))) %>%
  mutate(se = sqrt(as.numeric(v))) %>% #accoridng to https://stats.stackexchange.com/questions/8487/how-do-you-calculate-confidence-intervals-for-cohens-d
  mutate(AU = str_to_title(AU))

metaGG <- metaG[complete.cases(metaG$g),]

#publications in the sample
length(unique(metaGG$AU))

#the generic inverse variance method for meta-analysis
m.gen <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE
)

#I: Heterogeneity analysis is a method that treats the primary results as outcomes
#and studies their relationship to study-level covariates (Thompson and Higgins 2002)
m.gen[["sm"]] <- c("g")
summary(m.gen)

png(
  file = "FigureMetaMain.png",
  width = 2200,
  height = 2100,
  res = 300
)

forest.meta(
  m.gen,
  sortvar = TE,
  fontsize = 28,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g", "95% CI      "),
  leftcols = c("studlab", "nTotal",  "seTE"),
  leftlabs = c("                         Authors", "Sample Size   ", "SE     "),
  col.diamond = "#EEAE00",
  col.diamond.lines = "black",
  col.square = "#396AB2",
  col.square.lines = "black"
)

dev.off()

#subgroup analysis
#1ab
m.genTrialType <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = ifelse(StudyType == "RCT", "RCT", "QES"),
  print.subgroup.name = TRUE
)

m.genTrialType[["sm"]] <- c("g")

png(
  file = "SuppFigureRCT-QES.png",
  width = 2100,
  height = 2500,
  res = 300
)

forest.meta(
  m.genTrialType,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",  "seTE"),
  leftlabs = c("Authors", "Sample Size", "SE")
)
dev.off()

#2a-b PlatformType
m.genPlatform <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = ifelse(Platform == "Facebook", "Facebook", "Other"),
  print.subgroup.name = TRUE
)

m.genPlatform[["sm"]] <- c("g")

png(
  file = "SuppFigure2a-bPlatformType.png",
  width = 2800,
  height = 2400,
  res = 300
)

forest.meta(
  m.genPlatform,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal", "Platform", "seTE"),
  leftlabs = c("Author", "Sample Size", "Platform", "SE   ")
)
dev.off()

#3SourceTypea-b
m.genLabelSource <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = LabelOrCorrectionSourceClean,
  print.subgroup.name = TRUE
)

m.genLabelSource[["sm"]] <- c("g")

png(
  file = "SuppFigure3SourceTypea-b.png",
  width = 2800,
  height = 2400,
  res = 300
)

forest.meta(
  m.genLabelSource,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",   "seTE"),
  leftlabs = c("Author", "Sample Size",   "SE   "),
  print.subgroup.name	      = FALSE,
)
dev.off()

#4a-bElaboration Level
m.LabelOrCorrectionType <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = LabelOrCorrectionType,
  print.subgroup.name = TRUE
)

m.IssueType[["sm"]] <- c("g")

png(
  file = "SuppFigure4a-bElaboration Level.png",
  width = 2800,
  height = 2800,
  res = 300
)

forest.meta(
  m.LabelOrCorrectionType,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",   "seTE"),
  leftlabs = c("Author", "Sample Size",   "SE   "),
  print.subgroup.name	      = FALSE,
)
dev.off()

#5a-bFormat
m.LabelOrCorrectionFormat <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = LabelOrCorrectionFormat,
  print.subgroup.name = TRUE
)

m.LabelOrCorrectionFormat[["sm"]] <- c("g")

png(
  file = "SuppFigure5a-bFormat.png",
  width = 2800,
  height = 2800,
  res = 300
)

forest.meta(
  m.LabelOrCorrectionFormat,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",   "seTE"),
  leftlabs = c("Author", "Sample Size",   "SE   "),
  print.subgroup.name	      = FALSE,
)
dev.off()


#6a-bIssueType
m.IssueType <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup = IssueType,
  print.subgroup.name = TRUE
)

m.IssueType[["sm"]] <- c("g")

png(
  file = "SuppFigure6a-bIssueType.png",
  width = 2800,
  height = 2900,
  res = 300
)

forest.meta(
  m.IssueType,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",   "seTE"),
  leftlabs = c("Author", "Sample Size",   "SE   "),
  print.subgroup.name	      = FALSE,
)
dev.off()


#7a-bISampleSize
small <- 500

m.LabelOrCorrectionType <- metagen(
  TE = g,
  seTE = se,
  studlab = paste(AU, Year),
  data = metaGG,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "EB",
  hakn = FALSE,
  subgroup =
    case_when(nTotal < small ~ '<500',
              TRUE ~ '>500'),
  print.subgroup.name = TRUE
)

m.IssueType[["sm"]] <- c("g")

png(
  file = "SuppFigure71-bSampleSize.png",
  width = 2800,
  height = 2400,
  res = 300
)

forest.meta(
  m.LabelOrCorrectionType,
  sortvar = TE,
  print.tau = TRUE,
  print.tau.ci = TRUE,
  fontsize = 20,
  fontfamily = "Source Sans Pro",
  rightcols = c("effect", "ci"),
  rightlabs = c("g   ", "95%--CI   "),
  leftcols = c("studlab", "nTotal",   "seTE"),
  leftlabs = c("Author", "Sample Size",   "SE   "),
  print.subgroup.name	      = FALSE,
)
dev.off()


#################Bias Analysis##############
# Analyze with outliers removed for the tro types of the main subsets
subsetL = c(8, 9, 12) #labeling
subsetI = c(7, 8, 9, 11) #corrective information

#egger's test
metabias(m.gen, method.bias = "linreg")
metabias(m.gen, method.bias = "linreg",
         subset = -subsetL)

#publication bias analysis
par(mar = c(3, 3, 3, 3))
contour <- c(0.9, 0.95, 0.99)

# Define fill colors for contour
col.contour <- c("gray75", "gray85", "gray95")
ld <- c("p < 0.1", "p < 0.05", "p < 0.01")



#Duval & Tweedie Trim and Fill Method
# Using all studies
tf <- trimfill(m.gen)
summary(tf)

tf.no.out <- trimfill(update(m.gen,
                             subset = -subsetL))
summary(tf.no.out)

# Use 'par' to create two plots in one row (row, columns)
par(mfrow = c(1, 2))
#xlim for labeling: -3, 4
#xlim for corrective Information: -1, 2.5
# Contour-enhanced funnel plot (full data)
funnel.meta(tf,
            xlim = c(-3, 4),
            contour = contour,
            col.contour = col.contour)

# Contour-enhanced funnel plot (outliers removed)
funnel.meta(
  tf.no.out,
  xlim = c(-3, 4),
  contour = contour,
  col.contour = col.contour
)
