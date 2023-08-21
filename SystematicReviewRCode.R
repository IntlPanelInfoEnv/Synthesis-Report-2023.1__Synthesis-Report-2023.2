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

SF <- read.csv('SystematicReviewPublicationsIncluded.csv')

################Graphics##############
## Setup for Graphics --------------------------
ourDataME <- separate_rows(SF, MeasuresProposed, sep = ",") %>%
  mutate(NNM = make.unique(NN, sep = "_"))

#make it nice looking
ourDataME$MeasuresProposed <-
  case_when(
    ourDataME$MeasuresProposed == '0' ~ 'Nonspecific or Other',
    ourDataME$MeasuresProposed == '1' ~ 'Advertisement Policy',
    ourDataME$MeasuresProposed == '2' ~ 'Content Labeling',
    ourDataME$MeasuresProposed == '3' ~ 'Content or Account Moderation',
    ourDataME$MeasuresProposed == '4' ~ 'Content Reporting',
    ourDataME$MeasuresProposed == '5' ~ 'Content User Sharing',
    ourDataME$MeasuresProposed == '6' ~ 'Corrective Information',
    ourDataME$MeasuresProposed == '7' ~ 'Disinformation Disclosure',
    ourDataME$MeasuresProposed == '8' ~ 'Information & Media Literacy',
    ourDataME$MeasuresProposed == '9' ~ 'Redirection',
    ourDataME$MeasuresProposed == '10' ~ 'Security or Verification',
    ourDataME$MeasuresProposed == '11' ~ 'Nonspecific or Other',
    ourDataME$MeasuresProposed == '12' ~ 'Self-fact-checking',
    is.na(ourDataME$MeasuresProposed) ~ 'Nonspecific or Other',
    TRUE ~ ourDataME$MeasuresProposed
  )

# N=selected papers, streamlined categories
measures <- ourDataME %>%
  count(MeasuresProposed)

measuresL <- ourDataME %>%
  filter(Measures.Link == '1') %>%
  count(MeasuresProposed)

measuresE <- ourDataME %>%
  filter(MeasureEffects == '1') %>%
  count(MeasuresProposed)

measures <-
  measures %>% left_join(measuresL, by = "MeasuresProposed") %>% left_join(measuresE, by = "MeasuresProposed") %>%
  rename(
    Measures = n.x ,
    MeasuresLink = n.y  ,
    MeasuresEffect = n
  )  %>%
  na.omit() %>% 
  arrange(desc(Measures))

## Figure 2. Distribution of Measures by Country of First Author Affiliation.  -----------------------------------------------------------
combinations_simple <- ourDataME[ourDataME$MeasureEffects == '1' & ourDataME$MeasuresProposed != 'NA',] %>%
  count(MeasuresProposed, Country) %>%
  na.omit() %>%
  arrange(desc(n)) %>%
  mutate(Country = to_any_case(Country, "title", abbreviations = c("USA")))

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 3, 6, 11, 22)

combinations_simple %>%
  ggplot(aes(x = reorder(Country, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = 'Source Sans Pro', color = 'black'),
        axis.text.y = element_text(size = 28, family = 'Source Sans Pro', color = 'black'),
        legend.text = element_text(size = 28, family = "Source Sans Pro"),
        legend.title = element_text(size = 28, family = "Source Sans Pro")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", 
                       breaks=my_breaks, na.value="white", 
                       name = "Number of Publications",option = "viridis"
  ) + 
  theme(text = element_text(size = 12, family = 'Source Sans Pro')) 

ggsave(paste0('Figure 2. Distribution of Measures by Country of Author Affiliation. .png'), width = 2400, height = 1600, dpi = 300, units = 'px')

## Figures 3 a-b. Distribution of Measures by Journal Field.   -----------------------------------------------------------
#Use Journal list
JL <- data.frame(read_excel('JL.xlsx',
                            sheet = 'Scopus Sources October 2022')) %>%
  mutate(SO = toupper(SO))

temp <- ourDataME %>% left_join(select(JL, SO, HS, LS, PS, SS), by = c("SO" = "SO")) %>%
  unite(SType, c(HS, LS, PS, SS), sep = ",", remove = TRUE, na.rm = TRUE)

ourDataMJ <- separate_rows(temp, SType, sep=",") %>%
  mutate(NNM = make.unique(NN, sep="_"))  

#a
combinations_simple <- ourDataMJ %>%
  count(MeasuresProposed, SType) %>%
  filter(!(SType=="")) %>%
  mutate(MeasuresProposed = ifelse(MeasuresProposed == 'NA', NA, MeasuresProposed))%>% 
  mutate(SType = ifelse(SType == '', NA, SType))%>% 
  arrange(desc(n))%>%
  filter(n > 6) %>%
  mutate(MeasuresProposed = case_when(
    is.na(MeasuresProposed) ~ 'None',
    TRUE ~ MeasuresProposed))

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  na.omit() %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(8, 15, 31, 60, 103)

combinations_simple %>%
  ggplot(aes(x = reorder(SType, -n), y = MeasuresProposed , fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications", option = "viridis")

ggsave(paste0('Figure 3a. Distribution of Measures by Journal Field.png'), width = 1600, height = 1600, dpi = 300, units = 'px')

#b
combinations_simple <- ourDataMJ %>%
  filter((MeasureEffects == '1' )) %>%
  count(MeasuresProposed, SType) %>%
  filter(!(SType=="" )) %>%
  mutate(MeasuresProposed = ifelse(MeasuresProposed == 'NA', NA, MeasuresProposed))%>% 
  mutate(SType = ifelse(SType == '', NA, SType))%>% 
  arrange(desc(n)) %>%
  filter(n > 6)

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  na.omit() %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(7, 11, 19)

combinations_simple %>%
  ggplot(aes(x = reorder(SType, -n), y = MeasuresProposed , fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications", option = "viridis")

ggsave(paste0('Figure 3b. Distribution of Measures by Journal Field.png'), width = 1600, height = 1600, dpi = 300, units = 'px')

## Figures 4a-b. Distribution of Measures by a Publication Focus Identified through Keywords.   -----------------------------------------------------------

#keywords analysis
#test
tokenizers::tokenize_regex(SF$Keywords2, pattern = ";") %>%
  as.tokens() %>%
  dfm() %>% 
  topfeatures(n = 60) %>% 
  enframe() %>% print(n = 60)

healthK <- c("public health", "covid-19","vaccination","medical information","pandemic","coronavirus disease 2019","sars-cov-2","epidemiology",
             "betacoronavirus","epidemic")

politicsK <- c("politics","trust", "president", "trump", "elections")

psychologyK <- c("psychology", "attitude", "human experiment", "behavior")

china <- c("china", "chinese")

unitedStates <- c("united states", "US")

ourDataKM <- ourDataME %>%
  unite(col = "KW", Keywords:Keywords2, sep = ",",na.rm = TRUE) %>%
  drop_na (KW) %>%
  mutate(matches = str_extract_all(
    KW,
    str_c(healthK, collapse = "|") %>% regex(ignore_case = T)),
    Match = map_chr(matches, str_c, collapse = ","),
    HCount = map_int(matches, length)) %>%
  mutate(matches = str_extract_all(
    KW,
    str_c(politicsK, collapse = "|") %>% regex(ignore_case = T)),
    Match = map_chr(matches, str_c, collapse = ","),
    PoCount = map_int(matches, length))  %>%
  mutate(matches = str_extract_all(
    KW,
    str_c(psychologyK, collapse = "|") %>% regex(ignore_case = T)),
    Match = map_chr(matches, str_c, collapse = ","),
    PCount = map_int(matches, length))  %>%
  mutate(matches = str_extract_all(
    KW,
    str_c(china, collapse = "|") %>% regex(ignore_case = T)),
    Match = map_chr(matches, str_c, collapse = ","),
    CCount = map_int(matches, length))  %>%
  mutate(matches = str_extract_all(
    KW,
    str_c(unitedStates, collapse = "|") %>% regex(ignore_case = T)),
    Match = map_chr(matches, str_c, collapse = ","),
    UCount = map_int(matches, length))  %>%
  mutate(HCount = case_when(HCount != "0" ~ "Health")) %>%
  mutate(PCount = case_when(PCount != "0" ~ "Psychology")) %>%
  mutate(PoCount = case_when(PoCount != "0" ~ "Politics")) %>%
  mutate(CCount = case_when(CCount != "0" ~ "China")) %>%
  mutate(UCount = case_when(UCount != "0" ~ "United States"))  %>%
  unite(col = "KM", HCount:UCount, sep = ",",na.rm = TRUE) %>%
  select(-matches, -Match)

ourDataKM <- separate_rows(ourDataKM, KM, sep=",") %>%
  mutate(NNM = make.unique(NN, sep="_")) %>%
  mutate(MeasuresProposed = ifelse(MeasuresProposed == 'NA', NA, MeasuresProposed)) %>%
  filter(KM != '')

#a
combinations_simple <- ourDataKM %>%
  filter(KM != '')  %>% # run to determine the size: select (NN,MeasuresProposed) %>% na.omit() and after this: length((unique(combinations_simple$NN)))
  count(MeasuresProposed, KM) %>%
  na.omit() %>%
  arrange(desc(n)) 

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  na.omit()%>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 4, 8, 17,  30, 60)

combinations_simple %>%
  ggplot(aes(x = reorder(KM, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications",option = "viridis")

ggsave(paste0('Figure 4a. Distribution of Measures by a Publication Focus Identified through Keywords.png'), width = 1600, height = 1600, dpi = 300, units = 'px')

#b
combinations_simple <- ourDataKM %>%
  filter(MeasureEffects == '1') %>%
  count(MeasuresProposed, KM) %>%
  na.omit() %>%
  arrange(desc(n)) 

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  na.omit() %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 5, 11, 15)

combinations_simple %>%
  ggplot(aes(x = reorder(KM, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications",option = "viridis")

ggsave(paste0('Figure 4b. Distribution of Measures by a Publication Focus Identified through Keywords.png'), width = 1600, height = 1600, dpi = 300, units = 'px')

## Figure 5. Study Methods.-----------------------------------------------------------
#ungrouping comma-separated for measures&methods
ourDataMM <- separate_rows(ourDataME, Methods, sep=",") %>%
  filter(!(MeasuresProposed == "NA"))

ourDataMM$Methods <- 
  case_when(
    ourDataMM$Methods == '0' ~ 'None or Other',
    ourDataMM$Methods == '1' ~ 'Survey',
    ourDataMM$Methods == '2' ~ 'Interview',
    ourDataMM$Methods == '3' ~ 'Focus Groups',
    ourDataMM$Methods == '4' ~ 'None or Other',
    ourDataMM$Methods == '5' ~ 'Experiment',
    ourDataMM$Methods == '6' ~ 'Manual Content Analysis',
    ourDataMM$Methods == '7' ~ 'Computational Content Analysis',
    ourDataMM$Methods == '8' ~ 'Network Analysis',
    ourDataMM$Methods == '9' ~ 'Simulation',
    ourDataMM$Methods == '10' ~ 'Process Tracing or Case Study',
    ourDataMM$Methods == '100' ~ 'None or Other',
    is.na(ourDataMM$Methods) ~ 'None or Othe',
    TRUE ~ ourDataMM$Methods
  )

# N=selected papers, streamlined categories
methods <- ourDataMM %>%
  filter(!(MeasuresProposed == "None")) %>%
  count(Methods) %>%
  arrange(desc(n))

par(mar = c(4, 4, .1, .1))

ggplotly(
  ggplot(methods, aes(x = reorder(Methods, n), y = n)) + 
    geom_bar(stat = "identity", fill = "indianred")+
    theme_minimal()+
    coord_flip()+
    ylab('') +
    xlab('') +
    scale_y_continuous(breaks= pretty_breaks()) 
)

ggsave(paste0('Figure5Methods.png'), width = 2800, height = 2400, dpi = 300, units = 'px')


## Figures 6a-b. Study Methods, by Proposed Countermeasures. -----------------------------------------------------------
#a
combinations_simple <- ourDataMM %>%
  count(MeasuresProposed, Methods) %>%
  arrange(desc(n))

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 4, 10, 25, 40, 68)

combinations_simple %>%
  ggplot(aes(x = reorder(Methods, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Publications",option = "viridis")

ggsave(paste0('Figure 6a. Study Methods, by Proposed Countermeasures..png'), width = 1600, height = 1600, dpi = 300, units = 'px')

#b
#ungrouping comma-separated for measures&methods
combinations_simple <- ourDataMM %>%
  filter(Measures.Link == '1') %>%  
  count(MeasuresProposed, Methods) %>%
  arrange(desc(n))

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  na.omit()%>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 5, 10, 17, 50)

combinations_simple %>%
  ggplot(aes(x = reorder(Methods, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications",option = "viridis")

ggsave(paste0('Figure 6b. Study Methods, by Proposed Countermeasures.png'), width = 1600, height = 1600, dpi = 300, units = 'px')

## Figures 7c. Study Methods, by Proposed Countermeasures. -----------------------------------------------------------

#the same as 7a-b, just when there is an effect text
#ungrouping comma-separated for measures&methods
combinations_simple <- ourDataMM %>%
  filter(MeasureEffects == '1') %>% #length(unique(combinations_simple$NN))  
  count(MeasuresProposed, Methods) %>%
  arrange(desc(n))

o <- combinations_simple %>% 
  count(MeasuresProposed) %>%
  arrange(desc(-n))

o <- unique(o$MeasuresProposed)

my_breaks = c(1, 4, 12, 19)

combinations_simple %>%
  ggplot(aes(x = reorder(Methods, -n), y = MeasuresProposed, fill = n)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 28, family = "Source Sans Pro", color = "black"),
        axis.text.y = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.text = element_text(size = 28, family = "Source Sans Pro", color = "black"),
        legend.title = element_text(size = 28, family = "Source Sans Pro", color = "black")
  ) +
  scale_y_discrete(limits=o) +
  ylab("")  + 
  xlab("")  + 
  scale_fill_viridis_c(direction = -1, trans = "log", breaks=my_breaks, na.value="white", name = "Number of Publications",option = "viridis")

ggsave(paste0('Figure 7c. Study Methods, by Proposed Countermeasures..png'), width = 1600, height = 1600, dpi = 300, units = 'px')