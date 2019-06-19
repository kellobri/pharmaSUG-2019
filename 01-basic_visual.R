# OpenFDA and BigRQuery Demo Script

#Load Data Packages
library(bigrquery)
library(openfda)
#Load Data Query Packages
library(dplyr)
library(DBI)
#Load Visualization Packages
library(ggplot2)
library(ggthemes)

# Setup Connection to BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = 'bigquery-public-data',
  dataset = 'nlm_rxnorm',
  billing = bq_test_project()
)

pathways <- tbl(con, 'rxn_all_pathways_01_18')

# Get all brand name drugs from BigQuery
all_names <- pathways %>% 
  filter(TARGET_TTY == 'BN') %>% 
  select(TARGET_NAME) %>% 
  group_by(TARGET_NAME) %>% 
  summarise() %>% 
  collect()

# Helper Functions
get_adverse <- function(gender, brand_name, age) {
  fda_query("/drug/event.json") %>%
    fda_filter("patient.drug.openfda.brand_name", brand_name) %>% 
    fda_filter("patient.patientsex", gender) %>% 
    fda_filter("patient.patientonsetage", age) %>% 
    fda_count("patient.reaction.reactionmeddrapt.exact") %>% 
    fda_limit(10) %>% 
    fda_exec()
}

create_age <- function(min, max){#
  sprintf('[%d+TO+%d]', min, max)
}

# Collect adverse events data for Tylenol
sel_name <- 'Tylenol'
age <- create_age(20,60)

ingredients <- pathways %>%
  filter(TARGET_NAME==sel_name, TARGET_TTY == 'BN', SOURCE_TTY %in% c('IN', 'SY')) %>% 
  select(SOURCE_NAME, SOURCE_TTY) %>% 
  collect()

male <- get_adverse("1", sel_name,age) %>% 
  mutate(gender = 'male')

female <- get_adverse("2", sel_name, age) %>% 
  mutate(gender = 'female')

adverse <- rbind(male, female)

adverse

# Visualize!
adverse %>%
  group_by(term) %>%
  summarise(count = sum(count)) %>%
  ggplot() + geom_bar(aes(x = term, y = count), stat = "identity")

#
# Pro Tip: By default, geom_bar uses stat="count" which makes the height of the bar proportion 
# to the number of cases in each group (or if the weight aethetic is supplied, 
# the sum of the weights). If you want the heights of the bars to represent values in the data, 
# use stat="identity" and map a variable to the y aesthetic.
#

# Break out by gender

adverse %>%
  ggplot() +
  geom_bar(aes(reorder(term, count), count, fill = gender), stat = "identity") +
  facet_wrap(~gender) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(
    title = sel_name
  )
