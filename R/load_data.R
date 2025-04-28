library(idaifieldR)
library(dplyr)

## last used with idaifieldR 0.3.5!

options(digits = 20)
connection <- connect_idaifield(pwd = "hallo", 
                                project = "milet")

index <- get_field_index(connection)

bwkat_id <- index %>% filter(identifier == "Bauwerkskatalog") %>% pull(UID)
buildings_raw <- idf_query(connection, 
                           field = "relations.liesWithin", 
                           value = bwkat_id) %>%
  check_and_unnest()

buildings <- buildings_raw %>%
  simplify_idaifield(replace_uids = TRUE, keep_geometry = TRUE, uidlist = index)

data_mat <- buildings %>% idaifield_as_matrix() %>% as.data.frame()
rownames(data_mat) <- data_mat[,"identifier"]

periods_dat <- read.csv(file = "R/data/period_dat.csv", sep = ",", encoding = "UTF-8")
periods_dat <- periods_dat[,-1]
periods_dat <- periods_dat[!is.na(periods_dat$period),]
periods_dat$period <- factor(periods_dat$period, levels = periods_dat$period, labels = periods_dat$period, ordered = TRUE)

periods_groups <- read.csv(file = "R/data/period_cleanGroups.csv", sep = ",", encoding = "UTF-8")
periods_groups$group <- factor(periods_groups$group, levels = periods_groups$group, labels = periods_groups$group, ordered = TRUE)

