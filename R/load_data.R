library(idaifieldR)
## this will work only from idaifieldR 0.2.2!
options(digits = 20)
connection <- connect_idaifield(serverip = "127.0.0.1",
                                user = "R",
                                pwd = "hallo")

buildings_raw <- get_idaifield_docs(
  connection = connection,
  projectname = "milet", raw = FALSE) %>%
  select_by(by = "liesWithin", value = "Bauwerkskatalog")

buildings <- buildings_raw %>%
  simplify_idaifield(replace_uids = TRUE, keep_geometry = TRUE)

data_mat <- buildings %>% idaifield_as_matrix() %>% as.data.frame()
rownames(data_mat) <- data_mat[,"identifier"]

data_mat$identifier[which(is.na(data_mat$geometry))]

periods_dat <- read.csv(file = "import/period_dat.csv", sep = ",", encoding = "UTF-8")
periods_dat <- periods_dat[,-1]
periods_dat <- periods_dat[!is.na(periods_dat$period),]
periods_dat$period <- factor(periods_dat$period, levels = periods_dat$period, labels = periods_dat$period, ordered = TRUE)

periods_groups <- read.csv(file = "import/period_cleanGroups.csv", sep = ",", encoding = "UTF-8")
periods_groups$group <- factor(periods_groups$group, levels = periods_groups$group, labels = periods_groups$group, ordered = TRUE)

