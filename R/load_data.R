
library(idaifieldR)
## this will work only from idaifieldR 0.2.2!
options(digits = 20)
connection <- connect_idaifield(serverip = "127.0.0.1",
                                user = "R",
                                pwd = "hallo")


buildings <- get_idaifield_docs(
  connection = connection,
  projectname = "milet", raw = FALSE) %>%
  select_by(by = "liesWithin", value = "Bauwerkskatalog") %>%
  simplify_idaifield(replace_uids = TRUE, keep_geometry = TRUE)

data_mat <- buildings %>% idaifield_as_matrix() %>% as.data.frame()
rownames(data_mat) <- data_mat[,"identifier"]

data_mat$identifier[which(is.na(data$geometry))]



