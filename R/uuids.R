
source(file = "R/load_data.R")


uuids <- data.frame(UUID = unlist(lapply(buildings, function(x) x$id)),
                    identifier = unlist(lapply(buildings, function(x) x$identifier)), 
                    row.names = NULL)

write.csv(uuids, "R/export/UUIDs.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE)

