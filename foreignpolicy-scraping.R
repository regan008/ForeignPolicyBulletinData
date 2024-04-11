library(internetarchive)
library(tidyverse)

ia_keyword_search("Foreign Policy Bulletin")


ats_query <- c("sim_pubid" = "934", "volume" = "17")
ia_search(c("sim_pubid" = "934"))


volumenums <- 19:21
df <- data.frame(id=character())
for (i in 1:length(volumenums)) {
  print(volumenums[i])
  print(paste("'sim_pubid:934 AND volume:", volumenums[i], "'", sep=""))
  iasearchresult <- ia_keyword_search(paste("sim_pubid:934 AND volume:", volumenums[i], sep=""), num_results = 200)
  df <- add_row(df, id = iasearchresult)
}

metadata <- data.frame(id=character(), field=character(), value=character())
for (i in 1:length(df$id)) {
  issue <- ia_get_items(df[i,])
  meta <<- as.data.frame(ia_metadata(issue))
  metadata <<- add_row(metadata, meta)

}

final.metadata <- metadata %>% filter(field == "identifier" | field == "publisher" | field == "date" | field == "identifier-access")
final.metadata <- final.metadata %>% pivot_wider(names_from = "field", values_from = "value")

final.metadata$dup <- duplicated(final.metadata$date)
final.metadata <- final.metadata %>% filter(dup == FALSE)
#final.metadata <- final.metadata %>% separate_wider_delim(date, "-", names= c("year", "month", "day")) 
running.meta <- add_row(final.metadata, running.meta)

for (i in 1:length(final.metadata$id)){
  issue <- ia_get_items(final.metadata$id[i])
  print(paste("getting item #", final.metadata[i], "from", final.metadata$date[i]))
  ia_files(issue) %>% 
    filter(type == "txt") %>% 
    group_by(id) %>% 
    slice(1) %>% 
    ia_download(dir = "txt/", overwrite = FALSE)
}


download_row <- function(row, dir, extended_name, overwrite, silence) {
  if (extended_name) {
    row$local_file <- paste0(dir, "/", row$id, gsub("/", "-", row$file))
  } else {
    row$local_file <- paste0(dir, "/", row$id, ".", row$type)
  }
  
  if (overwrite | !file.exists(row$local_file)) {
    if (!silence) message(paste("Downloading", row$local_file))
    GET(row$url, write_disk(row$local_file, overwrite = overwrite, timeout(100)))
    row$downloaded <- TRUE
  }
  
  as_data_frame(row)
}
