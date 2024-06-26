library(internetarchive)
library(tidyverse)
#this script scrapes the Foreign Policy Notices from internet archive.
#denote which volumes you want
volumenums <- 15:25
#this creates an empty dataframe
df <- data.frame(id=character())
#go through and get a list of items for each issue. This returns a data frame with the search results for this volume range.
for (i in 1:length(volumenums)) {
  print(volumenums[i])
  print(paste("'sim_pubid:934 AND volume:", volumenums[i], "'", sep=""))
  iasearchresult <- ia_keyword_search(paste("sim_pubid:934 AND volume:", volumenums[i], sep=""), num_results = 200)
  df <- add_row(df, id = iasearchresult)
}
#Now we go get the metadata for each item we have found in our search. This takes a minute and creates a very large df. 
metadata <- data.frame(id=character(), field=character(), value=character())
for (i in 1:length(df$id)) {
  issue <- ia_get_items(df[i,])
  meta <<- as.data.frame(ia_metadata(issue))
  metadata <<- add_row(metadata, meta)
}
#this culls the metadata from above into a more manageable df. First we filter to only include the info we care about.
final.metadata <- metadata %>% filter(field == "identifier" | field == "publisher" | field == "date" | field == "identifier-access")
#make it tidy
final.metadata <- final.metadata %>% pivot_wider(names_from = "field", values_from = "value")
#figure out how many of these issues are duplicated. IA has duplicated scans of some of these items. We just want one volume for each date.
final.metadata$dup <- duplicated(final.metadata$date)
final.metadata <- final.metadata %>% filter(dup == FALSE)
#final.metadata <- final.metadata %>% separate_wider_delim(date, "-", names= c("year", "month", "day")) 
#copy this to our running.meta df so we don't lose this when re-running this for additional years. 
#running.meta <- add_row(final.metadata, running.meta)
#now go through each id in final.metadata and download the associated text file. Sometimes this fails, if it does it can be rerun and it'll check for already existing files and now redownload them.
df.files <- data.frame(id=character(), file=character(), type=character())
for (i in 1:length(final.metadata$id)){
  issue <- ia_get_items(final.metadata$id[i])
  print(paste("getting item #", final.metadata$id[i], "from", final.metadata$date[i]))
  final.metadata
  fileinfo <- ia_files(issue) %>% 
    filter(type == "txt") %>% 
    group_by(id)
  ia_files(issue) %>% 
    filter(type == "txt") %>% 
    group_by(id) %>% 
    slice(1) %>% 
    ia_download(dir = "txt/", overwrite = FALSE, extended_name = FALSE)
  df.files <<- add_row(df.files, fileinfo)
}
saved.copy.finalmetadata <- final.metadata
saved.copy.df.files <- df.files


remove.index <- final.metadata %>% filter(grepl("index", id, fixed = FALSE, ignore.case=TRUE))
remove.index$extension <- ".txt"
remove.index <- remove.index %>% unite(filename, id, extension, sep="")
for (i in 1:length(remove.index$identifier)) {
if (file.exists(paste("txt/", remove.index$filename[i], sep=""))) {
  print("the file does exist")
  file.remove(paste("txt/", remove.index$filename[i], sep=""))
  print(paste("removed: ", "txt/", remove.index$filename[i], sep=""))
}}

final.metadata <- final.metadata %>% filter(!grepl("index", id, fixed = FALSE, ignore.case=TRUE))
final.metadata$extension <- ".txt"
final.metadata <- final.metadata %>% unite(filename, id, extension, sep="")

write.csv(final.metadata, "metadata.csv")
metadata <- running.meta %>% filter(!grepl("index",id))
filenames <- as.data.frame(list.files("txt/"))

#There are 20 files that are index files and I think we want to take those and put them in a separate file. They could be useful but shouldn't be included with the main dataset.



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
