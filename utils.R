parseRequirements <- function() {
  lines <- trimws(dropComments(readLines("packages.txt")))
  nonemptyLines <- lines[lines != ""]
  lapply(nonemptyLines, getPackageAndVersion)
}

getPackageAndVersion <- function(line) {
  c(unlist(strsplit(line, "==")), NA)[1:2]
}

dropComments <- function(str) gsub("(#.*$)", "", str)

get_keywords <- function(path = "data") {
  list.files(path = "data") %>% 
    gsub("gtrends_|.csv", "", .) %>% 
    gsub("_", " ", .)
}

gtrends <- function(keyword, path = "data") {
  filepath <- file.path(path, glue("gtrends_{gsub(' ', '_', keyword)}.csv"))
  read.csv(filepath)
}

gtrends_plot <- function(gtrends_data_ts, forecast) {
  gtrends_data_ts %>% 
    filter(date >= yearmonth("2018 Jan")) %>% 
  autoplot() + 
    autolayer(forecast) + 
    guides(level = "none") + 
    theme_bw() +
    theme(
      legend.position = "bottom", 
      legend.title=element_blank(), 
      text = element_text(size = 24)
      )
}
