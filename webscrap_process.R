library(tidyverse)
library(rvest)

year_cycles = seq(2000, 2020, by=2)
base_url = "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/"
csv_data = NULL
for (year in year_cycles){
  print(year)
  raw_data = read_html(paste0(base_url, year)) %>%
    html_element("table.DataTable-Partial") %>%
    html_table() %>%
    rename(pac = `PAC Name (Affiliate)`,
           country_parent = `Country of Origin/Parent Company`,
           total = Total,
           dems = Dems,
           repubs = Repubs) %>% 
    mutate(country = gsub("/(.*)", "", country_parent),
           total = parse_number(gsub("[$,]", "", total)),
           dems = parse_number(gsub("[$,]", "", dems)),
           repubs = parse_number(gsub("[$,]", "", repubs))) %>% 
    select(country, total, dems, repubs) %>% 
    group_by(country) %>% 
    summarize(dems_sum = mean(dems),
              repubs_sum = mean(repubs),
              total_sum = mean(total),
              .groups="drop") %>% 
    mutate(year_cycle=year)
  
  csv_data = bind_rows(csv_data, raw_data)
}

write_csv(csv_data, file="combined_pac.csv")


