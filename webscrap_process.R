library(tidyverse)
library(rvest)

year_cycles = seq(2000, 2020, by=2)
base_url = "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/"
csv_data = NULL
for (year_num in year_cycles){
  print(year_num)
  raw_data = read_html(paste0(base_url, year_num)) %>%
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
    summarize(dems_sum = sum(dems),
              repubs_sum = sum(repubs),
              total_sum = sum(total),
              .groups="drop") %>% 
    mutate(year = year_num)
  
  write_csv(raw_data, paste0("data/foreign_pac_data/year_", year_num, ".csv"))
  
  csv_data = bind_rows(csv_data, raw_data)
}

write_csv(csv_data, file="data/foreign_pac_data/full_data.csv")

write_csv(csv_data, file="data/foreign_pac_data/clean_data.csv")




