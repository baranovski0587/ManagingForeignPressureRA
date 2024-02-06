setwd('C:\Users\yegor\Desktop\BaranovskiRA_work')

json_key_file <- "firstproject-key.json"
bigrquery::bq_auth(path = json_key_file_path)

sql_query<-"
SELECT Actor1CountryCode, Actor2CountryCode, Year, EventCode, GoldsteinScale FROM
  `gdelt-bq.full.events`
WHERE
  EventCode IN ('031', '032', '042', '043', '057', '060', '061', '062', '121' , '160', '0311', '0312', '1211', '1212')
  AND Actor1CountryCode != Actor2CountryCode
  AND Year <= 2023
GROUP BY
  Actor1CountryCode, Actor2CountryCode, Year, EventCode, GoldsteinScale
ORDER BY
  Actor1CountryCode, Actor2CountryCode, Year, EventCode, GoldsteinScale;
  
"
project_id <- "first-project-380014"
gdelt_data_frame <- bq_project_query(project_id, sql_query) %>% bq_table_download()
fwrite(gdelt_data_frame,"C:\Users\yegor\Desktop\BaranovskiRA_work\DataRAW\GDELT_pre2024.csv", )