
library(data.table)
library(dplyr)
library(readr)
library(tidyr)

new_country_codes <- c("SLO", "CZR", "GFR", "GDR", "YPR", "YAR", "TAW", "TAJ", "SRI", "ANG", "AUL", 
                       "BRU", "CAO", "CDI", "CRO", "INS", "MZM", "PHI", "BIH", "DDR", "MNE", "ROU", 
                       "SCG", "SVN", "TLS", "VDR", "YMD", "SUN", "USR", "YUG", "FRY", "MTN", "ROM", 
                       "CZA", "CSK", "YMS", "YMN", "TMP", "GME", "GMW", "DDR")
new_codes_df <- data.frame(country_code = new_country_codes)
cepii.codes <- read.csv('C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/cleaned.cepii.codes.csv')
cepii.codes <- cepii.codes[,-2]
cepii.codes <- c(cepii.codes, new_country_codes)

gdelt.type <- fread('C:/Users/yegor/Desktop/BaranovskiRA_work/DataRAW/GDELT_pre2024.csv')
match.values.all <- (gdelt.type$Actor1CountryCode %in% cepii.codes) & (gdelt.type$Actor2CountryCode %in% cepii.codes)
matched.all <- gdelt.type[(match.values.all), ]

# Count occurrences
df_count <- matched.all %>%
  group_by(Actor1CountryCode, Actor2CountryCode, Year, EventCode) %>%
  tally(name = "Count")
# Create a complete grid
df_complete <- df_count %>%
  ungroup() %>%
  complete(Actor1CountryCode, Actor2CountryCode, Year, EventCode, fill = list(Count = 0))


v1_codes <- c("057", "061", "062", "160", "042", "043", "031", "032", "060", "121", "0311", "0312", "1211", "1212")
v2_codes <- c("057", "061", "062", "160", "042", "043")
v3_codes <- c("057", "061", "062")
econ_codes <- c("061", "0311", "1211")
mil_codes <- c("062", "0312", "1212")

gdelt<-df_complete

gdelt$v1 <- as.numeric(gdelt$EventCode %in% v1_codes)
gdelt$v2 <- as.numeric(gdelt$EventCode %in% v2_codes)
gdelt$v3 <- as.numeric(gdelt$EventCode %in% v3_codes)
gdelt$ECON <- as.numeric(gdelt$EventCode %in% econ_codes)
gdelt$MIL <- as.numeric(gdelt$EventCode %in% mil_codes)

gdelt<- rename(gdelt, "cameocode" = "EventCode")
gdelt<- rename(gdelt, "a_ccodealp" = "Actor1CountryCode")
gdelt<- rename(gdelt, "b_ccodealp" = "Actor2CountryCode")

gdelt <- gdelt %>%
  mutate(
    a_ccodealp = if_else(a_ccodealp %in% c("SUN", "USR"), "RUS", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp %in% c("SUN", "USR"), "RUS", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp %in% c("YUG", "FRY", "SCG"), "SRB", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp %in% c("YUG", "FRY", "SCG"), "SRB", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "MTN", "MNE", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "MTN", "MNE", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "ROM", "ROU", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "ROM", "ROU", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp %in% c("CZA", "CSK"), "CZE", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp %in% c("CZA", "CSK"), "CZE", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "YMS", "YMD", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "YMS", "YMD", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "YMN", "YEM", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "YMN", "YEM", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "TMP", "TLS", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "TMP", "TLS", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "GME", "DDR", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "GME", "DDR", b_ccodealp),
    a_ccodealp = if_else(a_ccodealp == "GMW", "DEU", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "GMW", "DEU", b_ccodealp)
  )

fwrite(gdelt, 'C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/CleanedGDELT.csv')






