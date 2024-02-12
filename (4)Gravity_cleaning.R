setwd('C:/Users/yegor/Desktop/BaranovskiRA_work/DataRAW')

library(data.table)
library(dplyr)
library(readr)
library(tidyr)

gravity_data <- fread('Gravity_V202211.csv', select = c('year', 'country_id_o', 'country_id_d', 
                                                        'iso3_o', 'iso3_d', 'gdp_o', 'gdp_d', 'pop_o', 'pop_d', 
                                                        'gdpcap_o', 'gdpcap_d', 'tradeflow_comtrade_o',
                                                        'tradeflow_comtrade_d', 'tradeflow_baci', 
                                                        'tradeflow_imf_o', 'tradeflow_imf_d'))

setnames(gravity_data, old = c("iso3_o", "iso3_d"), new = c("a_ccodealp", "b_ccodealp"))


##Soviet Union -> Russia
gravity_data <- gravity_data %>%
  filter(!(a_ccodealp == "RUS" & year <= 1991)) %>%
  filter(!(b_ccodealp == "RUS" & year <= 1991)) %>%
  mutate(
    a_ccodealp = if_else(a_ccodealp == "SUN" & year <= 1991, "RUS", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "SUN" & year <= 1991, "RUS", b_ccodealp)
  ) %>%
  filter(!(a_ccodealp == "SUN" & year > 1991)) %>%
  filter(!(b_ccodealp == "SUN" & year > 1991))

##Yugoslavia -> Serbia
gravity_data <- gravity_data %>%
  filter(!(a_ccodealp == "SRB" & year <= 2005)) %>%
  filter(!(b_ccodealp == "SRB" & year <= 2005)) %>%
  mutate(
    a_ccodealp = if_else(a_ccodealp == "SCG" & year <= 2005, "SRB", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "SCG" & year <= 2005, "SRB", b_ccodealp)
  ) %>%
  filter(!(a_ccodealp == "SCG" & year > 2006)) %>%
  filter(!(b_ccodealp == "SCG" & year > 2006)) %>%
  filter(!(a_ccodealp == "SRB" & year <= 1991)) %>%
  filter(!(b_ccodealp == "SRB" & year <= 1991)) %>%
  mutate(
    a_ccodealp = if_else(a_ccodealp == "YUG" & year <= 1991, "SRB", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "YUG" & year <= 1991, "SRB", b_ccodealp)
  ) %>%
  filter(!(a_ccodealp == "YUG" & year > 1991)) %>%
  filter(!(b_ccodealp == "YUG" & year > 1991))


#Czechoslovakia -> czech republic
gravity_data <- gravity_data %>%
  filter(!(a_ccodealp == "CZE" & year <= 1992)) %>%
  filter(!(b_ccodealp == "CZE" & year <= 1992)) %>%
  mutate(
    a_ccodealp = if_else(a_ccodealp == "CSK" & year <= 1992, "CZE", a_ccodealp),
    b_ccodealp = if_else(b_ccodealp == "CSK" & year <= 1992, "CZE", b_ccodealp)
  ) %>%
  filter(!(a_ccodealp == "CSK" & year > 1992)) %>%
  filter(!(b_ccodealp == "CSK" & year > 1992))


#ANT, DEU, ETH, IDN, MYS, PAK, SDN, VNM, YEM
gravity_data <- gravity_data %>%
  filter(!(country_id_o == "SDN.1" & year >= 2011)) %>%
  filter(!(country_id_d == "SDN.1" & year >= 2011)) %>%
  filter(!(country_id_o == "SDN.2" & year < 2011)) %>%
  filter(!(country_id_d == "SDN.2" & year < 2011)) %>%
  filter(!(country_id_o == "VNM.1" & year >= 1976)) %>%
  filter(!(country_id_d == "VNM.1" & year >= 1976)) %>%
  filter(!(country_id_o == "VNM.2" & year < 1976)) %>%
  filter(!(country_id_d == "VNM.2" & year < 1976)) %>%
  filter(!(country_id_o == "YEM.1" & year >= 1990)) %>%
  filter(!(country_id_d == "YEM.1" & year >= 1990)) %>%
  filter(!(country_id_o == "YEM.2" & year < 1990)) %>%
  filter(!(country_id_d == "YEM.2" & year < 1990)) %>%
  filter(!(country_id_o == "PAK.1" & year >= 1971)) %>%
  filter(!(country_id_d == "PAK.1" & year >= 1971)) %>%
  filter(!(country_id_o == "PAK.2" & year < 1971)) %>%
  filter(!(country_id_d == "PAK.2" & year < 1971)) %>%
  filter(!(country_id_o == "ANT.1" & year >= 1986)) %>%
  filter(!(country_id_d == "ANT.1" & year >= 1986)) %>%
  filter(!(country_id_o == "ANT.2" & year < 1986)) %>%
  filter(!(country_id_d == "ANT.2" & year < 1986)) %>%
  filter(!(country_id_o == "MYS.1" & year >= 1965)) %>%
  filter(!(country_id_d == "MYS.1" & year >= 1965)) %>%
  filter(!(country_id_o == "MYS.2" & year < 1965)) %>%
  filter(!(country_id_d == "MYS.2" & year < 1965)) %>%
  filter(!(country_id_o == "IDN.1" & year >= 2002)) %>%
  filter(!(country_id_d == "IDN.1" & year >= 2002)) %>%
  filter(!(country_id_o == "IDN.2" & year < 2002)) %>%
  filter(!(country_id_d == "IDN.2" & year < 2002)) %>%
  filter(!(country_id_o == "DEU.1" & year >= 1990)) %>%
  filter(!(country_id_d == "DEU.1" & year >= 1990)) %>%
  filter(!(country_id_o == "DEU.2" & year < 1990)) %>%
  filter(!(country_id_d == "DEU.2" & year < 1990)) %>%
  filter(!(country_id_o == "ETH.1" & year >= 1993)) %>%
  filter(!(country_id_d == "ETH.1" & year >= 1993)) %>%
  filter(!(country_id_o == "ETH.2" & year < 1993)) %>%
  filter(!(country_id_d == "ETH.2" & year < 1993))

fwrite(gravity_data, 'C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/GravityClean.csv')



