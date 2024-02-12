
library(data.table)
library(dplyr)
library(readr)
library(tidyr)

gravity_data <- fread('C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/GravityClean.csv')

gravity_data$tradeflow_comtrade <- pmax(gravity_data$tradeflow_comtrade_d, gravity_data$tradeflow_comtrade_o)
gravity_data$tradeflow_imf <- pmax(gravity_data$tradeflow_imf_d, gravity_data$tradeflow_imf_o)
gravity_data<- select(gravity_data, -tradeflow_imf_o, -tradeflow_imf_d, -tradeflow_comtrade_d,-tradeflow_comtrade_o)
# Create a mirror dataset
data_mirror <- gravity_data%>%
  rename(
    a_ccodealp = b_ccodealp,
    b_ccodealp = a_ccodealp,
    tradeflow_comtrade_mirror = tradeflow_comtrade,
    tradeflow_imf_mirror = tradeflow_imf,
    tradeflow_baci_mirror = tradeflow_baci
  ) 
data_mirror <- select(data_mirror, a_ccodealp, b_ccodealp,year, tradeflow_comtrade_mirror, tradeflow_imf_mirror, tradeflow_baci_mirror)
# Full join with the original dataset
gravity.data.2 <- merge(gravity_data, data_mirror, by = c("a_ccodealp", "b_ccodealp", "year"), all = FALSE)
# Take the maximum of the two values + create aggregate max
gravity.data.2 <- gravity.data.2 %>%
  mutate(
    tradeflow_comtrade_int = pmax(tradeflow_comtrade, tradeflow_comtrade_mirror, na.rm = TRUE),
    tradeflow_imf_int=pmax(tradeflow_imf, tradeflow_imf_mirror, na.rm =TRUE), 
    tradeflow_baci_int=pmax(tradeflow_baci, tradeflow_baci_mirror, na.rm =TRUE)
  ) %>%
  mutate(
    trade_max = pmax(tradeflow_comtrade_int, tradeflow_imf_int, tradeflow_baci_int, na.rm = TRUE)
  )

trade_only_gravity <- select(gravity.data.2, year, a_ccodealp, b_ccodealp, trade_max)
fwrite(trade_only_gravity, 'C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/TradeOnlyGravityDataset.csv')

source_controls_only <- gravity.data.2 %>%
  group_by(a_ccodealp, year) %>%
  summarize(
    GDP_o = median(gdp_o),
    POP_o = median(pop_o),
    GDPpc_o = median(gdpcap_o)
  ) 

target_controls_only <- gravity.data.2 %>%
  group_by(b_ccodealp, year) %>%
  summarize(
    GDP_d = median(gdp_d),
    POP_d = median(pop_d),
    GDPpc_d = median(gdpcap_d)
  ) 

# Save the resulting dataset
write.csv(source_controls_only, "C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/Source-Controls-Only_Gravity_Dataset.csv")
write.csv(target_controls_only, "C:/Users/yegor/Desktop/BaranovskiRA_work/DataClean/Target-Controls-Only_Gravity_Dataset.csv")


