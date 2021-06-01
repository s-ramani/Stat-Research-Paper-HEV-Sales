pacman :: p_load(pacman, tidyverse, dplyr, ggplot2, cowplot, stringr, pastecs, 
                 tidyr, BSDA, readxl)

options(scipen = 9999)

#HEV Purchases

hev_sales <- read_excel("hev_sales.xlsx", "Sheet1")

colnames(hev_sales)[2] <- "Total HEV Sales"


#co2 emissions 

co2_emissions <- read_csv("co2_emissions.csv")

co2_emissions <- t(co2_emissions)
co2_emissions <- co2_emissions[-c(1:10), ]
co2_emissions <- co2_emissions[, -c(1, 3, 4, 5)]
co2_emissions <- as.data.frame(co2_emissions)
colnames(co2_emissions)[1] <- "Fossil fuel combustion: carbon dioxide"
co2_emissions$`Fossil fuel combustion: carbon dioxide` <- as.numeric(co2_emissions$`Fossil fuel combustion: carbon dioxide`)
log_co2 <- log(co2_emissions$`Fossil fuel combustion: carbon dioxide`)
log_co2 <- as.data.frame(log_co2)


#Oil Prices 

oil_prices <- read_excel("oil_prices.xls", sheet = "Data 1", skip = 2)


oil_prices <- oil_prices %>% 
  select(Date, `U.S. All Grades All Formulations Retail Gasoline Prices (Dollars per Gallon)`)

oil_prices <- oil_prices[-c(1:9), ]
oil_prices <- oil_prices[-c(21:22),]


# Alternative Fueling Stations 

alt_fueling_stations <- read_excel("alt_fueling_stations.xlsx")

alt_fueling_stations <- alt_fueling_stations %>% 
  select(Year, `Electric*`, Hydrogen)

alt_fueling_stations <- alt_fueling_stations[-c(1:7, 28), ]
log_electric <- log(alt_fueling_stations$`Electric*`)
log_hydrogen <- log(alt_fueling_stations$Hydrogen)


# Combining datasets

df <- cbind(hev_sales, log_co2, log_hydrogen, log_electric, oil_prices)
df <- df[, -c(6)]
row.names(df) <- NULL

colnames(df) <- c("Year", "hev_sales", "log_co2", "log_electric", "log_hydrogen", "gas_price")


# Understanding whether the independent data in question is normal

shapiro.test(df$hev_sales)
shapiro.test(df$gas_price)

# Both are normal given that their p-values are above 0.05. 

# Performing t-test on the hypothesis 

t.test(df$hev_sales, df$gas_price)

# The data is extremely signficant at the 0.05 sig level, indicating that there is a relationship between retail gasoline prices and HEV sales. 

# Conducting a multilinear regression 

model <- lm(hev_sales ~ gas_price + log_hydrogen + log_electric + log_co2, data = df)
model
summary(model)
plot(model)

# Producing an aesthetic display of the regression analysis 

library(gtsummary)

model %>% 
  tbl_regression() %>% 
  add_vif() %>%
  add_glance_source_note(include = r.squared)

