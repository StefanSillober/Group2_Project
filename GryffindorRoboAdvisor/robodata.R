library(reticulate)
library(lubridate)
library(tidyverse)
library(rlist)
library(jsonlite)

use_python(Sys.which("python"))
invest <- import("investpy")
ind_stocks_eu <- list('iShares STOXX Europe 600 Banks UCITS',
                      'iShares STOXX Europe 600 Basic Resources UCITS',
                      'iShares STOXX Europe 600 Chemicals UCITS',
                      'iShares STOXX Europe 600 Construction & Materials',
                      'iShares STOXX Europe 600 Financial Services UCITS',
                      'iShares STOXX Europe 600 Food & Beverage UCITS',
                      'iShares STOXX Europe 600 Health Care UCITS',
                      'iShares STOXX Europe 600 Industrial Goods & Servic',
                      'iShares STOXX Europe 600 Insurance UCITS',
                      'iShares STOXX Europe 600 Media UCITS',
                      'iShares STOXX Europe 600 Oil & Gas UCITS',
                      'iShares STOXX Europe 600 Personal & Household Good',
                      'iShares STOXX Europe 600 Real Estate UCITS',
                      'iShares STOXX Europe 600 Retail UCITS',
                      'iShares STOXX Europe 600 Technology UCITS',
                      'iShares STOXX Europe 600 Telecommunications UCITS',
                      'iShares STOXX Europe 600 Travel & Leisure UCITS',
                      'iShares STOXX Europe 600 Utilities UCITS')

ind_stocks_us <- list('STOXX North America 600 Banks USD Price',
                      'STOXX North America 600 Basic Resources USD Price',
                      'STOXX North America 600 Chemicals USD Price',
                      'STOXX North America 600 Construction & Materials U',
                      'STOXX North America 600 Financial Services USD Pri',
                      'STOXX North America 600 Food & Beverage USD Price',
                      'STOXX North America 600 Health Care USD Price',
                      'STOXX North America 600 Industrial Goods & Service',
                      'STOXX North America 600 Insurance USD Price',
                      'STOXX North America 600 Media USD Price',
                      'STOXX North America 600 Oil & Gas USD Price',
                      'STOXX North America 600 Personal & Household Goods',
                      'STOXX North America 600 REIT USD',
                      'STOXX North America 600 Retail USD Price',
                      'STOXX North America 600 Technology USD Price',
                      'STOXX North America 600 Telecommunications USD Pri',
                      'STOXX North America 600 Travel & Leisure USD Price',
                      'STOXX North America 600 Utilities USD Price')
ind_stocks_asia <- list('STOXX Asia/Pacific 600 Banks USD Price',
                        'STOXX Asia/Pacific 600 Basic Resources USD Price',
                        'STOXX Asia/Pacific 600 Chemicals USD Price',
                        'STOXX Asia/Pacific 600 Construction & Materials US',
                        'STOXX Asia/Pacific 600 Financial Services USD Pric',
                        'STOXX Asia/Pacific 600 Food & Beverage USD Price',
                        'STOXX Asia/Pacific 600 Health Care USD Price',
                        'STOXX Asia/Pacific 600 Industrial Goods & Services',
                        'STOXX Asia/Pacific 600 Insurance USD Price',
                        'STOXX Asia/Pacific 600 Media USD Price',
                        'STOXX Asia/Pacific 600 Oil & Gas USD Price',
                        'STOXX Asia/Pacific 600 Personal & Household Goods',
                        'STOXX Asia/Pacific 600 REIT USD',
                        'STOXX Asia/Pacific 600 Retail USD Price',
                        'STOXX Asia/Pacific 600 Technology USD Price',
                        'STOXX Asia/Pacific 600 Telecommunications USD Pric',
                        'STOXX Asia/Pacific 600 Travel & Leisure USD Price',
                        'STOXX Asia/Pacific 600 Utilities USD Price')

inds_eu <- list("EU_banks", "EU_resources", "EU_chemicals",
                "EU_construction","EU_financials", "EU_food","EU_health",
                "EU_industrial","EU_insurance", "EU_media", "EU_energy",
                "EU_personal","EU_estate", "EU_retail", "EU_tech", "EU_telecom",
                "EU_travel", "EU_utilities")
inds_us <- list("US_banks", "US_resources", "US_chemicals",
                "US_construction","US_financials", "US_food","US_health", "
                US_industrial","US_insurance", "US_media", "US_energy",
                "US_personal","US_estate", "US_retail", "US_tech", "US_telecom",
                "US_travel", "US_utilities")
inds_asia <- list("AS_banks", "AS_resources", "AS_chemicals",
                  "AS_construction","AS_financials", "AS_food","AS_health", 
                  "AS_industrial","AS_insurance", "AS_media", "AS_energy",
                  "AS_personal","AS_estate", "AS_retail", "AS_tech", "AS_telecom",
                  "AS_travel", "AS_utilities")


dfs <- list()

data_scrap <- function(stocks,industries,country, yrs, scraping_function){
  for (item in Map(list, stocks,industries)){
    
    df <- scraping_function(item[[1]],
                            country = country,
                            from_date= format((Sys.Date() - years(yrs)), '%d/%m/%Y'),
                            to_date= format(Sys.Date(), '%d/%m/%Y'), as_json = TRUE)
    #df$industry <- rep(item[[2]], nrow(df))
    
    
    df <- jsonlite::fromJSON(df) %>% as.data.frame() %>% subset(select = c('historical.date','historical.close'))
    
    names(df)[names(df) == 'historical.close'] <- item[[2]]
    names(df)[names(df) == 'historical.date'] <- 'Date'
    
    dfs <- list.append(dfs, df)
    
  }
  return(dfs)
}


EU <- data_scrap(ind_stocks_eu, inds_eu, 'Germany', 20,
                 invest$get_etf_historical_data) %>% reduce(inner_join, by = "Date")  
US <- data_scrap(ind_stocks_us, inds_us, 'world', 5,
                 invest$get_index_historical_data) %>% reduce(inner_join, by = "Date")  
AS <- data_scrap(ind_stocks_asia, inds_asia, 'world', 5,
                 invest$get_index_historical_data) %>% reduce(inner_join, by = "Date")

datastatic <- read.csv("histstock.csv", sep=";")

ovr <- dplyr::inner_join(EU,US, by = "Date") %>% inner_join(.,AS, by = "Date") %>% inner_join(.,datastatic, by = "Date")











