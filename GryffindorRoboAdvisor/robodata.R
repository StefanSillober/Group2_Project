library(reticulate)
library(lubridate)
library(rlist)
use_python(Sys.which("python"))
invest <- import("investpy")

ind_stocks <- list('iShares STOXX Europe 600 Automobiles & Parts UCITS',
               'iShares STOXX Europe 600 Banks UCITS',
               'iShares STOXX Europe 600 Basic Resources UCITS',
               'iShares STOXX Europe 600 Chemicals UCITS',
               #'iShares STOXX Europe 600 Construction & Materials UCITS',
               'iShares STOXX Europe 600 Financial Services UCITS',
               'iShares STOXX Europe 600 Food & Beverage UCITS',
               'iShares STOXX Europe 600 Health Care UCITS',
               #'iShares STOXX Europe 600 Industrial Goods & Services UCITS',
               'iShares STOXX Europe 600 Insurance UCITS',
               'iShares STOXX Europe 600 Media UCITS',
               'iShares STOXX Europe 600 Oil & Gas UCITS',
               #'iShares STOXX Europe 600 Personal & Household Goods UCITS',
               'iShares STOXX Europe 600 Real Estate UCITS',
               'iShares STOXX Europe 600 Retail UCITS',
               'iShares STOXX Europe 600 Technology UCITS',
               'iShares STOXX Europe 600 Telecommunications UCITS',
               'iShares STOXX Europe 600 Travel & Leisure UCITS',
               'iShares STOXX Europe 600 Utilities UCITS')
inds <- list("EU_auto","EU_banks", "EU_resources", "EU_chemicals",
            #"EU_construction", 
            "EU_financials", "EU_food","EU_health", #"EU_industrial", 
            "EU_insurance", "EU_media", "EU_energy", #"EU_personal",
            "EU_estate", "EU_retail", "EU_tech", "EU_telecom", "EU_travel", "EU_utilities")

dfs <- list()

for (item in Map(list, ind_stocks,inds)){

  df <- invest$get_etf_historical_data(etf = item[[1]],
                                       country = 'Germany',
                                       from_date= format((Sys.Date() - years(15)), '%d/%m/%Y'),
                                       to_date= format(Sys.Date(), '%d/%m/%Y'))
  #df$industry <- rep(item[[2]], nrow(df))
  
  df <- subset(df, select = c('Close'))
  
  names(df)[names(df) == 'Close'] <- item[[2]]
  
  
  dfs <- list.append(dfs, df)
  
}



 
