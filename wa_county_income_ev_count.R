# Analyze county income vs county EV counts
library(magrittr)

pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("MAIN_DB"),
  host = Sys.getenv("MAIN_HOST"),
  user = Sys.getenv("MAIN_USER"),
  password = Sys.getenv("MAIN_PWD"),
  port = Sys.getenv("MAIN_PORT")
)

# Get the BEV data from the database - source WA DOL
wa_bevs <- pool %>% dplyr::tbl("wa_bevs") %>% dplyr::collect()

# Grouby county and get counts
wa_bevs_county <-
  wa_bevs %>% dplyr::group_by(county) %>% dplyr::summarise(ev_counts = dplyr::n())

# Read the county income data - Source OFM: https://ofm.wa.gov/washington-data-research/economy-and-labor-force/median-household-income-estimates
wa_inc_county_raw <- readxl::read_excel("data-raw/median_household_income_estimates.xlsx", skip = 3)

# Remove NAs
wa_inc_county <- na.omit(wa_inc_county_raw)

# Rename first column to county
colnames(wa_inc_county)[1] <- "county"

# Remove first row -- values for the state
wa_inc_county <- wa_inc_county[-c(1), ]

# Merge the two dataframes, - keeping only rows from the income dataframe - since WA only has 39 counties
wa_inc_ev_county <- merge(x = wa_bevs_county, y = wa_inc_county[c("county", "2019")], by = "county", all.y = TRUE)

wa_inc_ev_county <- wa_inc_ev_county %>% dplyr::rename(income_2019 = `2019`)

ggpubr::ggscatter(wa_inc_ev_county, x = "income_2019", y = "ev_counts",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "2019 Mean Household Income (USD)", ylab = "EV Counts")

