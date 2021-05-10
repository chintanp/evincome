# Analyze zip code income vs zip EV counts
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

# Grouby ZIP and get counts
wa_bevs_zip <-
  wa_bevs %>% dplyr::group_by(zip_code) %>% dplyr::summarise(ev_counts = dplyr::n())

# Read zip code level income data from IRS tax returns
# Source: https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-2018-zip-code-data-soi

wa_inc_zip_raw <-
  readxl::read_excel("data-raw/18zp48wa.xlsx", skip = 3)
wa_inc_zip_total <- wa_inc_zip_raw[, c(1, 2, 20, 21)]

# Rename first column to county
colnames(wa_inc_zip_total)[1] <- "zip"
colnames(wa_inc_zip_total)[2] <- "income_levels"
colnames(wa_inc_zip_total)[3] <- "return_counts"
colnames(wa_inc_zip_total)[4] <- "total_inc_kusd"

# Remove NAs
wa_inc_zip_total <- na.omit(wa_inc_zip_total)
wa_inc_zip_total <-
  wa_inc_zip_total[!(wa_inc_zip_total$zip %in% c(0, 99999)), ]

# Groupby
wa_inc_zip_mean <-
  wa_inc_zip_total %>% dplyr::group_by(zip) %>% dplyr::summarise(mean_inc = sum(as.numeric(total_inc_kusd)) * 1000 / sum(as.numeric(return_counts)))


# merge dataframes
wa_inc_ev_zip <- merge(x = wa_bevs_zip, y = wa_inc_zip_mean, by.x = "zip_code", by.y = "zip", all.y = TRUE)
# replace NAAs by zero
wa_inc_ev_zip$ev_counts[is.na(wa_inc_ev_zip$ev_counts)] <- 0


ggpubr::ggscatter(wa_inc_ev_zip, x = "mean_inc", y = "ev_counts",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "2019 Mean Individual Income (USD)", ylab = "EV Counts")
