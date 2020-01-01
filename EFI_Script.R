library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(FactoMineR)
library(ggplot2)
library(naniar)
library(mice)
library(factoextra)
library(RColorBrewer)
library(rsample)
library(Metrics)
library(tibble)
library(caret)
library(rstanarm)

set.seed(281097)

setwd("/Users/nfsturm/Documents/Universitas/Datasets/Economic_Freedom_Index")

efi19 <- read_excel("index2019_data.xls", range = "A1:AH186")

colnames(efi19) <- c("country_id",	"country_name", "webname",	"region", "world_rank", "region_rank", "score19",	"property_rights", "judical_effectiveness",	"gov_integrity", "tax_burden",	"gov_spending",	"fiscal_health",	"business_freedom",	"labor_freedom",	"monetary_freedom",	"trade_freedom",	"invest_freedom", "financial_freedom",	"tariff_rate",	"income_tax_rate", "corp_tax_rate",	"tax_burden_pct", "gov_expend_pct", "country", "pop",	"gdp",	"gdp_growth",	"avg_gdp_growth_5",	"gdp_percap",	"unempl",	"inflation",	"fdi_mill", "public_debt_pct")

efi19$country_id <- NULL # Getting rid of redundant columns 
efi19$country <- NULL
efi19$webname <- NULL

cols <- 3:31

efi19_a <- map_at(efi19, .at = cols, .f = as.numeric) # Turning columns into numbers
efi19_a <- as_tibble(efi19_a)

missing_efi19 <- miss_case_summary(efi19_a)

drop_cases <- missing_efi19 %>%
  filter(pct_miss > 50) %>%
  select(case)

drop_cases_index <- pull(drop_cases)

efi19_b <-efi19_a[-drop_cases_index,]

imputed_data <- mice(efi19_b, m=1, maxit = 50, method = 'pmm', seed = 500)
efi19_c <- mice::complete(imputed_data)

drop_cols <- c("gdp", "pop", "score19", "world_rank", "region_rank", "country_name")

efi19_d <- efi19_c %>%
  select(-drop_cols)

efi19_e <- efi19_d[,3:13]

efi19_d$region <- as.factor(efi19_d$region)

scaled_efi <- scale(efi19_e)

efi19_f <- efi19_d %>%
  mutate(log_gdp_percap = log(gdp_percap))

# Plots

ggplot(efi19_f, aes(x = gov_integrity, y = log_gdp_percap, col = region)) + geom_point() + theme_classic() +
  labs(x= "Government Integrity", y = "GDP per capita (Log)", col = "Region") + scale_color_brewer(palette = "Pastel1")

ggplot(efi19_f, aes(x = business_freedom, y = log_gdp_percap, col = region)) + geom_point() + 
  theme_classic() + labs(x= "Business Freedom", y = "GDP per capita (Log)", col = "Region") + scale_color_brewer(palette = "Pastel2") 

ggplot(efi19_f, aes(x = tax_burden_pct, y = log_gdp_percap, col = region)) + geom_point() + theme_classic() + 
  labs(x= "Tax Burden Pct", y = "GDP per capita (Log)", col = "Region") + scale_color_brewer(palette = "Pastel2")

# PCA

pca1 <- PCA(scaled_efi, ncp = 10, graph = FALSE)
pca1$eig
fviz_screeplot(pca1, barfill = "#c9c9ff", barcolor = "#c9c9ff")
pca1$var$contrib

contrib <- pca1$var$contrib
rnames <- tibble::enframe((rownames(contrib)))
contrib <- as_tibble(contrib)
contrib_a <- bind_cols(contrib, rnames)

contrib_b <- contrib_a %>%
  mutate(total_contrib_pct = rowSums(contrib_a[,1:8])/800) %>%
  arrange(desc(total_contrib_pct))
contrib_b$total_contrib_pct