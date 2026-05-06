
# Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(stargazer)

# Sources:
# https://ourworldindata.org/grapher/electricity-generation.csv?v=1&csvType=full&useColumnShortNames=true
# https://ourworldindata.org/grapher/gdp-penn-world-table.csv?v=1&csvType=full&useColumnShortNames=true
# https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true

electricity_generation <- read.csv("electricity-generation.csv")
gdp_growth            <- read.csv("gdp-penn-world-table.csv")
HDI                   <- read.csv("human-development-index.csv")


# Drop world/region rows (no ISO code)
electricity_generation_countries <- electricity_generation %>%
  filter(code != "")

# Joined panel
final_dataset_HDI <- electricity_generation_countries %>%
  left_join(HDI, by = c("entity", "code", "year")) %>%
  inner_join(gdp_growth, by = c("entity", "code", "year"))


final_dataset_HDI <- final_dataset_HDI %>%
  mutate(total_generation__twh = round(total_generation__twh, 2))

# First and last year per country in the joined panel
final_dataset_start_HDI <- final_dataset_HDI %>%
  group_by(entity, code) %>%
  filter(year == min(year)) %>%
  ungroup()

final_dataset_end_HDI <- final_dataset_HDI %>%
  group_by(entity, code) %>%
  filter(year == max(year)) %>%
  ungroup()

change_panel <- final_dataset_end_HDI %>%
  select(entity, code,
         end_twh   = total_generation__twh,
         end_rgdpo = rgdpo) %>%
  inner_join(
    final_dataset_start_HDI %>%
      select(entity, code,
             start_twh   = total_generation__twh,
             start_rgdpo = rgdpo),
    by = c("entity", "code")
  ) %>%
  mutate(change_production = round(end_twh   - start_twh,   2),
         change_gdp        = round(end_rgdpo - start_rgdpo, 2))

# Descriptive statistics
summary(final_dataset_HDI$total_generation__twh)
summary(final_dataset_end_HDI$total_generation__twh)
summary(final_dataset_start_HDI$total_generation__twh)

cor(final_dataset_HDI$rgdpo, final_dataset_HDI$total_generation__twh,
    use = "complete.obs")

cor(final_dataset_HDI$hdi__sex_total,
    log10(final_dataset_HDI$total_generation__twh + 1),
    use = "complete.obs")

cor(change_panel$change_gdp, change_panel$change_production,
    use = "complete.obs")

model <- lm(rgdpo ~ total_generation__twh, data = final_dataset_end_HDI)
summary(model)

# Outliers
find_outliers <- function(x) {
  x <- x[!is.na(x)]
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  x[x < lower | x > upper]
}

find_outliers(final_dataset_end_HDI$total_generation__twh)
find_outliers(final_dataset_start_HDI$total_generation__twh)
find_outliers(final_dataset_HDI$total_generation__twh)

# Graphs
end_graph <- ggplot(final_dataset_end_HDI,
                    aes(x = total_generation__twh, y = rgdpo)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "End year", x = "Electricity production (TWh)", y = "Real GDP") +

  theme_minimal()

start_graph <- ggplot(final_dataset_start_HDI,
                      aes(x = total_generation__twh, y = rgdpo)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Start year", x = "Electricity production (TWh)", y = "Real GDP") +
  theme_minimal()

total_graph_rgdpo <- ggplot(final_dataset_HDI,
                            aes(x = total_generation__twh, y = rgdpo)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(title = "All years (raw)", x = "Electricity production (TWh)", y = "Real GDP") +
  theme_minimal()

total_graph_rgdpo_log10 <- ggplot(final_dataset_HDI,
                                  aes(x = total_generation__twh, y = rgdpo)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", formula = y ~ x) +
  labs(title = "All years (log10)", x = "Electricity production (TWh)", y = "Real GDP") +
  theme_minimal()

total_graph_HDI <- ggplot(final_dataset_HDI,
                          aes(x = total_generation__twh, y = hdi__sex_total, color = owid_region)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Change in HDI", x = "Electricity production (TWh)", y = "HDI", color = "Continent") +
  theme_minimal()

print(start_graph | end_graph | total_graph_rgdpo_log10)
print(total_graph_rgdpo)
print(total_graph_HDI)

print(
  ggplot(final_dataset_HDI, aes(x = total_generation__twh)) +
    geom_histogram() +
    scale_x_log10() +
    labs(title = "Electricity production", x = "TWh (log10)") +
    theme_minimal()
)

print(
  ggplot(final_dataset_HDI, aes(x = total_generation__twh)) +
    geom_histogram() +
    labs(title = "Electricity production", x = "TWh") +
    theme_minimal()
)

stargazer(model,
          type = "latex",
          out = "elektrina_model.tex",
          covariate.labels = c("total\\_generation\\_\\_twh"))