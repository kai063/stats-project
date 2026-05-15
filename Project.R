
#Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork) #used for the visual comparison of three graphs
library(car) #used to determine dataset validity for the use of linear regression
library(ggrepel)
library(stargazer) #not needed, only used to export to latex



#Original data sets
electricity_generation <- read.csv("electricity-generation.csv") #https://ourworldindata.org/grapher/electricity-generation.csv?v=1&csvType=full&useColumnShortNames=true
gdp_growth <- read.csv("gdp-penn-world-table.csv") #https://ourworldindata.org/grapher/gdp-penn-world-table.csv?v=1&csvType=full&useColumnShortNames=true
HDI <- read.csv("human-development-index.csv") #https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true 

#DATA CLEANING

#Energy generation data sets cleaning of world areas (they do not have codes)
electricity_generation_countries <- electricity_generation %>% 
  filter(code != "")


#final data sets (joined gdp growth HDI and electricity generation into one).
#We used left join for the HDI because the HDI data set has 187 entries instead of 181 due to the regions of the world
#also having codes. We also removed th owid_region column because it is useless in our further computations.
#We can observe 181 joined countries across those three datasets. The final datasets have 6 variables: entity (country name), 
#code (country code), year, total_generation__twh (total electricity generation in twh), rgdpo (real gdp).
#In the end we rounded up the electricity production column for ease of use.


final_dataset_HDI <- electricity_generation_countries %>% 
  left_join(HDI, by = c("entity", "code", "year")) %>% 
  inner_join(gdp_growth, by = c("entity", "code", "year")) %>%
  select(-owid_region)
  
final_dataset_HDI <- final_dataset_HDI %>%
  filter(total_generation__twh > 0, !is.na(rgdpo))

# Identify countries with both 2000 and 2023 observations
countries_complete <- final_dataset_HDI %>%
  group_by(entity, code) %>%
  summarise(
    has_start = 2000 %in% year,
    has_end = 2023 %in% year,
    .groups = "drop"
  ) %>%
  filter(has_start & has_end)

dropped_countries <- final_dataset_HDI %>%
  distinct(entity, code) %>%
  anti_join(countries_complete, by = c("entity", "code"))

print(dropped_countries)

# Restrict to complete countries and 2000-2023 window
final_dataset_HDI <- final_dataset_HDI %>%
  semi_join(countries_complete, by = c("entity", "code")) %>%
  filter(year >= 2000, year <= 2023) %>%
  mutate(total_generation__twh = round(total_generation__twh, digits = 2))

final_dataset_start_HDI <- final_dataset_HDI %>% filter(year == 2000)
final_dataset_end_HDI   <- final_dataset_HDI %>% filter(year == 2023)

final_dataset_end_HDI$change_gdp <- final_dataset_end_HDI$rgdpo - final_dataset_start_HDI$rgdpo



final_dataset_HDI <- final_dataset_HDI %>%
  mutate(elec_intensity = (total_generation__twh * 1e9) / rgdpo)

final_dataset_end_HDI <- final_dataset_end_HDI %>%
  mutate(elec_intensity = (total_generation__twh * 1e9) / rgdpo)


#DESCRIPTIVE STATISTICS
descriptive_stats_end <- final_dataset_end_HDI %>%
  summarise(across(c(total_generation__twh, rgdpo, hdi__sex_total, elec_intensity),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE)
                   ),
                   .names = "{.col}___{.fn}")) %>%
  pivot_longer(everything(), 
               names_to = c("variable", "stat"), 
               names_sep = "___") %>%
  pivot_wider(names_from = stat, values_from = value)

descriptive_stats_start <- final_dataset_start_HDI %>%
  summarise(across(c(total_generation__twh, rgdpo, hdi__sex_total),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        median = ~median(.x, na.rm = TRUE))))

print(descriptive_stats_start)
print(descriptive_stats_end)

#find outliers
find_outliers <- function(df, var) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  df %>% 
    filter(.data[[var]] < lower | .data[[var]] > upper) %>%
    select(entity, year, all_of(var)) %>%
    arrange(desc(.data[[var]]))
}

find_outliers(final_dataset_end_HDI, "total_generation__twh")
find_outliers(final_dataset_start_HDI, "total_generation__twh")

#Thesis outliers
model_outliers <- lm(log10(total_generation__twh) ~ log10(rgdpo), 
                     data = final_dataset_end_HDI %>% 
                       filter(total_generation__twh > 0, !is.na(rgdpo)))

final_dataset_end_HDI <- final_dataset_end_HDI %>%
  mutate(
    predicted_log_twh = predict(model_outliers),
    residual = log10(total_generation__twh) - predicted_log_twh
  )

# Countries producing far LESS electricity than GDP predicts (thesis outliers)
final_dataset_end_HDI %>%
  select(entity, rgdpo, total_generation__twh, residual) %>%
  arrange(residual) %>%
  head(10)

# Countries producing far MORE electricity than GDP predicts (opposite direction)
final_dataset_end_HDI %>%
  select(entity, rgdpo, total_generation__twh, residual) %>%
  arrange(desc(residual)) %>%
  head(10)

#CORRELATIONS

r_squared_linear_2023 <- cor(final_dataset_end_HDI$total_generation__twh, 
                             final_dataset_end_HDI$rgdpo)^2

r_squared_2000 <- cor(log10(final_dataset_start_HDI$total_generation__twh), 
                      log10(final_dataset_start_HDI$rgdpo))^2

r_squared_2023 <- cor(log10(final_dataset_end_HDI$total_generation__twh), 
                      log10(final_dataset_end_HDI$rgdpo))^2

cor(final_dataset_HDI$rgdpo, final_dataset_HDI$total_generation__twh)

cor.test(final_dataset_end_HDI$hdi__sex_total, 
         log10(final_dataset_end_HDI$total_generation__twh))

cor(final_dataset_end_HDI$hdi__sex_total, final_dataset_end_HDI$total_generation__twh, use = "complete.obs")

r_squared_HDI_2023 <- cor(final_dataset_HDI$hdi__sex_total, log10(final_dataset_HDI$total_generation__twh), use = "complete.obs")^2
r_squared_intensity_2023 <- cor(log10(final_dataset_end_HDI$elec_intensity), log10(final_dataset_end_HDI$rgdpo), use = "complete.obs")^2

#VISUALISATION

total_graph_rgdpo <- ggplot(final_dataset_end_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared_linear_2023, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  geom_text_repel(
    data = final_dataset_end_HDI %>%
      filter(total_generation__twh > 1000 | rgdpo > 5e12),
    aes(label = entity),
    size = 3, max.overlaps = Inf
  ) +
  labs(title = "2023 linear",
       x = "Electricity production (TWH)",
       y = "Real gdp") +
  theme_minimal()

start_graph <- ggplot(final_dataset_start_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared_2000, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  labs(title = "2000 log-log",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()

end_graph <- ggplot(final_dataset_end_HDI, aes(x = total_generation__twh, y = rgdpo)) + 
  geom_point() + 
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared_2023, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  labs(title = "2023 log-log",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()

total_graph_HDI <- ggplot(final_dataset_end_HDI, aes(x = total_generation__twh, y = hdi__sex_total)) +
  geom_point()+
  scale_x_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared_HDI_2023, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  labs(title = "HDI 2023",
       x = "Electricity production (TWH)",
       y = "HDI",
       caption = "Data: Our World in Data / Penn World Table")+
  theme_minimal()

total_graph_rgdpo

(start_graph|end_graph)

total_graph_HDI


ggplot(final_dataset_end_HDI, aes(x = total_generation__twh)) + 
  geom_histogram(color = "grey", fill = "grey") +
  scale_x_log10() +
  geom_vline(aes(xintercept = mean(final_dataset_HDI$total_generation__twh)), 
             color = "lightblue", linetype = "dashed") +
  geom_vline(aes(xintercept = median(final_dataset_HDI$total_generation__twh)), 
             color = "blue", linetype = "dashed") +
  annotate("text", 
           x = mean(final_dataset_HDI$total_generation__twh), 
           y = Inf, 
           label = "mean", 
           color = "lightblue", 
           size = 5,
           vjust = 1.5) +
  annotate("text", 
           x = median(final_dataset_HDI$total_generation__twh), 
           y = Inf, 
           label = "median", 
           color = "blue", 
           size = 5,
           vjust = 3) +
  labs(title = "Electricity production 2023",
       x = "TWH (log10)",
       y = "Count",
       caption = "Data: Our World in Data / Penn World Table") +
  theme_minimal()
  
  ggplot(final_dataset_end_HDI, aes(x = total_generation__twh))+
    scale_x_log10()+
    geom_boxplot(outlier.colour = "red", median.colour = "lightblue")+
    stat_boxplot(geom = "errorbar", 
                 width = 0.2, 
                 color = "black", 
                 linetype = 1) +
    labs(title = "Summary of electricity generation 2023",
         x = "TWH (log10)",
         caption = "Data: Our World in Data / Penn World Table")+
    theme_bw()
 
  
  elec_intesity_graph <- ggplot(final_dataset_end_HDI, aes(x = rgdpo, y = elec_intensity, color = hdi__sex_total))+
    scale_x_log10()+
    scale_y_log10()+
    geom_point()+
    scale_color_viridis_c(option = "plasma", name = "HDI Score") +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")+
    annotate("text", x = Inf, y = Inf, 
             label = paste("R² =", round(r_squared_intensity_2023, 3)),
             hjust = 1.1, vjust = 1.5, size = 4) +
    labs(title = "Electricity Intensity vs. Economic Development 2023",
      x = "Real GDP (Log Scale)",
      y = "Electricity Intensity (kWh per $RGDP)",
      caption = "Data: Our World in Data / Penn World Table")+
    theme_minimal()
  
  elec_intesity_graph


#EXTRA (not part of the main analysis)
  
#DurbinWatsonTest Regression diagnostics

model_rgdpo <- lm(rgdpo ~ total_generation__twh, final_dataset_end_HDI)
summary(model_rgdpo)

durbinWatsonTest(model_rgdpo)

model_HDI <- lm(hdi__sex_total ~ total_generation__twh, final_dataset_end_HDI)
summary(model_HDI)

durbinWatsonTest(model_HDI)


#latex export

stargazer(model_rgdpo, type = "latex", out = "elektrina_model.tex", covariate.labels = c("total\\_generation\\_\\_twh"))


