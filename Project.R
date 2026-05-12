
#Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork) #used for the visual comparison of three graphs
library(car) #used to determine dataset validity for the use of linear regression
library(stargazer) #not needed, only used to export to latex



#Original data sets
electricity_generation <- read.csv("electricity-generation.csv") #https://ourworldindata.org/grapher/electricity-generation.csv?v=1&csvType=full&useColumnShortNames=true
gdp_growth <- read.csv("gdp-penn-world-table.csv") #https://ourworldindata.org/grapher/gdp-penn-world-table.csv?v=1&csvType=full&useColumnShortNames=true
HDI <- read.csv("human-development-index.csv") #https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true 


#Energy generation data sets cleaning of world areas (they do not have codes)
electricity_generation_countries <- electricity_generation %>% 
  filter(code != "")


#final data sets (joined gdp growth HDI and electricity generation into one).
#We used left join for the HDI because the HDI data set has 187 entries instead of 181 due to the regions of the world
#also having codes. We also removed th owid_region column because it is useless in our further computations.
#We can observe 181 joined countries across those three datasets. The final datasets have 6 variables: entity (country name), 
#code (country code), year, total_generation__twh (total electricity generation in twh), rgdpo (real gdp).
#In the end we rounded up the electricity production column for ease of use.
#Also there are three datasets, one containing data for all the years (final_dataset_HDI), one containing the 
#first possible matching year (final_dataset_start_HDI) and one with the last possible matching year (final_dataset_end_HDI). 
#We chose them like this to try to show the change or no change in the graphs below throughout the years.

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


#Descriptive statistics
descriptive_stats <- final_dataset_end_HDI %>%
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

print(descriptive_stats)

cor(final_dataset_HDI$rgdpo, final_dataset_HDI$total_generation__twh)
#There is a strong correlation between the rise in real gdp and electricity generation which suggest that to have a growing 
#economy countries need electricity. 
#Further explanation in the linear regression graph down below.

cor(final_dataset_HDI$hdi__sex_total, log10(final_dataset_HDI$total_generation__twh + 1), use = "complete.obs")
#Because of the scale of HDI which only 0-1 we would consider this correlation high, ie. the value HDI has a ceiling.
#Further explanation in the linear regression graph down below.



model_rgdpo <- lm(rgdpo ~ total_generation__twh, final_dataset_end_HDI)
summary(model)

durbinWatsonTest(model_rgdpo)

model_HDI <- lm(hdi__sex_total ~ total_generation__twh, final_dataset_end_HDI)
summary(model_HDI)

durbinWatsonTest(model_HDI)

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

#graphs
#Apart from the total_graph_rgdpo graph all graphs are generated after being scaled via log10 due to several violations of the linear regression
#principles. The original model is U-shaped not complying with linearity, it has slightly worse score in the Durbin-Watson test and also has 0 p-value meaning the normality of 
#errors completely fails (the errors for the outliers like China and USA are much bigger than all other). The HDI model does pass all tests apart from linearity because of how the 
#HDI is calculated (it is a logarithmic curve). So we again log10 the graph to show the most clear graph and meet all conditions for the linear regression model.

end_graph <- ggplot(final_dataset_end_HDI, aes(x = total_generation__twh, y = rgdpo)) + 
  geom_point() + 
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "2023",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()

start_graph <- ggplot(final_dataset_start_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "2000",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()


total_graph_rgdpo <- ggplot(final_dataset_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "2000-2023",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()

total_graph_rgdpo_log10 <- ggplot(final_dataset_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "2000-2023",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()

total_graph_HDI <- ggplot(final_dataset_HDI, aes(x = total_generation__twh, y = hdi__sex_total)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "Change in HDI",
       x = "Electricity production (TWH)",
       y = "HDI",
       caption = "Data: Our World in Data / Penn World Table")+
  theme_minimal()

(start_graph|end_graph|total_graph_rgdpo_log10)

total_graph_rgdpo

#In all three log10 scaled graphs we can clearly see the trend and correlation between electricity production and gdp growth 
#using linear regression. Which supports our initial theory question. We can also see two "tails" in the non log10 scaled graph 
#which are in contradict with our theory that have significantly higher gpd than others yet have 
#relatively small electricity production. Those two countries are Japan and USA. This phenomena is likely caused by the 
#Decoupling effect which is when the gdp of a country continues to grow but it's electricity generation flattens out or 
#even declines. This can be due the the fact that those economies differ from those of China or Russia in sense that they are 
#growing their gdp by making production more efficient or moving to a more service based economy. Also these countries greatly 
#offshore a lot of energy intensive production into countries like China. 
#However those outliers can be also the product of the severe violations of the linear regression model prerequisites which
#are present in the non-log10 graph.
#In all three log10 scaled graphs we can clearly see duction and gdp growth using linear regression. Which supports our question 

#Try 

total_graph_HDI

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

#From this graph we can see that the correlation (also from the previous cor function) between HDI and electricity production is 
#smaller and the linear regression line is flatter. This is due to the fact that once a country reaches certain industrial level
#it does not need that much electricity (due to effectiveness, services, etc.) to increase it's HDI which more so from a certain 
#point depends on the social structures more than industrial production or other electricity dependent economic activities. There
#can be a point made about AI (needing large amounts of electricity for data centers) having impact on HDI but we yet do not have 
#sufficient data to support this claim.

ggplot(final_dataset_HDI, aes(x = total_generation__twh)) + 
  geom_histogram(color = "grey",
                 fill ="grey") +
  scale_x_log10()+
  geom_vline(aes(xintercept=mean(final_dataset_HDI$total_generation__twh)), #Do not mind those vlines I just wanted to make it pretty
             color="lightblue", 
             linetype="dashed",)+
  geom_text(aes(x = mean(final_dataset_HDI$total_generation__twh), 
                y =-50, 
                label = "mean"), 
            color = "lightblue", 
            size = 5)+
  geom_vline(aes(xintercept=median(final_dataset_HDI$total_generation__twh)), 
             color="blue", 
             linetype="dashed",)+
  geom_text(aes(x = median(final_dataset_HDI$total_generation__twh), 
                y =-50, 
                label = "median"), 
            color = "blue", 
            size = 5)+
  labs(title = "Electricity production",
       x = "TWH (log10)",
       vline = "mean",
       caption = "Data: Our World in Data / Penn World Table")+
  theme_minimal()
  
  ggplot(final_dataset_HDI, aes(x = total_generation__twh)) + 
    geom_histogram() +
    labs(title = "Electricity production",
         x = "TWH")
  theme_minimal()  
  
  ggplot(final_dataset_HDI, aes(x = total_generation__twh))+
    scale_x_log10()+
    geom_boxplot(outlier.colour = "red", median.colour = "lightblue")+
    stat_boxplot(geom = "errorbar", 
                 width = 0.2, 
                 color = "black", 
                 linetype = 1) +
    labs(title = "Summary of electricity generation",
         x = "TWH (log10)",
         caption = "Data: Our World in Data / Penn World Table")+
    theme_bw()
 
  #The main thing we can see from this graph is that electricity production is uneven across countries. Most are clustered 
  #around low to medium production with only a few outlier countries that produces high volumes of electricity. We can also translate 
  #this graph into a insight on the immediate economic activity of one country, because electricity really cannot be stored. So 
  #higher electricity output means greater potential for economic activity. There can also be an argument made that developing 
  #countries produce less electricity than developed countries based on the Pareto distribution of the non log10 graph
  
  elec_intesity_graph <- ggplot(final_dataset_HDI, aes(x = rgdpo, y = elec_intensity, color = hdi__sex_total))+
    scale_x_log10()+
    scale_y_log10()+
    geom_point()+
    scale_color_viridis_c(option = "plasma", name = "HDI Score") +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth",
                color = "black")+
    labs(title = "Electricity Intensity vs. Economic Development",
      x = "Real GDP (Log Scale)",
      y = "Electricity Intensity (kWh per $RGDP)",
      caption = "Data: Our World in Data / Penn World Table")+
    theme_minimal()
  
  elec_intesity_graph

#I also include this graph because I think the total gdp to total electricity production might just show that large 
#economies have large electricity production. Which might not reflect correctly the notion that electricity relates to 
#economic development. Also this graph is probably the best complete visual representation for the answer to our original question. 
#By plotting the log10 scales of Real GDP and Electricity intensity
  # http://127.0.0.1:45257/graphics/8747597c-7a21-40ba-9b29-c2c362fc08bf.png
#From this analysis we can clearly see a high correlation between gdp and electricity production as welhttp://127.0.0.1:45257/graphics/8747597c-7a21-40ba-9b29-c2c362fc08bf.pngl as HDI and electricity 
#production which answers our initial question positively. However as we have seen from the several outliers, particulary Japan
#and USA, electricity production is a necessary prerequisite for an economic development only to some extent. This effect shows that 
#developed economies can sustain high economic output through means other than pure electricity production related sectors.  
  
  
stargazer(model,  type = "latex", out = "elektrina_model.tex", covariate.labels = c("total\\_generation\\_\\_twh"))


