
#Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(stargazer) #not needed, only used to export to latex
library(ggrepel)




#Original data sets
electricity_generation <- read.csv("electricity-generation.csv") #https://ourworldindata.org/grapher/electricity-generation.csv?v=1&csvType=full&useColumnShortNames=true
gdp_growth <- read.csv("gdp-penn-world-table.csv") #https://ourworldindata.org/grapher/gdp-penn-world-table.csv?v=1&csvType=full&useColumnShortNames=true
HDI <- read.csv("human-development-index.csv") #https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true 


#Energy generation data sets cleaning of world areas
electricity_generation_countries <- electricity_generation %>% 
  filter(code != "")

#final data sets (joined gdp growth HDI and electricity generation into one).
#We used left join for the HDI because the HDI data set has 187 entries instead of 181 due to the regions of the world
#also having codes. We also removed th owid_region column because it is useless in our further computations.
#We can observe 181 joined countries across those three datasets. The final datasets have 6 variables: entity (country name), 
#code (country code), year, total_generation__twh (total electricity generation in twh), rgdpo (real gdp).
#In the end we rounded up the electricity production column for ease of use.
#Also there are three datasets, one containing data for all the years (final_datase_HDI), one containing the 
#first possible matching year (final_dataset_start_HDI) and one with the last possible matching year (final_dataset_end_HDI). 
#We chose them like this to try to show the change or no change in the graphs below throughout the years.

final_dataset_HDI <- electricity_generation_countries %>% 
  left_join(HDI, by = c("entity", "code", "year")) %>% 
  inner_join(gdp_growth, by = c("entity", "code", "year"))
  
final_dataset_HDI <- final_dataset_HDI %>% 
  select(-owid_region)

final_dataset_start_HDI <- electricity_generation_countries %>% 
  left_join(HDI, by = c("entity", "code", "year")) %>% 
  inner_join(gdp_growth, by = c("entity", "code", "year")) %>% 
  group_by(entity, code) %>% 
  filter(year == min(year)) %>% 
  ungroup()


final_dataset_end_HDI <- electricity_generation_countries %>%
  left_join(HDI, by = c("entity","code", "year")) %>% 
  inner_join(gdp_growth, by = c("entity","code", "year")) %>%
  group_by(entity, code) %>%
  filter(year == max(year)) %>%
  ungroup()

final_dataset_end_HDI %>% 
  mutate(total_generation__twh = round(total_generation__twh, digits = 2))

final_dataset_HDI %>% 
  mutate(total_generation__twh = round(total_generation__twh, digits = 2))

change_panel <- final_dataset_end_HDI %>%
  select(entity, code,
         end_twh = total_generation__twh, end_rgdpo = rgdpo) %>%
  inner_join(
    final_dataset_start_HDI %>%
      select(entity, code,
             start_twh = total_generation__twh, start_rgdpo = rgdpo),
    by = c("entity", "code")
  ) %>%
  mutate(change_production = round(end_twh   - start_twh,   2),
         change_gdp        = round(end_rgdpo - start_rgdpo, 2))

final_dataset_end_HDI$change_gdp <- final_dataset_end_HDI$rgdpo - final_dataset_start_HDI$rgdpo



#Descriptive statistics
summary(final_dataset_HDI$total_generation__twh)

summary(final_dataset_end_HDI$total_generation__twh)

summary(final_dataset_start_HDI$total_generation__twh)


cor(final_dataset_HDI$rgdpo, final_dataset_HDI$total_generation__twh)
#There is a strong correlation between the rise in real gdp and electricity generation which suggest that to have a growing 
#economy countries need electricity. 
#Further explanation in the linear regression graph down below.

cor(final_dataset_HDI$hdi__sex_total, log10(final_dataset_HDI$total_generation__twh + 1), use = "complete.obs")
#Because of the scale of HDI which only 0-1 we would consider this correlation high, ie. the value HDI has a ceiling.
#Further explanation in the linear regression graph down below.

cor(change_panel$change_gdp, change_panel$change_production, use = "complete.obs")


model <- lm(rgdpo ~ total_generation__twh, final_dataset_end_HDI)
summary(model)

#finding outliers
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  return(x[x < lower | x > upper])
}

find_outliers(final_dataset_end_HDI$total_generation__twh)
find_outliers(final_dataset_start_HDI$total_generation__twh)
find_outliers(final_dataset_HDI$total_generation__twh)

#graphs
#Apart from the total_graph_rgdpo graph all graphs are generated after being scaled via log10 due to high clustering near the 
#zero point with significant outliers making the normal scales non-readable. The total_graph_rgdpo is kept without scaling
#to show interesting outliers and their economic meaning.
end_graph <- ggplot(final_dataset_end_HDI, aes(x = total_generation__twh, y = rgdpo)) + 
  geom_point() + 
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "Change in gdp",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()


end_graph +
  geom_text_repel(
    data = final_dataset_end_HDI %>%
      filter(total_generation__twh > 1000 | rgdpo > 5e12),
    aes(label = entity),
    size = 3, max.overlaps = Inf
  )

start_graph <- ggplot(final_dataset_start_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "Change in gdp",
       x = "Electricity production (TWH)",
       y = "Real gdp")+
  theme_minimal()


total_graph_rgdpo <- ggplot(final_dataset_HDI, aes(x = total_generation__twh, y = rgdpo)) +
  geom_point()+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  labs(title = "Change in gdp",
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
  labs(title = "Change in gdp",
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
       y = "HDI")+
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
#offshore a lot of energy intensive production into countries like China. In all three log10 scaled graphs we can clearly see 
#the trend and correlation between electricity production and gdp growth using linear regression. Which supports our question 

total_graph_HDI

#From this graph we can see that the correlation (also from the previous cor function) between HDI and electricity production is 
#smaller and the linear regression line is flatter. This is due to the fact that once a country reaches certain industrial level
#it does not need that much electricity (due to effectiveness, services, etc.) to increase it's HDI which more so from a certain 
#point depends on the social structures more than industrial production or other electricity dependent economic activities. There
#can be a point made about AI (needing large amounts of electricity for data centers) having impact on HDI but we yet do not have 
#sufficient data to support this claim.

ggplot(final_dataset_HDI, aes(x = total_generation__twh)) + 
  geom_histogram() +
  scale_x_log10()+
  labs(title = "Electricity production",
       x = "TWH (log10)")
  theme_minimal()
  
  ggplot(final_dataset_HDI, aes(x = total_generation__twh)) + 
    geom_histogram() +
    labs(title = "Electricity production",
         x = "TWH")
  theme_minimal()  

#The main thing we can see from this graph is that electricity production is uneven across countries. Most are clustered 
#around low to medium production with only a few outlier countries that produces high volumes of electricity. We can also translate 
#this graph into a insight on the immediate economic activity of one country, because electricity really cannot be stored. So 
#higher electricity output means greater potential for economic activity. There can also be an argument made that developing 
#countries produce less electricity than developed countries based on the Pareto distribution of the non log10 graph

#From this analysis we can clearly see a high correlation between gdp and electricity production as well as HDI and electricity 
#production which answers our initial question positively. However as we have seen from the several outliers, particulary Japan
# and USA, electricity production is a necessary prerequisite for an economic development to some extent. This effect shows that 
#developed economies can sustain high economic output through means other than pure electricity production related sectors.  
  
  
stargazer(model,  type = "latex", out = "elektrina_model.pdf", covariate.labels = c("total\\_generation\\_\\_twh"))


