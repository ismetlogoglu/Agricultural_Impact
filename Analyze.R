install.packages("readxl")
library(readxl)

data <- read_xlsx("C:/Users/ismet/Documents/Agricultural_Impact_Data.xlsx")
head(data, n=20)

str(data)
summary(data)

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

table(data$Item)
barplot(data)

barplot(subset1$Year)

subset1 <- subset(data, data$Item == "Meat")
subset1

subset2 <- subset(data, data$Item == "Rice")

subset3 <- subset(data, data$Item == "Cereals")

ggplot(subset1, aes(x = Year, y = Production)) + geom_bar(stat = "identity", fun = "mean", position = "dodge") +
  labs(title = "Meat production by Yeras", y = "Meat Production")

ggplot(subset2, aes(x = Year, y = Production)) + geom_bar(stat = "identity", fun = "mean", position = "dodge") +
  labs(title = "Rice production by Yeras", y = "Rice Production")

ggplot(subset3, aes(x = Year, y = Production)) + geom_bar(stat = "identity", fun = "mean", position = "dodge") +
  labs(title = "Cereals production by Yeras", y = "Cereals Production")



subsetcapita <- subset(data, data$Development == "Developed")
subsetcapita1 <- subset(subsetcapita, subsetcapita$Item == "Cereals")
selected_years <- data[data$Year %in% c(1999, 2009, 2019), ]

ggplot(subsetcapita1, aes(Year, Production)) + boxplot()

str(data)

install.packages("magrittr")
library(magrittr)

filtered_data <- subset(data, Year >= 1995 & Year <= 2020 & Item %in% c("Meat", "Rice", "Cereals"))

production_totals <- aggregate(Production ~ SubRegion + Item, data = filtered_data, FUN = sum)

production_totals$Relative_Frequency <- ave(production_totals$Production, 
                                            production_totals$Item, FUN = function(x) x/sum(x))


ggplot(production_totals, aes(x = SubRegion, y = Relative_Frequency, fill = Item)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relative Frequency Distribution of Total Production Amount",
       x = "SubRegion",
       y = "Relative Frequency") +
  theme_minimal()

#b

rice_2020 <- subset(data, Year == 2020 & Item == "Rice")
total_emissions <- aggregate(`Emissions (CO2eq kt)` ~ Country, data = rice_2020, FUN = sum)
total_emissions <- total_emissions[order(-total_emissions$`Emissions (CO2eq kt)`), ]
total_emissions$cumulative_percentage <- cumsum(total_emissions$`Emissions (CO2eq kt)`) / sum(total_emissions$`Emissions (CO2eq kt)`)
top_countries <- total_emissions[total_emissions$cumulative_percentage <= 0.8, ]
barplot(top_countries$`Emissions (CO2eq kt)`, names.arg = top_countries$Country,
        main = "Top Countries by GHG Emissions due to Rice in 2020",
        xlab = "Country", ylab = "Total Emissions (CO2eq kt)",
        col = rainbow(nrow(top_countries)),
        cex.names = 0.7)
#c

filtered_data <- filter(data, Year %in% c(1999, 2009, 2019))
developed_countries <- filtered_data %>%
  filter(Development == "Developed")
developed_countries <- developed_countries %>%
  group_by(Year, Country) %>%
  summarize(Cereal_Production_Per_Capita = sum(Production) / sum(Population))
ggplot(developed_countries, aes(x = as.factor(Year), y = Cereal_Production_Per_Capita)) +
  geom_boxplot() +
  labs(title = "Cereal Production Amount per Capita in Developed Countries",
       x = "Year",
       y = "Cereal Production per Capita") +
  theme_minimal()

#d

rice_data <- filter(data, Year >= 2011 & Year <= 2020 & Item == "Rice")
rice_production <- rice_data %>%
  group_by(Country) %>%
  summarise(Total_Rice_Production = sum(Production),
            Total_GHG_Emissions = sum(`Emissions (CO2eq kt)`))

top_countries <- rice_production %>%
  arrange(desc(Total_Rice_Production)) %>%
  head(12)

print(top_countries)

#d'nin devam??
rice_data <- filter(data, Year >= 1995 & Year <= 2020 & Item == "Rice")

avg_ghg_emissions <- rice_data %>%
  group_by(Country) %>%
  summarise(Avg_GHG_Emissions = mean(`Emissions (CO2eq kt)`))

sorted_avg_ghg_emissions <- avg_ghg_emissions %>%
  arrange(Avg_GHG_Emissions)

filtered_avg_ghg_emissions <- sorted_avg_ghg_emissions[-(1:20), ]

# Remove the countries with the 20 highest average GHG emissions
filtered_avg_ghg_emissions <- filtered_avg_ghg_emissions[-((nrow(filtered_avg_ghg_emissions) - 19):nrow(filtered_avg_ghg_emissions)), ]

ggplot(filtered_avg_ghg_emissions, aes(x = Avg_GHG_Emissions)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Average GHG Emissions from Rice (1995-2020)",
       x = "Average GHG Emissions (CO2eq kt)",
       y = "Frequency") +
  theme_minimal()

#e
filtered_data <- filter(data, Item %in% c("Rice", "Cereals", "Meat"))

efficiency_data <- filtered_data %>%
  group_by(Year, Item) %>%
  summarise(Total_Production = sum(Production),
            Total_GHG_Emissions = sum(`Emissions (CO2eq kt)`))

efficiency_data <- efficiency_data %>%
  mutate(Emission_Efficiency = Total_Production / Total_GHG_Emissions)

max_efficiency <- efficiency_data %>%
  group_by(Year) %>%
  top_n(1, Emission_Efficiency)

efficiency_changes <- max_efficiency %>%
  select(Year, Item, Emission_Efficiency)

print(efficiency_changes)

#e part2

rice_data_2020 <- filter(data, Year == 2020 & Item == "Rice")

top_countries_2020 <- rice_data_2020 %>%
  group_by(Country) %>%
  summarise(Total_Rice_Production = sum(Production)) %>%
  arrange(desc(Total_Rice_Production)) %>%
  top_n(100)

efficiency_data <- rice_data_2020 %>%
  filter(Country %in% top_countries_2020$Country) %>%
  group_by(Country) %>%
  summarise(Total_Production = sum(Production),
            Total_GHG_Emissions = sum(`Emissions (CO2eq kt)`)) %>%
  mutate(Emission_Efficiency = Total_Production / Total_GHG_Emissions)

development_status <- rice_data_2020 %>%
  filter(Country %in% top_countries_2020$Country) %>%
  distinct(Country, Development)

efficiency_data <- left_join(efficiency_data, development_status, by = "Country")

contingency_table <- table(efficiency_data$Development, cut(efficiency_data$Emission_Efficiency, breaks = 5))

print(contingency_table)


#F

rice_data_2020 <- filter(data, Year == 2020 & Item == "Rice")

scatter_production_emissions <- ggplot(rice_data_2020, aes(x = Production, y = `Emissions (CO2eq kt)`)) +
  geom_point() +
  labs(title = "Relationship Between Rice Production and GHG Emissions (2020)",
       x = "Rice Production Amount",
       y = "GHG Emissions (CO2eq kt)")

scatter_production_withdrawals <- ggplot(rice_data_2020, aes(x = Production, y = `Freshwater withdrawals (kiloliter)`)) +
  geom_point() +
  labs(title = "Relationship Between Rice Production and Freshwater Withdrawals (2020)",
       x = "Rice Production Amount",
       y = "Freshwater Withdrawals (kiloliter)")

scatter_emissions_withdrawals <- ggplot(rice_data_2020, aes(x = `Emissions (CO2eq kt)`, y = `Freshwater withdrawals (kiloliter)`)) +
  geom_point() +
  labs(title = "Relationship Between GHG Emissions and Freshwater Withdrawals for Rice Production (2020)",
       x = "GHG Emissions (CO2eq kt)",
       y = "Freshwater Withdrawals (kiloliter)")

correlation_production_emissions <- cor(rice_data_2020$Production, rice_data_2020$`Emissions (CO2eq kt)`)
correlation_production_withdrawals <- cor(rice_data_2020$Production, rice_data_2020$`Freshwater withdrawals (kiloliter)`)
correlation_emissions_withdrawals <- cor(rice_data_2020$`Emissions (CO2eq kt)`, rice_data_2020$`Freshwater withdrawals (kiloliter)`)

# Print correlation coefficients
print(paste("Correlation between Rice Production and GHG Emissions:", correlation_production_emissions))
print(paste("Correlation between Rice Production and Freshwater Withdrawals:", correlation_production_withdrawals))
print(paste("Correlation between GHG Emissions and Freshwater Withdrawals:", correlation_emissions_withdrawals))

# Plot scatter plots
scatter_production_emissions
scatter_production_withdrawals
scatter_emissions_withdrawals


# Filter data for year 2020 and item "Rice"
rice_2020 <- data %>% filter(Year == 2020, Item == "Rice")

# Calculate total emissions due to rice in 2020
total_emissions <- sum(rice_2020$`Emissions (CO2eq kt)`)

# Calculate the cumulative sum of emissions and find the top countries that contribute to 80% of the total emissions
top_countries <- rice_2020 %>%
  arrange(desc(`Emissions (CO2eq kt)`)) %>%
  mutate(cumulative_sum = cumsum(`Emissions (CO2eq kt)`),
         cumulative_percent = cumulative_sum/total_emissions*100) %>%
  filter(cumulative_percent <= 80)

bar_chart <- ggplot(top_countries, aes(x = reorder(Country, - `Emissions (CO2eq kt)`), y = `Emissions (CO2eq kt)`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Countries Contributing to 80% of Total Emissions Due to Rice in 2020",
       x = "Country",
       y = "Emissions (CO2eq kt)") +
  theme_minimal()

# Create a line chart of the cumulative sum of emissions
line_chart <- ggplot(top_countries, aes(x = cumulative_percent, y = `Emissions (CO2eq kt)`, group = 1)) +
  geom_line() +
  labs(title = "Cumulative Emissions Due to Rice in 2020",
       x = "Cumulative Percentage",
       y = "Emissions (CO2eq kt)") +
  theme_minimal()

# Combine the two charts to create a Pareto chart
pareto_chart <- ggarrange(bar_chart, line_chart, ncol = 1, heights = c(3/4, 1/4))

# Print the Pareto chart
print(pareto_chart)

install.packages("ggtree")
install.packages("paleotree")
library(paleotree)
library(ggtree)
install.packages("egg")
library(egg)
