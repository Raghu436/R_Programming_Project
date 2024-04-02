library("dslabs")
library("dplyr")
library("tidyverse")

data <- read.csv(file.choose()) # importing data 
view(data)
head(data)
class(data)
tibble(data)
dim(data)
str(data) # Structure
summary(data) # summary
table(data$Region) # A short summary
table(data$Sales)
table(data$Product_Category)
colSums(is.na(data)) # Checking NA Values
all(duplicated(data)== TRUE) # Checking duplicate values

# Sales vs Region
data %>% 
  select(Sales, Region) %>%
  ggplot(aes(x = Region, y = log10(Sales), fill = Region)) +
  geom_bar(stat="identity")+
  ggtitle("Total Sales by Region")+
  scale_fill_discrete()

# Profit Vs Region
profit_by_region <- aggregate(data$Profit, by=list(Region=data$Region), FUN=sum)
ggplot(data=profit_by_region, aes(x=Region, y=x, fill=Region)) +
  geom_bar(stat="identity") +
  ggtitle("Total Profit by Region") +
  scale_fill_discrete()

# Profit Vs Region -2 
region_profit <- data %>% 
  group_by(Region) %>% 
  summarise(total_profit = sum(Profit))
region_profit <- region_profit %>%
  mutate(percent_profit = round(total_profit/sum(total_profit) * 100, 2))
ggplot(region_profit, aes(x = Region, y = total_profit, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent_profit, "%")),
            color = "red", size = 5, 
            show.legend = FALSE) +
  labs(title = "Total Profit in terms of Region",
       x = "Region",
       y = "Total sales count",
       fill = "Region") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# customer segmentations
ggplot(data, aes(x=Region, fill=Customer_Segment)) + 
  geom_bar(position="dodge") + 
  labs(title="Customer Segmentation by Region", x="Region", y="Count") +
  theme_minimal()

# box plot
Data_west <- filter(data, Region == "West")
ggplot(Data_west, aes(x = Customer_Segment, y = Sales, fill = Customer_Segment)) +
  geom_boxplot() +
  labs(x = "Region only on West", y = "Sales", fill = "Customer_Segment") +
  ggtitle("Sales by Customer Segment in the West Region") +
  theme(plot.title = element_text(hjust = 0.5))

# Histograph simulation
sales <- data$Sales
sales_mean <- mean(sales)
sales_sd <- sd(sales)
sales_dist <- rnorm(n = 10000, mean = sales_mean, sd = sales_sd)
sales_summary <- summary(sales_dist)
hist(sales_dist, main = "Monte Carlo Simulation of Sales", xlab = "Sales")

sales <- data$Profit
sales_mean <- mean(sales)
sales_sd <- sd(sales)
sales_dist <- rnorm(n = 10000, mean = sales_mean, sd = sales_sd)
sales_summary <- summary(sales_dist)
hist(sales_dist, main = "Monte Carlo Simulation of Profit", xlab = "Profit")

# Shipmode verses in all regions
ggplot(data, aes(x = Ship_Mode, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Region, nrow = 2) +
  labs(x = "Ship Mode", y = "Sales", fill = "Region") +
  ggtitle("Shipmode in all Regions") +
  theme_bw()

# Regression1
ggplot(data, aes(x = Sales, y = Profit)) + 
  geom_point() + 
  geom_smooth(method = "lm")+ggtitle("Over all Regression Plot")

# Regression2
ggplot(data, aes(x = Sales, y = Profit)) + 
  geom_point() + 
  geom_smooth(method = "lm") + ggtitle("Regression line for all Regions")+
  facet_grid(. ~ Region)
