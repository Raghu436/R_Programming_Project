# Load necessary libraries
library(dslabs)
library(dplyr)
library(ggplot2)

# Importing data
data <- read.csv("path/to/your/dataset.csv")

# Basic exploration
# Display the first few rows of the dataset
head(data)

# Check the class of the dataset
class(data)

# Convert the dataset to a tibble for better display
tibble(data)

# Display dimensions of the dataset
dim(data)

# Display structure of the dataset
str(data)

# Summary statistics of the dataset
summary(data)

# Cleaning
# Check for missing values
colSums(is.na(data))

# Check for duplicated rows
all(duplicated(data) == TRUE)

# Exploratory data analysis
# Summary tables
table(data$Region)
table(data$Sales)
table(data$Product_Category)

# Data visualization
# Sales by Region
data %>%
  ggplot(aes(x = Region, y = log10(Sales), fill = Region)) +
  geom_col() +
  scale_fill_discrete()

# Order Priority distribution
data %>%
  count(Order_Priority) %>%
  ggplot(aes(x = "", y = n, fill = Order_Priority)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_discrete(name = "Order_Priority") +
  geom_text(aes(label = paste0(Order_Priority, ": ", n)), position = position_stack(vjust = 0.5))

# Calculate profit by customer segment
profit_by_segment <- data %>% 
  group_by(Customer_Segment) %>% 
  summarise(Total_Profit = sum(Profit)) %>%
  mutate(Percent_Total_Profit = round(Total_Profit / sum(Total_Profit) * 100, 2))

# Plot total profit by customer segment
p1 <- ggplot(data = profit_by_segment, aes(x = Customer_Segment, y = Total_Profit, fill = Customer_Segment)) +
  geom_col() +
  geom_text(aes(label = paste0(Percent_Total_Profit, "%"), y = Total_Profit), vjust = -0.5) +
  scale_fill_manual(values = c("#FFC300", "#FF5733", "#C70039", "#900C3F", "#581845")) +
  labs(title = "Total Profit by Customer Segment", x = "Customer Segment", y = "Total Profit", fill = "Customer Segment")

# Calculate mean profit by customer segment
mean_profit_by_segment <- data %>% 
  group_by(Customer_Segment) %>% 
  summarise(Mean_Profit = round(mean(Profit), 2))

# Plot mean profit by customer segment
p2 <- ggplot(data = mean_profit_by_segment, aes(x = Customer_Segment, y = Mean_Profit, fill = Customer_Segment)) +
  geom_col() +
  geom_text(aes(label = round(Mean_Profit, 2), y = Mean_Profit), vjust = -0.5) +
  scale_fill_manual(values = c("#FFC300", "#FF5733", "#C70039", "#900C3F", "#581845")) +
  labs(title = "Mean Profit by Customer Segment", x = "Customer Segment", y = "Mean Profit", fill = "Customer Segment")

# Combine plots
library(cowplot)
plot_grid(p1, p2, ncol = 2, align = "h")

# Profit in terms of shipmode
library(gridExtra)

# Calculate sum of profit by ship mode
profit_by_shipmode_sum <- aggregate(data$Profit, by = list(data$Ship_Mode), sum)

# Calculate mean of profit by ship mode
profit_by_shipmode_mean <- aggregate(data$Profit, by = list(data$Ship_Mode), mean)

# Plot profit sum by ship mode
p1 <- ggplot(profit_by_shipmode_sum, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  ggtitle("Profit Sum by Ship Mode") +
  xlab("Ship Mode") +
  ylab("Profit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0("$", round(x, 2), " (", round(x / sum(x) * 100, 2), "%)")), vjust = -0.5, size = 4)

# Plot profit mean by ship mode
p2 <- ggplot(profit_by_shipmode_mean, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#87CEFA") +
  ggtitle("Profit Mean by Ship Mode") +
  xlab("Ship Mode") +
  ylab("Profit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0("$", round(x, 2), " (", round(x / sum(x) * 100, 2), "%)")), vjust = -0.5, size = 4)

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)

llibrary(gridExtra)

# Plot profit sum by ship mode
p3 <- ggplot(profit_by_shipmode_sum, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  ggtitle("Profit Sum by Ship Mode") +
  xlab("Ship Mode") +
  ylab("Profit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0("$", round(x, 2), " (", round(x / sum(x) * 100, 2), "%)")), vjust = -0.5, size = 4)

# Plot profit mean by ship mode
p4 <- ggplot(profit_by_shipmode_mean, aes(x = Group.1, y = x)) +
  geom_bar(stat = "identity", fill = "#87CEFA") +
  ggtitle("Profit Mean by Ship Mode") +
  xlab("Ship Mode") +
  ylab("Profit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0("$", round(x, 2), " (", round(x / sum(x) * 100, 2), "%)")), vjust = -0.5, size = 4)

# Arrange plots side by side
grid.arrange(p3, p4, ncol = 2)

# Profit Vs Region
library(ggplot2)

# Calculate total profit by region
region_profit <- data %>% 
  group_by(Region) %>% 
  summarise(total_profit = sum(Profit)) %>%
  mutate(percent_profit = round(total_profit / sum(total_profit) * 100, 2))

# Plot total profit by region
ggplot(region_profit, aes(x = Region, y = total_profit, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent_profit, "%")), color = "red", size = 5, show.legend = FALSE) +
  labs(title = "Total Profit by Region", x = "Region", y = "Total Profit", fill = "Region") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Customer segmentations
# Bar plot of customer segmentation by region
ggplot(data, aes(x = Region, fill = Customer_Segment)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Customer Segmentation by Region", x = "Region", y = "Count") +
  theme_minimal()

# Bar plot of region by customer segment
ggplot(data, aes(x = Customer_Segment, fill = Region)) + 
  geom_bar(position = "dodge", width = 0.9) + 
  labs(title = "Customer Segmentation by Region", x = "Region", y = "Count") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Set2")

# Correlation
library(corrplot)

# Calculate correlation matrix
data_cols <- c("Order_Quantity", "Sales", "Profit")
corr_matrix <- cor(data[, data_cols])

# Plot correlation matrix
corrplot(corr_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45)

# Density plots
ggplot(data, aes(x = Sales)) +
  geom_density(fill = "#69b3a2", alpha = 0.8) +
  labs(title = "Density plot of Sales", x = "Sales", y = "Density")

# Growth rates
growth_rates <- data %>% 
  arrange(Order_ID) %>% 
  group_by(Region) %>% 
  mutate(growth_rate = (Sales - lag(Sales)) / lag(Sales)) %>% 
  ungroup()

# Plot growth rates by region
ggplot(growth_rates, aes(x = Order_ID, y = growth_rate, color = Region)) +
  geom_line() +
  labs(title = "Growth Rates by Region", x = "Order ID", y = "Growth Rate") +
  scale_y_continuous(labels = scales::percent)

# Box plot
Data_west <- filter(data, Region == "West")

# Plot box plot for sales by customer segment in the West region
ggplot(Data_west, aes(x = Customer_Segment, y = Sales, fill = Customer_Segment)) +
  geom_boxplot() +
  labs(x = "Region only on West", y = "Sales", fill = "Customer_Segment") +
  ggtitle("Sales by Customer Segment in the West Region") +
  theme(plot.title = element_text(hjust = 0.5))

# Frequency plot
ggplot(data, aes(x = Product_Category)) +
  geom_bar()

# Histogram simulation for Sales
sales <- data$Sales
sales_mean <- mean(sales)
sales_sd <- sd(sales)
sales_dist <- rnorm(n = 10000, mean = sales_mean, sd = sales_sd)
sales_summary <- summary(sales_dist)
hist(sales_dist, main = "Monte Carlo Simulation of Sales", xlab = "Sales")

# Histogram simulation for Profit
profit <- data$Profit
profit_mean <- mean(profit)
profit_sd <- sd(profit)
profit_dist <- rnorm(n = 10000, mean = profit_mean, sd = profit_sd)
profit_summary <- summary(profit_dist)
hist(profit_dist, main = "Monte Carlo Simulation of Profit", xlab = "Profit")

# Shipmode in all regions
ggplot(data, aes(x = Ship_Mode, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Region, nrow = 2) +
  labs(x = "Ship Mode", y = "Sales", fill = "Region") +
  ggtitle("Shipmode in all Regions") +
  theme_bw()

# Monte Carlo simulation
simulate_sales <- function(data, n) {
  sales_mean <- mean(data$Sales)
  sales_sd <- sd(data$Sales)
  sales_dist <- rnorm(n, mean = sales_mean, sd = sales_sd)
  return(sales_dist)
}
n_simulations <- 5
sales_sims <- replicate(n_simulations, simulate_sales(data, 10000))
sales_stats <- sapply(sales_sims, summary)
par(mfrow = c(1, n_simulations))
for (i in 1:n_simulations) {
  hist(sales_sims[, i], main = paste("Simulation", i))
}

x <- data$Sales
y <- x * x
plot(x, y, log = "x")
plot(data$Sales, data$Profit)

plot(data$Order_ID, data$Profit)

# Correlation Plot & Correlation Matrix
corr_matrix <- cor(data[, c("Order_ID", "Order_Quantity", "Sales", "Profit")])
ggcorrplot(corr_matrix, type = "upper", hc.order = TRUE, 
           ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Correlation Graph for Sales Data")

# 2 type correlation
ggplot(data, aes(x = Sales, y = Profit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

data %>% 
  ggplot(aes(Order_Priority, Sales)) + geom_col()
data %>% 
  ggplot(aes(Ship_Mode, Sales)) + geom_col()

# Item Canon imageCLASS 2200 Advanced Copier - Piechart
pie_sales <- ggplot(sales_by_region, aes(x = "", y = total_sales, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Total Sales") +
  geom_text(aes(label = scales::comma(total_sales)), position = position_stack(vjust = 0.5))

pie_profit <- ggplot(profit_by_region, aes(x = "", y = total_profit, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Profit") +
  geom_text(aes(label = scales::comma(total_profit)), position = position_stack(vjust = 0.5))

grid.arrange(pie_sales, pie_profit, ncol = 2)

data %>% 
  ggplot(aes(Sales, Profit)) + geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) + 
  scale_x_discrete(breaks = data$Sales) +
  scale_y_continuous() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sales") + ylab("Profit")

# Profit & loss
region_data <- aggregate(data[, c("Profit", "Sales")], by = list(data$Region), sum)
colnames(region_data) <- c("Region", "Total_Profit", "Total_Sales")
region_data$Total_Loss <- -1 * region_data$Total_Profit
ggplot(region_data, aes(x = Region, y = Total_Profit, fill = "Profit")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = Total_Loss, fill = "Loss"), stat = "identity") +
  scale_fill_manual(values = c("Profit" = "green", "Loss" = "red")) +
  labs(x = "Region", y = "Amount", title = "Profit and Loss by Region") +
  theme_bw()

# 2 type profit and loss
region_data <- aggregate(data[, c("Profit", "Sales")], by = list(data$Region), sum)
colnames(region_data) <- c("Region", "Total_Profit", "Total_Sales")
region_data$Total_Loss <- -1 * region_data$Total_Profit
region_data$Percent_Profit <- region_data$Total_Profit / sum(region_data$Total_Profit) * 100
region_data$Percent_Loss <- region_data$Total_Loss / sum(region_data$Total_Loss) * 100

ggplot(region_data, aes(x = Region, y = c(Total_Profit, Total_Loss), fill = c("Profit", "Loss"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Profit" = "green", "Loss" = "red")) +
  labs(x = "Region", y = "Amount", title = "Profit and Loss by Region") +
  geom_text(aes(label = paste0(round(c(Percent_Profit, Percent_Loss), 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  theme_bw()
