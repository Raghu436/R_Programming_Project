library("dslabs")
library("tidyverse")
data <- read.csv("H://Course Related//CIS 5400//project//archive//Salesstore.csv") # importing csv file
view(data)
# Plot for sales Vs Region
data %>% 
  select(Sales, Region) %>%
  ggplot(aes(x = Region, y = log10(Sales), fill = Region)) +
  geom_col() +
  scale_fill_discrete()


# plot for profit vs region

ggplot(profit_by_region, aes(x="", y=x, fill=Group.1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Profits for all Region") +
  theme_void() +
  geom_label(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5))

# PLot for customer segmentation of regions
ggplot(data, aes(x = Customer_Segment, y = Profit, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Customer Segment", y = "Profit", fill = "Region") +
  theme_bw() +
  ggtitle("Overall Profit by Customer Segment and Region")

# Plot fro Customer Segmentation in all regions

ggplot(data, aes(x = Region, fill = Customer_Segment)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Segmentation in All Regions",
       x = "Region",
       y = "Count",
       fill = "Customer Segment")

# 
# Create a subset of the data that only includes the columns we're interested in
profit_loss_data <- data %>% 
  select(Product_Category, Product_Sub.Category, Profit, Region)

# Calculate the total profit and loss for each product category and sub-category in each region
profit_loss_data <- profit_loss_data %>% 
  group_by(Product_Category, Product_Sub.Category, Region) %>% 
  summarize(Total_Profit = sum(Profit))

# Identify the top 3 product categories by profit and loss across all regions
top_categories <- profit_loss_data %>% 
  group_by(Product_Category) %>% 
  summarize(Total_Profit = sum(Total_Profit)) %>% 
  arrange(desc(Total_Profit)) %>% 
  head(3) %>% 
  pull(Product_Category)

# Create a separate plot for each region
region_plots <- list()
for (region in unique(profit_loss_data$Region)) {
  region_data <- profit_loss_data %>% 
    filter(Region == region)
  
  region_plots[[region]] <- ggplot(region_data, aes(x = Product_Sub.Category, y = Total_Profit, fill = Product_Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(paste0("Profit and Loss by Product Category in ", region)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Combine the plots into one
all_regions_plot <- ggplot(profit_loss_data, aes(x = Product_Sub.Category, y = Total_Profit, fill = Product_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Profit and Loss by Product Category Across All Regions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Region, scales = "free")

# Show the plots
print(all_regions_plot)
for (region_plot in region_plots) {
  print(region_plot)top_profit_products_plot <- ggplot(top_profit_products, aes(x = Product_Name, y = Total_Profit, fill = Region)) +
  geom_col(position = "dodge") +
  labs(title = "Top 3 Profitable Products in each Region", x = "Product Name", y = "Total Profit")

top_loss_products_plot <- ggplot(top_loss_products, aes(x = Product_Name, y = Total_Loss, fill = Region)) +
  geom_col(position = "dodge") +
  labs(title = "Top 3 Loss-Making Products in each Region", x = "Product Name", y = "Total Loss")

(top_profit
}

# prtcce
library(ggplot2)
library(dplyr)

# Calculate the sum of profits by product sub-category
minprofit <- data %>% group_by(Product_Sub.Category) %>% summarize(sum = sum(Profit))

# Create a bar plot using ggplot
ggplot(minprofit, aes(y = Product_Sub.Category, x = sum)) +
  geom_bar(stat = "identity", fill = "green") +
  ggtitle("Product Sub-Category That Give More Profit & Loss") +
  theme(plot.title = element_text(hjust = 0.5))

# Add percentage labels to the bar plot
minprofit$percent <- scales::percent(minprofit$sum / sum(minprofit$sum))
ggplot(minprofit, aes(y = Product_Sub.Category, x = sum)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Product Sub-Category That Give More Profit & Loss") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = percent), vjust = -0.5)


# Profit & LOss

west_data <- data %>% filter(Region == "West")

# Group the data by product name and calculate the total profit and loss
profit_loss <- west_data %>% 
  group_by(Product_Name) %>% 
  summarize(total_profit = sum(Profit[Profit > 0]), total_loss = abs(sum(Profit[Profit < 0])))

# Find the top 4 products with the highest profit
top_profit <- profit_loss %>% arrange(desc(total_profit)) %>% head(4)

# Find the top 4 products with the highest loss
top_loss <- profit_loss %>% arrange(desc(total_loss)) %>% head(4)

# Combine the top profit and top loss data into a single data frame
plot_data <- rbind(cbind(top_profit, type = "Profit"),
                   cbind(top_loss, type = "Loss"))

# Create a bar plot of the top profit and loss products
ggplot(plot_data, aes(x = Product_Name, y = ifelse(type == "Profit", total_profit, -total_loss), fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00bfff", "#ff6347")) +
  coord_flip() +
  labs(x = "Product Name", y = "Profit/Loss", title = "Top 4 Products with Highest Profit and Loss in the West Region")

