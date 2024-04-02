library("dslabs")
library("tidyverse")

# Importing CSV file
data <- read.csv("H://Course Related//CIS 5400//project//archive//Salesstore.csv")

# Plot for Sales Vs Region
data %>% 
  select(Sales, Region) %>%
  ggplot(aes(x = Region, y = log10(Sales), fill = Region)) +
  geom_col() +
  scale_fill_discrete() +
  ggtitle("Total Sales by Region")

# Plot for Profit vs Region
profit_by_region <- aggregate(data$Profit, by = list(Region = data$Region), FUN = sum)
profit_by_region$percent <- round(profit_by_region$x / sum(profit_by_region$x) * 100, 2)

ggplot(profit_by_region, aes(x = "", y = x, fill = Group.1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Profits for all Region") +
  theme_void() +
  geom_label(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5))

# Plot for Customer Segmentation of Regions
ggplot(data, aes(x = Customer_Segment, y = Profit, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Customer Segment", y = "Profit", fill = "Region") +
  theme_bw() +
  ggtitle("Overall Profit by Customer Segment and Region")

# Plot for Customer Segmentation in all Regions
ggplot(data, aes(x = Region, fill = Customer_Segment)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Segmentation in All Regions",
       x = "Region",
       y = "Count",
       fill = "Customer Segment")

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

# Create separate plots for each region
region_plots <- lapply(unique(profit_loss_data$Region), function(region) {
  region_data <- profit_loss_data %>% 
    filter(Region == region)
  
  ggplot(region_data, aes(x = Product_Sub.Category, y = Total_Profit, fill = Product_Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(paste0("Profit and Loss by Product Category in ", region)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into one
all_regions_plot <- ggplot(profit_loss_data, aes(x = Product_Sub.Category, y = Total_Profit, fill = Product_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Profit and Loss by Product Category Across All Regions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Region, scales = "free")

# Show the plots
print(all_regions_plot)
lapply(region_plots, print)

# Plot for Product Sub-Category That Give More Profit & Loss
minprofit <- data %>% group_by(Product_Sub.Category) %>% summarize(sum = sum(Profit))
minprofit$percent <- scales::percent(minprofit$sum / sum(minprofit$sum))

ggplot(minprofit, aes(y = Product_Sub.Category, x = sum)) +
  geom_bar(stat = "identity", fill = "green") +
  ggtitle("Product Sub-Category That Give More Profit & Loss") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = percent), vjust = -0.5)

# Profit & Loss
west_data <- data %>% filter(Region == "West")
profit_loss <- west_data %>% 
  group_by(Product_Name) %>% 
  summarize(total_profit = sum(Profit[Profit > 0]), total_loss = abs(sum(Profit[Profit < 0])))

top_profit <- profit_loss %>% arrange(desc(total_profit)) %>% head(4)
top_loss <- profit_loss %>% arrange(desc(total_loss)) %>% head(4)

plot_data <- rbind(cbind(top_profit, type = "Profit"), cbind(top_loss, type = "Loss"))

ggplot(plot_data, aes(x = Product_Name, y = ifelse(type == "Profit", total_profit, -total_loss), fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00bfff", "#ff6347")) +
  coord_flip() +
  labs(x = "Product Name", y = "Profit/Loss", title = "Top 4 Products with Highest Profit and Loss in the West Region")
