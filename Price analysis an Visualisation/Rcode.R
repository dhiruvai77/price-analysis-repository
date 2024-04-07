#Install all the packages and libraries ----
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("tidyr")
install.packages("data.table")
install.packages("dplyr")
install.packages("cluster")
install.packages("forcats")
install.packages("factoextra")
install.packages("dendextend")
install.packages("ggrepel")

library(ggplot2)      # for data visualization
library(tidyr)        # for data tidying
library(tidyverse)    # for data manipulation and visualization (includes ggplot2 and dplyr)
library(lubridate)    # for working with date-time data
library(data.table)   # for fast data manipulation
library(dplyr)        # for data manipulation (part of tidyverse)
library(cluster)      # for k-means clustering
library(forcats)      # for manipulating factor levels
library(factoextra)   # for additional functionalities for factor analysis
library(dendextend)   # for manipulating dendrogram objects
library(ggrepel)      # for better label placement

#importing the csv file to rstudio----
pricedata <- fread("C:/Users/Lenovo/Desktop/University Files/Data Visualisation/Home Assignment/home_assignment_data_pricing.csv")
View(pricedata)

#Save as rds file and load it
saveRDS(pricedata, file = "pricedata.rds") 
pricedata <- readRDS("pricedata.rds")
print("CSV file imported susscessfully")


#knowing more about the price data provided to us
View(pricedata)
str(pricedata)
summary(pricedata)
nrow(pricedata)
colnames(pricedata)
unique(pricedata$store_id)
unique(pricedata$category)

#Finding total number of unique stores
num_unique_store_ids <- pricedata %>%
  distinct(store_id) %>%
  nrow()

#print(paste("Number of unique store IDs in the price data:", num_unique_store_ids))

#Finding total number of unique products
num_unique_product_ids <- pricedata %>%
  distinct(product_id) %>%
  nrow()

#print(paste("Number of unique product IDs in the price data:", num_unique_product_ids))



#Analyzing whole data ----
#Analyzing each products category available in the practiced
price_cellphones <- pricedata[pricedata$category == "Cellphones",]
View(price_cellphones)
str(price_cellphones)
summary(price_cellphones)
nrow(price_cellphones)
ncol(price_cellphones)

price_nintendo <- pricedata[pricedata$category == "Nintendo Wii U",]
View(price_nintendo)
str(price_nintendo)
summary(price_nintendo)
nrow(price_nintendo)
ncol(price_nintendo)

price_pc <- pricedata[pricedata$category == "Pc",]
View(price_pc)
str(price_pc)
summary(price_pc)
nrow(price_pc)
ncol(price_pc)

price_ps3 <- pricedata[pricedata$category == "Ps3",]
View(price_ps3)
str(price_ps3)
summary(price_ps3)
nrow(price_ps3)
ncol(price_ps3)

price_ps4 <- pricedata[pricedata$category == "Ps4",]
View(price_ps4)
str(price_ps4)
summary(price_ps4)
nrow(price_ps4)
ncol(price_ps4)

price_xbox360 <- pricedata[pricedata$category == "Xbox 360",]
View(price_xbox360)
str(price_xbox360)
summary(price_xbox360)
nrow(price_xbox360)
ncol(price_xbox360)

price_xboxone <- pricedata[pricedata$category == "Xbox One",]
View(price_xboxone)
str(price_xboxone)
summary(price_xboxone)
nrow(price_xboxone)
ncol(price_xboxone)

price_headphones <- pricedata[pricedata$category == "Headphones",]
View(price_headphones)
str(price_headphones)
summary(price_headphones)
nrow(price_headphones)
ncol(price_headphones)


price_speakers <- pricedata[pricedata$category == "Mobile Speakers",]
View(price_speakers)
str(price_speakers)
summary(price_speakers)
ncol(price_speakers)
nrow(price_speakers)






# Perform k-means clustering ----
# Calculate distinct product counts for each store and filter
store_product_counts <- pricedata %>%
  group_by(store_id) %>%
  summarise(distinct_product_count = n_distinct(product_id)) %>%
  filter(distinct_product_count > 400) # Filtering within summarise()
View(store_product_counts)
# Plot the clusters
k <- 3 # Number of clusters
kmeans_result <- kmeans(store_product_counts$distinct_product_count, centers = k)

# Add cluster labels to the dataframe
store_product_counts$cluster <- as.factor(kmeans_result$cluster)

# Recode cluster levels
store_product_counts$cluster <- recode_factor(store_product_counts$cluster,
                                              `1` = "High",
                                              `2` = "Medium",
                                              `3` = "Low")

# Define custom color palette for High, Medium, and Low clusters
cluster_colors <- c("High" = "blue", "Medium" = "green", "Low" = "red")

# Plot the clusters
# Plot the clusters
ggplot(store_product_counts, aes(x = 1, y = distinct_product_count, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = store_id), nudge_y = 10, segment.color = NA, size = 3) + 
  labs(title = "K-means Clustering of Store by Distinct Product Count",
       x = "",  # No label on x-axis
       y = "Distinct Product Count",
       color = "Cluster") +
  scale_color_manual(values = cluster_colors, name = "Cluster") + 
  theme_minimal() +
  ggtitle("Clustering of Stores with More Than 400 Distinct Products") + 
  theme(axis.text.x = element_blank(),  # Hide x-axis labels and ticks
        axis.ticks.x = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))
#Selecting the best stores from this clustering
cluster_three_stores <- store_product_counts %>%
  filter(cluster == 1)
# Sort the dataframe by distinct_product_count in descending order
cluster_three_stores <- cluster_three_stores %>%
  arrange(desc(distinct_product_count))
# Select the top 10 store IDs
top_10_cluster_three_stores <- head(cluster_three_stores$store_id, 10)
# Print or use the top 10 store IDs
print(top_10_cluster_three_stores)

# Selected store_ids
selected_store_ids <- c(3303, 428,  1917,  1260, 28603, 20443,   135, 11703, 12280,  7088) 

# Filter pricedata for selected store_ids
selected_store_data <- price_headphones %>%
  filter(store_id %in% selected_store_ids)

# Calculate price variation for each product across all stores
products_cluster <- selected_store_data %>%
  group_by(product_id) %>%
  summarise(price_variation = max(price) - min(price))

# View(products_cluster)


#Filtering our data for selected stores and products from k means clustering----
#Selecting only headphone category
price_headphones <- pricedata[pricedata$category == "Headphones",]
View(price_headphones)
str(price_headphones)
summary(price_headphones)
nrow(price_headphones)
ncol(price_headphones)


#Finding total number of unique stores
num_unique_store_ids <- price_headphones %>%
  distinct(store_id) %>%
  nrow()

print(paste("Number of unique store IDs in the headphone category data:", num_unique_store_ids))

#Finding total number of unique products
num_unique_product_ids <- price_headphones %>%
  distinct(product_id) %>%
  nrow()

print(paste("Number of unique product IDs in the headphone category data:", num_unique_product_ids))

# Define the store ID and product ID

new_store_id <- c("3303", "428", "1260") 
new_product_id <- c("1406701","1038763", "132943", "2764455")

# Filter data to include only four stores and four products
filtered_data <- subset(price_headphones, store_id %in% new_store_id & product_id %in% new_product_id)

# Write the extracted data to a new CSV file
write.csv(filtered_data, file = "C:/Users/Lenovo/Desktop/University Files/Data Visualisation/Home Assignment/filtered_data.csv", row.names = FALSE)

View(filtered_data)
ncol(filtered_data)
nrow(filtered_data)
summary(filtered_data)

#Define custom labels for store id
custom_labels <- c("3303" = "Store ID 3303",
                   "428" = "Store ID 428",
                   "1260" = "Store ID 1260")
#Plot the price change over years for all products with store id as some indicator
ggplot(filtered_data, aes(x = date, y = price, color = factor(product_id))) +
  geom_line(size = 1) +
  facet_wrap(~ store_id, ncol = 1, labeller = labeller(store_id = custom_labels)) +  # Separate plots for each store
  labs(title = "Comparison of Pricing of Selected Products Within Each Store Over Time",
       x = "Date", y = "Price",
       color = "Product ID") +
  theme_minimal()


View(filtered_data)

#filtering the data for individual products ----
# Subset data for the product ID 132943
product1_data <- filtered_data %>% filter(product_id == 1406701)
View(product1_data)
# Subset data for the product ID 132943
product2_data <- filtered_data %>% filter(product_id == 132943)
View(product2_data)
# Subset data for the product ID 132943
product3_data <- filtered_data %>% filter(product_id == 1038763)
View(product3_data)
# Subset data for the product ID 2764455
product4_data <- filtered_data %>% filter(product_id == 2764455)
View(product4_data)



#filtering the data for individual stores ----

# Subset data for the store ID 3303
store1_data <- filtered_data %>% filter(store_id == 3303)
# Subset data for the store ID 428
store2_data <- filtered_data %>% filter(store_id == 428)
# Subset data for the store ID 1260
store3_data <- filtered_data %>% filter(store_id == 1260)



#Descriptive analysis for products and stores ----
#For 1st product
summary_stats1 <- product1_data %>%
  group_by(product_id, store_id) %>%
  summarize(
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price)
  )

View(summary_stats1)

#for 2nd product
summary_stats2 <- product2_data %>%
  group_by(product_id, store_id) %>%
  summarize(
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price)
  )

View(summary_stats2)

#For third product
summary_stats3 <- product3_data %>%
  group_by(product_id, store_id) %>%
  summarize(
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price)
  )

View(summary_stats3)

#For 4th product
summary_stats4 <- product4_data %>%
  group_by(product_id, store_id) %>%
  summarize(
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price)
  )

View(summary_stats4)

#Calculating coefficient of variable for each products
cv <- filtered_data %>%
  group_by(product_id) %>%
  summarise(
    CV = sd(price) / mean(price) * 100
  )
View(cv)



# Analysis with different visualisations ---- 
#Visualising price of each products against time for all stores ----
#Creating custom labels
custom_labels <- c("Store 428", "Store 1260", "Store 3303")
# for product 1406701----
ggplot(product1_data, aes(x = date, y = cpi_adjusted_price, color = factor(store_id))) +
  geom_line(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess", se = FALSE, aes(color = factor(store_id)), size = 1) +  
  labs(title = "Price Data Over Years for Product 1406701",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),  
    legend.position = "top",  
    legend.title = element_blank(),  
    legend.key.width = unit(1, "cm"),  
    legend.key.height = unit(0.7, "cm"),  
    panel.grid.major = element_line(color = "lightgray"),  
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 12)  # Center the plot title
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
# for product 132943----

# Plot with customized facet labels and y-axis range, and smoothed line
ggplot(product2_data, aes(x = date, y = cpi_adjusted_price, color = factor(store_id))) +
  geom_line(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess", se = FALSE, aes(color = factor(store_id)), size = 1) +  
  labs(title = "Price Data Over Years for Product 132943",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),  
    legend.position = "top",  
    legend.title = element_blank(),  
    legend.key.width = unit(1, "cm"),  
    legend.key.height = unit(0.7, "cm"),  
    panel.grid.major = element_line(color = "lightgray"),  
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)  # Center the plot title
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# for product 1038763----

# Plot with customized facet labels and y-axis range, and smoothed line
ggplot(product3_data, aes(x = date, y = cpi_adjusted_price, color = factor(store_id))) +
  geom_line(size = 0.8, alpha = 0.6) +  # Regular line with reduced thickness and transparency
  geom_smooth(method = "loess", se = FALSE, aes(color = factor(store_id)), size = 1) +  # Smoothed line with store-based color and increased size
  labs(title = "Price Data Over Years for Product 1038763",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  scale_y_continuous(limits = c(0, 2000)) + 
  theme_minimal() +
  theme(
    text = element_text(face = "bold", size = 15),  
    legend.position = "top",  # Move legend to the top
    legend.title = element_blank(),   # Remove legend title
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.key.height = unit(0.7, "cm"),  # Increase legend key height
    panel.grid.major = element_line(color = "lightgray"),  # Add light gray gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )+
  scale_color_manual(values = c("red", "green", "blue"), labels = custom_labels) 

# for product 2764455----
# Plot with customized facet labels and y-axis range, and smoothed line
ggplot(product4_data, aes(x = date, y = cpi_adjusted_price, color = factor(store_id))) +
  geom_line(size = 0.5, alpha = 0.5) +  # Regular line with reduced thickness and transparency
  geom_smooth(method = "loess", se = FALSE, aes(color = factor(store_id)), size = 1) +  # Smoothed line with store-based color and increased size
  labs(title = "Price Data Over Years for Product 2764455",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  scale_y_continuous(limits = c(0, 800)) + 
  theme_minimal() +
  theme(
    text = element_text(face = "bold"), 
    legend.position = "top",  # Move legend to the top
    legend.title = element_blank(),  # Remove legend title
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.key.height = unit(0.7, "cm"),  # Increase legend key height
    panel.grid.major = element_line(color = "lightgray"),  # Add light gray gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )




#Calculating monthly average CPI prices and analyse price dispersion ----

#For product 1406701 Calculate average monthly CPI-adjusted price ----
# Calculate average monthly CPI-adjusted price for each product and store
average_monthly_price <- product1_data %>%
  group_by(product_id, store_id, date) %>%
  summarize(avg_cpi_adjusted_price = mean(cpi_adjusted_price))

# Plot
ggplot(average_monthly_price, aes(x = date, y = avg_cpi_adjusted_price, color = factor(store_id))) +
  geom_point(size = 2, shape = 21, fill = "white") +  # Larger points with solid borders
  labs(title = "Price Data Over Years for Product 1406701",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year", limits = c(as.Date("2012-01-01"), NA)) +  # Adjust x-axis date range
  theme_minimal() +
  theme(
    text = element_text(face = "bold", size = 14),  # Adjust font size
    legend.position = "top",  # Move legend to the top
    legend.title = element_blank(),   # Remove legend title
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.key.height = unit(0.7, "cm"),  # Increase legend key height
    panel.grid.major = element_line(color = "lightgray"),  # Add light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_color_manual(values = c("red", "green", "blue"), labels = custom_labels) +  # Adjust color scheme
  guides(color = guide_legend(override.aes = list(size = 5)))  # Increase size of legend dots

#For product 132943 Calculate average monthly CPI-adjusted price ----
average_monthly_price <- product2_data %>%
  group_by(product_id, store_id, date) %>%
  summarize(avg_cpi_adjusted_price = mean(cpi_adjusted_price))

# Plot
ggplot(average_monthly_price, aes(x = date, y = avg_cpi_adjusted_price, color = factor(store_id))) +
  geom_point(size = 2, shape = 21, fill = "white") +  # Larger points with solid borders
  labs(title = "Price Data Over Years for Product 132943",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year", limits = c(as.Date("2012-01-01"), NA)) +  # Adjust x-axis date range
  theme_minimal() +
  theme(
    text = element_text(face = "bold", size = 14),  # Adjust font size
    legend.position = "top",  # Move legend to the top
    legend.title = element_blank(),   # Remove legend title
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.key.height = unit(0.7, "cm"),  # Increase legend key height
    panel.grid.major = element_line(color = "lightgray"),  # Add light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_color_manual(values = c("red", "green", "blue"), labels = custom_labels) +  # Adjust color scheme
  guides(color = guide_legend(override.aes = list(size = 5)))  # Increase size of legend dots

#For product 1038763 Calculate average monthly CPI-adjusted price ----
average_monthly_price <- product3_data %>%
  group_by(product_id, store_id, date) %>%
  summarize(avg_cpi_adjusted_price = mean(cpi_adjusted_price))

# Plot
ggplot(average_monthly_price, aes(x = date, y = avg_cpi_adjusted_price, color = factor(store_id))) +
  geom_point(size = 2, shape = 21, fill = "white") +  
  labs(title = "Price Data Over Years for Product 1038763",
       x = "Year",
       y = "Avg Monthly CPI Adjusted Price",
       color = "Store ID") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year", limits = c(as.Date("2012-01-01"), NA)) +  
  theme_minimal() +
  theme(
    text = element_text(face = "bold", size = 14),  
    legend.position = "top",  
    legend.title = element_blank(),   
    legend.key.width = unit(1, "cm"),  
    legend.key.height = unit(0.7, "cm"),  
    panel.grid.major = element_line(color = "lightgray"),  
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.title.x = element_text(size = 12),  # Adjust x-axis title size
    axis.title.y = element_text(size = 12)   # Adjust y-axis title size
  ) +
  scale_color_manual(values = c("red", "green", "blue"), labels = custom_labels) +  
  guides(color = guide_legend(override.aes = list(size = 5)))  

#For product 2764455 Calculate average monthly CPI-adjusted price ----
average_monthly_price <- product4_data %>%
  group_by(product_id, store_id, date) %>%
  summarize(avg_cpi_adjusted_price = mean(cpi_adjusted_price))

# Plot
ggplot(average_monthly_price, aes(x = date, y = avg_cpi_adjusted_price, color = factor(store_id))) +
  geom_point(size = 2, shape = 21, fill = "white") +  # Larger points with solid borders
  labs(title = "Price Data Over Years for Product 2764455",
       x = "Year",
       y = "CPI Adjusted Price",
       color = "Store ID") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year", limits = c(as.Date("2012-01-01"), NA)) +  # Adjust x-axis date range
  theme_minimal() +
  theme(
    text = element_text(face = "bold", size = 14),  # Adjust font size
    legend.position = "top",  # Move legend to the top
    legend.title = element_blank(),   # Remove legend title
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.key.height = unit(0.7, "cm"),  # Increase legend key height
    panel.grid.major = element_line(color = "lightgray"),  # Add light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) +
  scale_color_manual(values = c("red", "green", "blue"), labels = custom_labels) +  # Adjust color scheme
  guides(color = guide_legend(override.aes = list(size = 2, shape = 21, linetype = 1, color = "black")))  # Increase size of legend dots and set boundary color

#Hierarchial clustering ---- 


# Function to plot hierarchical clustering dendrogram for a product ID
#We took the product id from k means clustering 
plot_dendrogram <- function(selected_product_id) {
  # Filter selected_store_data for the specified product ID
  product_data <- selected_store_data %>%
    filter(product_id == selected_product_id)
  
  # Aggregate data at the store level
  store_avg_price <- product_data %>%
    group_by(store_id) %>%
    summarise(avg_cpi_adjusted_price = mean(cpi_adjusted_price))
  
  # Perform hierarchical clustering
  hc <- hclust(dist(store_avg_price$avg_cpi_adjusted_price), method = "ward.D2")
  
  # Cut the dendrogram into clusters
  clusters <- cutree(hc, k = 3)  # Fixed to 3 clusters for demonstration, adjust as needed
  
  # Assign names to clusters based on average price
  cluster_names <- ifelse(store_avg_price$avg_cpi_adjusted_price < quantile(store_avg_price$avg_cpi_adjusted_price, 1/3), "Low",
                          ifelse(store_avg_price$avg_cpi_adjusted_price < quantile(store_avg_price$avg_cpi_adjusted_price, 2/3), "Medium", "High"))
  
  # Convert hc to a dendrogram object
  dend <- as.dendrogram(hc)
  
  # Color branches based on clusters
  dend_colored <- dend %>%
    color_branches(k = 3)  # Fixed to 3 clusters for demonstration, adjust as needed
  
  # Get maximum price difference for scaling Y-axis
  max_price_diff <- max(abs(diff(store_avg_price$avg_cpi_adjusted_price)))
  
  # Plot dendrogram with store IDs and customized parameters
  plot(dend_colored,
       ylim = c(1, max_price_diff),  # Set Y-axis limits
       ylab = "Price Difference", xlab = "",  # Empty X-axis label
       # main = "Hierarchical Price Clustering for Different Stores for the Products",  # Title
       sub = paste("Product ID:", selected_product_id),  # Product ID as subtitle
       axes = FALSE
  )  # Turn off axes
  
  # Add vertical store IDs without numbers
  axis(side = 1, at = 1:length(store_avg_price$store_id), labels = store_avg_price$store_id, las = 2)  # las = 2 for vertical labels
  
  
  # Add Y-axis scale with readable labels
  axis(side = 2, at = seq(0, max_price_diff, by = max_price_diff/5),
       labels = scales::comma(seq(0, max_price_diff, by = max_price_diff/5)))  # Add comma separator
  
}

# List of product IDs
product_ids <- c(1406701, 132943)

# Set up the canvas for multiple plots
par(mfrow=c(1, 2))

# Plot dendrograms for each product ID using loop
for (product_id in product_ids) {
  plot_dendrogram(product_id)
}
