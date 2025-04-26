##########################
#### TUTORIAL GGPLOT2 ####
#### ABRAHAM ARBELAEZ ####
##########################

################################################################################

# Let's load our libraries first!

library(readr) # to read csv
library(tidyverse)
library(ggplot2) # ggplot2 is already part of tidyverse


# Let's load our data!
url1 <- "https://raw.githubusercontent.com/abraham-arbelaez/ggplot-tutorial/refs/heads/main/data/housing.csv"
housing <- read_csv(url(url1))

# let's inspect it
head(housing)
View(housing)

## Time to plot!
# First, we will plot the coordinates of the different housing data!
plot(housing$longitude, housing$latitude)

# Let's try to make it a little bit better
plot(housing$longitude,                  # x-axis
     housing$latitude,                   # y-axis
     pch = 16,                           # shape (you can change it from 1-25) 
     cex = 0.5,                          # size (from 0.1 to as big as you want)
     col = "#522888",                    # color (you can also input regular colors)
     xlab = "Longitude",                 # Label for x-axis
     ylab = "Latitude",                  # Label for y-axis
     main = "Housing in California")     # Main title

# Now that it looks a little better, let's start getting some of the data attributes
# plot by ocean proximity
plot(housing$longitude, housing$latitude,
     pch = 16,                                  # shape (you can change it from 1-25) 
     cex = 0.5,                                 # size (from 0.1 to as big as you want)
     col = factor(housing$ocean_proximity),     # color (you can also input regular colors)
     xlab = "Longitude",                        # Label for x-axis
     ylab = "Latitude",                         # Label for y-axis
     main = "Housing in California by Ocean Proximity") 

# Let's make it better now
colors <- c("red", "blue", "green", "orange", "purple")  # pick whatever colors you like

plot(housing$longitude, housing$latitude,
     pch = 16,
     cex = 0.5,
     col = colors[factor(housing$ocean_proximity)],  # map factor levels to colors
     xlab = "Longitude",
     ylab = "Latitude",
     main = "Housing in California by Ocean Proximity")

legend("topright",
       legend = c("NEAR BAY", "<1H OCEAN", "INLAND", "NEAR OCEAN", "ISLAND"),
       col = colors,
       pch = 16)

# Okay, cool...Can we ACTUALLY make it look BETTER?
ggplot(data = housing,
       aes(x = longitude, 
           y = latitude,
           col = ocean_proximity),
       size = 0.5)+
  geom_point() + 
  labs(
    title = "Housing in California by Ocean Proximity",
    x = "Longitude",
    y = "Latitude",
    color = "Ocean Proximity") +
  theme_linedraw()


# Even better...
ggplot(data = housing,
       aes(x = longitude, 
           y = latitude,
           col = ocean_proximity),
       size = 0.5) +
  geom_point() + 
  labs(
    title = "Housing in California by Ocean Proximity",
    x = "Longitude",
    y = "Latitude",
    color = "Ocean Proximity") +
  theme_linedraw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1)
  )


## Last step
# Using ggmap to create VERY fancy maps
# registering key (this is my API key, you may need to get API keys)
library(ggmap)
register_stadiamaps("15a9bb13-20bb-4bde-a2a4-a69e08500425")

# if you don't have API keys...
library(leaflet)
leaflet(housing) %>% 
  addCircleMarkers(radius = 1,
                   color = "#512888") %>% 
  addTiles()

# # Creating boundaries
basemap <- get_stadiamap(bbox = c(left = min(housing$longitude) - 0.1,
                                  bottom = min(housing$latitude) - 0.1,
                                  right = max(housing$longitude) + 0.1,
                                  top = max(housing$latitude) + 0.1),
                         zoom = 7,
                         maptype = "stamen_watercolor")

# Visualizing boundaries
ggmap(basemap)

# Checking coordinates
ggmap(basemap) +
  geom_point(data = housing, 
             aes(x = longitude, 
                 y = latitude, 
                 color = ocean_proximity), 
             size = 0.8,    
             alpha = 0.7) +
  scale_color_viridis_d(option = "magma") +
  labs(
    title = "Housing in California by Ocean Proximity",
    x = "Longitude",
    y = "Latitude",
    color = "Ocean Proximity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("gray", 0.7), color = "black"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) 



# Let's check median Income for households within a block of houses (tens of thousands USD)
# Checking coordinates
ggmap(basemap) +
  geom_point(data = housing, 
             aes(x = longitude, 
                 y = latitude, 
                 color = median_income), 
             size = 1,    
             alpha = 0.7) +
  scale_color_viridis_c(option = "B") +  # Continuous scale!
  labs(
    title = "Housing in California by Median Income",
    x = "Longitude",
    y = "Latitude",
    color = "Median Income (10,000s USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("gray", 0.7), color = "black"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_blank()
  ) +
  guides(color = guide_colorbar(barwidth = 0.5, barheight = 6))



# Not everything is spatial (special)...
# Let's plot now housing median age
# We can have some idea of our data through the creation of a table
table(housing$housing_median_age)

# Now let's plot a histogram
hist(housing$housing_median_age,
     breaks = 20)

# Let's make it much better with ggplot
ggplot(housing, aes(x = housing_median_age)) +
  geom_histogram(bins = 20, fill = "#522888", color = "white", alpha = 0.5) +
  labs(
    title = "Distribution of Housing Median Age",
    x = "Housing Median Age (years)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank()
  )

# We can add a density curve on top to make it look fancier!
ggplot(housing, aes(x = housing_median_age)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#522888", color = "white", alpha = 0.5) +
  geom_density(color = "#522888", size = 1) +
  labs(
    title = "Distribution of Housing Median Age with Density Curve",
    x = "Housing Median Age (years)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank()
  )


# Now let's do median income vs median house value...should be positively correlated, right?
# Plot housing median income vs median house value
plot(housing$median_income, housing$median_house_value)

# An improved version with ggplot
ggplot(housing, aes(x = median_income, y = median_house_value)) +
  geom_point(alpha = 0.5, color = "#522888", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(
    title = "Relationship Between Median Income and House Value",
    x = "Median Income (tens of thousands USD)",
    y = "Median House Value (USD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank()
  )

# Density plot to see some trends
ggplot(housing, aes(x = median_income, y = median_house_value)) +
  geom_density2d_filled(contour_var = "ndensity", alpha = 0.8) +
  labs(
    title = "2D Density of Median Income vs. House Value",
    x = "Median Income (10,000s USD)",
    y = "Median House Value (USD)",
    fill = "Density"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal(base_size = 14)

# Getting rid of the $500K bias
# creating a new dataset
housing_clean <- housing[housing$median_house_value < 500001, ]

# Plotting!
ggplot(housing_clean, aes(x = median_income, y = median_house_value)) +
  geom_point(alpha = 0.3, size = 0.8, color = "#522888") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Median Income vs. House Value (Under $500K)",
    x = "Median Income (10,000s USD)",
    y = "Median House Value (USD)"
  ) +
  coord_cartesian(ylim = c(0, 500000)) + 
  theme_minimal(base_size = 14)
