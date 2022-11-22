library("viridis")
library("ggplot2")
library("gridExtra")
library("reshape2")
library("GGally")
library("tidyverse")
library("truncnorm")
library("stringr")
library("tidygeocoder")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgdal")
library("broom")
library("geojsonio")
library("caret")
library("devtools")
library("ggbiplot")

df <- read.csv("./House_Rent_Dataset.csv")

outline <- "#b3b3b3"

area <- "#edb7b7"

# Data preprocessing

# Dimension of data
dim(df)

# Snapshot of data
head(df)

# Summary of data
summary(df)

# Get number of n.a. cell in dataset
colSums(is.na(df))

# Get number of each unique value from discrete and object/string value
object_feature <- setdiff(names(df), c("Rent", "Size"))
unique_value <- c()
for (column in object_feature) {
   n <- length((unique(df[[column]])))
   unique_value <- c(unique_value, n)
}

df_unique <- data.frame("Features" = object_feature, "Number of Uniques Value" = unique_value)

df_unique

# Remove entries that has errors
# Remove entries which has unstandardized values in Floor such as "Ground" (should be "Ground out of x")
# Corresponds to Analysis 1-6

floor_matrix <- str_split_fixed(df$Floor, " out of ", 2)

df$Current.Floor <- floor_matrix[, 1]
df$Current.Floor[df$Current.Floor %in% c("Ground", "Lower Basement", "Upper Basement")] <- "0"
df$Current.Floor <- as.integer(df$Current.Floor)

df$Total.Floor <- floor_matrix[, 2]

df <- subset(df, !Total.Floor == "")

df$Total.Floor <- as.integer(df$Total.Floor)

# Question 1: How is the distribution of each feature of the dataset/Do a univariate analysis on feature
# Analysis 1-1: Plot the distribution of BHK
df$BHK.String = as.character(df$BHK)

ggplot(df, aes(x=BHK.String, fill=BHK.String)) +
geom_bar() +
ggtitle("Bar Chart of BHK") +
labs(x="BHK", y="Frequency")

# Analysis 1-2: Distribution of Rent

ggplot(df, aes(x=Rent)) +
geom_histogram(bins=150) +
geom_vline(xintercept = mean(df$Rent), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Rent), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Rent), label="Mean", y=2000), color="red", angle=90) +
geom_text(aes(x=median(Rent), label="Median", y=1500), color="blue", angle=90) +
ggtitle("Histogram of Rent") +
labs(x="Rent", y="Frequency")

# Analysis 1-4: Change Rent outlier's value with random samples of Rent's normal distribution into Rent.Adjusted and plot the histogram
quartiles <- quantile(df[, "Rent"], probs=c(0.25, 0.75), na.rm = FALSE)
IQR <- IQR(df[, "Rent"])

lower <- quartiles[1] - 1.5 * IQR
higher <- quartiles[2] + 1.5 * IQR

data_no_outlier <- df[(df[, "Rent"] > lower) & (df[, "Rent"] < higher), ]

mean_rent <- mean(data_no_outlier$Rent)

sd_rent <- sd(data_no_outlier$Rent)

tmp <- tibble(Rent = df$Rent, Rent.Sample = rtruncnorm(n = 4742,a = min(df$Rent),b = Inf,mean = mean_rent,sd = sd_rent))

df_adjusted <- tmp %>% 
select(Rent, Rent.Sample) %>% 
mutate (

    Rent.Adjusted = case_when(
        (Rent < lower | Rent > higher) ~ Rent.Sample,
        TRUE ~ as.numeric(Rent)),

    Rent.Outlier = case_when(
        (Rent < lower | Rent > higher) ~ "Outlier",
        TRUE ~ "Not Outlier")
    )

df$Rent.Adjusted <- df_adjusted$Rent.Adjusted

ggplot(df, aes(x=Rent.Adjusted)) +
geom_histogram(bins=35, color=outline, fill=area) +
geom_vline(xintercept = mean(df$Rent.Adjusted), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Rent.Adjusted), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Rent.Adjusted), label="Mean", y=400), color="red", angle=90) +
geom_text(aes(x=median(Rent.Adjusted), label="Median", y=500), color="blue", angle=90) +
ggtitle("Histogram of Rent Adjusted") +
labs(x="Rent.Adjusted", y="Frequency")

# Analysis 1-4: Categorize the Rent feature into Rent.Factor and plot the bar chart

remove_outlier <- function(df, column) {
    quartiles <- quantile(df[, column], probs=c(0.25, 0.75), na.rm = FALSE)
    IQR <- IQR(df[, column])

    lower <- quartiles[1] - 1.5 * IQR
    higher <- quartiles[2] + 1.5 * IQR

    data_no_outlier <- df[(df[, column] > lower) & (df[, column] < higher), ]
    return (data_no_outlier)
}

df_no_outlier <- remove_outlier(df, "Rent")

mean_no_outlier <- mean(df_no_outlier$Rent)

sd_no_outlier <- sd(df_no_outlier$Rent)

break_points <- c(-Inf, mean_no_outlier - sd_no_outlier, mean_no_outlier, mean_no_outlier + sd_no_outlier, mean_no_outlier + 2 * sd_no_outlier, Inf)

df <- df %>%
mutate(
    Rent.Factor = Rent %>%
    cut(breaks=break_points) %>%
    fct_recode(
        "< mean - sd" = "(-Inf,5.46e+03]",
        "< mean" = "(5.46e+03,1.93e+04]",
        "< mean + sd" = "(1.93e+04,3.31e+04]",
        "< mean + 2sd" = "(3.31e+04,4.7e+04]",
        "Inf" = "(4.7e+04, Inf]"
    )
)

unique(df$Rent.Factor)

ggplot(df, aes(x=Rent.Factor, fill=Rent.Factor)) +
geom_bar() +
ggtitle("Bar Graph of Rent.Factor") +
labs(x="Rent.Factor", y="Frequency")

# Analysis 1-5: Plot the distribution of Size

ggplot(df, aes(x=Size)) +
geom_histogram(bins=40) +
ggtitle("Histogram of Size") +
geom_vline(xintercept = mean(df$Size), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Size), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Size), label="Mean", y=400), color="red", angle=90) +
geom_text(aes(x=median(Size), label="Median", y=500), color="blue", angle=90) +
labs(x="Size", y="Frequency")

# Analysis 1-6: Change Size outlier's value with random samples of Size's normal distribution into Size.Adjusted and plot the histogram
quartiles_size <- quantile(df[, "Size"], probs=c(0.25, 0.75), na.rm = FALSE)
IQR_size <- IQR(df[, "Size"])

lower <- quartiles_size[1] - 1.5 * IQR_size
higher <- quartiles_size[2] + 1.5 * IQR_size

data_no_outlier_size <- df[(df[, "Size"] > lower) & (df[, "Size"] < higher), ]

mean_size <- mean(data_no_outlier_size$Size)

sd_size <- sd(data_no_outlier_size$Size)

tmp_size <- tibble(Size = df$Size, Size.Sample = rtruncnorm(n = 4742,a = min(df$Size),b = Inf,mean = mean_size,sd = sd_size))

df_adjusted <- tmp_size %>% 
select(Size, Size.Sample) %>% 
mutate (

    Size.Adjusted = case_when(
        (Size < lower | Size > higher) ~ Size.Sample,
        TRUE ~ as.numeric(Size)),

    Size.Outlier = case_when(
        (Size < lower | Size > higher) ~ "Outlier",
        TRUE ~ "Not Outlier")
    )

df$Size.Adjusted <- df_adjusted$Size.Adjusted

ggplot(df, aes(x=Size.Adjusted)) +
geom_histogram(bins=35, color=outline, fill=area) +
geom_vline(xintercept = mean(df$Size.Adjusted), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Size.Adjusted), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Size.Adjusted), label="Mean", y=400), color="red", angle=90) +
geom_text(aes(x=median(Size.Adjusted), label="Median", y=500), color="blue", angle=90) +
ggtitle("Histogram of Size Adjusted") +
labs(x="Size.Adjusted", y="Frequency")

# Analysis 1-7: Categorize the Size feature into Size.Factor and plot the bar chart
df_no_outlier <- remove_outlier(df, "Size")

mean_no_outlier <- mean(df_no_outlier$Size)

sd_no_outlier <- sd(df_no_outlier$Size)

break_points <- c(-Inf, mean_no_outlier - sd_no_outlier, mean_no_outlier, mean_no_outlier + sd_no_outlier, mean_no_outlier + 2 * sd_no_outlier, Inf)

df <- df %>%
mutate(
    Size.Factor = Size %>%
    cut(breaks=break_points) %>%
    fct_recode(
        "< mean - sd" = "(-Inf,432]",
        "< mean" = "(432,877]",
        "< mean + sd" = "(877,1.32e+03]",
        "< mean + 2sd" = "(1.32e+03,1.77e+03]",
        "Inf" = "(1.77e+03, Inf]"
    )
)

unique(df$Size.Factor)

ggplot(df, aes(x=Size.Factor, fill=Size.Factor)) +
geom_bar() +
ggtitle("Bar Graph of Rent.Factor") +
labs(x="Rent.Factor", y="Frequency")

# Analysis 1-8: Plot the ditribution of Floor
length(unique(df$Floor))

current_floor_hist <- ggplot(df, aes(x=Current.Floor)) +
geom_histogram(bins=40) +
ggtitle("Histogram of Current.Floor") +
geom_vline(xintercept = mean(df$Current.Floor), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Current.Floor), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Current.Floor), label="Mean", y=400), color="red", angle=90) +
geom_text(aes(x=median(Current.Floor), label="Median", y=500), color="blue", angle=90) +
labs(x="Current.Floor", y="Frequency")

total_floor_hist <- ggplot(df, aes(x=Total.Floor)) +
geom_histogram(bins=40) +
ggtitle("Histogram of Current.Floor") +
geom_vline(xintercept = mean(df$Total.Floor), linetype="dashed", color="red") +
geom_vline(xintercept = median(df$Total.Floor), linetype="dashed", color="blue") +
geom_text(aes(x=mean(Total.Floor), label="Mean", y=400), color="red", angle=90) +
geom_text(aes(x=median(Total.Floor), label="Median", y=500), color="blue", angle=90) +
labs(x="Current.Floor", y="Frequency")

grid.arrange(current_floor_hist, total_floor_hist, nrow=2)

# Analysis 1-9: Plot the distribution of Area.Type
ggplot(df, aes(x=Area.Type, fill=Area.Type)) +
geom_bar() +
ggtitle("Bar Chart of Area.Type") +
labs(x="Area.Type", y="Frequency")

# Analysis 1-10: Plot the distribution of Area.Locality
length(unique(df$Area.Locality))

# locs <- tibble(Adress=df$Area.Locality)

# coordinates <- locs %>% geocode(Adress)

# write.table(coordinates, file="./lat-lon.csv")

coordinates <- read.table("./lat-lon.csv", sep=" ", header=TRUE, row.names=1)

df$Lat <- coordinates$lat

df$Long <- coordinates$long

df_coor <- coordinates %>% drop_na()

df_coor_sf <- st_as_sf(df_coor, coords = c("long", "lat"), crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

# Lat_Long Plot 
ggplot(data = world) +
geom_sf() +
geom_sf(data = df_coor_sf, size = 4, shape = 23, fill = "darkred")


# Analysis 1-11: Plot the distribution of Area.Locality in India
# India map, India State, Croped Lat-Long
ind_states <- st_read("./India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")

ind_states <- st_as_sf(ind_states, crs=4326)

india_map <- st_read("./India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_Country_Boundary.shp")

india_map <- st_as_sf(india_map, crs=4326)

crop_factor <- st_bbox(c(xmin=68, xmax=98, ymin=8, ymax=38), crs= st_crs(df_coor_sf))

df_coor_sf_cropped <- st_crop(df_coor_sf, crop_factor)

ggplot() + geom_sf(data=india_map) + geom_sf(data=ind_states, fill=NA) + geom_sf(data=df_coor_sf_cropped , size=4, shape=23, color="darkred")

# Analysis 1-12: Plot the distribution of City
ggplot(df, aes(x=City, fill=City)) +
geom_bar() +
ggtitle("Bar Chart of City") +
labs(x="City", y="Frequency")

df.City.coords <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor)

ggplot(df.City.coords, aes(x=City, fill=City)) +
geom_bar() +
ggtitle("Bar Chart of City") +
labs(x="City", y="Frequency")

# Analysis 1-13: Plot the distribution of Area.Locality in Kolkata

geojson_kolkata <- geojson_read("./City/kolkata.geojson", what="sp")

geojson_kolkata <- tidy(geojson_kolkata)

crop_factor_kolkata <- st_bbox(c(xmin=88.20, xmax=88.5, ymin=22.45, ymax=22.65), crs= st_crs(df_coor_sf))

df_coor_sf_kolkata <- st_crop(df_coor_sf, crop_factor_kolkata)

ggplot() + geom_polygon(data=geojson_kolkata, aes(x=long, y=lat, group=group), fill=area, color="white") + geom_sf(data=df_coor_sf_kolkata)

# Analysis 1-14: Plot the distribution of Area.Locality in Mumbai

geojson_mumbai <- geojson_read("./City/BMC_Wards.geojson", what="sp")

geojson_mumbai <- tidy(geojson_mumbai)

crop_factor_mumbai <- st_bbox(c(xmin=72.75, xmax=73, ymin=18.85, ymax=19.3), crs= st_crs(df_coor_sf))

df_coor_sf_mumbai <- st_crop(df_coor_sf, crop_factor_mumbai)

ggplot() + geom_polygon(data=geojson_mumbai, aes(x=long, y=lat, group=group), fill=area, color="white") + geom_sf(data=df_coor_sf_mumbai)

# Analysis 1-15: Plot the distribution of Area.Locality in Bangalore

geojson_bangalore <- geojson_read("./City/BBMP.geojson", what="sp")

geojson_bangalore <- tidy(geojson_bangalore)

crop_factor_bangalore <- st_bbox(c(xmin=77.5, xmax=77.8, ymin=12.8, ymax=13.15), crs= st_crs(df_coor_sf))

df_coor_sf_bangalore <- st_crop(df_coor_sf, crop_factor_bangalore)

ggplot() + geom_polygon(data=geojson_bangalore, aes(x=long, y=lat, group=group), fill=area, color="white") + geom_sf(data=df_coor_sf_bangalore)

# Analysis 1-16: Plot the distribution of Area.Locality in Delhi
geojson_delhi <- geojson_read("./City/Delhi_Wards.geojson", what="sp")

geojson_delhi <- tidy(geojson_delhi)

crop_factor_delhi <- st_bbox(c(xmin=76.8, xmax=77.4, ymin=28.4, ymax=28.9), crs= st_crs(df_coor_sf))

df_coor_sf_delhi <- st_crop(df_coor_sf, crop_factor_delhi)

ggplot() + geom_polygon(data=geojson_delhi, aes(x=long, y=lat, group=group), fill=area, color="white") + geom_sf(data=df_coor_sf_delhi)

# Analysis 1-17: Plot the distribution of Area.Locality in Chennai
geojson_chennai <- geojson_read("./City/Wards.geojson", what="sp")

geojson_chennai <- tidy(geojson_chennai)

crop_factor_chennai <- st_bbox(c(xmin=80.1, xmax=80.35, ymin=12.8, ymax=13.3), crs= st_crs(df_coor_sf))

df_coor_sf_chennai <- st_crop(df_coor_sf, crop_factor_chennai)

ggplot() + geom_polygon(data=geojson_chennai, aes(x=long, y=lat, group=group), fill=area, color="white")  + geom_sf(data=df_coor_sf_chennai)

# Analysis 1-18: Plot the distribution of Area.Locality in Hyberabad
geojson_hyberabad <- geojson_read("./City/ghmc-wards.geojson", what="sp")

geojson_hyberabad <- tidy(geojson_hyberabad)

crop_factor_hyberabad <- st_bbox(c(xmin=78.25, xmax=78.65, ymin=17.35, ymax=17.6), crs= st_crs(df_coor_sf))

df_coor_sf_hyberabad <- st_crop(df_coor_sf, crop_factor_hyberabad)

ggplot() + geom_polygon(data=geojson_hyberabad, aes(x=long, y=lat, group=group), fill=area, color="white") + geom_sf(data=df_coor_sf_hyberabad)

summary(df)
# Analysis 1-19: Plot the distribution of Furnishing.Status
ggplot(df, aes(x=Furnishing.Status, fill=Furnishing.Status)) +
geom_bar() +
ggtitle("Bar Chart of Furnishing.Status") +
labs(x="Furnishing.Status", y="Frequency")

# Analysis 1-20: Plot the distribution of Tenant.Prefered
ggplot(df, aes(x=Tenant.Preferred, fill=Tenant.Preferred)) +
geom_bar() +
ggtitle("Bar Chart of Tenant.Preferred") +
labs(x="Tenant.Preferred", y="Frequency")

# Analysis 1-21: Plot the distribution of Bathroom
df$Bathroom.String = as.character(df$Bathroom)

ggplot(df, aes(x=Bathroom.String, fill=Bathroom.String)) +
geom_bar() +
ggtitle("Bar Chart of Bathroom.String") +
labs(x="Bathroom.String", y="Frequency")

# Analysis 1-22: Plot the distribution of Point.of.Contact
ggplot(df, aes(x=Point.of.Contact, fill=Point.of.Contact)) +
geom_bar() +
ggtitle("Bar Chart of Point.of.Contact") +
labs(x="Point.of.Contact", y="Frequency")

# Question 2: How does the Rent feature relates with other feature
# Analysis 2-1: Plot the correlation map between Rent and other numerical feature
correlation_map <- cor(df[, c("BHK", "Bathroom", "Size", "Rent")])

ggplot(melt(correlation_map), aes(x=Var1, y=Var2, fill=value )) + geom_tile() + scale_fill_viridis()

# Analysis 2-2: Plot the regression line between Rent and BHK
regression_line.BHK <- ggplot(df, aes(x=BHK, y=Rent)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
regression_line.BHK.Adjusted <- ggplot(df, aes(x=BHK, y=Rent.Adjusted)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
grid.arrange(regression_line.BHK, regression_line.BHK.Adjusted, nrow=2)

# Analysis 2-3: Plot the boxplot between Rent and BHK
box_plot.BHK <- ggplot(df, aes(x=BHK.String, y=Rent)) + geom_boxplot()
box_plot.BHK.Adjusted <- ggplot(df, aes(x=BHK.String, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.BHK, box_plot.BHK.Adjusted, nrow=2)

# Analysis 2-4: Plot the bar plot of Rent's mean and BHK and Rent.Factor
bar_plot.BHK <- ggplot(data=aggregate(Rent ~ BHK.String, data=df, mean), aes(x=BHK.String, y=Rent, fill=BHK.String)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.BHK.Factor <- ggplot(df, aes(x=BHK.String, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.BHK, bar_plot.BHK.Factor, nrow=2)

# Analysis 2-5: Plot the regression line between Rent and Bathroom
regression_line.Bathroom <- ggplot(df, aes(x=Bathroom, y=Rent)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
regression_line.Bathroom.Adjusted <- ggplot(df, aes(x=Bathroom, y=Rent.Adjusted)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
grid.arrange(regression_line.Bathroom, regression_line.Bathroom.Adjusted, nrow=2)

# Analysis 2-6: Plot the boxplot between Rent and Bathroom
box_plot.Bathroom <- ggplot(df, aes(x=Bathroom.String, y=Rent)) + geom_boxplot()
box_plot.Bathroom.Adjusted <- ggplot(df, aes(x=Bathroom.String, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Bathroom, box_plot.Bathroom.Adjusted, nrow=2)

# Analysis 2-7: Plot the bar plot of Rent's mean and Bathroom and Rent.Factor
bar_plot.Bathroom <- ggplot(data=aggregate(Rent ~ Bathroom.String, data=df, mean), aes(x=Bathroom.String, y=Rent, fill=Bathroom.String)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Bathroom.Factor <- ggplot(df, aes(x=Bathroom.String, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Bathroom, bar_plot.Bathroom.Factor, nrow=2)

# Analysis 2-8: Plot the regression line between Rent and Size
regression_line.Size <- ggplot(df, aes(x=Size, y=Rent)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
regression_line.Size.Adjusted <- ggplot(df, aes(x=Size, y=Rent.Adjusted)) + geom_point() + stat_smooth(method="lm", formula= y ~ x, geom="smooth")
grid.arrange(regression_line.Size, regression_line.Size.Adjusted, nrow=2)

# Analysis 2-9: Plot the boxplot between Rent and Size
box_plot.Size <- ggplot(df, aes(x=Size.Factor, y=Rent)) + geom_boxplot()
box_plot.Size.Adjusted <- ggplot(df, aes(x=Size.Factor, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Size, box_plot.Size.Adjusted, nrow=2)

# Analysis 2-10: Plot the bar plot of Rent's mean and Bathroom and Rent.Factor
bar_plot.Size <- ggplot(data=aggregate(Rent ~ Size.Factor, data=df, mean), aes(x=Size.Factor, y=Rent, fill=Size.Factor)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Size.Factor <- ggplot(df, aes(x=Size.Factor, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Size, bar_plot.Size.Factor, nrow=2)

# Analysis 2-11: Plot the boxplot between Rent and Area.Type
box_plot.Area.Type <- ggplot(df, aes(x=Area.Type, y=Rent)) + geom_boxplot()
box_plot.Area.Type.Adjusted <- ggplot(df, aes(x=Area.Type, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Area.Type, box_plot.Area.Type.Adjusted, nrow=2)

# Analysis 2-12: Plot the bar plot of Rent's mean and Area.Type and Rent.Factor
bar_plot.Area.Type <- ggplot(data=aggregate(Rent ~ Area.Type, data=df, mean), aes(x=Area.Type, y=Rent, fill=Area.Type)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Area.Type.Factor <- ggplot(df, aes(x=Area.Type, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Area.Type, bar_plot.Area.Type.Factor, nrow=2)

# Analysis 2-13: Plot the Rent and Area Locality
ggplot() + geom_sf(data=india_map) + geom_sf(data=ind_states) + geom_sf(data=df_coor_sf_cropped , size=4, shape=23, aes(color=df.City.coords$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-14: Plot the Rent in Kolkata
df.City.coords.kolkata <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_kolkata)
ggplot() + geom_polygon(data=geojson_kolkata, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_kolkata, size=4, shape=23, aes(color=df.City.coords.kolkata$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-15: Plot the Rent in Mumbai
df.City.coords.mumbai <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_mumbai)
ggplot() + geom_polygon(data=geojson_mumbai, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_mumbai, size=4, shape=23, aes(color=df.City.coords.mumbai$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-16: Plot the Rent in Bangalore
df.City.coords.bangalore <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_bangalore)
ggplot() + geom_polygon(data=geojson_bangalore, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_bangalore, size=4, shape=23, aes(color=df.City.coords.bangalore$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-17 Plot the Rent in Delhi
df.City.coords.delhi <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_delhi)
ggplot() + geom_polygon(data=geojson_delhi, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_delhi, size=4, shape=23, aes(color=df.City.coords.delhi$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-18 Plot the Rent in Chennai
df.City.coords.chennai <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_chennai)
ggplot() + geom_polygon(data=geojson_chennai, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_chennai, size=4, shape=23, aes(color=df.City.coords.chennai$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-19 Plot the Rent in Hyberabad
df.City.coords.hyberabad <- st_crop(st_as_sf(df %>% drop_na(), coords=c("Long", "Lat"), crs=4326), crop_factor_hyberabad)
ggplot() + geom_polygon(data=geojson_hyberabad, aes(x=long, y=lat, group=group), color="white") + geom_sf(data=df_coor_sf_hyberabad, size=4, shape=23, aes(color=df.City.coords.hyberabad$Rent.Adjusted)) + scale_color_viridis()

# Analysis 2-20: Plot the boxplot between Rent and Furnishisng.Status
box_plot.Furnishing.Status <- ggplot(df, aes(x=Furnishing.Status, y=Rent)) + geom_boxplot()
box_plot.Furinshing.Status.Adjusted <- ggplot(df, aes(x=Furnishing.Status, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Furnishing.Status, box_plot.Furinshing.Status.Adjusted, nrow=2)

# Analysis 2-21: Plot the bar plot of Rent's mean and Furnishing.Status and Rent.Factor
bar_plot.Furnishing.Status <- ggplot(data=aggregate(Rent ~ Furnishing.Status, data=df, mean), aes(x=Furnishing.Status, y=Rent, fill=Furnishing.Status)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Furnishing.Status.Factor <- ggplot(df, aes(x=Furnishing.Status, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Furnishing.Status, bar_plot.Furnishing.Status.Factor, nrow=2)

# Analysis 2-22: Plot the boxplot between Rent and Tenant.Preferred
box_plot.Tenant.Preferred <- ggplot(df, aes(x=Tenant.Preferred, y=Rent)) + geom_boxplot()
box_plot.Tenant.Preferred.Adjusted <- ggplot(df, aes(x=Tenant.Preferred, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Tenant.Preferred, box_plot.Tenant.Preferred.Adjusted, nrow=2)

# Analysis 2-23: Plot the bar plot of Rent's mean and Furnishing.Status and Rent.Factor
bar_plot.Tenant.Preferred <- ggplot(data=aggregate(Rent ~ Tenant.Preferred, data=df, mean), aes(x=Tenant.Preferred, y=Rent, fill=Tenant.Preferred)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Tenant.Preferred.Factor <- ggplot(df, aes(x=Tenant.Preferred, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Tenant.Preferred, bar_plot.Tenant.Preferred.Factor, nrow=2)

# Analysis 2-24: Plot the boxplot between Rent and Point.of.Contact
box_plot.Point.of.Contact <- ggplot(df, aes(x=Point.of.Contact, y=Rent)) + geom_boxplot()
box_plot.Point.of.Contact.Adjusted <- ggplot(df, aes(x=Point.of.Contact, y=Rent.Adjusted)) + geom_boxplot()
grid.arrange(box_plot.Point.of.Contact, box_plot.Point.of.Contact.Adjusted, nrow=2)

# Analysis 2-25: Plot the bar plot of Rent's mean and Point.of.Contact and Rent.Factor
bar_plot.Point.of.Contact <- ggplot(data=aggregate(Rent ~ Point.of.Contact, data=df, mean), aes(x=Point.of.Contact, y=Rent, fill=Point.of.Contact)) +
geom_bar(stat="identity") + 
geom_hline(yintercept = mean(df$Rent)) + 
geom_text(aes(x=1, label="Mean", y=mean(df$Rent)), color="red")

bar_plot.Point.of.Contact.Factor <- ggplot(df, aes(x=Point.of.Contact, fill=Rent.Factor)) + geom_bar(position="dodge")

grid.arrange(bar_plot.Point.of.Contact, bar_plot.Point.of.Contact.Factor, nrow=2)

# Question 3: How can the dataset be clustered
# Analysis 3-1: Perform an PCA analysis
create_pca <- function(df_encode) {
    dmy <- dummyVars("~ .", data=df_encode, fullRank = T)

    df_encode.t <- data.frame(predict(dmy, newdata=df_encode))

    df_encode.t <- df_encode.t %>% drop_na()

    df_encode.pca <- prcomp(df_encode.t, center=TRUE, scale.=TRUE)

    return(df_encode.pca)
}

df_encode <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long))
df.pca <- create_pca(df_encode)

ggbiplot(df.pca)

summary(df.pca)

# Analysis 3-2: Group the PCA using BHK
df_encode.BHK <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, BHK, BHK.String))

df.pca.BHK <- create_pca(df_encode.BHK)

ggbiplot(df.pca.BHK, ellipse = T, groups=df$BHK.String)

# Analysis 3-3: Group the PCA using Bathroom
df_encode.Bathroom <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Bathroom, Bathroom.String))

df.pca.Bathroom <- create_pca(df_encode.Bathroom)
ggbiplot(df.pca.Bathroom, ellipse = T, groups=df$Bathroom.String)

# Analysis 3-4: Group the PCA using Rent.Factor
df_encode.Rent.Factor <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Rent, Rent.Adjusted, Rent.Factor))

df.pca.Rent.Factor <- create_pca(df_encode.Rent.Factor)
ggbiplot(df.pca.Rent.Factor, ellipse = T, groups=df$Rent.Factor)

# Analysis 3-5: Group the PCA using Size.Factor
df_encode.Size.Factor <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Size, Size.Adjusted, Size.Factor))

df.pca.Size.Factor <- create_pca(df_encode.Size.Factor)
ggbiplot(df.pca.Size.Factor, ellipse = T, groups=df$Size.Factor)

# Analysis 3-6: Group the PCA using City
df_encode.City <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, City))

df.pca.City <- create_pca(df_encode.City)
ggbiplot(df.pca.City, ellipse = T, groups=df$City)

# Analysis 3-7: Group the PCA using Area.Type
df_encode.Area.Type <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Area.Type))

df.pca.Area.Type <- create_pca(df_encode.Area.Type)
ggbiplot(df.pca.Area.Type, ellipse = T, groups=df$Area.Type)

# Analysis 3-8: Group the PCA using Point.of.Contact
df_encode.Point.of.Contact <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Point.of.Contact))

df.pca.Point.of.Contact <- create_pca(df_encode.Point.of.Contact)
ggbiplot(df.pca.Point.of.Contact, ellipse = T, groups=df$Point.of.Contact)

# Analysis 3-9: Group the PCA using Rent
df_encode.Rent <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Point.of.Contact, Rent, Rent.Adjusted, Rent.Factor))

df.pca.Rent <- create_pca(df_encode.Rent)

df.pca.Rent <- as.data.frame(df.pca.Rent$x)

biplot.Rent <- ggplot(data=df.pca.Rent, aes(x=PC1, y=PC2)) + geom_point(aes(color=df$Rent.Adjusted)) + scale_color_viridis()
biplot.Rent.facet <- ggplot(data=df.pca.Rent, aes(x=PC1, y=PC2)) + geom_point(aes(color=df$Rent.Adjusted)) + scale_color_viridis() + facet_wrap(~df$Rent.Factor, ncol=3)

grid.arrange(biplot.Rent, biplot.Rent.facet, nrow=2)

# Analysis 3-10: Group the PCA using Size
df_encode.Size <- subset(df, select= -c(Posted.On, Floor, BHK.String, Area.Locality, Bathroom.String, Lat, Long, Point.of.Contact, Size, Size.Adjusted, Size.Factor))

df.pca.Size <- create_pca(df_encode.Size)

df.pca.Size <- as.data.frame(df.pca.Size$x)

biplot.Size <- ggplot(data=df.pca.Size, aes(x=PC1, y=PC2)) + geom_point(aes(color=df$Size.Adjusted)) + scale_color_viridis()
biplot.Size.facet <- ggplot(data=df.pca.Rent, aes(x=PC1, y=PC2)) + geom_point(aes(color=df$Rent.Adjusted)) + scale_color_viridis() + facet_wrap(~df$Size.Factor, ncol=3)

grid.arrange(biplot.Size, biplot.Rent.facet, nrow=2)
