# Justin Kennard Sulaiman
# TP064042

# TODO: Add 3d graph
# TODO: Don't remove outlier
# TODO: Add barchart of mean
# TODO: Transform rent data into categorical data
# Explain why boxplot is not approrpriate graph
library("viridis")
library("ggplot2")
library("gridExtra")
library("reshape2")
library("GGally")

df <- read.csv("./House_Rent_Dataset.csv")

# Data preperation

head(df)
summary(df)


object_feature <- setdiff(names(df), c("Rent", "Size"))

# Get n levels of each categorical columns
unique_value <- c()
for (column in object_feature) {
   n <- length((unique(df[[column]])))
   unique_value <- c(unique_value, n)
}

df_unique <- data.frame("Features" <- object_feature, "Number of Uniques Value" <- unique_value)

df_unique

# Remove outlier
removeOutlier <- function(df, column, low, high) {
    quartiles <- quantile(df[, column], probs=c(low, high), na.rm = FALSE)
    IQR <- IQR(df[, column])

    Lower <- quartiles[1] - 1.5 * IQR
    High <- quartiles[2] + 1.5 * IQR

    data_no_outlier <- df[(df[, column] > Lower) & (df[, column] < High), ]

    mean_df <- mean(data_no_outlier$Rent)

    df %>% 
    select (Rent) %>%
    mutate (
        Rent.Adjusted = ifelse((Rent > Lower) & (Rent < High), Rent, mean_df)
    )

    return(data_no_outlier)
}

df <- removeOutlier(df, "Rent", 0.25, 0.75)

# Split floor feature
# Remove unnecessary feature

# Question 1: What is the correlation between numerical features
# Analysis 1-1: Calculate the correlation matrix
numerical_features <- c("BHK", "Bathroom", "Size", "Rent")

correlation_matrix <- cor(df[, numerical_features])

# Analysis 1-2: Plot the heatmap for the correlation matrix
correlation_matrix <- melt(correlation_matrix)
ggplot(correlation_matrix, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + scale_fill_viridis(discrete = FALSE)

# Analaysis 1-3: Plot the pair plot
ggpairs(df, columns = numerical_features)

# Question 2: How does the city/location effect the rent
# Analysis 2-1: Plot the distribution of City

value_count <- function(df, column, is_density) {
    df_table <- table(df[[column]])
    column_name = names(df_table)
    value <- as.integer(df_table)

    ifelse(is_density, value <- value / nrow(df), value <- value)

    return(data.frame(VAL1=column_name, VAL2=value))
}

city_df <- value_count(df, "City", TRUE)

ggplot(city_df, aes(x=VAL1, y=VAL2)) + geom_bar(stat="identity") + ggtitle("City Barplot") + labs(y="Density", x="City") + theme_light()

# Analysis 2-2: Remove Outlier and plot the histogram and denisty of rent for each city
rent_hist_city <- ggplot(df, aes(x=Rent, fill=City)) + geom_histogram() + ggtitle("City Rent Distribution") + labs(y="Frequency", x="Rent City") + theme_light()
rent_den_city <- ggplot(df, aes(x=Rent, fill=City)) + geom_density() + ggtitle("Tenant Prefered Rent Distribution") + labs(y="Density", x="Rent City") + theme_light()
grid.arrange(rent_hist_city, rent_den_city, nrow=2)

# Analysis 2-3: Plot the histogram and denisty of rent for Kolkata
kolkata <- df$City == "Kolkata"
kolkota_rent_hist <- ggplot(df[kolkata, ], aes(x=Rent)) + geom_histogram() + ggtitle("Kolkota Rent Distribution") + labs(y="Frequency", x="Rent Kolkota") + theme_light()
kolkota_den <- ggplot(df[kolkata, ], aes(x=Rent)) + geom_density() + ggtitle("Kolkota Rent Distribution") + labs(y="Density", x="Rent Kolkota") + theme_light()
grid.arrange(kolkota_rent_hist, kolkota_den, nrow=2)

# Analysis 2-4: Plot the histogram and denisty of rent for Mumbai
mumbai <- df$City == "Mumbai"
mumbai_rent_hist <- ggplot(df[mumbai, ], aes(x=Rent)) + geom_histogram() + ggtitle("Mumbai Rent Distribution") + labs(y="Frequency", x="Rent Mumbai") + theme_light()
mumbai_den <- ggplot(df[mumbai, ], aes(x=Rent)) + geom_density() + ggtitle("Mumbai Rent Distribution") + labs(y="Density", x="Rent Mumbai") + theme_light()
grid.arrange(mumbai_rent_hist, mumbai_den, nrow=2)

# Analysis 2-5: Plot the histogram and denisty of rent for Delhi
delhi <- df$City == "Delhi"
delhi_rent_hist <- ggplot(df[delhi, ], aes(x=Rent)) + geom_histogram() + ggtitle("Delhi Rent Distribution") + labs(y="Frequency", x="Rent Delhi") + theme_light()
delhi_den <- ggplot(df[delhi, ], aes(x=Rent)) + geom_density() + ggtitle("Delhi Rent Distribution") + labs(y="Density", x="Rent Delhi") + theme_light()
grid.arrange(delhi_rent_hist, delhi_den, nrow=2)

# Analysis 2-6: Plot the histogram and denisty of rent for Chennai
chennai <- df$City == "Chennai"
chennai_rent_hist <- ggplot(df[chennai, ], aes(x=Rent)) + geom_histogram() + ggtitle("Chennai Rent Distribution") + labs(y="Frequency", x="Rent Chennai") + theme_light()
chennai_den <- ggplot(df[chennai, ], aes(x=Rent)) + geom_density() + ggtitle("Chennai Rent Distribution") + labs(y="Density", x="Rent Cehnnai") + theme_light()
grid.arrange(chennai_rent_hist, chennai_den, nrow=2)

# Analysis 2-7: Plot the histogram and denisty of rent for Hyberabad
hyberabad <- df$City == "Hyderabad"
hyberabad_rent_hist <- ggplot(df[hyberabad, ], aes(x=Rent)) + geom_histogram() + ggtitle("Hyberabad Rent Distribution") + labs(y="Frequency", x=" Rent Hyberabad") + theme_light()
hyberabad_den <- ggplot(df[hyberabad, ], aes(x=Rent)) + geom_density() + ggtitle("Hyberabad Rent Distribution") + labs(y="Density", x="Rent Hyberabad") + theme_light()
grid.arrange(hyberabad_rent_hist, hyberabad_den, nrow=2)

# Analysis 2-8: Plot the histogram and denisty of rent for Hyberabad
bangalore <- df$City == "Bangalore"
bangalore_rent_hist <- ggplot(df[bangalore, ], aes(x=Rent)) + geom_histogram() + ggtitle("Bangalore Rent Distribution") + labs(y="Frequency", x=" Rent Banagalore") + theme_light()
bangalore_den <- ggplot(df[bangalore, ], aes(x=Rent)) + geom_density() + ggtitle("Bangalore Rent Distribution") + labs(y="Density", x="Rent Bangalore") + theme_light()
grid.arrange(bangalore_rent_hist, bangalore_den, nrow=2)

grid.arrange(rent_hist_city, rent_den_city, kolkota_rent_hist, kolkota_den, mumbai_rent_hist, mumbai_den, delhi_rent_hist, delhi_den, chennai_rent_hist, chennai_den, hyberabad_rent_hist, hyberabad_den, bangalore_rent_hist, bangalore_den, nrow=7, ncol=2)
# GDP and GDP per pacipta of each city

# Analysis 2-9: Plot the boxplot of city (Same as before just give diffrent perspective)
ggplot(df, aes(x=City, y=Rent)) + geom_violin() + ggtitle("Boxplot of City and Rent Price") + labs(y="Rent Price", x="City") + theme_light()

# Analysis 2-10: Plot bar mean of each city

data <- df

data$City[!data$City %in% c("Kolkata", "Mumbai")] <- "Other"

# Question 3: How does rent prices vary between tenant prefered
# Analysis 3-1: Univariate analysis of rent price and tenant prefered
rent_hist <- ggplot(df, aes(x=Rent)) + geom_histogram() + ggtitle("Rent Histogram") + labs(y="Frequency", x="Rent") + theme_light()

tenant_df <- value_count(df, "Tenant.Preferred", TRUE)

tenant_barplot <- ggplot(tenant_df, aes(x=VAL1, y=VAL2)) + geom_bar(stat="identity") + ggtitle("Tenant Barplot") + labs(y="Density", x="Tenant Prefered") + theme_light()

grid.arrange(rent_hist, tenant_barplot, nrow=2)

# Analysis 3-2: Plot the histogram and density of rent for each tenant prefered
bachelor <- df$Tenant.Preferred == "Family"
family <- df$Tenant.Preferred == "Bachelors"
bachelor_family <- df$Tenant.Preferred == "Bachelors/Family"

rent_hist_tenant <- ggplot(df, aes(x=Rent, fill=Tenant.Preferred)) + geom_histogram() + ggtitle("Tenant Prefered Rent Distribution") + labs(y="Frequency", x="Rent Tenant Prefered") + theme_light()
rent_den_tenant <- ggplot(df, aes(x=Rent, fill=Tenant.Preferred)) + geom_density() + ggtitle("Tenant Prefered Rent Distribution") + labs(y="Density", x="Rent Tenant Prefered") + theme_light()
grid.arrange(rent_hist, rent_den, nrow=2)

# Analysis 3-3: Plot the historgram and density of rent for bachelors
bachelor_rent_hist <- ggplot(df[bachelor, ], aes(x=Rent)) + geom_histogram() + ggtitle("Bachelor Rent Distribution") + labs(y="Frequency", x="Rent Bachelor") + theme_light()
bachelor_rent_den <- ggplot(df[bachelor, ], aes(x=Rent)) + geom_density() + ggtitle("Bachelor Rent Distribution") + labs(y="Density", x="Rent Bachelor") + theme_light()
grid.arrange(bachelor_rent_hist, bachelor_rent_den, nrow=2)

# Analysis 3-4: Plot the histogram and density of rent for family
family_rent_hist <- ggplot(df[family, ], aes(x=Rent)) + geom_histogram() + ggtitle("Family Rent Distribution") + labs(y="Frequency", x="Rent Bachelor and Family") + theme_light()
family_rent_den <- ggplot(df[family, ], aes(x=Rent)) + geom_density() + ggtitle("Family Rent Distribution") + labs(y="Density", x="Rent Bachelor and Family") + theme_light()
grid.arrange(family_rent_hist, family_rent_den, nrow=2)

# Analysis 3-5: Plot the histogram and density of rent for bachelor/family
bachelor_family_rent_hist <- ggplot(df[bachelor_family, ], aes(x=Rent)) + geom_histogram() + ggtitle("Bachelor and Family Rent Distribution") + labs(y="Frequency", x="Rent Family") + theme_light()
bachelor_family_rent_den <- ggplot(df[bachelor_family, ], aes(x=Rent)) + geom_density() + ggtitle("Bachelor and Family Rent Distribution") + labs(y="Density", x="Rent Family") + theme_light()
grid.arrange(bachelor_family_rent_hist, bachelor_family_rent_den, nrow=2)

grid.arrange(rent_hist_tenant, rent_den_tenant, bachelor_rent_hist, bachelor_rent_den, family_rent_hist, family_rent_den, bachelor_family_rent_hist, bachelor_family_rent_den, nrow=4, ncol=2)

# Analysis 3-6: Plot a boxplot for each tenant prefered
ggplot(df, aes(x=Tenant.Preferred, y=Rent)) + geom_boxplot() + ggtitle("Boxplot of Tenants Prefered and Rent Price") + labs(y="Rent Price", x="Tenants Prefered") + theme_light()


# Question 4: How does rent prices vary between furnishing status
# Analysis 4-1: Univariate analysis for furnishing status
# df <- read.csv("./House_Rent_Dataset.csv")

furnishing_df <- value_count(df, "Furnishing.Status", TRUE)

ggplot(furnishing_df, aes(x=VAL1, y=VAL2)) + geom_bar(stat="identity") + ggtitle("Furnishing Barplot") + labs(y="Density", x="Furnishing Status") + theme_light()

# Analysis 4-2: Plot histogram and denisty of rent for each furnishing type
unfurnished <- df$Furnishing.Status == "Unfurnished"
semi_furnished <- df$Furnishing.Status == "Semi-Furnished"
furnished <- df$Furnishing.Status == "Furnished"

rent_hist_furnish <- ggplot(df, aes(x=Rent, fill=Furnishing.Status)) + geom_histogram() + ggtitle("Furnishing Status Rent Distribution") + labs(y="Frequency", x="Furnishing Status Prefered") + theme_light()
rent_den_furnish <- ggplot(df, aes(x=Rent, fill=Furnishing.Status)) + geom_density() + ggtitle("Furnishing Status Rent Distribution") + labs(y="Density", x="Furnishing Status Prefered") + theme_light()

grid.arrange(rent_hist, rent_den, nrow=2)
# Analysis 4-3: Plot histogram and density of rent for unfurnished
unfurnished_rent_hist <- ggplot(df[unfurnished, ], aes(x=Rent)) + geom_histogram() + ggtitle("Unfurnished Rent Distribution") + labs(y="Frequency", x="Rent Unfurnished") + theme_light()
unfurnished_rent_den <- ggplot(df[unfurnished, ], aes(x=Rent)) + geom_density() + ggtitle("Unfurnished Rent Distribution") + labs(y="Density", x="Rent Unfurnished") + theme_light()

grid.arrange(unfurnished_rent_hist, unfurnished_rent_den, nrow=2)
# Analysis 4-4: Plot histogram and density of rent for semi furnished
semi_unfurnished_rent_hist <- ggplot(df[semi_furnished, ], aes(x=Rent)) + geom_histogram() + ggtitle("Semi-Furnished Distribution") + labs(y="Frequency", x="Rent Semi-Furnished") + theme_light()
semi_furnished_rent_den <- ggplot(df[semi_furnished, ], aes(x=Rent)) + geom_density() + ggtitle("Semi-Furnished Rent Distribution") + labs(y="Density", x="Rent Semi-Furnished") + theme_light()

grid.arrange(semi_unfurnished_rent_hist, semi_furnished_rent_den, nrow=2)
# Analysis 4-5 Plot the histogram and denisty of rent for furnished
furnished_rent_hist <- ggplot(df[furnished, ], aes(x=Rent)) + geom_histogram() + ggtitle("Furnished Rent Distribution") + labs(y="Frequency", x="Rent Furnished") + theme_light()
furnished_rent_den <- ggplot(df[furnished, ], aes(x=Rent)) + geom_density() + ggtitle("Furnished Rent Distribution") + labs(y="Density", x="Rent Furnished") + theme_light()

grid.arrange(furnished_rent_hist, furnished_rent_den, nrow=2)
# Analysis 4-6: Combine the plot for each furnished
grid.arrange(rent_hist_furnish, rent_den_furnish, unfurnished_rent_hist, unfurnished_rent_den, semi_unfurnished_rent_hist, semi_furnished_rent_den, furnished_rent_hist, furnished_rent_den, nrow=4, ncol=2)

# Analysis 4-7: Plot a boxplot for each furnishing type
ggplot(df, aes(x=Furnishing.Status, y=Rent)) + geom_boxplot() + ggtitle("Boxplot of Furnishing Status and Rent Price") + labs(y="Rent Price", x="Furnishing Status") + theme_light()
# Unfurnished small variance, semi-furnished medium variance, furnished high variance

# Question 5: Why is the rent for furnished house has a large range of value
# Analysis 5-1: Plot the BHK for furnished status
# Analysis 5-2: Plot the bathroom for furnihsed status
# Analysis 5-3: Plot the size for furnished status
# Question 3: How does rent prices vary between bhk

# average, median, min, max, rent for each bhk
# average, median, min, max, rent for each tenant prefered
# Rent price on loation
# What house does bachelors prefered
# What house does family prefered
# Correlation matrix
# location and price of house, tenant prefered