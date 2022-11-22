library("viridis")
library("ggplot2")
library("gridExtra")
library("reshape2")
library("GGally")
library("tidyverse")
library("Hmisc")
library("factoextra")

df <- read.csv("./House_Rent_Dataset.csv")

# Get overview
head(df)
summary(df)

# Get n levels of each categorical columns
unique_value <- c()
for (column in object_feature) {
   n <- length((unique(df[[column]])))
   unique_value <- c(unique_value, n)
}

object_feature <- setdiff(names(df), c("Rent", "Size"))
df_unique <- data.frame("Features" <- object_feature, "Number of Uniques Value" <- unique_value)

df_unique

unique(df$Rent.factor)

# Question 1: What is the distribution of the rent price and how it relates with other features
# Analysis 1-1: Plot the distribution of the rent
rent_dist <- ggplot(df, aes(x=Rent)) + geom_histogram(color="black", fill="white")
rent_den <- ggplot(df, aes(x=Rent)) + geom_density(fill="#FF6666")

grid.arrange(rent_dist, rent_den, nrow=2)

# Analysis 1-2: Categorize rent into quantiles and plot bar graph

df <- df %>% mutate (
    Rent.factor = 
    Rent %>% 
    Hmisc::cut2(g=8) %>%
    fct_recode(
        "12.5%" = "[ 1200,   7600)",
        "25%" = "[ 7600,  10500)",
        "37.5%" = "[10500,  13500)",
        "50%" = "[13500,  16500)",
        "62.5%" = "[16500,  22500)",
        "75%" = "[22500,  33003)",
        "87.5%" = "[33003,  61000)",
        "100%" = "[61000,3500000]",
    )

)
ggplot(df, aes(x=Rent.factor)) + geom_bar()


summary(df)

object_feature <- setdiff(names(df), c("Rent", "Size", "BHK", "Bathroom", "Floor", "Area.Locality"))

newdata <- dcast(data = melt(df[, object_feature], id.vars = "Posted.On"), Posted.On ~ variable + value, length)

newdata <- cbind(newdata, df[, c("Rent", "Size", "BHK", "Bathroom")])

png(file = "KMeansExample.png")

km <- kmeans(newdata, centers=8, nstart=100)

fviz_cluster(km, data = newdata)

dev.off()