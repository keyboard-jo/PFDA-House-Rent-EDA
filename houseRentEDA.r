# Load data
df <- read.csv("./House_Rent_Dataset.csv", header = TRUE)

# Get snapshot of data
head(df)
tail(df)

# Get summary of each columns
summary(df)

# Get size of data
dim(df)

# Check data type of each columns
str(df)

# Group columns with different data type
numerical_feature <- c("BHK", "Rent", "Size", "Bathroom")

object_feature <- setdiff(names(df), numerical_feature)

# Get n levels of each categorical columns
unique_value <- c()
for (column in object_feature) {
   n <- length((unique(df[[column]])))
   unique_value <- c(unique_value, n)
}

df_unique <- data.frame("Features" <- object_feature, "Number of Uniques Value" <- unique_value)

df_unique

# Plot histogram of each numerical value
pdf("distribution_plot_numerical_feature.pdf")
par(mfrow=c(length(numerical_feature), 2))
for (column in numerical_feature) {
    hist(df[[column]], main=paste("Histogram of ", column), xlab=column)
    d <- density(df[[column]])
    plot(d, main=paste("Density plot of ", column), xlab=column)
    polygon(d, col="red", border = "blue")
}
dev.off()

# Correlation matrix
cor_matrix <- cor(df[, numerical_feature])
round(cor_matrix, 2)

# Tenants prefered and numerical feature
double_features <- c("Rent", "Size")
pdf("distribution_plot_tenants.pdf")
par(mfrow=c(length(numerical_feature), 2))
for (column in numerical_feature) {
    hist(df[df$Tenant.Preferred == "Bachelors/Family", column], main=paste("Histogram of Bachelors/Family", column, sep=" "), xlab=column, col="blue", freq=FALSE, breaks=15)
    hist(df[df$Tenant.Preferred == "Bachelors", column], main=paste("Histogram of Bachelors", column, sep=" "), xlab=column, col="red", freq=FALSE, breaks=15)
}
dev.off()
# Rent should be added
# Size shoudl be seperated

df[df$Tenant.Preferred == "Bachelors", "Rent"]
