if (!requireNamespace("mnormt", quietly = TRUE)) {
    install.packages("mnormt")
}

if (!requireNamespace("psych", quietly = TRUE)) {
    install.packages("psych")
}

DataSummary <- function(df) {
    print(head(df))
    print(tail(df))
    print(summary(df))
}

Distribution <- function(df) {
    barplot(table(df$shape), main = "Barplot Bentuk Daun", xlab = "Bentuk Daun", ylab = "Frekuensi", col = "orange", border = "black")

    par(mar = c(10, 4, 4, 2))
    barplot(table(df$plant), main = "Barplot Tanaman yang Diobservasi", xlab = "Tanaman", ylab = "Frekuensi", col = "orange", border = "black", las = 2)

    print(table(df$shape))
    print(table(df$plant))
}

HistogramScatterPlot <- function(df, only_numeric=FALSE) {
    library(psych)
    if (only_numeric) {
        columns <- unlist(strsplit("length,width,lw_ratio,veins,lobes,sinus", ","))
        df_selected <- subset(df, select = columns)

        pairs.panels(df_selected, var.labels = columns)
        print(cor(df_selected))
    }else {
        pairs.panels(df)
        print(cor(df))
    }
}

showCorrelation <- function(df, columns = unlist(strsplit("length,width,lw_ratio,veins,lobes,sinus", ","))) {
    library(corrplot)
    df_selected <- df[, columns, drop = FALSE]

    correlation_matrix <- cor(df_selected)

    print(correlation_matrix)

    corrplot(correlation_matrix,
        method = "color",
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        order = "hclust",
        col = colorRampPalette(c("red", "white", "blue"))(100),
        addrect = 2
    )
}

singleHistogram <- function(df, col, plant, color) {
    subset_df <- df[df$plant == plant, ]
    hist(subset_df[, col], col = color, main = paste(plant, "leaf's", col), xlab = col, border = "black")
}

singleBoxplot <- function(df, col, plant, color) {
    df$plant <- factor(df$plant) # Convert 'plant' to factor
    subset_df <- df[df$plant == plant, ]
    boxplot(subset_df[, col] ~ df$plant[df$plant == plant],
        col = color,
        main = paste("leaf's", col), xlab = plant, ylab = col, border = "black", outline=TRUE
    )
}

multipleHistograms <- function(df, cols) {
    unique_plants <- unique(df$plant)

    par(mfrow = c(length(unique_plants), length(cols)))

    for (plant in unique_plants) {
        par(mfrow = c(2, 3))
        for (i in 1:length(cols)) {
            singleBoxplot(df, cols[i], plant, "lightgreen")
        }
        par(mfrow= c(1,1))
    }

    par(mfrow = c(1, 1))
}

piePlot <- function(df, col) {
    if (col == "plant") {
        counts <- table(df$plant)
        labels <- paste0(names(counts), "\n(", counts, ")")
        pie(counts, labels = labels)
    }
    
    if (col == "shape") {
       counts <- table(df$shape)
       labels <- paste0(names(counts), "\n(", counts, ")")
       pie(counts, labels = labels)
    }
}

histOf <- function(df, col) {
    uniques <- unique(df$plant)

    par(mfrow = c(length(uniques), 1), mar = c(2, 4, 1, 1))

    for (i in seq_along(uniques)) {
        plant <- uniques[i]

        subset_df <- df[df$plant == plant, col]

        hist(subset_df, main = paste("Leaf", col, "of" , plant), xlab = col)

        cat("Histogram for", col, "in Plant:", plant, "\n")
        hist_info <- hist(subset_df, plot = FALSE)

        for (j in seq_along(hist_info$breaks)) {
            cat("Bin:", hist_info$breaks[j], "Frequency:", hist_info$counts[j], "\n")
        }
    }

    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

}

histNumericPerPlant <- function(df) {
    numeric_cols <- c("length", "width", "lw_ratio", "veins")
    for (col in numeric_cols) {
        histOf(df, col)
    }
}

boxplotNumericPerPlant <- function(df) {
    numeric_cols <- c("length", "width", "lw_ratio", "veins")

    for (col in numeric_cols) {
        boxplotOf(df, col)
    }
}

boxplotOf <- function(df, col) {
    uniques <- unique(df$plant)

    par(mfrow = c(3, 2), mar = c(2, 4, 1, 1))

    for (i in seq_along(uniques)) {
        plant <- uniques[i]

        subset_df <- df[df$plant == plant, col]

        boxplot(subset_df, main = paste("Boxplot of", col, "for Plant:", plant), lty = "solid", col = "lightblue")
        summary_stats <- boxplot.stats(subset_df)

        cat("Summary stats for ", plant, "'s", col, "\n")
        cat("Median:", median(subset_df), "\n")
        cat("Mode:", as.numeric(names(table(subset_df))[which.max(table(subset_df))]), "\n")
        cat("Mean:", mean(subset_df), "\n")
        cat("Min: ", min(subset_df), "\n")
        cat("Max: ", max(subset_df), "\n")
    }

    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}

df <- read.csv("dataset.csv")
histNumericPerPlant(df)