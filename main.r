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

Distribusi <- function(df) {
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

showCorrelation <- function(df, columns=unlist(strsplit("length,width,lw_ratio,veins,lobes,sinus", ","))) {
    df_selected <- df[, columns, drop = FALSE]
    
    correlation_matrix <- cor(df_selected)

    print(correlation_matrix)

    heatmap(correlation_matrix,
        xlab = "Columns",
        ylab = "Columns",
        main = "Correlation Matrix",
        col = colorRampPalette(c("red", "white", "blue"))(100),
        symm = TRUE
    )
}

df <- read.csv("dataset.csv")
HistogramScatterPlot(df, only_numeric = TRUE)
