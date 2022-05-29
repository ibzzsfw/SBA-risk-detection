library(fmsb)

network <- data.frame(
  row.names = c("Ladder", "Cross I&II", "Cross III", "glm ~top4", "glm ~Term"),
  Accuracy = c(0.8418, 0.8767, 0.9205, 0.8336, 0.8282),
  Kappa = c(0.4685, 0.6285, 0.7726, 0.4426, 0.5169),
  Precision = c(0.8653, 0.9148, 0.9607, 0.8612, 0.9058),
  Recall = c(0.9454, 0.9292, 0.9369, 0.9392, 0.8715),
  F1 = c(0.9036, 0.9220, 0.9486, 0.8985, 0.8883),
  DetectionRate = c(0.7411, 0.7284, 0.7344, 0.7362, 0.6832),
  DetectionPrevalence = c(0.8564, 0.7962, 0.7644, 0.8549, 0.7543),
  BalancedAccuracy = c(0.7058, 0.8077, 0.8989, 0.6950, 0.7713)
  )

range <- c(1, 0)
max_min <- data.frame(
  Accuracy = range, Kappa = range, Precision = range,
  Recall = range, F1 = range, DetectionRate = range,
  DetectionPrevalence = range, BalancedAccuracy = range
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, network)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, .25, .5, .75, 1),
  color = c("#00AFBB", "#E7B800", "#FC4E07", "#CF9FFF", "#ABEBC6")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07", "#CF9FFF", "#ABEBC6"),
  text.col = "black", cex = 1, pt.cex = 1.5
)