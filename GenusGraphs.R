library(bio2020)
library(dplyr)
library(janitor)

#import data
csd <- read.csv("Coralspawning.csv")

#filter for only necessary columns
filtcsd <- csd[1:6178, c(9, 21, 24)]

#clean situation and genus columns
filtcsd$Situation <- gsub("[^A-Za-z]", "", filtcsd$Situation)
filtcsd$Genus <- gsub("[^A-Za-z]", "", filtcsd$Genus)

#ensure spawning values are numeric
filtcsd$DoSRtNFM <- as.numeric(as.character(filtcsd$DoSRtNFM))

#create summary table for genus/ situation
g_result <- filtcsd %>%
  group_by(Genus, Situation) %>%
  summarise(count = n()) %>%
  spread(key = Situation, value = count, fill = 0)

#remove 0s
g_result <- g_result[apply(g_result[, c("Exsitu", "Insitu")], 1, function(x) all(x != 0)), ]

#over 6 observations (genus/ situation)
u_g <- g_result[g_result$Exsitu >= 6 & g_result$Insitu >= 6, ]

#count spawning values per situation per genus
vsg <- filtcsd %>%
  group_by(Genus, Situation, DoSRtNFM) %>%
  summarise(Count = n(), .groups = "drop")

#filter vsg based on u_g
hist <- vsg %>%
  semi_join(u_g, by = "Genus")

#check results
unique(hist$Genus)
unique(u_g$Genus)

# plot for each genus
genera_list <- unique(hist$Genus)

# create and print plots in a loop
for (i in seq_along(genera_list)) {
  genus <- genera_list[i]
  
  subset_data <- subset(hist, Genus == genus)
  
  plot <- ggplot(subset_data, aes(x = DoSRtNFM, y = Count, fill = Situation)) +
    geom_bar(stat = "identity", position = "identity", colour = "black", linewidth = 0.4) +
    scale_fill_manual(values = c("Exsitu" = "lightskyblue3", "Insitu" = alpha("wheat2", 0.5)),
                      labels = c("Ex situ", "In situ")) +
    scale_x_continuous(
      breaks = c(-15, -10, -5, 0, 5, 10, 15),  
      labels = c(-15, -10, -5, 0, 5, 10, 15),
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.y = element_line(color = "black", linewidth = 0.3),
      axis.ticks.y = element_line(color = "black", linewidth = 0.3),
      axis.text.y = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black", linewidth = 0.3),
      axis.title.x = element_text(color = "black"),
      axis.title.y = element_text(color = "black")
    ) +
    geom_segment(aes(x = -15, xend = 15, y = 0, yend = 0), color = "black", linewidth = 0.3) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Day of Spawning Relative to Nearest Full Moon (DoSRtNFM)", y = "Frequency", title = genus) +
    guides(fill = guide_legend(title = NULL))
  
#assign and the plot
  assign(paste0("P", i), plot)
  print(plot)
}         
