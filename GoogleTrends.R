# Libraries
library(readr)
library(ggplot2)


# Path management
getfrom <- "//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Google_Trends"

# Import data

multiTimeline <- read_csv("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Google_Trends/multiTimeline.csv", 
                          skip = 1)
geoMap <- read_csv("//smb.uni-oldenburg.de/fk2angmikro/User/Erdmann/AirOld/data/external/Google_Trends/geoMap.csv", 
                   skip = 1)
# transform to wide
multiwide <- multiTimeline %>%
  pivot_longer(cols = c(),names_to = "Topic", values_to = "Index" )

# Plot
ggplot(data = multiwide, mapping = aes(x = Woche, y = Index, group = Topic, color = Topic)) +
  geom_line() + theme_minimalist() + labs(x = "Week", y = "Index")



