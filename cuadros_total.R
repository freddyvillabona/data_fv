
library(treemapify)
h <-G20



data <- read_rds("data1.rds", refhook = NULL)
data$date <- as.Date(data$date)
data[is.na(data)] <- 0
g <- select(data, continent, location, total_cases)




g <-  g %>%
   group_by(continent, location) %>%
  summarise(total_cases = sum(total_cases))





g <- na.omit(g)

g <- g[- (1:13),]


g <- g[order(-g$total_cases), ]
g <- head(g,50)
ggplot(g, aes(area = total_cases, 
                fill = continent, 
                label = location, 
                subgroup = continent)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
#  facet_wrap( ~ hemisphere) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    title = "50 países con mayor número de casos",
    caption = "data",
    fill = "Region"
  )


ggplot(G20, aes(area = gdp_mil_usd, fill = region, label = country, subgroup = region)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  facet_wrap( ~ hemisphere) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    title = "The G-20 major economies by hemisphere",
    caption = "The area of each tile represents the country's GDP as a
      proportion of all countries in that hemisphere",
    fill = "Region"
  )
