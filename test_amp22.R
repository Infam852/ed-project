# extract world data
world <- ne_countries(scale = "medium", returnclass = "sf")
world$country = as.factor(world$iso_a3)
joined <- left_join(x=world, y=visit_per_country, by="country")
joined$n<- replace_na(joined$n, 0)


breaks_seq <- seq(0, 10000, by=2000)
# create cut off points in Population
joined$nC <- base::cut(joined$n,
                       breaks = breaks_seq, 
                       labels = 1:(length(breaks_seq)-1), right = F, ordered_result = T)
theme_set(theme_bw())
# load package for colors
library(RColorBrewer)
# define colors
palette = colorRampPalette(brewer.pal(n=7, name='Oranges'))(7)
palette = c("white", palette)
# create map
ggplot() +
  geom_sf(data = world, aes(fill = joined$nC)) +
  scale_fill_manual(values = palette) +
  # customize legend title
  labs(fill = "Population Size") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # surpress legend
        legend.position = "none")
# display map
worldpopmap