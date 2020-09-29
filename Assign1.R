# ID: 1054226
# Name: Shuyu LI
# Data source: Real Clear Politics
# Reference: R for Data Science, url: https://r4ds.had.co.nz/
#            BBC R Cookbook, url: https://bbc.github.io/rcookbook/
#            usmap Documentation, url: https://www.rdocumentation.org/packages/usmap/versions/0.5.0


# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load pkgs
requiredpackages <- c('ggplot2', 'tidyverse', 'ggrepel', 'usmap', 'stringr', 'ggpubr')
for (pkg in requiredpackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {library(pkg, character.only = TRUE)}
}

# load data
poll_summary <- read_csv('poll_summary.csv')
poll <- read_csv('poll.csv')

# palette
palette_attitude <- c("#1055b6", "#67b5e2", "#cccccc","#ffaca3","#ed4748")
background_color <- "#fdf1e5"
segment_color <-"#434343"
source_color <- "#434343"

################ plot 1 ################

# factorize attitude to maintain the legend order
attitude <- poll_summary$attitude
n_votes <- poll_summary$n_votes
poll_summary$attitude <- factor(rev(attitude), levels = attitude)

# basic stacked bar plot
plot1_0 <- ggplot(data = poll_summary) + 
  geom_bar(stat = 'identity', mapping = aes(x = group, 
                                            y = n_votes, fill = attitude), width = 0.12) + 
  geom_text(aes(label = n_votes, x = group,  y = n_votes, group = attitude), 
            size = 5, position = position_stack(vjust = .5), colour = 'white', family="Courier") + 
  scale_fill_manual(values=rev(palette_attitude)) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE), size = 7) +
  coord_flip()

# theme setting
plot1_1 <- plot1_0 + 
  scale_x_discrete(expand = expansion(add = c(0.35, 0.55))) +
  scale_y_continuous(limits = c(0, 538), expand = expansion(mult = c(0, 0))) +
  theme(plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color),
        legend.background = element_rect(fill = background_color),
        plot.margin = margin(10, 10, 0, 10),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(lineheight= 1, face="bold", size = 12),
        plot.subtitle = element_text(lineheight=.8, size = 10),
        legend.title = element_blank(),
        legend.position = 'top')

# BBC-style cut-off
plot1_2 <- plot1_1 +
  geom_segment(aes(x = 0.94, xend = 1.06, y = sum(n_votes)/2, yend = sum(n_votes)/2), 
               linetype = 'dashed', colour = segment_color) + 
  geom_point(aes(x = 0.91, y = 269), shape = 17, size = 2.5) + 
  geom_text(label = "270 to win", x = 1.095, y = 270, 
            size = 4, colour = segment_color, family="Courier")

# add candidate information
plot1_3 <- plot1_2 + 
  geom_text(label = "298", x = 1.11, y = 0, size = 9, color = palette_attitude[1], hjust = 0.05, family="Courier") + 
  geom_text(label = "119", x = 1.11, y = sum(n_votes), size = 9, color = palette_attitude[5], hjust = 0.95, family="Courier") +
  geom_text(label = "Joe Biden", x = 1.13, y = 40, size = 4, color = 'black', hjust = -0.3, fontface = 'bold') +
  geom_text(label = "DEMOCRAT", x = 1.085, y = 40, size = 2.5, color = 'black', hjust = -0.4) + 
  geom_text(label = "Donald Trump", x = 1.13, y = sum(n_votes) - 40, size = 4, color = 'black', hjust = 1.21, fontface = 'bold') +
  geom_text(label = "REPUBLICAN", x = 1.085, y = sum(n_votes) - 40, size = 2.5, color = 'black', hjust = 1.35)

# add note 
plot1_4 <- plot1_3 + 
  geom_text(label = str_wrap('States where the difference between the two candidates is more than 10 percentage points are classified as \'Solid\'; between 5 to 10 percentage are classfied as \'Leaning\'; less than 5 percentage are classfied as \'Toss-up\'.', 
                             width = 155), 
            x = 0.85, y = 0, size = 2.5, hjust = 0)

# add titles
plot1_5 <- plot1_4 + 
  ggtitle('The Path to Victory is Getting Narrower for Trump') + 
  labs(subtitle = "2020 U.S. Election Outcome Suggested by Polls up to Aug 13, 2020")

# custom source caption
plot1_6 <- plot1_5 + geom_vline(xintercept = 0.78, size = 0.5, color = 'black') +
  geom_text(label = "Source: Real Clear Politics", x = 0.74, y = 0, 
            size = 2.5, colour = source_color, hjust = 0)
################ plot 1 End ################

################ plot 2 ################

# factorize Attitude to maintain the legend order
poll$fips <-fips(poll$State)
poll$Attitude <- factor(poll$Attitude, levels = attitude)

# load state centroid data and join 'poll' and mutate labels
centroid_labels <- utils::read.csv(system.file("extdata", paste0("us_", "states", "_centroids.csv"), package = "usmap"), stringsAsFactors = FALSE)
poll_with_centroid_coord <- poll %>% 
  left_join(centroid_labels, by = c('Code' = 'abbr')) %>% 
  mutate(label = paste(Code, n_votes, sep = ', '))

# complete us map having Alaska and Hawaii moved to a convenient spot
plot2_0 <- plot_usmap(data = poll, values = "Attitude",  color = 'white', labels=FALSE) + 
  scale_fill_manual(values=palette_attitude) + guides(fill = FALSE)

# theme setting
plot2_1 <- plot2_0 +
  theme(plot.background = element_rect(color = NA,fill = background_color),
        panel.background = element_rect(color = NA, fill = background_color),
        plot.title = element_text(lineheight = 3, face="bold", size = 12),
        plot.subtitle = element_text(lineheight = .8, size = 10),
        plot.margin = margin(10, 10, 10, 10))

# add labels 
# 
plot2_2 <- plot2_1 + 
  ggrepel::geom_label_repel(data = poll_with_centroid_coord, 
                            aes(x = x, y = y, label = label), alpha = 0.7, 
                            label.r=unit(0.5, 'lines'), label.size = 0.3, 
                            segment.size = 0.5, segment.alpha = 0.8, seed = 123)

# add titles
plot2_3 <- plot2_2 +
  labs(title = 'Republicans Are Losing Their Grounds', 
       subtitle = '2020 U.S. Electoral College Map Suggested by Polls up to Aug 13, 2020')

# add source information 
plot2_4 <- plot2_3 +
  geom_hline(yintercept = -2.5e+6, size = 0.5, color = 'black') +
  geom_text(label = "Source: Real Clear Politics", x = -2.1e+6, y = -2.6e+6,
            size = 2.5, colour = source_color, hjust = 0.185) 

################ plot 2 End ################

# output
maingraph <- ggarrange(plot1_6, plot2_4, ncol = 1, nrow = 2, align = 'v', heights = c(1,1.2))
ggexport(maingraph, filename = 'output.pdf', width = 7, height = 10)