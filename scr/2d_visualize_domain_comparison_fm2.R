# Face Morph 3 Graphs (Domain Comparisons)
# 11.5.21 JMS

# Load libraries and functions
library(plyr); library(dplyr); library(forcats); library(ggplot2); library(wesanderson)
source('scr/SummarySE2.R')

# Load data
d1 <- read.csv('data/study_2/fm2_all_faces_ratings.csv', header = TRUE)

# Rename columns
d1 <- dplyr::rename(d1, c('rating_type' = 'domain', 'magnitude' = 'level'))

# Create new domain column
d1 <- d1 %>% dplyr::mutate(domain = case_when(emotion == 'win' | emotion == 'lose' ~ 'monetary',
                                              emotion == 'h' | emotion == 'a' | emotion == 's' ~ 'social'))

# Remove variable column (not necessary)
d1 <- select(d1, -variable)

# Run summary function on d1
d2 <- summarySE2(data = d1, measurevar = 'rating', groupvars = c('subnum', 'age', 'emotion', 'magnitude', 'rating_type', 'domain'), na.rm = TRUE)

# Make age groups and convert to factor
d2$agegrp <- ntile(d2$age, 3)
d2$agegrp <- factor(d2$agegrp, levels = c(1, 2, 3), labels = c('Younger', 'Middle Age', 'Older'))

# Convert emotion to factor and define levels
d2$emotion <- factor(d2$emotion)
d2$emotion <- fct_collapse(d2$emotion, positive = c('h', 'win'), negative = c('a', 's', 'lose'))

# Convert magnitude to ordered factor
d2$magnitude <- factor(d2$magnitude, ordered = TRUE, levels = c('low', 'med', 'full'))

# Run summary function on d2
d3 <- summarySE2(data = d2, measurevar = 'rating', groupvars = c('agegrp', 'emotion', 'magnitude', 'rating_type', 'domain'), na.rm = TRUE)

# Separate by arousal and valence
arsl <- filter(d3, rating_type == 'arsl')
vln <- filter(d3, rating_type == 'vln')

# Arousal graph
arsl_plot <- ggplot(arsl, aes(x = agegrp, y = rating, fill = magnitude)) +
  geom_bar(stat = 'identity', position = 'dodge')

arsl_grid <- arsl_plot + facet_grid(emotion ~ domain, scales = 'free', space = 'free') +
  geom_errorbar(aes(ymin = rating - se, ymax = rating + se), size = 0.5, width = 0.25, position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(1,7)) + scale_y_continuous(breaks = seq(1, 7, 1)) +
  theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face = "bold", size = 18), axis.text.x  = element_text(size = 12)) +
  theme(text = element_text(size = 18)) + scale_fill_manual(values = wes_palette("GrandBudapest1")) + xlab("Age Group") + ylab("Arousal Rating")
arsl_grid

# Save plot
ggsave('plots/fm2_soc_mon_arsl_comparison.png', width = 6, height = 7, units = "in", bg = "transparent")

# Valence graph
vln_plot <- ggplot(vln, aes(x = agegrp, y = rating, fill = magnitude)) +
  geom_bar(stat = 'identity', position = 'dodge')

vln_grid <- vln_plot + facet_grid(emotion ~ domain, scales = 'free', space = 'free') +
  geom_errorbar(aes(ymin = rating - se, ymax = rating + se), size = 0.5, width = 0.25, position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(1,7)) + scale_y_continuous(breaks = seq(1, 7, 1)) +
  theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face = "bold", size = 18), axis.text.x  = element_text(size = 12)) +
  theme(text = element_text(size = 18)) + scale_fill_manual(values = wes_palette("GrandBudapest1")) + xlab("Age Group") + ylab("Valence Rating")
vln_grid

# Save plot
ggsave('plots/fm2_soc_mon_vln_comparison.png', width = 6, height = 7, units = "in", bg = "transparent")
