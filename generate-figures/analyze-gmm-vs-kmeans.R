library(ggplot2)
library(tidyverse)
library(readr)
library(colorspace)

#### RUN ALL THE CODE BELOW TO REPRODUCE FIGURES 11 AND 12 ####

#### Steps to reproduce Figure 11 ####

data100 <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-benchmark-100percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 100)
data200 <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-benchmark-200percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 200)
data1000 <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-benchmark-1000percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 1000)
data_agg <- full_join(data100, data200) %>% full_join(data1000)

data_long <- data_agg %>%
  pivot_longer(cols=c('kmeans','gauss'), values_to='AMI', names_to='method') %>%
  mutate('method' = fct_recode(method,
                                  "K-Means"="kmeans",
                                  "GMM"="gauss",
                                  )) %>%
  rename('Method'='method') %>%
  mutate('archetype' = fct_reorder(.f = archetype, .x = AMI,
                                   .fun = median, .desc=T)) %>%
  rename('Archetype'='archetype') %>%
  mutate('Archetype' = fct_recode(Archetype,
                           "Normal, Easy"="normal_easy",
                           "Normal, Highly Variable"="normal_highlyvariable",
                           "Normal, High-Dimensional"="highdim_shallow",
                           "Non-Normal, High Overlap"="expon_highoverlap",
                           "Heavy Tails, Low Overlap"="heavytailed",
                          )) %>%
  rename('Performance'='AMI')
  
data <- data_agg %>%
  mutate('performance_diff' = gauss-kmeans) %>%
  mutate('archetype' = fct_reorder(.f = archetype, .x = performance_diff,
                                   .fun = median, .desc=T)) %>%
  rename('Archetype'='archetype') %>%
  mutate('Archetype' = fct_recode(Archetype,
                                  "Normal, Easy"="normal_easy",
                                  "Normal, Highly Variable"="normal_highlyvariable",
                                  "Normal, High-Dimensional"="highdim_shallow",
                                  "Non-Normal, High Overlap"="expon_highoverlap",
                                  "Heavy Tails, Low Overlap"="heavytailed",
  )) %>%
  rename('Performance Gap' = performance_diff) %>%
  mutate('# data points per cluster' = factor(samples_per_cluster,ordered=TRUE))
  
cols <- c("100" = "lightgray", "200" = "gray", "1000" = "darkgray")

diff_color <- data %>%
  group_by(Archetype,`# data points per cluster`) %>%
  summarise(col = median(`Performance Gap`), sd = sd(`Performance Gap`)) %>%
  ungroup()

data <- left_join(data, diff_color) %>%
  rename("# Data Points / Cluster (Avg)" = "# data points per cluster") %>%
  mutate(Winner = ifelse(col > sd/sqrt(300), 'GMM', ifelse(col < -sd/sqrt(300), 'K-Means', 'Tie')))

# winner is GMM if >0 else KMeans

ggplot(data=data) +
  facet_grid(. ~ Archetype) +
  geom_boxplot(mapping=aes(x=`# Data Points / Cluster (Avg)`, y=`Performance Gap`,
                           fill=Winner),
               linewidth=0.5, color='black') +
  ggtitle('Performance Gap by Data Set Archetype') +
  theme_bw(base_size = 19.5) +
  theme(legend.text=element_text(size=rel(1.15))) +
  theme(plot.title=element_text(size=rel(1.25))) +
  theme(axis.title.x=element_text(size=rel(1.2))) +
  theme(axis.text.x=element_text(size=rel(1.025))) +
  theme(axis.title.y=element_text(size=rel(1.2))) +
  geom_hline(aes(yintercept=0), linetype='dashed', linewidth=0.5) + 
  theme(legend.position = c(1.075,0.545)) +
  theme(plot.margin = unit(c(0,5.25,1.0,0), "cm")) +
  scale_fill_manual(values=c("GMM"="#92c5de", "K-Means"="#f4a582"))

ggplot(data=data_long %>% filter(samples_per_cluster==200)) +
  facet_grid(. ~ Archetype) +
  geom_boxplot(mapping=aes(x=Method, y=Performance),
               linewidth=0.5, color='black') +
  ggtitle('Clustering Performance by Data Set Archetype (~200 Data Points / Cluster)') +
  theme_bw(base_size = 19.5) +
  theme(legend.text=element_text(size=rel(1.15))) +
  theme(plot.title=element_text(size=rel(1.25))) +
  theme(axis.title.x=element_text(size=rel(1.2))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(size=rel(1.025))) +
  theme(axis.title.y=element_text(size=rel(1.2))) +
  theme(legend.position = c(1.075,0.545)) +
  theme(plot.margin = unit(c(0,5.25,1.5,0), "cm")) +
  scale_fill_manual(values=c("GMM"="#92c5de", "K-Means"="#f4a582"))



#### Steps to reproduce Figure 12 ####

data100_hv <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-highlyvariable-100percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 100)
data200_hv <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-highlyvariable-200percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 200)
data1000_hv <- read_csv("../data/gmm-vs-kmeans/gmm-vs-kmeans-highlyvariable-1000percluster.csv")[,-1] %>%
  mutate('samples_per_cluster' = 1000)
data_agg_hv <- full_join(data100_hv, data200_hv) %>% full_join(data1000_hv)

data_hv <- data_agg_hv %>%
  mutate('performance_diff' = gauss-kmeans) %>%
  mutate('archetype' = fct_reorder(.f = archetype, .x = performance_diff,
                                   .fun = function(x) 0.1*min(x) + 0.9*median(x), .desc=T)) %>%
  rename('Archetype'='archetype') %>%
  mutate('Archetype' = fct_recode(Archetype,
                                  "Normal"="normal_highlyvariable",
                                  "Exponential"="nonnormal_highlyvariable",
                                  "t (df=3)"="heavytails_med_highlyvariable",
                                  "t (df=2)"="heavytails_hard_highlyvariable",
                                  "t (df=1.5)"="heavytails_hardhard_highlyvariable")) %>%
  rename('Performance Gap' = performance_diff) %>%
  mutate("# Data Points / Cluster (Avg)" = factor(samples_per_cluster,ordered=TRUE))

diff_color_hv <- data_hv %>%
  group_by(Archetype,`# Data Points / Cluster (Avg)`) %>%
  summarise(col = median(`Performance Gap`), sd = sd(`Performance Gap`)) %>%
  ungroup()

data_hv <- left_join(data_hv, diff_color_hv) %>%
  mutate(Winner = ifelse(col > sd/sqrt(300), 'GMM', ifelse(col < -sd/sqrt(300), 'K-Means', 'Tie')))

ggplot(data=data_hv) +
  facet_grid(. ~ Archetype) +
  geom_boxplot(mapping=aes(x=`# Data Points / Cluster (Avg)`, y=`Performance Gap`,
                           fill=Winner),
               linewidth=0.5, color='black') +
  ggtitle('Performance Gap on "Highly Variable" Clusters, by Distribution') +
  theme_bw(base_size = 19.5) +
  theme(legend.text=element_text(size=rel(1.15))) +
  theme(plot.title=element_text(size=rel(1.25))) +
  theme(axis.title.x=element_text(size=rel(1.2))) +
  theme(axis.text.x=element_text(size=rel(1.025))) +
  theme(axis.title.y=element_text(size=rel(1.2))) +
  geom_hline(aes(yintercept=0), linetype='dashed', linewidth=0.75) + 
  theme(legend.position = c(1.05,0.545)) +
  theme(plot.margin = unit(c(0,3.5,1.0,0), "cm")) +
  scale_fill_manual(values=c("GMM"="#92c5de", "K-Means"="#f4a582"))
