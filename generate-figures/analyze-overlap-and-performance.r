library(ggplot2)
library(tidyverse)
library(readr)
library(haven)

#### RUN ALL OF THE CODE BELOW TO REPRODUCE FIGURES 6 AND 9 ####

data <- read_csv("../data/overlap-and-performance.csv")

# Select columns from the data
data <- data %>% 
  mutate("Overlap" = (max_o_exact+min_o_exact)/2,
         "LDA Overlap" = (max_o_lda+min_o_lda)/2,
         "C2C Overlap" = (max_o_heuristic+min_o_heuristic)/2,
         "Performance" = ami_kmeans,
         "# Dimensions" = as.character(dim),
         "# Clusters" = factor(k,ordered=TRUE))
#  select(-c(overlap,ami_kmeans,dim,k))

# Plot Clustering Performance vs Overlap
ggplot(data) +
  geom_point(mapping=aes(y=`Performance`, x=Overlap,
                         color=`# Dimensions`, size=`# Clusters`),alpha=0.9) +
  geom_smooth(mapping=aes(y=`Performance`, x=Overlap, 
                          group=`# Dimensions`), color='black',alpha=0.1,
              linetype=2, method='gam', se=T, position='jitter') +
  scale_x_continuous(trans='log10') +
  ggtitle('Clustering Performance vs Overlap') +
  theme_bw(base_size = 19.5) +
  theme(legend.position = c(.906, .735)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size=rel(1.075))) +
  theme(legend.title=element_text(size=rel(1.075))) +
  theme(plot.title=element_text(size=rel(1.5))) +
  theme(axis.title.x=element_text(size=rel(1.4))) +
  theme(axis.text.x=element_text(size=rel(1.05))) +
  theme(axis.title.y=element_text(size=rel(1.4))) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Line up the different overlap approximations with exact overlap

# Correlation between Exact and Approximate Overlaps
lda_oneminuscorr <- 1 - cor(data$Overlap, data$`LDA Overlap`)
c2c_corr <- round(cor(data$Overlap, data$`C2C Overlap`),2)
  
data %>% 
  select(c("Overlap", "LDA Overlap", "C2C Overlap",
           "# Dimensions", "# Clusters")) %>%
  rename("LDA"="LDA Overlap", "C2C" = "C2C Overlap",
         "Exact Overlap" = "Overlap") %>%
  pivot_longer(c('LDA', 'C2C'),
               names_to="Method", values_to="Approximate Overlap") %>%
  mutate("Method" = as.factor(Method)) %>%
  ggplot() +
  geom_point(mapping=aes(y=`Approximate Overlap`, x=`Exact Overlap`,
                         color=Method), 
             alpha=0.5, size=3) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_smooth(mapping=aes(y=`Approximate Overlap`, x=`Exact Overlap`, 
                          group=Method), color='black',alpha=0.1,
              linetype=2, method='gam', se=T, size=1.5) +
  ggtitle('Approximate vs Exact Overlap, by Method') +
  theme_bw(base_size = 19.5) +
  theme(legend.position = c(.796, .374)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.text=element_text(size=rel(1.075))) +
  theme(legend.title=element_text(size=rel(1.075))) +
  theme(legend.position="none") +
  annotate("text",x=0.004,y=0.00075,
           label=paste('list(bold(LDA),~italic(r))>', 0.99), size=7, parse=TRUE) +
  annotate("text",x=0.000635,y=0.2, label="C2C", fontface=2, size=7) +
  annotate("text",x=0.00175,y=0.2,
           label=paste("list(,~italic(r)==", c2c_corr,")",sep=""),
           size=7, parse=TRUE) +
  theme(plot.title=element_text(size=rel(1.5))) +
  theme(axis.title.x=element_text(size=rel(1.4))) +
  theme(axis.text.x=element_text(size=rel(1.05))) +
  theme(axis.title.y=element_text(size=rel(1.4))) +
  guides(color = guide_legend(override.aes = list(size = 4)))


# Overlap Approximation Error
data %>% 
  select(c("Overlap", "LDA Overlap", "C2C Overlap",
           "# Dimensions", "# Clusters")) %>%
  rename("LDA"="LDA Overlap", "C2C" = "C2C Overlap",
         "Exact Overlap" = "Overlap") %>%
  pivot_longer(c('LDA', 'C2C'),
               names_to="Method", values_to="ApproxOverlap") %>%
  mutate("Relative Error"= abs(ApproxOverlap - `Exact Overlap`)/abs(`Exact Overlap`)) %>%
  filter(`Relative Error` >= 1e-07) %>%
  ggplot() +
    geom_point(mapping=aes(y=`Relative Error`, x=`Exact Overlap`,
                           color=Method), 
               alpha=0.5, size=3) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    geom_smooth(mapping=aes(y=`Relative Error`, x=`Exact Overlap`, 
                            group=Method), color='black',alpha=0.1,
                            linetype=2, method='gam', se=T, size=1.5) +
    ggtitle('Overlap Approximation Error, by Method') +
    theme_bw(base_size = 19.5) +
    theme(legend.position = c(.906, .8725)) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.text=element_text(size=rel(1.075))) +
    theme(legend.title=element_text(size=rel(1.075))) +
    theme(legend.position="none") +
    annotate("text",x=0.022,y=0.00175,label="LDA", size=7, fontface=2) +
    annotate("text",x=0.022,y=25,label="C2C", size=7, fontface=2) +
    theme(plot.title=element_text(size=rel(1.5))) +
    theme(axis.title.x=element_text(size=rel(1.4))) +
    theme(axis.text.x=element_text(size=rel(1.05))) +
    theme(axis.title.y=element_text(size=rel(1.4))) +
    guides(color = guide_legend(override.aes = list(size = 4)))
