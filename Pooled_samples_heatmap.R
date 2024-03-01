#Heatmap of pooled samples

#Example:https://rgup.gitlab.io/research_cycle/02_ggplot.html
# generate two sets of correlated variables (a and b)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
# heatmap <- tibble(
#   a1 = rnorm(100),
#   b1 = rnorm(100)
# ) %>% 
#   mutate(
#     a2 = a1 + rnorm(100),
#     a3 = a1 + rnorm(100),
#     a4 = a1 + rnorm(100),
#     b2 = b1 + rnorm(100),
#     b3 = b1 + rnorm(100),
#     b4 = b1 + rnorm(100)
#   ) %>%
#   cor() %>% # create the correlation matrix
#   as.data.frame() %>% # make it a data frame
#   rownames_to_column(var = "V1") %>% # set rownames as V1
#   gather("V2", "r", a1:b4) # wide to long (V2)
# http://127.0.0.1:29613/graphics/plot_zoom_png?width=1200&height=900


dat <- read.csv("./copy_numbers_pooled_samples WP1_heatmap_240124.csv", sep = ";")
options(scipen=999)

library(stringr)
navn <- names(dat)
str(dat)
navn <- gsub(".", " ", navn, fixed = TRUE)
names(dat) <- navn
dat$location
#rlist::list.reverse(x)
y_level_names <- dat$location
y_level_names <- rlist::list.reverse(y_level_names)
x_level_names <- c("Candidatus Branchiomonas cysticola", "Ichthyobodo","Piscichlamydia salmonis" , "Candidatus Syngnamydia salmonis" , "Paranucleospora theridion"    ,      "Parvicapsula pseudobranchicola" ,    "Loma salmonae"    ,                 
"Ichthyophthirius multifiliis"   ,    "Piscine orthoreovirus 3"       ,     "Ichthyophonus hoferi"    ,           "Myxobolus arcticus"          ,      
 "Tenacibaculum maritimum"       ,     "Atlantic salmon paramyxovirus"     , "Tenacibaculum dicentrarchi" )
navn
dat <- dat %>%
  gather(key = "assay", value = value, -location)

head(dat)
dat$log_copy_number <- ''
dat$log_copy_number<- log(dat$value)
#dat$value[dat$value<=0] <- 0
#dat$value[dat$value<0] <-
#dat$log_copy_number[dat$log_copy_number<0] <- 0

str(dat)
tmp <- dat %>%
  group_by(assay) %>%
  dplyr::summarise(max_value = max(value))

dat <- merge(dat, tmp, by="assay", all.x=TRUE)
dat$RIB[is.na(dat$RIB)] <- 0
tmp <- dat %>%
  group_by(location) %>%
  dplyr::summarise(RIB = sum(RIB))

head(dat)
dat$RIB <- dat$value/dat$max_value
dat$RIB[dat$RIB==0] <- NA
dat$RIB[dat$RIB=='NaN'] <- NA

library(ggplot2)
p1 <-ggplot(dat, aes(factor(assay, levels = x_level_names), factor(location, levels=y_level_names), fill=log_copy_number)) +
  geom_tile() + xlab("Pathogen") + ylab("Location") + ggtitle("a)") +
  scale_fill_viridis() + theme_classic(base_size = 18) + theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))# + scale_fill_continuous(name = "Dose")
p1


p2 <-ggplot(tmp, aes(factor(location, levels=y_level_names), y = as.numeric(RIB))) +
  geom_col() + xlab("Location") + ylab("Relative infection burden (RIB)") + ggtitle("b)") +
  scale_fill_viridis() + theme_classic(base_size = 18) + theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))# + scale_fill_continuous(name = "Dose")
p2

library(ggpubr)
p3 <- ggarrange(p1,p2, ncol = 2, nrow = 1, heights = c(1, 1))
ggsave("./data/modified_data/log_copy_number_and_RIB.png", p3, units="cm", width=40, height=20, dpi=600)



ggsave(p1, file="./data/modified_data/heatmap_pathogens_pooled_RIB.tiff", units="cm", width=25, height=25, dpi=600, compression = 'lzw', limitsize = FALSE)
ggsave(p1, file="./data/modified_data/heatmap_pathogens_pooled_RIB.png", units="cm", width=25, height=25, dpi=600, limitsize = FALSE)

                                                                                                                                       