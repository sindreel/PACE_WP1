
code <- read_delim("code.csv", ";", escape_double = FALSE, 
                   trim_ws = TRUE)

presence <- read_delim("presence.csv", ";", escape_double = FALSE, 
                   trim_ws = TRUE)

library(tidyverse)
library(ggplot2)
dat=presence %>% 
  as.data.frame() %>%
  pivot_longer(-c(patho), names_to = "samples", values_to = "counts") %>% 
  mutate(pres=(counts==0)-1)



dat %>% 
  left_join(code,by=c("patho"="code")) %>% 
  ggplot(aes(x=name, y=samples, fill=pres)) + 
  geom_raster() +
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1, face=2))
view(dat)



