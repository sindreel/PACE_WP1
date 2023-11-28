
d<-data %>% 
  as_tibble %>% 
  mutate(presence=as.numeric(presence)) %>% 
  group_by(pathogen, location, lon, lat) %>% 
  pivot_wider(names_from=pathogen, values_from=presence) %>% 
  mutate_if(is.numeric, function(x) replace_na(x, 0))

ggplot()+
  geom_scatterpie(data=d, aes(lon, lat, group=location))
