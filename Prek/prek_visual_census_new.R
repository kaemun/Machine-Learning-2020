library(tidyverse)
library(sf)

# 2010 Census tracts: https://opendata.dc.gov/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8
tracts10 <- sf::st_read('https://opendata.arcgis.com/datasets/6969dd63c5cb4d6aa32f15effb8311f3_8.geojson')
tracts10 <- tracts10 %>%
  select(census_tract = TRACT, GEOID) %>%
  mutate_if(is.factor, as.character)
for(.y in 2001:2016){ assign(paste0("geo", .y), paste0("geo", .y) %>% get %>% left_join(tracts10, by = "census_tract")) }

# creating data input for mapping
dist_gen <- function(year) {
  #####
  # get the yearly data
  dta <- paste0("geo", year) %>% get

  dta <- dta %>%
    filter(wage < summarise(.data = dta, quantile(wage, probs = 0.997, na.rm = T, type = 1))[[1]])

  # creating summary data for plotting #####
  sf <- dta %>%
    # MAR
    # filter(type == 2 | type == 3) %>%
    # HOH
    # filter(type == 5 | type == 1 | type == 0 | type == 7) %>%
    # Ward
    filter(ward == 1 | ward == 4) %>%
    filter(!is.na(type))
  
  ### bottom 30% income bracket #####
  ### refinements ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  # subset bottom 30% cutoff
  sf <- sf %>%
    semi_join(
      dta %>%
        filter(wage < quantile(wage, probs = .30, type = 1) & adj_inc < quantile(adj_inc, probs = .30, type = 1))
      , by = "ssn")

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #####
  #####
  
  sf <- sf %>% 
    mutate(d_kid3 = ifelse(paste0(year-0, "1231") > kid_dob & kid_dob > paste0(year-1, "0930"), 1, 0)) %>% 
    mutate(d_kid79 = ifelse(paste0(year-6, "1001") > kid_dob & kid_dob > paste0(year-9, "0930"), 1, 0)) %>%
    arrange(census_tract) %>% 
    group_by(census_tract) %>%
    summarise(fy = first(fy), pop_kid3 = sum(d_kid3), pop_kid79 = sum(d_kid79)) %>% 
    ungroup() %>% 
    mutate(pop_kid = pop_kid3 + pop_kid79) 
  
  tot_pop <- (sf %>% select(starts_with("pop_")) %>% summarise_all(sum))
  
  sf <- sf %>% 
    mutate(pop_kid3 = round(pop_kid3 / tot_pop[['pop_kid3']], 5) * 100) %>%
    mutate(pop_kid79 = round(pop_kid79 / tot_pop[['pop_kid79']], 5) * 100) %>% 
    mutate(pop_kid = round(pop_kid / tot_pop[['pop_kid']], 5) * 100)
  
  sf <- tracts10 %>% 
    left_join(sf, by="census_tract") %>% 
    select(-census_tract) %>% 
    mutate(fy = year) %>% 
    mutate_at(vars(starts_with("pop")), ~(ifelse(is.na(.), 0, .)))
  
  sf <- sf %>% 
    rename("Age 3" = pop_kid3, "Age 7-9" = pop_kid79, "All" = pop_kid) %>% 
    gather(key = "variable", value = "pct", -c(GEOID, fy, geometry)) %>% 
    mutate(variable = factor(variable, levels = c("Age 3", "Age 7-9", "All")))

  sf <- list(sf, tot_pop)
  
  return(sf)
  
  #####
}

dist_sf <- function(.printinfo = T){
  for (.y in 2001:2016) { assign(paste0("sf", .y), .y %>% dist_gen) }
  sf_long <- eval(parse(text = paste0("rbind(", paste0("sf", 2001:2016, "[[1]]", collapse=", "), ")")))
  pop_long <- eval(parse(text = paste0("rbind(", paste0("sf", 2001:2016, "[[2]]", collapse=", "), ")")))
  
  if (.printinfo == TRUE) {
    pop_long <- pop_long %>% 
      bind_rows(pop_long %>% summarise_all(mean) %>% mutate_all(round)) %>% 
      rename('Age 3' = 1, 'Age 7-9' = 2, 'Total' = 3)
    
    cat(crayon::silver("\n############################\n"))
    cat(crayon::yellow("Pop. of 1 Child by Age in DC\n"))
    print.data.frame(pop_long, row.names = c(2001:2016, "avg."))
    cat(crayon::silver("############################\n"))

  }
  return(invisible(sf_long))
}

# avg plot
#####

geo_long_avg <- dist_sf() %>%
  group_by(GEOID, variable) %>% 
  summarise(pct = mean(pct)) %>% 
  ungroup

geo_long_avg %>% 
  ggplot() +
  facet_grid(cols = vars(variable)) +
  geom_sf(aes(fill = pct, color = pct)) +
  viridis::scale_fill_viridis() +
  viridis::scale_color_viridis(guide = FALSE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "transparent")) +
  labs(title = "Distribution of Children in the District of Columbia 2001~2016 (avg)") +
  ggsave(paste0("census_plot_", Sys.time() %>% format.POSIXct(format = "%m%d%H%M%S"),".png"), 
         width = 10, height = 5)