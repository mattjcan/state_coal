# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(knitr)
library(xaringan)
library(rmarkdown)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(scales)
library(easynls)
library(gridExtra)
library(readxl)
library(survey)
library(vcd)
library(XML)
library(xml2)
library(sp)
library(rgdal)
library(leaflet)
library(mapview)
library(widgetframe)
library(rgeos)


# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Documents/git/state_coal/" # parent directory for the data

sa2_emp <- read_csv(paste0(d,"data/ABS 2016 - ind_empl_sa2.csv"), skip = 9)

sa2_emp <- sa2_emp[2:2311, 1:717]

sa2_map_org <- readOGR(paste0(d,"data/SA2_2016_AUST.shx"))

qld_elec <- readOGR(paste0(d, "data/State_electoral_boundaries_2017.shx"))



# TIDY ---- 

# tidy

sa2_emp <- sa2_emp %>% 
  rename(region = "INDP - 4 Digit Level",
         SA2_5DIG16 = "SA2_5DIGITCODE_2016")

sa2_emp <- sa2_emp %>% 
  gather(key = ind, value = emp, -c("region", "SA2_5DIG16"))

sa2_emp <- sa2_emp %>% 
  group_by(SA2_5DIG16) %>% 
  mutate(p_emp = (emp / sum(emp, na.rm = T)) * 100)

list_min <- unique(sa2_emp$ind)[65:86]

sa2_coal <- sa2_emp %>% 
  filter(ind %in% c("Coal Mining")) 

sa2_min <- sa2_emp %>% 
  filter(ind %in% list_min) %>% 
  group_by(region, SA2_5DIG16) %>% 
  summarise(mining = sum(emp)) %>% 
  ungroup()

sa2_sum <- sa2_emp %>% 
  group_by(SA2_5DIG16) %>% 
  summarise(emp = sum(emp)) %>% 
  ungroup()

sa2_min <- left_join(sa2_min, sa2_sum, by = "SA2_5DIG16")

sa2_min <- sa2_min %>% 
  filter(emp != 0) %>% 
  mutate(p_emp = mining / emp * 100,
         ind = "Mining") %>% 
  select(region, SA2_5DIG16, ind, mining, p_emp) %>% 
  rename(emp = mining) %>% 
  ungroup()

sa2_min <- bind_rows(sa2_coal, sa2_min)

sa2_min <- sa2_min %>% 
  select(-emp) %>% 
  spread(key = ind, value = p_emp) %>% 
  rename(coal = "Coal Mining",
         mining = Mining) %>% 
  arrange(-coal)

sa2_emp$st_code <- str_sub(sa2_emp$SA2_5DIG16,1,1)

sa2_qld <- sa2_emp %>% 
  filter(st_code == "3")

tot_qld <- sum(sa2_qld$emp, na.rm = T)

sa2_coal$st_code <- str_sub(sa2_coal$SA2_5DIG16,1,1)

sa2_coal_qld <- sa2_coal %>% 
  filter(st_code == "3")

tot_qld_coal <- sum(sa2_coal_qld$emp, na.rm = T)

p_qld_coal <- tot_qld_coal / tot_qld * 100

tot_aus <- sum(sa2_emp$emp, na.rm = T)

tot_coal <- sum(sa2_coal$emp, na.rm = T)

p_coal <- tot_coal / tot_aus * 100


# maps

list_tier1 <- c("THURINGOWA", "BURDEKIN", "WHITSUNDAY", "MIRANI", "KEPPEL", "TOOWOOMBA NORTH")

list_tier2 <- c("MUNDINGBURRA", "MACKAY", "ROCKHAMPTON", "MARYBOROUGH", "MANSFIELD", "BULIMBA", "IPSWICH WEST")

list_tier <- c(list_tier1, list_tier2)

qld_elec_tier <- qld_elec[qld_elec$NAME %in% list_tier, ]

sa2_map_qld <- sa2_map_org[sa2_map_org$STE_NAME16 %in% "Queensland", ]

sa2_map_coal <- sp::merge(sa2_map_qld, sa2_min, by = "SA2_5DIG16", all=F)

pal_coal <- colorBin(c("#999999", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = sa2_min$coal, bins = c(0, 0.46, 1.25, 10, 50))

labels <- sprintf(
  "<strong>%s</strong><br/>Coal employment: %g %%<br/>Mining employment: %g %%",
  sa2_map_coal$SA2_NAME16, round(sa2_map_coal$coal,1), round(sa2_map_coal$mining,0)
) %>% lapply(htmltools::HTML)

m_sa2_coal <- leaflet(data = sa2_map_coal) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_coal(coal), fillOpacity = 1, weight = 0.2, color = "white", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = FALSE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = qld_elec_tier, color = "red", weight = 1, opacity = 1, fill = FALSE, label = qld_elec_tier$NAME, highlight = highlightOptions(weight = 3, color = "#696969", bringToFront = TRUE), labelOptions = labelOptions(noHide = T,  direction = "center", style = list("font-weight" = "normal", padding = "0px 0px"))) %>% 
  addPolygons(fillColor = ~pal_coal(coal), fillOpacity = 1, weight = 0.2, color = "white", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = FALSE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = qld_elec, color = "red", weight = 1, opacity = 1, fill = FALSE, label = qld_elec$NAME, highlight = highlightOptions(weight = 3, color = "#696969", bringToFront = FALSE))

# saveWidget(m_sa2_coal, file=paste0(d,"maps/m_sa2_coal.html"))



