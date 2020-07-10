# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(htmltools)
library(sp)
library(rgdal)
library(mapview)
library(rgeos)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(axis.text = element_text(size = 12, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) +
  theme(axis.text.y = element_blank())

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Documents/R/sa_map/" # parent directory for the data

sa2_pl <- read_csv(paste0(d,"data/Reps TPP by SA2.csv"), skip = 0)

sa1_pl <- read_csv(paste0(d,"data/TPP and TCP by SA1.csv"), skip = 0)

sa1_map_org <- readOGR(paste0(d,"data/SA1_2016_AUST.shx"))

sa2_map_org <- readOGR(paste0(d,"data/SA2_2016_AUST.shx"))

sa2_emp <- read_csv(paste0(d,"data/ABS 2016 - ind_empl_sa2.csv"), skip = 9)

sa2_emp <- sa2_emp[2:2311, 1:717]

fed_elec <- readOGR(paste0(d,"data/COM_ELB_region.shx"))

qld_elec <- readOGR(paste0("C:/Users/matt/Documents/R/election_2019_nd/data/State_electoral_boundaries_2017.shx"))


# TIDY ---- 

sa2_s <- sa2_pl %>% 
  rename(SA2_5DIG16 = SA2_5digitcode_2016,
         v_lnp_sa2 = LNP_TPP_2019,
         v_alp_sa2 = ALP_TPP_2019,
         p_lnp_sa2 = LNP_TPP_2019_pc,
         p_alp_sa2 = ALP_TPP_2019_pc,
         p_lnp_sa2_16 = LNP_TPP_2016_pc,
         swing = LNP_TPP_swing) %>% 
  mutate(v_t_sa2 = v_lnp_sa2 + v_alp_sa2) %>% 
  select(SA2_5DIG16, v_lnp_sa2, v_alp_sa2, v_t_sa2, p_lnp_sa2, p_alp_sa2, p_lnp_sa2_16, swing)

sa2_map_s <- sp::merge(sa2_map_org, sa2_s, by = "SA2_5DIG16", all=F)

pal_t_s <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa2_map_s$swing, bins = c(-20, -10, -5, 0, 5, 10, 25))

labels <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
  sa2_map_s$SA2_NAME16, round(sa2_map_s$swing,1), round(sa2_map_s$v_t_sa2,0)
) %>% lapply(htmltools::HTML)

m_sa2_s <- leaflet(data = sa2_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s(swing), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa2_s, file="m_sa2_s.html")

# mapshot(m_sa2_s, file = "m_sa2_s.png")

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

mean_coal <- mean(sa2_min$coal, na.rm = T)

mean_min <- mean(sa2_min$mining, na.rm = T)

sa2_min$SA2_5DIG16 <- as.numeric(sa2_min$SA2_5DIG16)

sa2_s_emp <- left_join(sa2_s, sa2_min, by = "SA2_5DIG16")

sa2_s_emp$coal_lab <- ifelse(sa2_s_emp$coal >= mean_coal, "coal", "non_coal")

sa2_s_emp$min_lab <- ifelse(sa2_s_emp$mining >= mean_min, "mining", "non_mining")

sa2_s_emp <- sa2_s_emp[, c(9, 1:8, 10:13)]

sa2_s_emp <- sa2_s_emp %>% 
  arrange(-swing)

t_mean_min <- sa2_s_emp %>%
  ungroup() %>% 
  group_by(min_lab) %>% 
  summarise(avg_s = mean(swing, na.rm = T))

t_mean_coal <- sa2_s_emp %>%
  ungroup() %>% 
  group_by(coal_lab) %>% 
  summarise(avg_s = mean(swing, na.rm = T))

sa2_s_emp_100 <- sa2_s_emp %>% 
  filter(v_t_sa2 > 100)

sa2_s_emp_100 <- sa2_s_emp_100[1:100, ]

num_coal <- sa2_s_emp_100 %>% 
  filter(coal_lab == "coal") %>% 
  nrow()

num_coal_tot <- sa2_s_emp %>% 
  filter(coal_lab == "coal") %>% 
  nrow()

per_coal <- num_coal_tot / nrow(sa2_s_emp) * 100

num_min <- sa2_s_emp_100 %>% 
  filter(min_lab == "mining") %>% 
  nrow()

write_csv(sa2_s_emp, "sa2_coal_swings.csv")

p_lm_coal <- sa2_s_emp %>% 
  filter(v_t_sa2 > 1000) %>% 
  ggplot(aes(x = coal, y = swing)) + 
  geom_point(size = 1, color = "grey", alpha = 0.7) +
  geom_smooth(method='lm') +
  theme_mc +
  labs(title = paste("Relationship between share of coal jobs and swing to LNP"), subtitle = "", x = "Proportion of employment in coal (%)", y = "Swing to the LNP") +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) 

lm_coal <- lm(swing~coal, data = sa2_s_emp) 

lm_mining <- lm(swing~mining, data = sa2_s_emp) 

summary(lm_mining)

t_min_swing <- tibble("measure" = c("Above average\ncoal employment", "Above average\nmining employment", "Australia\nwide"), swing = c(4.87, 3.45, 1.17))

sa2_s_coal <- sa2_s_emp %>% 
  filter(coal_lab == "coal")

# maps

sa2_map_s_100 <- sp::merge(sa2_map_org, sa2_s_emp_100, by = "SA2_5DIG16", all=F)

pal_t_s <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa2_map_s_100$swing, bins = c(-20, -10, -5, 0, 5, 10, 25))

labels <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
  sa2_map_s_100$SA2_NAME16, round(sa2_map_s_100$swing,1), round(sa2_map_s_100$v_t_sa2,0)
) %>% lapply(htmltools::HTML)

m_sa2_s_100 <- leaflet(data = sa2_map_s_100) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s(swing), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa2_s_100, file="m_sa2_s_100.html")

# mapshot(m_sa2_s_100, file = "m_sa2_s_100.png")

# maps coal 

sa2_map_s_coal <- sp::merge(sa2_map_org, sa2_s_coal, by = "SA2_5DIG16", all=F)

pal_t_s <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa2_map_s_coal$swing, bins = c(-20, -10, -5, 0, 5, 10, 25))

labels <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g<br/>Coal employment: %g %%",
  sa2_map_s_coal$SA2_NAME16, round(sa2_map_s_coal$swing,1), round(sa2_map_s_coal$v_t_sa2,0), round(sa2_map_s_coal$coal,1)
) %>% lapply(htmltools::HTML)

m_sa2_s_coal <- leaflet(data = sa2_map_s_coal) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s(swing), fillOpacity = 1, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) 

# saveWidget(m_sa2_s_coal, file="m_sa2_s_coal.html")


# plots

p_min_swing <- t_min_swing %>% 
  ggplot(aes(x = measure, y = swing))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color) +
  theme_mc +
  labs(title = "Two party preferred swing to LNP", subtitle = "%, by SA2s", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing,1))), vjust = -1, size=4) +
  ylim(0, 6)

# sa1 maps

sa1_s <- sa1_pl %>% 
  rename(SA1_7DIG16 = SA1_7DIGITCODE_2016,
         state_ab = State,
         v_lnp_sa1 = LNP_TPP,
         v_alp_sa1 = ALP_TPP,
         p_lnp_sa1 = LNP_TPP_pc,
         p_alp_sa1 = ALP_TPP_pc,
         swing = LNP_TPP_swing) %>% 
  mutate(v_t_sa1 = v_lnp_sa1 + v_alp_sa1) %>% 
  select(SA1_7DIG16, state_ab, div_nm, v_lnp_sa1, v_alp_sa1, v_t_sa1, p_lnp_sa1, p_alp_sa1, swing)


list_wc <- c("Rankin", "Blair", "Lilley", "Forde", "Oxley", "Longman", "Petrie", "Bowman", "Bonner")

sa1_s_wc <- sa1_s %>% 
  filter(div_nm %in% list_wc)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_wc, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

wc_map <- fed_elec[fed_elec$Elect_div %in% list_wc, ]

sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

pal_t_s_wc <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s_wc$swing, bins = c(-20, -10, -5, 0, 5, 10, 25))

labels_s_wc <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s_wc$SA1_7DIG16, round(sa1_map_s_wc$swing,1)) %>% lapply(htmltools::HTML)

m_sa1_s_wc <- leaflet(data = sa1_map_s_wc) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_wc(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_wc,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = wc_map, color = "#696969", weight = 2, opacity = 1, fill = FALSE, label = wc_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_wc, values = c(-20, 20), position = "bottomright")

# saveWidget(m_sa1_s_wc, file="m_sa1_s_wc_qld.html")


# nats

list_nats <- c("Dawson", "Capricornia", "Flynn", "Wide Bay", "Hinkler", "Maranoa", "Richmond", "Page", "New England", "Lyne", "Cowper", "Hunter", "Calare", "Parkes", "Mallee", "Nicholls", "Indi", "Gippsland", "Durack", "Cowper", "Riverina", "Lingiari", "Gilmore", "Kennedy", "Eden-Monaro", "Whitlam", "Braddon", "Lyons", "O'connor", "Barker", "Pearce", "Bass")


sa1_s_nats <- sa1_s %>% 
  filter(div_nm %in% list_nats)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_nats, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

nats_map <- fed_elec[fed_elec$Elect_div %in% list_nats, ]

# sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

pal_t_s_nats <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s$swing, bins = c(-35, -10, -5, 0, 5, 10, 35))

labels_s_nats <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)

m_sa1_s_nats <- leaflet(data = sa1_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_nats,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = nats_map, color = "#696969", weight = 2, opacity = 1, fill = FALSE, label = nats_map$Elect_div, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright")

# saveWidget(m_sa1_s_nats, file="m_sa1_s_nats_qld.html")

# nats individual seats

sa1_s_nat_seat <- sa1_s %>% 
  filter(div_nm %in% "Dawson")

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_nat_seat, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

pal_t_s_nats <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s$swing, bins = c(-35, -10, -5, 0, 5, 10, 35))

nats_map <- fed_elec[fed_elec$Elect_div %in% "Dawson", ]

# sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

labels_s_nats <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)

m_sa1_daw <- leaflet(data = sa1_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_nats,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = nats_map, color = "#696969", weight = 2, opacity = 1, fill = FALSE, label = nats_map$Elect_div, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright")

# nats function

pal_t_s_nats <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s$swing, bins = c(-35, -10, -5, 0, 5, 10, 35))

# sa1

f_nats_maps <- function(x) {

sa1_s_nat_seat <- sa1_s %>% 
  filter(div_nm %in% x)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_nat_seat, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

nats_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]

# sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

labels_s_nats <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)


m1 <- leaflet(data = sa1_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_nats,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
  addPolygons(data = nats_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))

saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/nats/m_sa1_", x, ".html"), selfcontained = T)

}

# p_nats_maps <- map(list_nats, f_nats_maps)

# names(p_nats_maps) <- unique(list_nats)

# individual nats sa1 seats

x = "Eden-Monaro"

sa1_s_nat_seat <- sa1_s %>% 
  filter(div_nm %in% x)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_nat_seat, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

nats_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]

# sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

labels_s_nats <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)


m1 <- leaflet(data = sa1_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_nats,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
  addPolygons(data = nats_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))

m1 <- m1 %>% 
  setView(lng = 149.149490, lat = -35.960780, zoom = 8)

# saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/nats/m_sa1_", x, ".html"), selfcontained = T)


# sa2



f_nats_maps_sa2 <- function(x) {
  
  nats_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]
  
  sa2_map_s_seat <- raster::intersect(sa2_map_s, nats_map_seat)
  
  labels_s_nats <- sprintf(
    "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
    sa2_map_s_seat$SA2_NAME16, sa2_map_s_seat$swing, round(sa2_map_s_seat$v_t_sa2,0)
  ) %>% lapply(htmltools::HTML)
  
  m1 <- leaflet(data = sa2_map_s_seat) %>% 
    addProviderTiles("CartoDB") %>%  
    addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
      weight = 3,
      color = "white",
      fillOpacity = 1,
      bringToFront = TRUE),
      label = labels_s_nats,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto")) %>% 
    addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
    addPolygons(data = nats_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))
  
  saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/nats/m_sa2_", x, ".html"), selfcontained = T)
  
}

# p_nats_maps_sa2 <- map(list_nats, f_nats_maps_sa2)

# names(p_nats_maps) <- unique(list_nats)

# indiviudal nats seats

# list_nats <- c("Braddon", "Bass" )

# x <- "Braddon"

# nats_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]

# sa2_map_s_seat <- raster::intersect(sa2_map_s, nats_map_seat)
# 
# labels_s_nats <- sprintf(
#   "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
#   sa2_map_s_seat$SA2_NAME16, sa2_map_s_seat$swing, round(sa2_map_s_seat$v_t_sa2,0)
# ) %>% lapply(htmltools::HTML)
# 
# m1 <- leaflet(data = sa2_map_s_seat) %>% 
#   addProviderTiles("CartoDB") %>%  
#   addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
#     weight = 3,
#     color = "white",
#     fillOpacity = 1,
#     bringToFront = TRUE),
#     label = labels_s_nats,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "12px",
#       direction = "auto")) %>% 
#   addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
#   addPolygons(data = nats_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))
# 
# saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/nats/m_sa2_", x, ".html"), selfcontained = T)
# 



# lnp sa1 seats ---- 

list_lnp <- c("Blair", "Bonner", "Bowman", "Brisbane", "Dickson", "Fadden", "Fairfax", "Fisher", "Forde", "Griffith", "Groom", "Herbert", "Leichhardt", "Lilley", "Longman", "McPherson", "Moncrieff", "Moreton", "Oxley", "Petrie", "Rankin", "Ryan", "Wright")

f_lnp_maps <- function(x) {
  
  sa1_s_lnp_seat <- sa1_s %>% 
    filter(div_nm %in% x)
  
  sa1_map_s <- sp::merge(sa1_map_org, sa1_s_lnp_seat, by = "SA1_7DIG16", all=F, duplicateGeoms = T)
  
  
  lnp_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]
  
  # sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)
  
  labels_s_lnp <- sprintf(
    "<strong>%s</strong><br/>LNP Swing: %g %%",
    sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)
  
  m1 <- leaflet(data = sa1_map_s) %>% 
    addProviderTiles("CartoDB") %>%  
    addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
      weight = 3,
      color = "white",
      fillOpacity = 1,
      bringToFront = TRUE),
      label = labels_s_lnp,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto")) %>% 
    addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
    addPolygons(data = lnp_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))
  
  saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/lnp/m_sa1_", x, ".html"), selfcontained = T)
  
}

# p_lnp_maps <- map(list_lnp, f_lnp_maps)

# names(p_lnp_maps) <- unique(list_lnp)

# individual lnp sa1 seat

x <- "McPherson"

sa1_s_lnp_seat <- sa1_s %>% 
  filter(div_nm %in% x)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_lnp_seat, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

lnp_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]

# sa1_map_s_wc <- raster::intersect(sa1_map_s, wc_map)

labels_s_lnp <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s$SA1_7DIG16, round(sa1_map_s$swing,1)) %>% lapply(htmltools::HTML)

m1 <- leaflet(data = sa1_map_s) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_lnp,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
  addPolygons(data = lnp_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))

m1 <- m1 %>% 
  setView(lng = 153.465840, lat = -28.123740, zoom = 11)

# saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/lnp/m_sa1_", x, ".html"), selfcontained = T)



# sa2

f_lnp_maps_sa2 <- function(x) {
  
  lnp_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]
  
  sa2_map_s_seat <- raster::intersect(sa2_map_s, lnp_map_seat)
  
  labels_s_lnp <- sprintf(
    "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
    sa2_map_s_seat$SA2_NAME16, sa2_map_s_seat$swing, round(sa2_map_s_seat$v_t_sa2,0)
  ) %>% lapply(htmltools::HTML)
  
  m1 <- leaflet(data = sa2_map_s_seat) %>% 
    addProviderTiles("CartoDB") %>%  
    addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
      weight = 3,
      color = "white",
      fillOpacity = 1,
      bringToFront = TRUE),
      label = labels_s_lnp,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto")) %>% 
    addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>% 
    addPolygons(data = lnp_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))
  
  saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/lnp/m_sa2_", x, ".html"), selfcontained = T)
  
}

# p_lnp_maps_sa2 <- map(list_lnp, f_lnp_maps_sa2)

# names(p_lnp_maps) <- unique(list_lnp)

# individual lnp seat

x <- "Mcpherson"

lnp_map_seat <- fed_elec[fed_elec@data$Elect_div == x, ]

sa2_map_s_seat <- raster::intersect(sa2_map_s, lnp_map_seat)

labels_s_lnp <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
  sa2_map_s_seat$SA2_NAME16, sa2_map_s_seat$swing, round(sa2_map_s_seat$v_t_sa2,0)) %>% lapply(htmltools::HTML)

m1 <- leaflet(data = sa2_map_s_seat) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal_t_s_nats(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_lnp,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>%
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_nats, values = c(-20, 20), position = "bottomright") %>%
  addPolygons(data = lnp_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))

# saveWidget(m1, file=paste0("C:/Users/matt/Documents/R/sa_map/lnp/m_sa2_", x, ".html"), selfcontained = T)

# CQ ---- 

list_cq <- c("Capricornia", "Flynn", "Maranoa")

sa1_s_cq <- sa1_s %>% 
  filter(div_nm %in% list_cq)

sa1_map_s <- sp::merge(sa1_map_org, sa1_s_cq, by = "SA1_7DIG16", all=F, duplicateGeoms = T)

cq_map <- fed_elec[fed_elec$Elect_div %in% list_cq, ]

sa1_map_s_cq <- raster::intersect(sa1_map_s, cq_map)

pal_t_s_cq <- colorBin(c("#990000", "#ff0000", "#ff9999", "white", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s_cq$swing, bins = c(-20, -10, -5, 0, 5, 10, 25))

labels_s_cq <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%",
  sa1_map_s_cq$SA1_7DIG16, round(sa1_map_s_cq$swing,1)) %>% lapply(htmltools::HTML)

m_sa1_s_cq <- leaflet(data = sa1_map_s_cq) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_cq(swing), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_cq,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = cq_map, color = "#696969", weight = 2, opacity = 1, fill = FALSE, label = cq_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_cq, values = c(-20, 20), position = "bottomright")

# saveWidget(m_sa1_s_cq, file="m_sa1_s_cq.html")

# sa2 

cq_map_seat <- fed_elec[fed_elec@data$Elect_div %in% list_cq, ]

sa2_map_s_cq <- raster::intersect(sa2_map_s, cq_map_seat)

labels_s_cq <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
  sa2_map_s_cq$SA2_NAME16, sa2_map_s_cq$swing, round(sa2_map_s_cq$v_t_sa2,0)
) %>% lapply(htmltools::HTML)

m_sa2_s_cq <- leaflet(data = sa2_map_s_cq) %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(fillColor = ~pal_t_s_cq(swing), fillOpacity = 0.5, weight = 0.5, color = "white", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_s_cq,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_t_s_cq, values = c(-20, 20), position = "bottomright") %>% 
  addPolygons(data = cq_map_seat, color = "#696969", weight = 1, opacity = 1, fill = FALSE, label = x, highlight = highlightOptions(weight = 1, color = "black", bringToFront = TRUE))

# saveWidget(m_sa2_s_cq, file="m_sa2_s_cq.html")

# sa1 TPP 

pal_t <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = sa1_map_s_cq$p_lnp_sa1, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

labels_cq <- sprintf(
  "<strong>%s</strong><br/>%g per cent",
  sa1_map_s_cq$SA1_7DIG16, sa1_map_s_cq$p_lnp_sa1
 ) %>% lapply(htmltools::HTML)


m_sa1_cq <- leaflet(data = sa1_map_s_cq) %>%
   addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal_t(p_lnp_sa1), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
         weight = 3,
         color = "white",
         fillOpacity = 1,
         bringToFront = TRUE),
         label = labels_cq,
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "12px",
           direction = "auto")) %>% 
  addPolygons(data = cq_map, color = "white", weight = 2, opacity = 1, fill = FALSE, label = cq_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_t, values = c(0, 100), position = "bottomright")

# saveWidget(m_sa1_cq, file="m_sa1_cq.html")

# sa2 TPP CQ

pal_t <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = sa2_map_s_cq$p_lnp_sa2, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

labels_cq <- sprintf(
  "<strong>%s</strong><br/>LNP Swing: %g %%<br/>Total votes: %g",
  sa2_map_s_cq$SA2_NAME16, round(sa2_map_s_cq$p_lnp_sa2,1), round(sa2_map_s_cq$v_t_sa2,0)
) %>% lapply(htmltools::HTML)

m_sa2_cq <- leaflet(data = sa2_map_s_cq) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal_t(p_lnp_sa2), fillOpacity = 0.5, weight = 0.5, color = "black", smoothFactor = 0, highlight = highlightOptions(
    weight = 3,
    color = "white",
    fillOpacity = 1,
    bringToFront = TRUE),
    label = labels_cq,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")) %>% 
  addPolygons(data = cq_map, color = "white", weight = 2, opacity = 1, fill = FALSE, label = cq_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_t, values = c(0, 100), position = "bottomright")

# saveWidget(m_sa2_cq, file="m_sa2_cq.html")

# CQ state seats

# data

list_cq_qld <- c("CALLIDE", "GREGORY", "ROCKHAMPTON", "KEPPEL", "BURNETT", "MIRANI", "GLADSTONE", "NANANGO")

qld_elec_cq <- qld_elec[qld_elec$NAME %in% list_cq_qld,]

m_cq_state <- leaflet() %>% 
  addProviderTiles("CartoDB") %>%  
  addPolygons(data = qld_elec_cq, color = "red", weight = 1, opacity = 1, fill = FALSE, label = qld_elec_cq$NAME, highlight = highlightOptions(weight = 2, color = "#696969", bringToFront = TRUE), labelOptions = labelOptions(noHide = T,  direction = "center", style = list("font-weight" = "normal", padding = "0px 0px"))) %>% 
  addPolygons(data = cq_map, color = "grey", weight = 0.5, opacity = 0.4, fill = FALSE, label = cq_map$Elect_div, highlight = highlightOptions(weight = 2, color = "#696969", bringToFront = TRUE)) 



# EXPORT ---- 

png("images/p_min_swing.png", width = 6, height = 3, units = "in", res = 300)
p_min_swing
dev.off()

png("images/p_lm_coal.png", width = 6, height = 3, units = "in", res = 300)
p_lm_coal
dev.off()


# write_csv(sa1_s, "C:/Users/matt/Documents/R/sa_map/data/sa1_s.csv")

# write_csv(sa2_s, "C:/Users/matt/Documents/R/sa_map/data/sa2_s.csv")

