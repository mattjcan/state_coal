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
library(kableExtra)
# library(raster)
library(sf)
library(janitor)


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

q_tpp <- read_csv(paste0(d,"data/tpp17.csv"), skip = 0)

q_pd <- read_csv(paste0(d,"data/pd.csv"), skip = 0)

q_pb <- read_csv(paste0(d,"data/pb17.csv"), skip = 0)

sa1_pl <- read_csv(paste0(d,"data/TPP and TCP by SA1.csv"), skip = 0)

coal_div <- read_csv(paste0(d,"data/coal_sed.csv"), skip = 0)

emp_div <- read_csv(paste0(d,"data/emp_sed.csv"), skip = 0)


sa2_map_org <- readOGR(paste0(d,"data/SA2_2016_AUST.shx"))

sa1_map_org <- readOGR(paste0(d,"data/SA1_2016_AUST.shx"))

qld_elec <- readOGR(paste0(d, "data/State_electoral_boundaries_2017.shx"))



# TIDY ---- 

# tidy

# sa2 data

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

tot_min <- sum(sa2_min$mining, na.rm = T)

sa2_min <- sa2_min %>% 
  filter(emp != 0) %>% 
  mutate(p_emp = mining / emp * 100,
         ind = "Mining") %>% 
  dplyr::select(region, SA2_5DIG16, ind, mining, p_emp) %>% 
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

p_min <- tot_min / tot_aus * 100

# sa1 data

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

# Qld elec employment data

emp_div <- emp_div %>% 
  gather(key = "ind", value = "emp", -div)

emp_div <- emp_div %>% 
  group_by(div) %>% 
  mutate(p = emp / sum(emp, na.rm = T) * 100)

emp_div_t <- emp_div %>% 
  group_by(div) %>% 
  summarise(emp = sum(emp, na.rm = T))

names(emp_div_t) <- c("div", "tot")

coal_div <- left_join(coal_div, emp_div_t, by = "div")

coal_div <- coal_div %>% 
  mutate(p = emp / tot * 100)

coal_div <- coal_div[,c(1:3, 5)]

emp_div <- bind_rows(coal_div, emp_div)

emp_tot <- emp_div %>% 
  filter(!(ind %in% c("Coal Mining")))%>% 
  group_by(ind) %>% 
  summarise(emp = sum(emp, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(p = emp / sum(emp) * 100)

# QLD 2017 TPP 

q_tpp_w <- q_tpp %>% filter(Prop > 50)

q_tpp_w[is.na(q_tpp_w$Party), ]$Party <- "IND"

party_n <- as_tibble(tabyl(q_tpp_w$Party, sort = TRUE))

names(party_n) <- c("party", "n", "p")

party_n <- party_n %>% 
  arrange(-n)

q_tpp_w$NAME <- toupper(q_tpp_w$div17)

q_tpp_w$m <- ifelse(q_tpp_w$Party == "LNP", q_tpp_w$Prop - 50, -1 *(q_tpp_w$Prop - 50))

q_tpp_w <- q_tpp_w %>% 
  arrange(-m)

q_tpp_w_t <- q_tpp_w %>% 
  ungroup() %>% 
  select(div17, Party, m) %>% 
  mutate(m = round(m,2)) %>% 
  rename(div = div17)

coal_t <- coal_div %>% 
  select(div, p) %>%
  mutate(p = round(p,1)) %>% 
  rename(p_coal = p)

min_t <- emp_div %>% 
  filter(ind == "Mining") %>% 
  select(div, p) %>% 
  mutate(p = round(p,1))

q_tpp_w_t <- left_join(q_tpp_w_t, coal_t, by = "div")

q_tpp_w_t <- left_join(q_tpp_w_t, min_t, by = "div")

names(q_tpp_w_t) <- c("Electorate", "Party", "Margin (%)", "Coal Mining (%)", "Mining (%)")

t_pend <- q_tpp_w_t %>% 
  kable("html", escape = F, booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(which(q_tpp_w_t$`Margin (%)` < 0 & q_tpp_w_t$`Margin (%)` > -3.07), bold = T, color = "white", background = "#ff5e5e") 

q_target <- q_tpp_w_t

names(q_target) <- c("Electorate", "Party", "Margin (%)", "coal", "mining")

targ1 <- c("Burdekin", "Whitsunday", "Keppel", "Thuringowa", "Mirani") 
           
targ2 <- c("Rockhampton", "Mackay", "Townsville", "Mundingburra", "Toowoomba North")

q_target$pr <- ifelse(q_target$Electorate %in% targ1, 1, ifelse(q_target$Electorate %in% targ2, 2, 0))

q_target <- q_target %>% 
  filter(pr != 0) %>% 
  select(-pr)

names(q_target) <- c("Electorate", "Party", "Margin (%)", "Coal Mining (%)", "Mining (%)")

t_target <- q_target %>% 
  kable("html", escape = F, booktabs = T) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(which(q_target$Electorate %in% targ1), bold = T, color = "white", background = "#ff5e5e") %>% 
  row_spec(which(q_target$Electorate %in% targ2), bold = T, color = "white", background = "#8080ff") 

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

# sa1 maps

sa1_map_qld <- sa1_map_org[sa1_map_org$STE_NAME16 %in% "Queensland", ]

qld_sa1_join <- gContains(qld_elec, sa1_map_qld, byid = T)

sa1_s_nat_seat <- sa1_s %>% 
  filter(div_nm %in% )

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



