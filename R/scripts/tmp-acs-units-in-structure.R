
# SETUP -------------------------------------------------------------------

devtools::load_all(".")
loadd(waterbodies)
library(tigris)
library(tidyverse)
library(sf)
library(snakecase)
library(tidycensus)
library(mapview)
library(tmap)
library(cutr) #devtools::install_github("moodymudskipper/cutr")
library(patchwork) #devtools::install_github("thomasp85/patchwork")

g <- glimpse

options(tigris_use_cache = TRUE,
        tigris_class = "sf")

# GET DATA ----------------------------------------------------------------

wa_counties <- tigris::counties(state = "53", cb = TRUE)


all_census_vars <- tidycensus::load_variables(2017, "acs5", cache = TRUE) %>%
  dplyr::transmute(VARIABLE = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                   VARIABLE_SUBTOTAL = name,
                   LABEL = label,
                   TOPIC = concept)

tenure_by_uis_raw <- get_acs(geography = "tract",table = "B25032",year = 2017, state= "53", county = "033",geometry = TRUE, survey = "acs5") %>%
  dplyr::rename_if(not_sfc, to_screaming_snake_case) %>%
  dplyr::rename(VARIABLE_SUBTOTAL = VARIABLE)


# PREPARE DATA ------------------------------------------------------------

tenure_by_uis_raw %>%
  dplyr::inner_join(all_census_vars, by = "VARIABLE_SUBTOTAL") %>% g


tracts_sf <- tenure_by_uis_raw %>%
  dplyr::filter(VARIABLE_SUBTOTAL %in% "B25032_001") %>%
  dplyr::select(GEOID)

uis_var_role <- tenure_by_uis_raw %>%
  dplyr::inner_join(all_census_vars, by = "VARIABLE_SUBTOTAL") %>%
  dplyr::mutate(VARIABLE_ROLE = dplyr::case_when(
    stringr::str_detect(LABEL, "Total$") ~ "TOTAL",
    stringr::str_detect(LABEL, "2$") ~ "COUNT",
    stringr::str_detect(LABEL, "3 or 4$") ~ "COUNT",
    stringr::str_detect(LABEL, "5 to 9$") ~ "COUNT",
    stringr::str_detect(LABEL, "10 to 19$") ~ "COUNT",
    stringr::str_detect(LABEL, "20 to 49$") ~ "COUNT",
    stringr::str_detect(LABEL, "50 or more$") ~ "COUNT",
    TRUE ~ "OMIT"))

check_uis_var_role <- function(){

  uis_var_role %>%
    sf::st_drop_geometry() %>%
    count(LABEL,VARIABLE_ROLE) %>% print(n=Inf)
}

uis_pct <- uis_var_role %>%
  st_drop_geometry() %>%
  dplyr::group_by(GEOID,NAME,VARIABLE, VARIABLE_ROLE) %>%
  dplyr::summarise(ESTIMATE = sum(ESTIMATE),
                   MOE = tidycensus::moe_sum(MOE, ESTIMATE)) %>%
  dplyr::ungroup() %>%
  tidyr::gather(VALUETYPE, VALUE, ESTIMATE, MOE) %>%
  tidyr::unite("VALUETYPE_ROLE", c(VALUETYPE,VARIABLE_ROLE)) %>%
  tidyr::spread(VALUETYPE_ROLE, VALUE) %>%
  dplyr::transmute(
    GEOID,
    NAME,
    VARIABLE,
    ESTIMATE_SUM = ESTIMATE_COUNT,
    ESTIMATE_TOTAL = ESTIMATE_TOTAL,
    ESTIMATE_PCT = plyr::round_any(ESTIMATE_COUNT/ESTIMATE_TOTAL,.01),
    MOE_SUM = MOE_COUNT,
    MOE_TOTAL,
    MOE_PCT = tidycensus::moe_prop(num = ESTIMATE_COUNT,
                                   denom = ESTIMATE_TOTAL,
                                   moe_num = MOE_COUNT,
                                   moe_denom = MOE_TOTAL)
  )




# READY DATA --------------------------------------------------------------

uis_sf <- tracts_sf %>%
  dplyr::full_join(uis_pct, by = "GEOID") %>%
  st_sf() %>%
  st_transform(2926) %>%
  st_difference(st_union(waterbodies))


wa_counties <- wa_counties %>% st_transform(2926)


wa_counties_ready <- st_crop(wa_counties,st_bbox(wa_counties[wa_counties$NAME %in% "King","geometry"])) %>%
  st_union(by_feature = TRUE) %>%
  st_difference(st_union(waterbodies))

kc_waterbodies <- st_crop(waterbodies,st_bbox(wa_counties[wa_counties$NAME %in% "King","geometry"])) %>%
  st_union()


# VISUALIZE DATA ----------------------------------------------------------


check_histogram_facet <- function(){

  uis_pct_long <- uis_pct %>%
    tidyr::gather(ESTIMATE_TYPE, ESTIMATE, dplyr::matches("ESTIMATE"))


  ggplot(data = uis_pct_long,
         aes(x = ESTIMATE)) +
    geom_histogram() +
    facet_wrap(~ ESTIMATE_TYPE, ncol = 1, scales = "free")


}

check_histogram_pct <- function(){

  breaks <- seq(0,1,by = 0.1)

  uis_hist_count <- uis_pct %>%
    mutate(HIST_GRP = smart_cut(ESTIMATE_PCT,
                                breaks,
                                format_fun = scales::percent,
                                simplify = F,
                                closed = "right")) %>%
    group_by(HIST_GRP) %>%
    mutate(N = n()) %>%
    ungroup()

  ggplot(data = uis_hist_count,
         aes(x = HIST_GRP, fill = HIST_GRP)) +
    geom_bar() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          # axis.text.x = element_blank(),
          legend.position = "none")



}


check_histogram_pct2 <- function(){

  percent_flat <- percent_format(accuracy = 1)

  breaks <- seq(0,1,by = 0.1)

  labels <- map_chr(breaks[1:10], ~ as.character(glue::glue("{percent_flat(.x)} to {percent_flat(.x+0.09)}")))

  uis_hist_sf <- uis_sf %>%
    mutate(HIST_GRP = smart_cut(ESTIMATE_PCT,
                                breaks,
                                labels = labels,
                                format_fun = scales::percent,
                                simplify = F,
                                closed = "right"))

  ggplot(data = uis_hist_sf,
         aes(x = HIST_GRP, fill = HIST_GRP)) +
    geom_bar() +
    theme_minimal() +
    ylab("# of tracts") +
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 12),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none") +
    labs(title = "Multifamily Residences",
                                 subtitle = "As a Share (%) of All Occupied Housing Units per Census Tract")



}


check_mapview <- function(){

  mapview(uis_sf, zcol = "ESTIMATE_PCT", legend = TRUE)


}

check_tmap_pct <- function(){

  brks <- seq(0,1,by = 0.1)

  tmap_mode("plot")

  tm_shape(shp = uis_sf,projection = 2926) +
    tm_polygons("ESTIMATE_PCT",
                palette = "viridis",
                breaks = brks,
                border.col = "transparent",
                title = "Multifamily Residences\nas a Share (%) of All Homes",
                legend.size.is.portrait=FALSE) +
    tm_shape(shp = uis_sf,projection = 2926) +
    tm_borders(col = "black", lwd = 1) +
    tm_layout(frame = FALSE,
              inner.margins = c(0.10, 0.25, 0.10, 0.08)) +
    tm_legend(legend.position = c("left","center"),
              legend.format = list(fun=scales::percent))


}

check_tmap_sum <- function(){


  tmap_mode("plot")

  tm_shape(shp = uis_sf,projection = 2926) +
    tm_polygons("ESTIMATE_SUM",
                palette = "plasma",
                border.col = "transparent",
                title = "Multifamily Residences\n(occupied housing units)",
                legend.size.is.portrait=FALSE) +
    tm_shape(shp = uis_sf,projection = 2926) +
    tm_borders(col = "black", lwd = 1) +
    tm_layout(inner.margins = c(0.10, 0.25, 0.10, 0.08)) +
    tm_legend(legend.position = c("left","center"),
              legend.format = list(fun=scales::comma))


}

check_ggplot_pct_no_legend <- function(){

  percent_flat <- percent_format(accuracy = 1)

  breaks <- seq(0,1,by = 0.1)

  labels <- map_chr(breaks[1:10], ~ as.character(glue::glue("{percent_flat(.x)} to {percent_flat(.x+0.09)}")))

  uis_hist_sf <- uis_sf %>%
    mutate(HIST_GRP = smart_cut(ESTIMATE_PCT,
                                breaks,
                                labels = labels,
                                format_fun = scales::percent,
                                simplify = F,
                                closed = "right"))

  ggplot() +
    geom_sf(data = wa_counties_ready, fill = "grey", color = "white") +
    geom_sf(data = uis_hist_sf, aes(fill = HIST_GRP), color = "transparent") +
    geom_sf(data = uis_hist_sf, fill = "transparent",color = "black") +
    scale_fill_viridis_d() +
    theme_void() +
    coord_sf(datum = NA, expand = FALSE) +
    theme(legend.position="none",
          plot.margin = margin(t = 0,r = -0.1,b = 0,l = 0,unit = "npc"),
          plot.caption = element_text(size = 10)
    ) +
    labs(caption = "Data: ACS Table B25032; 2013 - 2017 5-year span; Census Tracts in King County, WA")


}

check_ggplot_pct_horizontal_legend <- function(){

  percent_flat <- percent_format(accuracy = 1)

  breaks <- seq(0,1,by = 0.1)

  labels <- map_chr(breaks[1:10], ~ as.character(glue::glue("{percent_flat(.x)} to {percent_flat(.x+0.09)}")))

  uis_hist_sf <- uis_sf %>%
    mutate(HIST_GRP = smart_cut(ESTIMATE_PCT,
                                breaks,
                                labels = labels,
                                format_fun = scales::percent,
                                simplify = F,
                                closed = "right"))

  ggplot(data = uis_hist_sf) +
    geom_sf(aes(fill = HIST_GRP), color = "transparent") +
    geom_sf(fill = "transparent",color = "black") +
    scale_fill_viridis_d(guide = guide_legend(
      direction = "horizontal",
      label.position = "bottom",
      nrow = 1
    )) +
    theme_void() +
    coord_sf(datum = NA) +
    theme(legend.title = element_blank(),
          legend.position="top",
          legend.key.height = unit(0.02, "npc"),
          legend.key.width=unit(0.070, "npc"),
          legend.text = element_text(size = 7),
          plot.margin = margin(t = 0.05, b = 0.05, l = 0,r = 0, unit = "npc")
    )


}

plot1 <- check_histogram_pct2()

plot1 <- plot1 + check_ggplot_pct_no_legend()

plot1 <- plot1 + plot_layout(ncol = 1,
                             heights = c(0.33, 1))
