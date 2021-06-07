ce_geojson_url <- 'https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-23-mun.json'

ce_geojson <- rjson::fromJSON(file=ce_geojson_url)

dengue_ce_subset <- dengue_ce %>%
  filter(reported_year == 2019) %>%
  group_by(nome_municipio, id_municip) %>%
  summarise(case_incidence = sum(case_incidence)) 

palette <- tibble(z = c(0, 0.33, 0.33, 0.66, 0.66, 1),
                  color = c("038927", "038927", "FFC908", "FFC908","#FF6F0A", "FF6F0A"))

plot_ly() %>%
  add_trace(
    type = "choroplethmapbox",
    geojson = ce_geojson,
    locations = dengue_ce_subset$nome_municipio,
    z = dengue_ce_subset$case_incidence,
    zmin = 0,
    zmax = 300,
    featureidkey = "properties.name",
    colorscale = palette,
    marker = list(line = list(color = "white",
                              width = 0.55))
  ) %>%
  layout(
    title = "Incidência de casos de Dengue em 2019 por 100 mil habitantes",
    mapbox = list(
      style = "carto-positron",
      zoom = 6,
      center = list(lon = -39.3767561, lat = -5.1746363)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
