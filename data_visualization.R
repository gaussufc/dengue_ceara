ce_geojson_url <- 'https://raw.githubusercontent.com/tbrugz/geodata-br/master/geojson/geojs-23-mun.json'

ce_geojson <- rjson::fromJSON(file=ce_geojson_url)

dengue_ce_subset <- dengue_ce %>%
  filter(reported_year == 2019) %>%
  group_by(nome_municipio, id_municip) %>%
  summarise(case_incidence = sum(case_incidence)) 

plot_ly() %>%
  add_trace(
    type = "choroplethmapbox",
    geojson = ce_geojson,
    locations = dengue_ce_subset$nome_municipio,
    z = dengue_ce_subset$case_incidence,
    zmin = 0,
    zmax = 300,
    featureidkey = "properties.name"
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 5.25,
      center = list(lon = -38.8941727, lat = -5.576749)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
