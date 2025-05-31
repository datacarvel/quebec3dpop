### Almost entirely taken from a video tutorial by @milos-makes-maps
### https://www.youtube.com/watch?v=qTDf5VVnjMM

### This one by Spencer Schien also helped me in the last few steps
### https://www.youtube.com/watch?v=zgFXVhmKNbU

libs <- c("R.utils", "devtools", "httr", "tidyverse", "R.utils", "httr", "sf", "stars", "rayshader", "av", "magick", "MetBrewer", "colorspace", "crayon")

invisible(lapply(libs, library, character.only = T))

### 1. DOWNLOAD & UNZIP DATA
### ------------------------

url1 <-
  "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_CA_20231101.gpkg.gz"
file_name1 <- "qc-canada-population.gpkg.gz"

get_population_data1 <- function() {
  res <- httr::GET(
    url1,
    write_disk(file_name1),
    progress()
  )
  
  R.utils::gunzip(file_name1, remove = F)
}

get_population_data1()

### 2. LOAD DATA
### -------------

load_file_name1 <- gsub(".gz", "", file_name1)
# Lambert projection

crsLAEA1 <- "+proj=laea +lat_0=60 +lon_0=-106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

get_population_data1 <- function() {
  pop_df1 <- sf::st_read(
    load_file_name1
  ) |>
    sf::st_transform(crs = crsLAEA1)
}

pop_sf1 <- get_population_data1()

head(pop_sf1)
ggplot() +
  geom_sf(
    data = pop_sf1,
    color = "grey10", fill = "grey10"
  )

### ISOLATING QUEBEC

provinces <- st_read("lpr_000b16a_e/lpr_000b16a_e.shp")
quebecbound <- provinces %>% filter(PRENAME == "Quebec")

pop_qc_transf <- st_transform(quebecbound, st_crs(pop_sf1))

pop_qc <- st_intersection(pop_sf1, pop_qc_transf)

### BACK TO THE TUTORIAL

ggplot() +
  geom_sf(
    data = pop_qc,
    fill = "blue", color = NA
  )

# bounding boxes : x for width (longitude), y for height (latitude)
# shp to raster

bb <- sf::st_bbox(pop_qc)

get_raster_size1 <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )

if (height > width) {
  height_ratio <- 1
  width_ratio <- width / height
} else {
  width_ratio <- 1
  height_ratio <- height / width
}

return(list(width_ratio, height_ratio))
  
}


width_ratio <- get_raster_size1()[[1]]
height_ratio <- get_raster_size1()[[2]]

size <- 6000
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

get_population_raster1 <- function() {
  pop_rast1 <- stars::st_rasterize(
    pop_qc |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  return(pop_rast1)
}

pop_rast1 <- get_population_raster1()

pop_mat1 <- pop_rast1 |>
  as("Raster") |>
  rayshader::raster_to_matrix()

cols1 <- c(
  "#00429d", "#3c85ba",
  "#75c9d3", "#ffffe0"
)
cols11 <- rev(cols1)

cols111 <- rev(c(
  "darkblue", "blue", "#25d7ec"
))

colsGREEN <- rev(c(
  "green", "#aaff7b", "#c9fc9f"
))

colsPINK <- rev(c(
  "#e00350", "#800f83", "#41199e"
))

c1 <- met.brewer("Hokusai2")
c2 <- met.brewer("Tam")

swatchplot(c2) #useful to see your palette as a plot

# just trying multiple color palettes
# #7FF9F7FF, #0B2483FF, #0F8CDEFF, #4C4CFFFF, #040509FF, #23B4DCFF, #3C7084FF
texture1 <- grDevices::colorRampPalette(cols1)(256)
texture2 <- grDevices::colorRampPalette(c1, bias = 2)(256)
texture3 <- grDevices::colorRampPalette(c1[2:4], bias = 2)(50)
texture4 <- grDevices::colorRampPalette(c2[1:5], bias = 2)(256)
#"#D8D97A" "#C8D377" "#B8CE74" "#A8C971" "#99C46E" "#0A2E57"
swatchplot(texture4)

pop_mat1 |>
  rayshader::height_shade(texture = texture4) |>
  rayshader::plot_3d(
    heightmap = pop_mat1,
    solid = F,
    zscale = 20, # ratio between z and y and z, if z decrease = taller spikes and vice versa, 0 to 100
    shadowdepth = 0,
    windowsize = c(800, 800)
  )

# adjusting camera view, useful to get back to exactly what you want after manually playing with the view

render_camera(theta = 45, phi = 50, zoom = .6, shift_vertical = -150, fov = 5)

# og size = 1080 x 800

outfile <- "images/30maiAdjust1.png"
{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
render_highquality(
  filename = outfile,
  lightdirection = 9,
  lightaltitude = c(30, 80),
  lightintensity = c(800, 200),
  lightcolor = c(c2[3],"white"),
  interactive = F,
  samples = 450,
  width = 1080 * 10,
  height = 800 * 5
)
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

# Doing a simple snapshot of your current view

render_snapshot(filename = "23mai1", software_render = TRUE, width = 6400, height = 3600, background = "#152d34")

# Making a video of your rotating view

render_movie(
  filename = "orangecamtest1", 
  fps = 60, 
  vignette = T,
  vignette_color = "turquoise",
  title_color = "white",
  title_bar_color = "purple",
  title_position = "north"
  )


