suppressPackageStartupMessages({
  library(rgdal)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(shiny)
  library(shinydashboard)
  library(markdown)
  library(htmlwidgets) 
  library(jsonlite)
  library(aster) # devtools::install_github('FrissAnalytics/ohi-aster', subdir='asterHTMLwidget')
  library(visNetwork)
  library(colorspace) # hex()
})

# load rdata for faster startup
rdata = 'data/default.Rdata'
if (!file.exists(rdata)){
  # TODO: enable drop-down for selecting alternate scenarios
  # TODO: could have github dir, check for latest updates, and enable selection by commit
  
  # get selectable layers ----
  dir_scenario = './data/ohi-global/eez2015'
  
  # read goals and layers
  layers = read_csv(file.path(dir_scenario, 'layers.csv'))
  goals  = read_csv(file.path(dir_scenario, 'conf/goals.csv'))
  scores = read_csv(file.path(dir_scenario, 'scores.csv')) # AO,BD,CP,CS,CW,ECO,FIS,FP,HAB,ICO,Index,LE,LIV,LSP,MAR,NP,SP,SPP,TR
  dims   = read_csv(file.path(dir_scenario, 'conf/dimensions.csv'))
  pressures_matrix  = read_csv(file.path(dir_scenario, 'conf/pressures_matrix.csv'))
  resilience_matrix = read_csv(file.path(dir_scenario, 'conf/resilience_matrix.csv'))
  
  
  # read spatial, custom simplified using rmapshaper
  # TODO: set the path spatial for rgn_id in config.R, which should be converted to YAML with spatials registered
  # skipping other spatial fields: 'saup_id','fao_id' # note: 'cntry_key' for www2013, 'country_id' for nature2012
  rgns = readOGR('./data/rgn_offshore_gcs_mapshaper-simplify_x2_eez-only.geojson', 'OGRGeoJSON', verbose = F)
  
  goals_colors = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(nrow(goals))
  goals = goals %>%
    arrange(order_color) %>%
    mutate(color = goals_colors)
  
  # prep output score data ----
  output_goals = c(
    '0 Index'='Index', 
    setNames(
      goals$goal, 
      sprintf('%g %s (%s)', goals$order_hierarchy, goals$name, goals$goal)))
  
  # prep input layer data ----
  
  # filter to layers having rgn_id and numeric values
  layers = layers %>%
    filter(!is.na(fld_val_num)) %>%     # TODO: fix to use fld_val_chr for layers (n=5, ICO spatial): ico_spp_extinction_status, ico_spp_popn_trend, rgn_georegion_labels, rgn_global, rgn_labels
    filter(!is.na(fld_id_num)) %>%      # TODO: fix to use fld_id_chr=='cntry_key'(n=24, LE MAR TR) } or fld_id_chr=='fao_saup_id'(n=1, FIS)
    filter(fld_id_num == 'rgn_id') %>%  # TODO: fix to use other fld_id_num. so far only layers (n=2, FS): fis_b_bmsy, fis_proparea_saup2rgn
    arrange(layer)
  
  # get layers by target goal
  layers_by_target = layers %>%
    select(layer, targets) %>%
    separate(targets, c('target1','target2','target3'), ' ', fill='right') %>%
    gather(target_num, target, target1:target3) %>%
    select(layer, target) %>%
    filter(!is.na(target)) %>%
    left_join(layers, by='layer')
  
  # get available targets
  layer_targets = layers_by_target %>%
    distinct(target) %>%
    select(target) %>%
    left_join(
      bind_rows(
        goals %>%
          select(target = goal, order_hierarchy, name),
        data.frame(
          target = c('pressures','resilience','spatial'),
          order_hierarchy = c(100, 101, 102),
          name = c('pressures','resilience','spatial'))),
      by='target') %>%
    arrange(order_hierarchy, target) %>%
    select(target, order_hierarchy, name) %>%
    mutate(target_label = sprintf('%g %s (%s)', order_hierarchy, name, target))
  
  # paste data together for later selecting appropriate category and year values
  if (exists('d_lyrs')) rm(d_lyrs)
  for (i in 1:nrow(layers)){ # i=74
    # layers[i,] %>% select(-description, -fld_id_chr, -fld_val_chr)
    
    # read in data layer
    d = read_csv(file.path(dir_scenario, 'layers', layers$filename[i]))
    
    # convert to lower names
    names(d) = tolower(names(d))
    
    # rename fld_id_num, fld_val_num
    d = rename_(d, 'fld_id_num' = layers$fld_id_num[i], 'fld_val_num' = layers$fld_val_num[i])
    
    # rename fld_category
    if(!is.na(layers$fld_category[i])){
      d = rename_(d, 'fld_category' = layers$fld_category[i])
      d$fld_category = as.character(d$fld_category)
    } else {
      d$fld_category = NA
    }
    
    # rename fld_year
    if(!is.na(layers$fld_year[i])){
      d = rename_(d, 'fld_year' = layers$fld_year[i])
    } else {
      d$fld_year = NA
    }
    
    # set layer name
    d = d %>%
      mutate(layer = layers$layer[i]) %>%
      select(layer, fld_category, fld_year, fld_id_num, fld_val_num) %>%
      arrange(layer, fld_category, fld_year, fld_id_num)
    
    # bind rows
    if (exists('d_lyrs')){
      d_lyrs = bind_rows(d_lyrs, d)
    } else {
      d_lyrs = d
    }
  }
  
  # prep nodes and edges for network Components ----
  
  # goals to subgoals 
  nodes = goals %>%
    mutate(
      group = ifelse(is.na(parent), 'goal', 'subgoal'),
      id    = goal,
      title = sprintf('<strong>%s</strong>: %s', goal, name)) %>%
    select(id, label=goal, group, title)
  
  edges = goals %>%
    filter(!is.na(parent)) %>%
    select(from = parent, to = goal)
  
  # goals to index
  nodes = nodes %>%
    bind_rows(data_frame(
      id = 'Index',
      label = 'Index',
      group = 'Index',
      title = 'Index'))
  
  edges = edges %>%
    bind_rows(
      goals %>%
        filter(is.na(parent)) %>%
        select(to = goal) %>%
        mutate(from = 'Index'))
  
  # layers for status
  status_layers = layers_by_target %>%
    filter(!target %in% c('pressures', 'resilience', 'spatial')) %>%
    mutate(
      group = 'layer for status',
      id    = sprintf('%s-%s'  , target, layer),
      label = sprintf('%s - %s', target, layer),
      title = sprintf('<strong>%s</strong>: %s', layer, name)) %>%
    select(id, label, group, title, target)
  
  nodes = nodes %>%
    bind_rows(
      status_layers %>%
        select(id, label, group, title))
  
  edges = edges %>%
    bind_rows(
      status_layers %>%
        select(from = target, to = id))
  
  # layers for pressures
  pressure_layers = pressures_matrix %>%
    gather(layer, value, -goal, -element, -element_name) %>%
    filter(!is.na(value)) %>%
    select(goal, layer) %>%
    # TODO: add individual elements vs for now getting distinct
    distinct() %>%
    left_join(layers, by='layer') %>%
    select(goal, layer, name) %>%
    mutate(
      group = 'layer for pressures',
      id    = sprintf('%s-p-%s'  , goal, layer),
      label = sprintf('%s -p- %s', goal, layer),
      title = sprintf('<strong>%s</strong>: %s', layer, name)) %>%
    select(id, label, group, title, goal, layer)
  
  nodes = nodes %>%
    bind_rows(
      pressure_layers %>%
        select(id, label, group, title))
  
  edges = edges %>%
    bind_rows(
      pressure_layers %>%
        select(from = goal, to = id))
  
  # layers for resilience
  resilience_layers = resilience_matrix %>%
    gather(layer, value, -goal, -element) %>%
    filter(!is.na(value)) %>%
    select(goal, layer) %>%
    # TODO: add individual elements vs for now getting distinct
    distinct() %>%
    left_join(layers, by='layer') %>%
    select(goal, layer, name) %>%
    mutate(
      group = 'layer for resilience',
      id    = sprintf('%s-r-%s'  , goal, layer),
      label = sprintf('%s -r- %s', goal, layer),
      title = sprintf('<strong>%s</strong>: %s', layer, name)) %>%
    select(id, label, group, title, goal, layer)
  
  nodes = nodes %>%
    bind_rows(
      resilience_layers %>%
        select(id, label, group, title))
  
  edges = edges %>%
    bind_rows(
      resilience_layers %>%
        select(from = goal, to = id))
  
  # order legend: Index > goal > subgoal > layer for status > layer for pressures > layer for resilience
  nodes = nodes %>%
    mutate(
      order = c(
        'Index' = 1, 
        'goal'  = 2,
        'subgoal' = 3,
        'layer for status' = 4,
        'layer for pressures' = 5,
        'layer for resilience' = 6)[group]) %>%
    arrange(order, label)
  
  # edges = edges %>%
  #   left_join(nodes, by=c('from','id')) %>%
  #   select(from, to, from_id=id, from_label=label, from_group=group) %>%
  #   left_join(nodes, by=c('to','id')) %>%
  #   select(from, to, to_id=id, to_label=label, to_group=group) %>%
  #   
  #   arrange()
  
  # save to rdata
  save.image(rdata)
  
} else {
  load(rdata)
}