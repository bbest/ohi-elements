suppressPackageStartupMessages({
  library(rgdal)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(shiny)
  library(shinydashboard)
  library(markdown)
  library(htmlwidgets) 
  library(jsonlite)
  library(aster) # devtools::install_github('FrissAnalytics/ohi-aster', subdir='asterHTMLwidget')
  #library(visNetwork)
  library(colorspace) # hex()
  library(sunburstR)  # devtools::install_github('timelyportfolio/sunburstR')
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
  

  # elements for sunburstR ----
  
  # set colors for layers (pressures, resilience, status)
  layer_colors = RColorBrewer::brewer.pal(5,'Pastel1') %>%
    .[c(1,3,2)] %>%
    setNames(c('pressure','resilience','status'))
  
  # goals, subgoals 
  subgoals = goals %>% filter(!is.na(parent)) %>% .$goal
  nodes = goals %>%
    mutate(
      id         = goal,
      category   = ifelse(is.na(parent), 'goal', 'subgoal'),
      path       = ifelse(is.na(parent), goal, sprintf('%s-%s', parent, goal))) %>%
    select(id, name, category, path, color, order=order_color, description)
  # add goal-goal element to balance goal-subgoal
  nodes = nodes %>%
    bind_rows(
      nodes %>%
        filter(!id %in% subgoals) %>%
        mutate(
          path = sprintf('%s-%s', id, id)))
  # NOTE: nodes$id repeats!
  
  # layers
  targets_path = nodes %>%
    filter(str_detect(path, '-')) %>%
    select(target=id, parent=path)
  nodes = nodes %>%
    bind_rows(
      # layers for status
      layers_by_target %>%
        filter(target %in% goals$goal) %>% 
        left_join(
          targets_path, by='target') %>% 
        mutate(
          id       = sprintf('s_%s', layer),
          category = 'status',
          path     = sprintf('%s-s_%s', parent, layer),
          order    = 100,
          color    = layer_colors[['status']]) %>%
        select(id, name, category, path, color, order, color, description),
      # layers for pressures
      pressures_matrix %>%
        gather(layer, value, -goal, -element, -element_name) %>%
        filter(!is.na(value)) %>%
        select(target=goal, layer) %>%
        left_join(
          targets_path, by='target') %>%
        # TODO: add individual elements vs for now getting distinct
        distinct() %>%
        left_join(
          layers, by='layer') %>%
        mutate(
          id    = sprintf('p_%s', layer),
          category = 'pressure',
          path     = sprintf('%s-p_%s', parent, layer),
          order    = 200,
          color    = layer_colors[['pressure']]) %>%
        select(id, name, category, path, color, order, color, description),
      # layers for resilience
      resilience_matrix %>%
        gather(layer, value, -goal, -element) %>%
        filter(!is.na(value)) %>%
        select(target=goal, layer) %>%
        left_join(
          targets_path, by='target') %>%
        # TODO: add individual elements vs for now getting distinct
        distinct() %>%
        left_join(
          layers, by='layer') %>%
        mutate(
          id    = sprintf('r_%s', layer),
          category = 'resilience',
          path     = sprintf('%s-r_%s', parent, layer),
          order    = 300,
          color    = layer_colors[['resilience']]) %>%
        select(id, name, category, path, color, order, color, description))
  
  # filter paths to terminal paths
  paths = nodes
  for (pth in paths$path){ # pth = paths$path[29]
    if (
      sum(str_detect(paths$path, sprintf('%s-.*', pth))) > 0 | 
      # remove goal nodes without layers, including goal-goal having subgoals and no goal-goal-layer such as LE,FP have
      str_count(pth, '-') < 2){
      paths = filter(paths, path != pth)
    }
  }
  
  # set layer weight based on matching goals weight based on number of layers
  paths2 = paths %>%
    mutate(
      path1 = str_replace(path, '(.+)-(.+)-(.+)','\\1'),
      path2 = str_replace(path, '(.+)-(.+)-(.+)','\\2')) %>%
    left_join(
      goals %>% 
        select(goal, path2_weight=weight),
      by=c('path2'='goal')) %>%
    left_join(
      goals %>% 
        select(goal, path1_weight=weight),
      by=c('path1'='goal'))
  goals_with_subgoals = goals %>% filter(goal %in% c(goals$parent)) %>% .$goal
  path1_with_goalgoallayer = filter(paths2, path2 %in% goals_with_subgoals) %>% .$path1
  paths = bind_rows(
    # goal-goal-layers or goal-subgoal-layers
    paths2 %>%
      filter(!path1 %in% path1_with_goalgoallayer) %>%
      group_by(path2, path2_weight) %>%
      mutate(
        layer_weight = path2_weight / n()) %>%
      ungroup(),
    # goal-subgoal-layers with goal-goal-layer, eg: FP-FP-s_fp_wildcaught_weight, LE-LE-s_le_sector_weight
    paths2 %>%
      filter(path1 %in% path1_with_goalgoallayer) %>%
      group_by(path1, path1_weight) %>%
      mutate(
        layers_weight = path1_weight / n()) %>%
      ungroup() %>%
      group_by(path2, path2_weight) %>%
      mutate(
        layer_weight = ifelse(
          path2 %in% path1_with_goalgoallayer,
          layers_weight,
          (path2_weight - layers_weight) / n())) %>%
      ungroup())
  
  cols = list(
    range  = c(
      unname(layer_colors),
      distinct(nodes, id, color) %>% .$color), # plotCol(clr)
    domain = c(
      names(layer_colors),
      distinct(nodes, id, color) %>% .$id))
  
  id_order = nodes %>%
    distinct(id, order) %>% 
    bind_rows(
      data_frame(
        id = names(layer_colors),
        order = (-1*length(layer_colors)):-1)) %>%
    arrange(order, id) %>%
    mutate(
      d = row_number(),
      s = sprintf('%s:%d', id, d))
  
  sort_fxn = paste(
    "
    function(a,b){
    abb = {", paste(id_order$s, collapse=',\n    '), "  }
    return abb[a.name] - abb[b.name];
    }
    ",sep="\n")
  
  # save to rdata
  save.image(rdata)
  
} else {
  load(rdata)
}
