circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

theme_court = function(base_size = 16, bg_color = "white") {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#C0C0C0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

## function that defines court
make_court = function(){
  width = 50
  height = 94 / 2
  key_height = 19
  inner_key_width = 12
  outer_key_width = 16
  backboard_width = 6
  backboard_offset = 4
  neck_length = 0.5
  hoop_radius = 0.75
  hoop_center_y = backboard_offset + neck_length + hoop_radius
  three_point_radius = 23.75
  three_point_side_radius = 22
  three_point_side_height = 14
  
  # define court
  court_points = data.frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2, 
          outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2,
          -backboard_width / 2, backboard_width / 2, 
          0, 0),
    y = c(height, 0, 0, height, height, 0, key_height, key_height, 0,
          backboard_offset, backboard_offset, 
          backboard_offset, backboard_offset + neck_length),
    desc = c(rep("perimeter", 5), rep("outer_key", 4), rep("backboard", 2),
             rep("neck", 2))
  )
  
  # define foul circle
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
  foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom")
  
  # define hoop
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop") 
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  # define 3-point line
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
  three_point_line = data.frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted, three_point_line)
  court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))
  
  
  ## plot
  court = ggplot() +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc, linetype = dash),
              color = "black") +
    scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
    theme_court(base_size = 22)
  
  court  
}