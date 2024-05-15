calc_flows <- function(){

  ode_arrows <- ode_flows %>%
    left_join(ode_data, by = c("to"="name")) %>%
    rename_with(\(.x) paste0(.x, "end"), .cols = c(x,y)) %>%
    select(-matches("^state_")) %>%
    left_join(ode_data, by = c("from"="name")) %>%
    group_by(name, from, to) %>%
    summarize(cpts = connection_points(x, y, xend, yend, state_width, state_height)) %>%
    unnest(cpts) %>%
    mutate(x_mid = (x + xend)/2,
           y_mid = (y + yend)/2,
           slope = (yend - y)/(xend-x),
           theta = ifelse(slope < 0, atan(slope)-3*pi/2, atan(slope)+3*pi/2),
           metrics = list(systemfonts::string_metrics_dev(from))) %>%
    unnest(metrics) %>%
    mutate(box = list(box_xy(x_mid, y_mid, 0.08*width/0.882, 0.07*ascent/.318, theta))) %>%
    ungroup() %>%
    mutate(box = lapply(box, \(.x) rename(.x, bx_x = x, bx_y = y)))

}
