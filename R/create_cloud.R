#
# max_length <- function(...){
#   max(
#     sapply(
#       list(...),
#       length)
#   )
# }
#
# check_length <- function(variable, max_len){
#   if(length(variable) != max_len){
#     return(rep(variable[1], max_len))
#   }else{
#     return(variable)
#   }
# }
#
# calc_ellipse <- function(segments = 51, a = 2, b = 1,
#                          x_offset = 0, y_offset = 0, theta_rot = 0,
#                          theta_begin = 0, theta_end = 2*pi){
#
#   max_len <- max_length(segments, a, b, x_offset, y_offset, theta_rot,
#                         theta_begin, theta_end)
#
#   segments <- check_length(segments, max_len)
#   a <- check_length(a, max_len)
#   b <- check_length(b, max_len)
#   x_offset <- check_length(x_offset, max_len)
#   y_offset <- check_length(y_offset, max_len)
#   theta_rot <- check_length(theta_rot, max_len)
#   theta_begin <- check_length(theta_begin, max_len)
#   theta_end <- check_length(theta_end, max_len)
#
#   # Handle cases where theta_begin is below x-axis
#   theta_begin[theta_begin > theta_end] <- theta_begin[theta_begin > theta_end] - 2*pi
#   theta_rot <- theta_rot + theta_begin
#   theta_end <- theta_end - theta_begin
#   theta_begin[] <- 0
#
#   ellipse <- vector(mode = "list", length = max_len)
#
#   for(i in 1:max_len){
#
#     angles <- (theta_begin[i]:segments[i])*theta_end[i]/segments[i]
#
#     # Theoretically, 2*pi should equal 0, but use 0 for end of last
#     # segment to avoid rounding errors and ensure last segment ends
#     # exactly where first segment began
#     if(theta_begin[i] == 0 & theta_end[i] == 2*pi){
#       angles[length(angles)] <- angles[1]
#     }
#
#     # Ellipse coordinates centered on origin
#     x_origin <- a[i]*cos(angles)
#     y_origin <- b[i]*sin(angles)
#
#     # Rotate ellipse by theta_rot and shift center to (x_offset, y_offset)
#     ellipse[[i]] <- data.frame(
#       x = x_origin*cos(theta_rot[i]) - y_origin*sin(theta_rot[i]) + x_offset[i],
#       y = x_origin*sin(theta_rot[i]) + y_origin*cos(theta_rot[i]) + y_offset[i]
#     )
#
#   }
#
#   if(length(ellipse) == 1){
#     return(ellipse[[1]])
#   }else{
#     return(ellipse)
#   }
# }
#
# circ_intersect <- function(x1 = 1, y1 = 1, r1 = 1,
#                            x2 = 0, y2 = 0, r2 = 2){
#   # Based on derivation from:
#   #  https://math.stackexchange.com/questions/256100/how-can-i-find-the-points-at-which-two-circles-intersect
#   d <- dist(matrix(c(x1, x2, y1, y2), nrow = 2))
#   rdiff <- abs(r1 - r2)
#   rsum <- abs(r1 + r2)
#   if(d > rsum | d == 0){
#     # No intersection points
#     x0 <- NA_real_
#     y0 <- NA_real_
#   }else{
#     l = (r1^2-r2^2+d^2)/(2*d)
#     h = sqrt(r1^2-l^2)
#     if(d == rsum){
#       # One intersection point
#       x0 <- l*(x2 - x1)/d + h*(y2 - y1)/d + x1
#       y0 <- l*(y2 - y1)/d - h*(x2 - x1)/d + y1
#     }else if(rdiff <= d & d<= rsum){
#       # Two intersection points
#       x0 <- c(
#         l*(x2 - x1)/d + h*(y2 - y1)/d + x1,
#         l*(x2 - x1)/d - h*(y2 - y1)/d + x1
#       )
#       y0 <- c(
#         l*(y2 - y1)/d - h*(x2 - x1)/d + y1,
#         l*(y2 - y1)/d + h*(x2 - x1)/d + y1
#       )
#     }
#   }
#   return(data.frame(x = x0, y = y0))
# }
#
# calc_theta <- function(c_x, c_y, c_r, x, y){
#
#   theta <- acos((x - c_x)/c_r)
#
#   theta[y < c_y] <- 2*pi - theta[y < c_y]
#
#   return(theta)
# }
#
# x_cloud <- 0
# y_cloud <- 0
# theta <- 0
# height <- 1.8
# width <- 4
#
# el_orig <- calc_ellipse(segments = 10,
#                         a = width/2,
#                         b = height/2,
#                         x_offset = x_cloud,
#                         y_offset = y_cloud) %>%
#   head(-1) %>%
#   mutate(cnum = 1:n())
#
# r_circ <- width/4*0.8
#
# circs <- el_orig %>%
#   rename_with(~str_c(., "_el"), .cols = c(x, y)) %>%
#   rowwise() %>%
#   mutate(circles = list(
#     calc_ellipse(a = r_circ, b = r_circ, x_offset = x_el, y_offset = y_el)
#   )) %>%
#   unnest(circles)
#
# circs <-  el_orig %>%
#   rename(x2 = x, y2 = y) %>%
#   mutate(cnum = ifelse(cnum == max(cnum), 1, cnum + 1)) %>%
#   full_join(el_orig) %>%
#   rename(x1 = x, y1 = y) %>%
#   arrange(cnum) %>%
#   select(cnum, x1, y1, x2, y2) %>%
#   rowwise() %>%
#   mutate(int_pts = list(circ_intersect(x1, y1, r1 = r_circ,
#                                        x2, y2, r2 = r_circ))) %>%
#   unnest(int_pts) %>%
#   slice(chull(select(., x, y))) %>%
#   rename(x_begin = x, y_begin = y) %>%
#   arrange(cnum) %>%
#   mutate(x_end = c(tail(x_begin, -1), head(x_begin, 1)),
#          y_end = c(tail(y_begin, -1), head(y_begin, 1))) %>%
#   mutate(theta_begin = calc_theta(c_x = x1, c_y = y1, c_r = r_circ,
#                                   x = x_begin, y = y_begin),
#          theta_end = calc_theta(c_x = x1, c_y = y1, c_r = r_circ,
#                                 x = x_end, y = y_end)) %>%
#   mutate(arc = calc_ellipse(a = r_circ, b = r_circ,
#                             x_offset = x1, y_offset = y1,
#                             theta_begin = theta_begin,
#                             theta_end = theta_end)) %>%
#   unnest(arc)
#
# circs %>%
#   select(x, y) %>%
#   deparse() %>%
#   clipr::write_clip()
#   ggplot(aes(x = x, y = y)) +
#   geom_polygon(fill = "white", color = "black", linewidth = 2)+
#   coord_fixed()
