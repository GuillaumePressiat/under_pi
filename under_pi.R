require(sp)
require(rgeos)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

x <- 0.5 # center x
y <- 0.5 # center y
n <- 1000 # nr of pts
r <- 0.5 # radius
# pts <- seq(0, 2 * pi, length.out = n)
# plot(sin(pts), cos(pts), type = 'l', asp = 1) # test
# 
# 
#   xy <- cbind(c(cos(pi/3), cos(- pi / 3), cos(pi)), c(sin(pi/3), sin(- pi/3), sin(pi)))
#   sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
#   plot(sq, add = TRUE)
# 
# # xy <- cbind(c(-1, 1, 1, -1), c(-1, -1, 1, 1))
# # sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
# # plot(sq, add = TRUE)
# 
# xy <- cbind(c(cos(pi/2), cos(pi), cos(3*pi/2), cos(0)), c(sin(pi/2), sin(pi), sin(3*pi/2), sin(pi)))
# sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
# plot(sq, add = TRUE)
# 
# xy <- cbind(c(cos(pi/4), cos(2 * pi/4), cos(3 * pi / 4), cos(4*pi/4),
#               cos(5*pi/4),cos(6*pi/4),cos(7*pi/4),cos(0)), 
#             c(sin(pi/4), sin(2 * pi/4), sin(3 * pi / 4), sin(4*pi/4),
#               sin(5*pi/4),sin(6*pi/4),sin(7*pi/4), sin(pi)))
# sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
# plot(sq, add = TRUE)
# 
# xy <- cbind(c(cos(2 * pi/5), cos(4 * pi/5), cos(6 * pi / 5), cos(8*pi/5),
#               cos(0)), 
#             c(sin(2 *pi/5), sin(2 *2 * pi/5), sin(2 *3 * pi / 5), sin(2 *4*pi/5),
#               sin(pi)))
# sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
# plot(sq, add = TRUE)
# 
# xy <- cbind(c(cos(1 * pi/6), cos(2 * pi/6), cos(3 * pi / 6), cos(4*pi/6),cos(5*pi/6),cos(6*pi/6),
#               cos(7*pi/6), cos(8*pi/6),cos(9*pi/6),cos(10*pi/6),cos(11*pi/6), cos(0)), 
#             c(sin(1 *pi/6), sin(1 *2 * pi/6), sin(1 *3 * pi / 6), sin(1 *4*pi/6), sin(1 *5*pi/6),sin(1 *6*pi/6),
#               sin(7*pi/6), sin(8*pi/6),sin(9*pi/6),sin(10*pi/6),sin(11*pi/6), sin(pi)))
# 
# sq <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "polygon")))
# plot(sq, add = TRUE)

n = 6
poly <- function(n){
  library(dplyr, warn.conflicts = FALSE)
  j <- n %% 2
  theta <- (2 - j):(2 * (n) + 1)
  
  u <- tibble(theta = theta, x = cos(theta * 2 * pi / n), y = sin(theta * 2 * pi / n))
  
  v <- SpatialPolygons(list(Polygons(list(Polygon(cbind(u$x,u$y))), "polygon")))
  
  v <- ggplot2::fortify(v) %>% mutate(n = n)
  
  return(list(df = u, poly = v))
}

n = 12
plot(poly(n)$df$x,poly(n)$df$y, type = 'l')

x <- 0.5 # center x
y <- 0.5 # center y
n <- 1000 # nr of pts
r <- 0.5 # radius
pts <- seq(0, 2 * pi, length.out = n)
plot(sin(pts), cos(pts), type = 'l', asp = 1) # test

# p1 <- ggplot() + 
#   geom_point(aes(cos(pts), sin(pts)), col = 'cornflowerblue') + 
#   theme_void()
# 
# p2 <- ggplot() + 
#   geom_point(aes(cos(pts), sin(pts)), col = 'cornflowerblue') + 
#   theme_void()
# 
# for (n in 3:30){
#   p2 <- p2 +
#     geom_polygon(data = poly(n)$poly, aes(x = long, y = lat), col = 'grey30') +
#     ggtitle(paste0('~ Pi : ', 1 * sqrt(2 - 2 * cos(pi / n)) * n))
#   print(p2)
#   Sys.sleep(1.3)
#   p <- p1 +
#     geom_polygon(data = poly(n)$poly, aes(x = long, y = lat), col = 'grey30', lwd = 1.7) +
#     ggtitle(paste0('n = ', n, ' - ~ Pi : ', 1 * sqrt(2 - 2 * cos(pi / n)) * n))
#   print(p)
#   Sys.sleep(1.5)
# }

nmax = 60
alls <- 3:nmax %>% purrr::map(function(x)poly(x)$poly) %>% bind_rows()  %>% 
  mutate(estimate = round(sqrt(2 - 2 * cos(pi / n)) * n, 9))


circles <- bind_rows(lapply(tibble(x = cos(pts), y = sin(pts)), rep, nmax - 2 )) %>% 
  mutate(n = rep(3:nmax, each = 1000)) %>% 
  as_tibble() %>% 
  mutate(estimate = round(sqrt(2 - 2 * cos(pi / n)) * n, 9),
         estimate = paste0('Pi estimate : ', estimate, '\n', 'difference : ', round(pi - estimate, 9)))

library(ggplot2)
library(gganimate)
p <- ggplot() +
  geom_point(data = circles, aes(x = x, y = y), col = 'cornflowerblue', lwd = 3, alpha = 0.5) + 
  theme_void() + 
  geom_polygon(data = alls, aes(x = long, y = lat, group = n), col = 'grey30', lwd = 1.3, fill = 'cornflowerblue', alpha = 0.5) + 
  geom_text(data = distinct(circles, n, .keep_all = TRUE), aes(x = 0, y = 0, label = estimate), size = 6) + 
  labs(title = 'Segments number : {frame_time}') + 
  transition_time(n)

image <- animate(p, nframes =  nmax - 2, fps = 6)
anim_save('~/Documents/Developpements/Github/under_pi/under_pi.gif', image)

# ggplot(data = ggplot2::fortify(sq)) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = 'white', col = 'grey50')
# 
# # http://mathcentral.uregina.ca/QQ/database/QQ.09.07/h/lindsay2.html
# 1 * sqrt(2 - 2 * cos(pi / 3)) * 3
# 1 * sqrt(2 - 2 * cos(pi / 10)) * 10
# 
# 1 * sqrt(2 - 2 * cos(pi / 20)) * 20
# 
# plot(p, add = TRUE)
# sq <- SpatialPolygons(list(Polygons(list(Polygon(p)), "polygon")))
