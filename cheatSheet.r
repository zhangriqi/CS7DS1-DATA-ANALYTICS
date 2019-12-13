library(ggplot2)
data('midwest',package = 'ggplot2')
midwest = read.csv("http://goo.gl/G1K41K")
# Continuous
a <- ggplot(midwest, aes(poptotal))
a + geom_area(stat="bin", bins=5)
a + geom_density(kernel="gaussian")
a + geom_dotplot(alpha = 0.5, fill = 'blue', color = 'green')
a + geom_freqpoly(color = 'pink')
a + geom_histogram(binwidth = 10) 

# Discrete
#b <- ggplot(midwest, aes(county))
b <- ggplot(mpg, aes(fl))
b + geom_bar(color = 'purple')

c <- ggplot(midwest, aes(area, poptotal))
c + geom_polygon()

d <- ggplot(economics, aes(date, unemploy))
d + geom_path(lineend = "butt", linejoin = "round", linemitre = 1)
d + geom_ribbon(aes(ymin=unemploy-900, ymax=unemploy+900), color = 'red', fill = 'white')

e <- ggplot(seals, aes(x=lat, y=long))
e + geom_segment(aes(xend=lat+delta_lat, yend=long+delta_long), color='green')
e + geom_rect(aes(xmin = long, ymin  = lat, xmax = long+delta_long, ymax = lat+delta_lat), color = 'yellow', fill = 'red')

f <- ggplot(mpg, aes(cty, hwy))
f + geom_blank()
f + geom_jitter(fill = 'red', color = 'red', shape='square')
f + geom_point(fill = 'red', color = 'blue', size=4)
f + geom_quantile(color = 'brown')
f + geom_rug(sides = 'bl', color = 'red')
f + geom_smooth(model = lm, color = 'green')
f + geom_text(aes(label = cty), alpha = 0.5, color = 'pink')

g <- ggplot(mpg, aes(class, hwy))
g + geom_bar(stat='identity', color = 'white')
g + geom_boxplot(color = 'green')
g + geom_dotplot(binaxis = 'y', stackdir = 'center', color = 'purple', binwidth = 0.7)
g + geom_violin(scale='area', color = 'green', fill = 'blue')

h <- ggplot(diamonds, aes(cut, color))
h + geom_jitter()

i <- ggplot(midwest, aes(PID, area))
i + geom_bin2d(binwidth = c(5, 0.5))
i + geom_density2d(color = 'brown')
i + geom_hex(color = 'green', fill = 'blue')

j <- ggplot(economics, aes(date, unemploy))
j + geom_area(color = 'pink', fill = 'purple')
j + geom_line(color = 'red', linetype='longdash', size = 2)
j + geom_step(direction = 'hv', color = 'purple')

df <- data.frame(grp = c('A', 'B'), fit = 4:5, se = 1:2)
k <- ggplot(df, aes(grp, fit, ymin=fit-se, ymax=fit+se))
k + geom_crossbar(fatten=2, color = 'pink', alpha = 0.4, linetype = 'twodash')
k + geom_errorbar(color = 'red', alpha = 1, linetype = 'dotdash')
k + geom_linerange(color = 'green', size = 5)
k + geom_pointrange(color = 'brown', fill = 'brown', alpha = 0.7)

data <- data.frame(murder = USArrests$Murder,
                   state = tolower(rownames(USArrests)))
map <- map_data('state')
l <- ggplot(data, aes(fill=murder))
l + geom_map(aes(map_id = state), map = map) + 
    expand_limits(x = map$long, y = map$lat)

seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))
m <- ggplot(seals, aes(long, lat))
m + geom_contour(aes(z=z), size = 0.5, color = 'green')
m + geom_raster(aes(fill=z), hjust=0.5, vjust=0.5, interpolate = FALSE)
m + geom_tile(aes(fill=z))

n <- b + geom_bar(aes(fill = fl))
n
n + scale_fill_manual(
  values = c('skyblue', 'royalblue', 'blue', 'navy'),
  limits = c('d', 'e', 'p', 'r'), breaks = c('d', 'e', 'p', 'r'),
  name = 'fuel', labels = c('D', 'E', 'P', 'R')
)
n + scale_fill_brewer(palette = 'Blues')
n + scale_fill_grey(start = 0.2, end = 0.8, na.value='red')

p <- f + geom_point(aes(shape=fl), color = 'navy')
p + scale_shape(solid = FALSE)
p + scale_shape_manual(values = c(3:7))

q <- f + geom_point(aes(size=cyl), color = 'skyblue', size = 3)
q + scale_size_area(max_size = 6)


# Coordinate Systems
r <- b + geom_bar()
r + coord_cartesian(xlim = c(0,5))
r + coord_fixed(ratio = 1/2)
r + coord_flip()
r + coord_polar(theta = 'x', direction=2)
r + coord_trans(y = 'sqrt') #What's the difference compared to coord_cartesian()

#Position Adjustments
#s <- ggplot(midwest, aes(county, fill = category))
s <- ggplot(mpg, aes(fl, fill=drv))
s + geom_bar(position = 'dodge')
s + geom_bar(position = 'fill') #stack & normalize height
s + geom_bar(position = 'stack') #stack
f + geom_point(position = 'jitter') #add reandom noise to X and Y to avoid overfitting
#compare with geom_point() without position parameter
f + geom_jitter()
s + geom_bar(position = position_dodge(width=0.9))

# Faceting
t <- ggplot(mpg, aes(cty, hwy)) + geom_point()
t + facet_grid(.~fl)
t + facet_grid(year~.)
t + facet_grid(year ~fl)
t + facet_wrap(~fl)
t + facet_grid(year~., scales = 'free')
t + facet_grid(.~fl, labeller = label_both) # to adjust facet labels
t + facet_grid(.~fl, labeller = label_bquote(alpha ^ .(x)))
t + facet_grid(.~fl, labeller = label_parsed)

# Labels
t + ggtitle("New Plot Title Testing")
t + xlab("New X label")
t + ylab("New Y label")
t + labs(title = "New title", X = "New X_cty", Y = "New Y_hwy")

# Legends
s + geom_bar(position = 'dodge') + theme(legend.position = 'left')
s + guides(color = 'none')
s + geom_bar(position = 'dodge') + scale_fill_discrete(name = 'Title', labels = c('A','B','C'))

# Themse
r + theme_bw()
r + theme_gray()
r + theme_classic() #white wiht no gridlines
r + theme_minimal() #minimal theme

# Zooming
t + coord_cartesian(xlim = c(0, 100), ylim = c(10,20))
t + xlim(0, 100) + ylim(10,20)
t + scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100))

