# over the years ...

library(tidyverse)
library(lubridate)
library(showtext)

library(hrbrthemes)
library(igraph)
library(ggraph)
library(colormap)

palette <- c("#000000", "#E1AF00", "#78B7C5", "#EBCC2A", "#3B9AB2", "#EAEA00")
font_add_google(family = 'Press Start 2P', 'Press Start 2P')


connect <- 
  data.frame(
    from = c("Stefan-A.", "Sascha", "Jürgen","Jürgen"),
    to = c("Jürgen",  "Stefan-A.","Sascha","Stefan-A."),
    value = c(1,1,1,1))

# Number of connection per person
coauth <- 
  c( as.character(connect$from), as.character(connect$to)) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarize(n=n()) 
colnames(coauth) <- c("name", "n")
#dim(coauth)

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )


com <- walktrap.community(mygraph)
#max(com$membership)

#Reorder dataset and make the graph
coauth <- coauth %>% 
  mutate(grp = com$membership) %>%
  arrange(grp) %>%
  mutate(name=factor(name, name)) %>%
  
  
  # keep only this people in edges
  connect <- connect %>%
  filter(from %in% coauth$name) %>%
  filter(to %in% coauth$name)

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

# prepare a vector of n color in the viridis scale
mycolor <- colormap(colormap=colormaps$viridis, nshades=max(coauth$grp))
mycolor <- sample(mycolor, length(mycolor))

# Make the graph
ggraph(mygraph, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.2, edge_width=0.3, fold=TRUE) +
  geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
  scale_size_continuous(range=c(0.5,8)) +
  scale_color_manual(values=mycolor) +
  geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=5) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0.4,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 










# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )



# 1 Salzburg

# 5 Bad Aussee
BA <- 10
# 10 DE
DE <- 20
# 15 Fribourg CH
FR <- 30
# 20 Wien
W <- 40
# 25 Norwegen
N <- 45
# 30
# 35

history <- tibble(name = c('Andreas', 'Denise', 'Josefine', 'Matthias', 
                           'Ines', 'Susa', 'Chrissie', 'Jesko', 'Yann', 'Elisa', 
                           'Simon', 'Sascha', 'Andrea', 'Toni', 'Tomas', 'Paul', 'Katrin', 
                           'Christof', 'Sandi', 'Ruth', 'Nina', 'Markus', 'Jürgen', 'Ingrid', 
                           'Rima', 'Stefan-A.', 'Frank', 'Karoline', 'Samuel', 'Liv', 'Rainer', 
                           'Anne', 'Johannes', 'Dries', 'Laura', 'Eva', 'Stefan', 'Flori', 'Dirk', 'Bernhard'), 
                  ort = c('S', 'BA', 'BA', 'BA', 'BA', 'DE', 'CH', 'CH', 'CH', 'CH', 'CH', 'S', 'W', 'S', 'DE', 'S', 'S', 'S', 'S',
                                'S', 'S', 'S', 'S', 'S', 'DE', 'S', 'DE', 'S', 'DE', 'NO', 'DE', 'DE', 'DE', 'DE', 'DE', 'S', 'S', 'S', 'CH', 'W'), 
                  earliest_meet =  c("1996-12-31", "20220604", "20220604", "20220604", 
                                     "20220604", "20091109", "2002-01-01", "2004-01-01", "2017-06-17", "2017-06-17",
                                     "2000-11-01", "1991-09-20", "20160917", "1993-05-01", "2012-10-01", "1991-10-20", "1998-01-20",
                                     "2002-12-12", "2002-12-12", "1992-09-01", "1996-01-01", "1996-01-01", "1991-09-20", "19940503",
                                     "2016-04-25", "1991-09-20", "2007-11-01", "2002-10-31", "2012-08-11", "2007-06-16", "2012-11-01",
                                     "2012-10-01", "2012-10-01", "20141001", "20150201", "2002-12-12", "2002-12-12", "20060217", "2011-04-01", "2006-02-10"))

history <- 
history %>%
  mutate_at(vars(contains('earliest_meet')), .funs = ymd) %>%
  mutate(today = ymd(20220604),
         diff = today - earliest_meet,
         ort = case_when( 
                   ort == 'BA' ~ 'Bad Aussee',
                   ort == 'S'   ~ 'Salzburg',
                   ort == 'DE' ~ 'Deutschland',
                   ort == 'CH' ~ 'Schweiz',
                   ort == 'W' ~ 'Wien',
                   ort == 'NO' ~ 'Norwegen'
                         ))

ggplot(history, aes(reorder(name, diff), diff)) + 
  geom_bar(stat = 'identity', aes(fill = ort)) +
  geom_text(aes(y = 900, label = diff), family="Press Start 2P", size = 2) +
  #theme_bw() +
  scale_fill_manual(values = palette) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        text = element_text(family="Press Start 2P", size = 8)
        ) +
  labs(y = 'Tage',
  x = '',
  title = 'Woher kennen wir uns und wie lange?') + 
  coord_flip()
  


ggsave('history.png', width = 8, height = 4)
 