# revisão
# Fenologia e Mudanças Climaticas 
# dia 06 de dezembro, 2022
# Carina Isabella Motta

#1 CARREGAR PACOTES-------------------------------------------------------------

rm(list=ls()) #clean global environment 

package.list <- c("here", 
                  "vegan",
                  "tidyverse", 
                  "dplyr", 
                  "ggplot2",
                  "maps"
                  )


#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}


#2 CARREGAR OS DADOS------------------------------------------------------------

planilha.rev <- readr::read_csv(here::here("revisao",
                                             "dados", 
                                       "revisão.csv"))


planilha.coor <- readr::read_csv(here::here("revisao",
                                           "dados", 
                                           "coordinados_paises.csv"))

planilha.rev <- as_tibble(planilha.rev) 

planilha.coor <- as_tibble(planilha.coor) 

#line graph of publications over time

artigos.tempo <- 
  planilha.rev  %>%  
  group_by(year)  %>%  
  summarise(n.artigos = n_distinct(ID_AR))

p1 <- ggplot(artigos.tempo, aes(x=year, y=n.artigos)) +
  geom_line(color = "blue") +
  geom_point()

tempo <- p1 + scale_x_continuous(breaks = seq(2009, 2022, by = 1)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  expand_limits(y = 1) +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(y = "Numero de Artigos", x = "Ano")

tempo

# scopus vs. web of science-----------------------------------------------------


database <- 
  planilha.rev  %>%  
  group_by(database)  %>%  
  summarise(n.database = n())

database[2, 1] <- "Scopus & WoS"

database[3, 1] <- "WoS"

# Compute percentages
database$fraction = database$n.database / sum(database$n.database)

# Compute the cumulative percentages (top of each rectangle)
database$ymax = cumsum(database$fraction)

# Compute the bottom of each rectangle
database$ymin = c(0, head(database$ymax, n=-1))

# Compute label position
database$labelPosition <- (database$ymax + database$ymin) / 2

# Compute a good label
database$label <- paste0(database$database, "\n ", database$n.database)

# Make the plot
source <- ggplot(database, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=database)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=database), size=5) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

source

# map---------------------------------------------------------------------------

subset.rev <- planilha.rev %>% select(1, 9)

colnames(subset.rev)[2] ="name"


mapa <- merge(x=subset.rev, y=planilha.coor,
                    by="name", all.x =T)

mapa <- mapa %>% drop_na()

artigos.mapa <- 
  mapa %>%  
  group_by(name)  %>%  
  summarise(n.artigos = n_distinct(ID_AR))

mapa.df <- merge(x=artigos.mapa, y=mapa,
              by="name", all.x =F)

mapa.df <- mapa.df %>%
  filter(duplicated(name) == FALSE)

world_map <- map_data("world")

base_plot <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
mapa  <- base_plot + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="light gray", fill="light gray") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  geom_point(data=mapa.df, 
             aes(x=longitude, y=latitude, size=n.artigos), colour="red", 
             fill="Pink",pch=21, alpha=I(0.7)) 

mapa


# salvar e exportar as figuras--------------------------------------------------

ggsave(
  filename = here::here("revisao", "graficos", "mapa.jpg"),
  plot = mapa, 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 720
)

ggsave(
  filename = here::here("revisao", "graficos", "tempo.jpg"),
  plot = tempo, 
  width = 15, 
  height = 10, 
  units = "cm", 
  dpi = 720
)

ggsave(
  filename = here::here("revisao", "graficos", "source.jpg"),
  plot = source, 
  width = 10, 
  height = 10, 
  units = "cm", 
  dpi = 720
)



