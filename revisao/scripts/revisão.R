package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #mantel test
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "stringr", #data cleaning
                  "ggplot2", #mantel test visualization 
                  "remotes",
                  "psych",
                  "bipartite",
                  "scales",
                  "janitor",
                  "webr"
)


#installing the packages if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}



#LOAD DATA


load_dados <- readr::read_csv(here::here("dados", "references_fenologia.csv"))

load_database <- readr::read_csv(here::here("dados", "database.csv"))

#transform into a tibble 
dados <- as_tibble(load_dados)

database <- as_tibble(load_database)

colnames(database)[2] ="source"


dados_database <- merge(x=dados, y=database,
                    by="DOI", all.x =T)



write.csv(dados_database, here::here("dados_database.csv"))
