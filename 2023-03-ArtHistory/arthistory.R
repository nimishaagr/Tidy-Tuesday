#Loading data
if(!require(tidytuesdayR, quietly=T)) install.packages("tidytuesdayR", repos = "https://cloud.r-project.org/")
library(tidytuesdayR, quietly=T)

tuesdata <- tt_load(2023, week=3)
arthistory <- tuesdata$artists

#writing to local project folder to avoid too many Github API calls
write.csv(arthistory, "arthistory.csv")
arthistory <- read.csv("arthistory.csv")

###############################################################################
#Data overview
if(!require(tidyverse, quietly=T)) install.packages("tidyverse", repos = "https://cloud.r-project.org/")
library(tidyverse, quietly=T)
if(!require(lubridate, quietly=T)) install.packages("lubridate", repos = "https://cloud.r-project.org/")
library(lubridate, quietly=T)

head(arthistory) 
str(arthistory)
summary(arthistory)

sum(is.na(arthistory)) #58
names(which(colSums(is.na(arthistory))>0))
# [1] "artist_ethnicity"
#some N/As are written as character
arthistory <- arthistory |>
  mutate(artist_gender=ifelse(str_detect(artist_gender,"N/A"),NA,artist_gender),
         artist_race=ifelse(str_detect(artist_race,"N/A"),NA,artist_race),
         artist_nationality=ifelse(str_detect(artist_nationality,"N/A"),NA,artist_nationality))
sum(is.na(arthistory)) #168
names(which(colSums(is.na(arthistory))>0))
# [1] "artist_nationality" "artist_gender"      "artist_race"        "artist_ethnicity" 

#Categorical variables:
# edition_number
# artist_nationality
# artist_nationality_other
# artist_gender
# artist_race
# artist_ethnicity
# book
# artist_race_nwi
#converting to factor variables
categ_cols <- c("edition_number","artist_nationality","artist_nationality_other",
                "artist_gender","artist_race","artist_ethnicity","book","artist_race_nwi")
arthistory[categ_cols] <- lapply(arthistory[categ_cols], factor) 
#convert year to integral date column
arthistory <- arthistory |>
  mutate(year = as_date(paste("01-01-",year,sep=""),format="%d-%m-%Y"))  

str(arthistory)
summary(arthistory)

###############################################################################
#Viz1
#nationality prominence
# Janson, was a Russian-American scholar of art history best known for his History of Art
# Helen Gardner (1878–1946) was an American art historian and educator. Her Art Through the Ages remains a standard text for American art history classes.
if(!require(ggrepel, quietly=T)) install.packages("ggrepel", repos = "https://cloud.r-project.org/")
library(ggrepel, quietly=T)

###############################################################################
#Viz2
#Page Area
if(!require(treemapify, quietly=T)) install.packages("treemapify", repos = "https://cloud.r-project.org/")
library(treemapify, quietly=T)
if(!require(RColorBrewer, quietly=T)) install.packages("RColorBrewer", repos = "https://cloud.r-project.org/")
library(RColorBrewer, quietly=T)
if(!require(viridis, quietly=T)) install.packages("viridis", repos = "https://cloud.r-project.org/")
library(viridis, quietly=T)

plot1b_render <- function(bookSelector,n_nationalities,show_artists) {
  top_n_nations <- arthistory |>
    filter(!is.na(artist_nationality)) |>
    group_by(artist_nationality) |>
    summarise(space_ratio=mean(space_ratio_per_page_total)) |>
    slice_max(space_ratio, n=n_nationalities) |>
    ungroup()
  
    art_nation_space <- arthistory |>
      filter(if (input$bookSelector!="All") {book==input$bookSelector} else {TRUE},
             !is.na(artist_nationality)) |>
      mutate(nationality_n=as.factor(ifelse(artist_nationality %in% (top_n_nations$artist_nationality),as.character(artist_nationality), "Other"))) |>
      group_by(nationality_n, artist_name) |>
      summarise(space_ratio=mean(space_ratio_per_page_total)) |>
      ungroup()
    
    top_20_area_artists <- art_nation_space |>
      slice_max(space_ratio, n=20)
    
    plot_area <- art_nation_space |>
      ggplot(aes(area=space_ratio,
                 subgroup=nationality_n,
                 fill=nationality_n,
                 label=ifelse(show_artists==T,ifelse(artist_name %in% (top_20_area_artists$artist_name),artist_name, ""),""))) +
      geom_treemap() +
      geom_treemap_subgroup_text(place = "centre", size=12, grow = T, reflow=T, alpha = 0.6, fontface = "bold", colour="white") +
      geom_treemap_text(fontface = "italic", place = "centre", size=5, grow=F, reflow=T, colour="white") +
      theme(legend.position="none") +
      scale_fill_viridis(discrete=T,option="D")
  
  return (plot_area)
}


