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

# head(arthistory) 
# str(arthistory)
# summary(arthistory)
# 
# sum(is.na(arthistory)) #58
# names(which(colSums(is.na(arthistory))>0))
# [1] "artist_ethnicity"
#some N/As are written as character
arthistory <- arthistory |>
  mutate(artist_gender=ifelse(str_detect(artist_gender,"N/A"),NA,artist_gender),
         artist_race=ifelse(str_detect(artist_race,"N/A"),NA,artist_race),
         artist_nationality=ifelse(str_detect(artist_nationality,"N/A"),NA,artist_nationality))
# sum(is.na(arthistory)) #168
# names(which(colSums(is.na(arthistory))>0))
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

# str(arthistory)
# summary(arthistory)

###############################################################################

# Janson, was a Russian-American scholar of art history best known for his History of Art
# Helen Gardner (1878–1946) was an American art historian and educator. Her Art Through the Ages remains a standard text for American art history classes.

if(!require(treemapify, quietly=T)) install.packages("treemapify", repos = "https://cloud.r-project.org/")
library(treemapify, quietly=T)
if(!require(RColorBrewer, quietly=T)) install.packages("RColorBrewer", repos = "https://cloud.r-project.org/")
library(RColorBrewer, quietly=T)
if(!require(ggstream, quietly=T)) install.packages("ggstream", repos = "https://cloud.r-project.org/")
library(ggstream, quietly=T)

plot1b_render <- function(book_selector,n_nationalities,show_artists) {
  top_n_nations <- arthistory |>
    filter(!is.na(artist_nationality)) |>
    group_by(artist_nationality) |>
    summarise(space_ratio=mean(space_ratio_per_page_total)) |>
    slice_max(space_ratio, n=n_nationalities) |>
    ungroup()
  
    art_nation_space <- arthistory |>
      filter(if (book_selector!="All") {book==book_selector} else {TRUE},
             !is.na(artist_nationality)) |>
      mutate(nationality_n=as.factor(ifelse(artist_nationality %in% (top_n_nations$artist_nationality),as.character(artist_nationality), "Other"))) |>
      group_by(nationality_n, artist_name) |>
      summarise(space_ratio=mean(space_ratio_per_page_total)) |>
      ungroup()
    
    top_50_space_artists <- art_nation_space |>
      filter(!str_detect(artist_name,"N/A")) |>
      slice_max(space_ratio, n=50)

    plot_area <- art_nation_space |>
      ggplot(aes(area=space_ratio,
                 subgroup=nationality_n,
                 fill=nationality_n,
                 label=ifelse(show_artists & artist_name %in% (top_50_space_artists$artist_name), artist_name,""))) +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "white", size = 5) +
      geom_treemap_subgroup_text(place = "centre", size=12, grow = T, reflow=T, alpha = 0.6, fontface = "bold", colour="white") +
      geom_treemap_text(fontface = "italic", place = "centre", size=8, grow=F, reflow=T, colour="white") +
      theme(legend.position="none") +
      scale_fill_manual(values=rep(brewer.pal(8,"Dark2"),times=7)) +
      labs(title="Average space ratio on the page of a book given to different nationalities")
  
  return (plot_area)
}

plot2b_render <- function(book_selector,show_artists) {
  
  art_gender_space <- arthistory |>
    filter(if (book_selector!="All") {book==book_selector} else {TRUE},
           !is.na(artist_gender)) |>
    group_by(artist_gender, artist_name) |>
    summarise(space_ratio=mean(space_ratio_per_page_total)) |>
    ungroup()
  
  top_50_space_artists <- art_gender_space |>
    filter(!str_detect(artist_name,"N/A")) |>
    slice_max(space_ratio, n=50)
  
  plot_area <- art_gender_space |>
    ggplot(aes(area=space_ratio,
               subgroup=artist_gender,
               fill=artist_gender,
               label=ifelse(show_artists & artist_name %in% (top_50_space_artists$artist_name), artist_name,""))) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 5) +
    geom_treemap_subgroup_text(place = "centre", size=12, grow = T, reflow=T, alpha = 0.6, fontface = "bold", colour="white") +
    geom_treemap_text(fontface = "italic", place = "centre", size=8, grow=F, reflow=T, colour="white") +
    theme(legend.position="none") +
    scale_fill_manual(values= c("Male"="dodgerblue4", "Female"="mediumvioletred")) +
    labs(title="Average space ratio on the page of a book given to males/females")
  
  return (plot_area)
}

plot3b_render <- function(book_selector,show_artists) {
  
  art_race_space <- arthistory |>
    filter(if (book_selector!="All") {book==book_selector} else {TRUE},
           !is.na(artist_race)) |>
    group_by(artist_race, artist_name) |>
    summarise(space_ratio=mean(space_ratio_per_page_total)) |>
    ungroup()
  
  top_50_space_artists <- art_race_space |>
    filter(!str_detect(artist_name,"N/A")) |>
    slice_max(space_ratio, n=50)
  
  plot_area <- art_race_space |>
    ggplot(aes(area=space_ratio,
               subgroup=artist_race,
               fill=artist_race,
               label=ifelse(show_artists & artist_name %in% (top_50_space_artists$artist_name), artist_name,""))) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 5) +
    geom_treemap_subgroup_text(place = "centre", size=12, grow = T, reflow=T, alpha = 0.4, fontface = "bold", colour="black") +
    geom_treemap_text(fontface = "italic", place = "centre", size=8, grow=F, reflow=T, colour="black") +
    theme(legend.position="none") +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title="Average space ratio on the page of a book given to different races")
  
  return (plot_area)
}

plot1a_render <- function(exbn_selector) {
  art_nation_ex_count <- arthistory |>
    pivot_longer(cols=c("moma_count_to_year","whitney_count_to_year"),
                 names_to="exhibition",
                 values_to="tot_count") |>
    filter(exhibition==exbn_selector) |>
    mutate(year=year(year)) |>
    group_by(artist_name) |>
    arrange(year, .by_group = T) |>
    mutate(ex_count=tot_count-lag(tot_count)) |>
    filter(!is.na(ex_count)) |>
    group_by(artist_nationality_other, exhibition, year) |>
    summarize(ex_count=sum(ex_count)) |>
    ungroup()
  
  plot_area <- art_nation_ex_count |>
    group_by(artist_nationality_other) |>
    filter(sum(ex_count)!=0) |>
    ggplot(aes(x=year, y=ex_count, fill=artist_nationality_other, group=artist_nationality_other)) +
    geom_stream(type="proportion") +
    geom_stream_label(type="proportion", aes(label = artist_nationality_other), size=5, colour="white", fontface="bold") +
    theme_linedraw() +
    labs(title="Proportions of different nationalities in the exhibition over the years",
         subtitle="for artists mentioned in Janson and Gardner",
         y="Proportional number of exhibitions",
         x="Year") +
    theme(legend.position = "none") +
    scale_fill_brewer(palette="Dark2")
  
  return (plot_area)
  
}

plot2a_render <- function(exbn_selector) {
  art_gender_ex_count <- arthistory |>
    pivot_longer(cols=c("moma_count_to_year","whitney_count_to_year"),
                 names_to="exhibition",
                 values_to="tot_count") |>
    filter(exhibition==exbn_selector) |>
    mutate(year=year(year)) |>
    group_by(artist_name) |>
    arrange(year, .by_group = T) |>
    mutate(ex_count=tot_count-lag(tot_count)) |>
    filter(!is.na(ex_count)) |>
    group_by(artist_gender, exhibition, year) |>
    summarize(ex_count=sum(ex_count)) |>
    ungroup()
  
  plot_area <- art_gender_ex_count |>
    group_by(artist_gender) |>
    filter(sum(ex_count)!=0) |>
    ggplot(aes(x=year, y=ex_count, fill=artist_gender, group=artist_gender)) +
    geom_stream(type="proportion") +
    geom_stream_label(type="proportion", aes(label = artist_gender), size=5, colour="white", fontface="bold") +
    theme_linedraw() +
    labs(title="Proportions of males/females in the exhibition over the years",
         subtitle="for artists mentioned in Janson and Gardner",
         y="Proportional number of exhibitions",
         x="Year") +
    theme(legend.position = "none") +
    scale_fill_manual(values= c("Male"="dodgerblue4", "Female"="mediumvioletred"))
  
  return (plot_area)
  
}

plot3a_render <- function(exbn_selector) {
  art_race_ex_count <- arthistory |>
    pivot_longer(cols=c("moma_count_to_year","whitney_count_to_year"),
                 names_to="exhibition",
                 values_to="tot_count") |>
    filter(exhibition==exbn_selector) |>
    mutate(year=year(year)) |>
    group_by(artist_name) |>
    arrange(year, .by_group = T) |>
    mutate(ex_count=tot_count-lag(tot_count)) |>
    filter(!is.na(ex_count)) |>
    group_by(artist_race, exhibition, year) |>
    summarize(ex_count=sum(ex_count)) |>
    ungroup()
  
  plot_area <- art_race_ex_count |>
    group_by(artist_race) |>
    filter(sum(ex_count)!=0) |>
    ggplot(aes(x=year, y=ex_count, fill=artist_race, group=artist_race)) +
    geom_stream(type="proportion") +
    geom_stream_label(type="proportion", aes(label = artist_race), size=5, colour="black", fontface="bold") +
    theme_linedraw() +
    labs(title="Proportions of different races in the exhibition over the years",
         subtitle="for artists mentioned in Janson and Gardner",
         y="Proportional number of exhibitions",
         x="Year") +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Pastel1")
  
  return (plot_area)
  
}

plot1c_render <- function(book_selector,n_nationalities) {
  top_n_nations <- arthistory |>
    filter(!is.na(artist_nationality)) |>
    group_by(artist_nationality) |>
    summarise(feature_count=n()) |>
    slice_max(feature_count, n=n_nationalities) |>
    ungroup()
  
  art_nation_count <- arthistory |>
    filter(if (book_selector!="All") {book==book_selector} else {TRUE},
           !is.na(artist_nationality)) |>
    mutate(decade=year(year) - (year(year) %% 10)) |>
    mutate(nationality_n=as.factor(ifelse(artist_nationality %in% (top_n_nations$artist_nationality),as.character(artist_nationality), "Other"))) |>
    group_by(nationality_n, decade) |>
    summarise(feature_count=n()) |>
    ungroup()
  
  plot_area <- art_nation_count |>
    ggplot(aes(x=decade+5, fill=feature_count, y=nationality_n)) +
    geom_rect(aes(xmin=1920, xmax=2030, ymin=-Inf, ymax=Inf), fill="thistle") +
    geom_tile() +
    geom_text(aes(label=feature_count, colour=(feature_count>10)), size = 3, nudge_y=0, nudge_x=0) +
    scale_fill_gradient(trans="log10", low="lightpink", high="deeppink4") +
    theme_linedraw() +
    labs(title="How many times do different nationalities feature in the book(s) over the years?",
         y="Nationality",
         x="Decade",
         fill="Feature\nCount") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color="none") +
    scale_color_manual(values=c("TRUE"="white", "FALSE"="black")) +
    scale_x_continuous(breaks = seq(1920,2030,10))
  
  return (plot_area)
}

plot2c_render <- function(book_selector) {
  
  art_gender_count <- arthistory |>
    filter(if (book_selector!="All") {book==book_selector} else {TRUE},
           !is.na(artist_gender)) |>
    mutate(decade=year(year) - (year(year) %% 10)) |>
    group_by(artist_gender, decade) |>
    summarise(feature_count=n()) |>
    ungroup()
  
  plot_area <- art_gender_count |>
    ggplot(aes(x=decade+5, fill=feature_count, y=artist_gender)) +
    geom_rect(aes(xmin=1920, xmax=2030, ymin=-Inf, ymax=Inf), fill="thistle") +
    geom_tile() +
    geom_text(aes(label=feature_count, colour=(feature_count>10)), size = 3, nudge_y=0, nudge_x=0) +
    scale_fill_gradient(trans="log10", low="lightpink", high="deeppink4") +
    theme_linedraw() +
    labs(title="How many times do males/females feature in the book(s) over the years?",
         y="Gender",
         x="Decade",
         fill="Feature\nCount") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color="none") +
    scale_color_manual(values=c("TRUE"="white", "FALSE"="black")) +
    scale_x_continuous(breaks = seq(1920,2030,10))
  
  return (plot_area)

}

plot3c_render <- function(book_selector) {
  
  art_race_count <- arthistory |>
    filter(if (book_selector!="All") {book==book_selector} else {TRUE},
           !is.na(artist_race)) |>
    mutate(decade=year(year) - (year(year) %% 10)) |>
    group_by(artist_race, decade) |>
    summarise(feature_count=n()) |>
    ungroup()
  
  plot_area <- art_race_count |>
    ggplot(aes(x=decade+5, fill=feature_count, y=artist_race)) +
    geom_rect(aes(xmin=1920, xmax=2030, ymin=-Inf, ymax=Inf), fill="thistle") +
    geom_tile() +
    geom_text(aes(label=feature_count, colour=(feature_count>10)), size = 3, nudge_y=0, nudge_x=0) +
    scale_fill_gradient(trans="log10", low="lightpink", high="deeppink4") +
    theme_linedraw() +
    labs(title="How many times do different races feature in the book(s) over the years?",
         y="Race",
         x="Decade",
         fill="Feature\nCount") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color="none") +
    scale_color_manual(values=c("TRUE"="white", "FALSE"="black")) +
    scale_x_continuous(breaks = seq(1920,2030,10))
  
  return (plot_area)
}
