#Loading data
if(!require(tidytuesdayR, quietly=T)) install.packages("tidytuesdayR", repos = "https://cloud.r-project.org/")
library(tidytuesdayR, quietly=T)

tuesdata <- tt_load(2023, week=18)
plots <- tuesdata$plots
species <- tuesdata$species
surveys <- tuesdata$surveys

#writing to local project folder to avoid too many Github API calls
write.csv(plots, "plots.csv")
write.csv(species, "species.csv")
write.csv(surveys, "surveys.csv")
plots <- read.csv("plots.csv")
species <- read.csv("species.csv")
surveys <- read.csv("surveys.csv")

###############################################################################
#Data overview
if(!require(tidyverse, quietly=T)) install.packages("tidyverse", repos = "https://cloud.r-project.org/")
library(tidyverse, quietly=T)

head(plots)
str(plots)
#plot should be a factor, don't need X
summary(plots)
sum(is.na(plots)) #0
plots <- plots |> mutate(plot=as.factor(plot)) |> select(-X)
plots |> group_by(treatment) |> summarize(n()) 
#equal distribution of plots by treatment
# control       4
# exclosure     4

head(species)
str(species)
summary(species)
#don't need X, censustarget (1), unidentified(0), rodent(1), taxa(rodent)
sum(is.na(species)) #4
species <- species |> select(-X,-censustarget, -unidentified, -rodent, -taxa)
sum(duplicated(species$commonname)) #0
# Since species, scientific name and common name represent the same thing, we will use only common name, which is easiest understood for visualization purposes.
species <- species |> select(-scientificname)
species <- species |> mutate(granivore=as.factor(granivore))

head(surveys)
str(surveys)
summary(surveys)
#don't need X
sum(is.na(surveys)) #234382
surveys <- surveys |> select(-X)

###############################################################################
#Viz1
#Heaviest rodents
if(!require(magick, quietly=T)) install.packages("magick", repos = "https://cloud.r-project.org/")
library(magick, quietly=T)

heavy_species <- species |> filter(meanwgt>90)
plot1 <- species |>
  ggplot(aes(x=meanwgt, y=meanhfl)) +
  geom_point(colour="grey32") +
  geom_point(data=heavy_species, aes(x=meanwgt, y=meanhfl), colour="brown2", size=3)+
  geom_text(data=heavy_species, aes(x=meanwgt, y=meanhfl, label=commonname), hjust=1.07, vjust=0.5, colour="brown2", fontface=4) +
  theme_linedraw() +
  labs(x="Mean Weight",
       y="Mean Hindfoot Length",
       title="The heaviest and long-footed Chihuahuan desert rodents"
  )
btkr <- image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/884388/large.jpg")
image1 <- image_fill(btkr, 'none')
raster1 <- as.raster(image1)

wtw <- image_read("https://inaturalist-open-data.s3.amazonaws.com/photos/55637/large.jpg")
image2 <- image_fill(wtw, 'none')
raster2 <- as.raster(image2)
plot1 + 
  annotation_raster(raster1, 64, 115, 37, 48) + 
  annotation_raster(raster2, 112, 159, 21, 30)
ggsave("big_heavy_rodents.jpg") 

###############################################################################
#Viz2
if(!require(ggpmisc, quietly=T)) install.packages("ggpmisc", repos = "https://cloud.r-project.org/")
library(ggpmisc, quietly=T)

species |> 
  ggplot(aes(x=meanwgt, y=meanhfl, group=granivore, colour=granivore)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n")), label.y=c(0.94,0.99)) +
  theme_linedraw() +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  labs(x="Mean Weight",
       y="Mean Hindfoot Length",
       colour="Granivore?",
       title="Linear relationship between mean hindfoot length and weight of Chihuahuan desert rodents, divided by granivorous nature",
       subtitle="There appears to be a statistically significant strong linear relationship between the mean hindfoot length and the weight of Chihuahuan desert rodents\nin the study. Granivorous rodents have a steeper positive relationship."
  )
ggsave("hfl_wgt.jpg", width=12, height=8) 

###############################################################################
#Viz3
if(!require(ggridges, quietly=T)) install.packages("ggridges", repos = "https://cloud.r-project.org/")
library(ggridges, quietly=T)
if(!require(paletteer, quietly=T)) install.packages("paletteer", repos = "https://cloud.r-project.org/")
library(paletteer, quietly=T)

#Need to join original species and surveys tables for species name
species2 <- species |> select(species, commonname)
surveys2 <- merge(x=surveys, y=species2, by="species", all.X=T)

surveys2 <- surveys2 |>
  mutate(censusdate = as.Date(censusdate))
  
surveys2 |>
  ggplot(aes(x=censusdate, fill=commonname, y=commonname)) +
  geom_density_ridges(alpha=0.8) +
  theme_linedraw() +
  labs(x="Census Date",
       y="",
       title="A timeline of occurence densities of different Chihuahuan desert rodents for different treatments"
  ) +
  facet_wrap(.~treatment) +
  theme(legend.position="none") +
  scale_fill_paletteer_d("ggsci::default_igv")

ggsave("spcies_density_trtmt.jpg", width=15, height=10) 

###############################################################################
#Viz4
#filtering out those species with extremely low (<=2) observations
pregnant_2 <- surveys2 |> 
  filter(!is.na(pregnant)) |>
  group_by(commonname) |>
  summarise(n=n()) |>
  ungroup() |>
  filter(n>2)

surveys2 |> filter(!is.na(pregnant) & (commonname %in% pregnant_2$commonname)) |>
  ggplot(aes(x=month, y=commonname, fill=commonname, height=..density..)) +
  geom_density_ridges(stat = "density", trim = TRUE, alpha=0.8) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  labs(x="Month",
       y="",
       title="Pregnancy of Chihuahuan desert rodents across the months",
       subtitle="The height of the bars represents the density of pregnant rodents. Peaks show the months where pregnancy is likelier.\nBlank spaces show no recorded pregnancy. Some species reproduce evenly throughout the year, whereas others like\nthe Bailey's pocket mouse show no pregnancy in select months (Dec-Feb).",
       caption="Rats are usually pregnant for 21-23 days before giving birth.") +
  theme_linedraw() +
  scale_fill_paletteer_d("ggsci::default_igv") +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank() ) 

ggsave("species_pregnant.jpg", width=11, height=13) 

#Similarly, lactation and bodily changes can be studied.
