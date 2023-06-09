#Loading data
if(!require(tidytuesdayR, quietly=T)) install.packages("tidytuesdayR", repos = "https://cloud.r-project.org/")
library(tidytuesdayR, quietly=T)

tuesdata <- tt_load(2023, week=20)
tornados <- tuesdata$tornados

#writing to local project folder to avoid too many Github API calls
write.csv(tornados, "tornados.csv")
tornados <- read.csv("tornados.csv")

###############################################################################
#Data overview
if(!require(tidyverse, quietly=T)) install.packages("tidyverse", repos = "https://cloud.r-project.org/")
library(tidyverse, quietly=T)

head(tornados) 
#some ending latitudes and longitudes are 0 (which is likely incorrect)
str(tornados)
summary(tornados)
sum(is.na(tornados)) #27926
names(which(colSums(is.na(tornados))>0))
# [1] "mag"  "loss"

###############################################################################
#Viz1
#magnitude, area, loss, inj, fat
# The EF Scale primarily classifies tornadoes based on the damage left behind. 
# By surveying the damage, scientists assign a wind speed that is likely to have caused that damage.
if(!require(ggpubr, quietly=T)) install.packages("ggpubr", repos = "https://cloud.r-project.org/")
library(ggpubr, quietly=T)

#within the same magnitude, are we getting better over time?
#does magnitude relate with injuries and fatalities since based on property damage?

#Monetary loss information is highly suspect and should be used with caution, if at all. 
summ_inj_fat <- tornados |>
  group_by(om, yr, mag) |>
  summarize(tfat=sum(fat),
            tinj=sum(inj)) |>
  ungroup() |>
  mutate(decade = yr - (yr %% 10),
         fat_cat = tfat - (tfat %% 10),
         inj_cat = tinj - (tinj %% 100)) |>
  subset(!is.na(mag)) |>
  subset(mag != 0)

summ_line <- summ_inj_fat |>
  group_by(decade, mag) |>
  summarize(n_tot=n()) |>
  ungroup() 
  
plot_num <- summ_line |>
  ggplot(aes(x=decade)) +
  geom_line(aes(y=n_tot)) +
  facet_wrap(mag~., ncol=5) +
  theme_linedraw() +
  labs(x="",
       y="# of Tornadoes",
       title="Trends in fatalities and injuries from tornadoes over the decades",
       subtitle="While the number of tornadoes has gone down, there appears to be no clear decrease the fatalities and injuries caused by them.") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(1940,2030,10))

plot_fat <- summ_inj_fat |>
  group_by(decade, mag, fat_cat) |>
  summarize(n_fat=n()) |>
  ungroup() |>
  ggplot(aes(x=decade, y=fat_cat, group=decade)) +
  geom_raster(aes(fill=n_fat), hjust=0, vjust=1) +
  geom_text(aes(label=n_fat, colour=(n_fat>10)), size = 2, nudge_y=5, nudge_x=-5) +
  scale_fill_gradient(trans="log10", low="lightpink", high="deeppink4") +
  facet_wrap(mag~., ncol=5) +
  theme_linedraw() +
  labs(x="",
       y="# of Fatalities") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank(),
        legend.position="none") +
  scale_color_manual(values=c("TRUE"="white", "FALSE"="black")) +
  scale_y_continuous(minor_breaks = seq(0, 160, 10)) +
  scale_x_continuous(breaks = seq(1940,2030,10)) 

plot_inj <- summ_inj_fat |>
  group_by(decade, mag, inj_cat) |>
  summarize(n_inj=n()) |>
  ungroup() |>
  ggplot(aes(x=decade, y=inj_cat, group=decade)) +
  geom_raster(aes(fill=n_inj), hjust=0, vjust=1) +
  geom_text(aes(label=n_inj, colour=(n_inj>10)), size = 2, nudge_y=50, nudge_x=-5) +
  scale_fill_gradient(trans="log10", low="lightblue", high="midnightblue") +
  facet_wrap(mag~., ncol=5) +
  theme_linedraw() +
  labs(x="Decade",
       y="# of Injuries",
       caption="Each data tile includes both endpoints and excludes both start points.") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank(),
        legend.position="none") +
  scale_color_manual(values=c("TRUE"="white", "FALSE"="black")) +
  scale_y_continuous(minor_breaks = seq(0, 1800, 100)) +
  scale_x_continuous(breaks = seq(1940,2030,10))

ggarrange(plot_num ,plot_fat, plot_inj, nrow=3)

ggsave("mags.jpg", width=10.5, height=8) 

###############################################################################