#Loading data
if(!require(tidytuesdayR, quietly=T)) install.packages("tidytuesdayR", repos = "https://cloud.r-project.org/")
library(tidytuesdayR, quietly=T)

tuesdata <- tt_load(2023, week=19)
childcare_costs <- tuesdata$childcare_costs
counties <- tuesdata$counties

# #writing to local project folder to avoid too many Github API calls
write.csv(childcare_costs, "childcare_costs.csv")
write.csv(counties, "counties.csv")
childcare_costs <- read.csv("childcare_costs.csv")
counties <- read.csv("counties.csv")

###############################################################################
#Data overview
if(!require(tidyverse, quietly=T)) install.packages("tidyverse", repos = "https://cloud.r-project.org/")
library(tidyverse, quietly=T)

head(childcare_costs)
str(childcare_costs)
summary(childcare_costs)
sum(is.na(childcare_costs)) #88636
names(which(colSums(is.na(childcare_costs))>0))
# [1] "h_under6_single_m" "h_6to17_single_m"  "mcsa"              "mfccsa"           
# [5] "mc_infant"         "mc_toddler"        "mc_preschool"      "mfcc_infant"      
# [9] "mfcc_toddler"      "mfcc_preschool" 

head(counties)
str(counties)
summary(counties)
sum(is.na(counties)) #0

cc2 <- merge(x=childcare_costs, y=counties, by="county_fips_code", all.X=T)

###############################################################################
#Viz1

if(!require(scales, quietly=T)) install.packages("scales", repos = "https://cloud.r-project.org/")
library(scales, quietly=T)
if(!require(ggpubr, quietly=T)) install.packages("ggpubr", repos = "https://cloud.r-project.org/")
library(ggpubr, quietly=T)

unique(cc2$state_name)
#taking statewise averages
cc_summary <- cc2 |>
  group_by(study_year, state_name) |>
  summarize(households=sum(households),
            h_under6_both_work=sum(h_under6_both_work),
            h_under6_f_work=sum(h_under6_f_work),
            h_under6_m_work=sum(h_under6_m_work),
            h_under6_single_m=sum(h_under6_single_m),
            h_6to17_both_work=sum(h_6to17_both_work),
            h_6to17_fwork=sum(h_6to17_fwork),
            h_6to17_mwork=sum(h_6to17_mwork),
            h_6to17_single_m=sum(h_6to17_single_m)
            ) |>
  ungroup()

avg_father_utah_u6 <-round((100*sum(cc_summary$h_under6_f_work[cc_summary$state_name=="Utah"])/(sum(cc_summary$h_under6_m_work[cc_summary$state_name=="Utah"])+sum(cc_summary$h_under6_f_work[cc_summary$state_name=="Utah"]) + sum(cc_summary$h_under6_both_work[cc_summary$state_name=="Utah"]))),1)
avg_mother_utah_u6 <-round((100*sum(cc_summary$h_under6_m_work[cc_summary$state_name=="Utah"])/(sum(cc_summary$h_under6_m_work[cc_summary$state_name=="Utah"])+sum(cc_summary$h_under6_f_work[cc_summary$state_name=="Utah"]) + sum(cc_summary$h_under6_both_work[cc_summary$state_name=="Utah"]))),1)
avg_both_utah_u6 <-round((100*sum(cc_summary$h_under6_both_work[cc_summary$state_name=="Utah"])/(sum(cc_summary$h_under6_m_work[cc_summary$state_name=="Utah"])+sum(cc_summary$h_under6_f_work[cc_summary$state_name=="Utah"]) + sum(cc_summary$h_under6_both_work[cc_summary$state_name=="Utah"]))),1)

u6 <- cc_summary |>
  filter((h_under6_both_work <= h_under6_f_work) | (h_under6_both_work <= h_under6_m_work)) |>
  ggplot(aes(x=study_year)) +
  geom_line(aes(y=h_under6_both_work), colour="brown") +
  geom_line(aes(y=h_under6_f_work), colour="blue") +
  geom_line(aes(y=h_under6_m_work), colour="magenta") +
  geom_text(label=sprintf("Both working (avg %s%%)",avg_both_utah_u6), x=2016, y=114000, color="brown") +
  geom_text(label=sprintf("Only father working (avg %s%%)",avg_father_utah_u6), x=2016, y=133000, color="blue") +
  geom_text(label=sprintf("Only mother working (avg %s%%)",avg_mother_utah_u6), x=2016, y=9000, color="magenta") +
  facet_wrap(state_name~.) +
  labs(x="Year",
       y="Number of households",
       title="Unique Utah",
       subtitle="It is the ONLY US state where more two-parent households with children under 6 had only one parent (here, the father) working.") +
  theme_linedraw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2008:2018)) +
  scale_y_continuous(labels = label_comma())

b6_to_17 <- cc_summary |>
  filter(state_name=="Utah") |>
  ggplot(aes(x=study_year)) +
  geom_line(aes(y=h_6to17_both_work), colour="brown") +
  geom_line(aes(y=h_6to17_fwork), colour="blue") +
  geom_line(aes(y=h_6to17_mwork), colour="magenta") +
  geom_text(label="Both working", x=2016, y=265000, color="brown") +
  geom_text(label="Only father working", x=2016, y=200000, color="blue") +
  geom_text(label="Only mother working", x=2016, y=20000, color="magenta") +
  facet_wrap(state_name~.) +
  labs(x="Year",
       y="Number of households",
       subtitle="The trend does not hold for two-parent households with children between 6 and 17, and matches the trend of all other states.")+
  theme_linedraw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(2008:2018)) +
  scale_y_continuous(labels = label_comma())

ggarrange(u6, b6_to_17, nrow=2)
ggsave("utah.jpg", width=10, height=10) 

###############################################################################
#Viz2
# Percent of civilians aged 16 years old and older in the county
# emp_m : management, business, science, and arts occupations
# emp_service	: service occupations 
# emp_sales	 : sales and office occupations 
# emp_n	: natural resources, construction, and maintenance occupations
# emp_p : production, transportation, and material moving occupations

if(!require(maps, quietly=T)) install.packages("maps", repos = "https://cloud.r-project.org/")
library(maps, quietly=T)
if(!require(mapdata, quietly=T)) install.packages("mapdata", repos = "https://cloud.r-project.org/")
library(mapdata, quietly=T)
if(!require(magick, quietly=T)) install.packages("magick", repos = "https://cloud.r-project.org/")
library(magick, quietly=T)
if(!require(gifski, quietly=T)) install.packages("gifski", repos = "https://cloud.r-project.org/")
library(gifski, quietly=T)

cc_emp <- cc2 |>
  select(county_fips_code, county_name, state_name, study_year, emp_m, emp_service, emp_sales, emp_n, emp_p)

#selecting the category of maximum employment in every row
cc_emp <- cc_emp |> 
  mutate(max_emp = colnames(cc_emp)[max.col(cc_emp[,5:9],ties.method="first")+4])

data(county.fips)
county_geo <- map_data("county")

county_geo_fips  <- county_geo %>%
  mutate(polyname = paste(region,subregion,sep=",")) %>%
  left_join(county.fips, by="polyname")

cc_emp_geo <- cc_emp |>
  left_join(county_geo_fips, by=c('county_fips_code' = 'fips'))

cc_emp_geo2008 <- cc_emp_geo |>
  filter(study_year==2008) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2008", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE),
         plot.title=element_text(size=20))
ggsave(filename = "2008.png", plot=cc_emp_geo2008, width=14,height=10,units="in")
d.2008 <- image_read("2008.png")

cc_emp_geo2009 <- cc_emp_geo |>
  filter(study_year==2009) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2009", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2009.png", plot=cc_emp_geo2009, width=14,height=10,units="in")
d.2009 <- image_read("2009.png")

cc_emp_geo2010 <- cc_emp_geo |>
  filter(study_year==2010) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2010", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2010.png", plot=cc_emp_geo2010, width=14,height=10,units="in")
d.2010 <- image_read("2010.png")

cc_emp_geo2011 <- cc_emp_geo |>
  filter(study_year==2011) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2011", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2011.png", plot=cc_emp_geo2011, width=14,height=10,units="in")
d.2011 <- image_read("2011.png")

cc_emp_geo2012 <- cc_emp_geo |>
  filter(study_year==2012) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2012", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2012.png", plot=cc_emp_geo2012, width=14,height=10,units="in")
d.2012 <- image_read("2012.png")

cc_emp_geo2013 <- cc_emp_geo |>
  filter(study_year==2013) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2013", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2013.png", plot=cc_emp_geo2013, width=14,height=10,units="in")
d.2013 <- image_read("2013.png")

cc_emp_geo2014 <- cc_emp_geo |>
  filter(study_year==2014) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2014", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2014.png", plot=cc_emp_geo2014, width=14,height=10,units="in")
d.2014 <- image_read("2014.png")

cc_emp_geo2015 <- cc_emp_geo |>
  filter(study_year==2015) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2015", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2015.png", plot=cc_emp_geo2015, width=14,height=10,units="in")
d.2015 <- image_read("2015.png")

cc_emp_geo2016 <- cc_emp_geo |>
  filter(study_year==2016) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2016", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2016.png", plot=cc_emp_geo2016, width=14,height=10,units="in")
d.2016 <- image_read("2016.png")

cc_emp_geo2017 <- cc_emp_geo |>
  filter(study_year==2017) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2017", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2017.png", plot=cc_emp_geo2017, width=14,height=10,units="in")
d.2017 <- image_read("2017.png")

cc_emp_geo2018 <- cc_emp_geo |>
  filter(study_year==2018) |>
  ggplot(aes(x=long, y=lat, group = group, fill=max_emp)) + 
  scale_fill_manual(labels = c("Management, business, science, and arts", 
                               "Natural resources, construction, and maintenance",
                               "Production, transportation, and material moving",
                               "Sales and office",
                               "Service"), 
                    values=c("antiquewhite2",
                             "darkolivegreen2",
                             "azure4",
                             "deepskyblue2",
                             "darksalmon")) +
  geom_polygon() +
  coord_quickmap() +
  labs(x="", y="", title="Production and Sales employment have lost dominance", subtitle="2018", 
       fill="Category of\nmax employment           ",
       caption=" ")+
  theme(legend.position="bottom",
        plot.subtitle = element_text(size=20, hjust=1),
        plot.title=element_text(size=20)) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))
ggsave(filename = "2018.png", plot=cc_emp_geo2018, width=14,height=10,units="in")
d.2018 <- image_read("2018.png")

img <- c(d.2008,d.2009,d.2010,d.2011,d.2012,d.2013,d.2014,d.2015,d.2016,d.2017,d.2018)
my.animation<-image_animate(image_scale(img, "1400x1000"), fps = 1, dispose = "previous", loop=1)
image_write_gif(my.animation, "emp-spread.gif", loop=T, delay=1/3)

###############################################################################

