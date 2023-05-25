#Loading data
if(!require(tidytuesdayR, quietly=T)) install.packages("tidytuesdayR", repos = "http://cran.us.r-project.org")
library(tidytuesdayR, quietly=T)

tuesdata <- tt_load('2023-04-11')
eggproduction <- tuesdata$`egg-production`
cagefreepercentages <- tuesdata$`cage-free-percentages`

#writing to local project folder to avoid too many Github API calls
write.csv(eggproduction, "eggproduction.csv")
write.csv(cagefreepercentages, "cagefreepercentages.csv")
eggproduction <- read.csv("eggproduction.csv")
cagefreepercentages <- read.csv("cagefreepercentages.csv")

###############################################################################
#Data overview
if(!require(tidyverse, quietly=T)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse, quietly=T)

head(eggproduction)
str(eggproduction)
summary(eggproduction)
sum(is.na(eggproduction)) #0

head(cagefreepercentages)
str(cagefreepercentages)
summary(cagefreepercentages)
sum(is.na(cagefreepercentages)) #42

###############################################################################
#Viz 1
if(!require(lubridate, quietly=T)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate, quietly=T)

#where data not available for the month in egg markets, take computed
cagefreepercentages$obs_eom <- ceiling_date(as.Date(cagefreepercentages$observed_month), "month") - days(1)
cagefreepercentages <- cagefreepercentages |>
  mutate(percent_hens_2 = ifelse((duplicated(obs_eom) | duplicated(obs_eom, fromLast=TRUE)) & source=="computed" , NA, percent_hens))

cagefreepercentages |> 
  ggplot(aes(x=obs_eom)) +
  geom_line(data=cagefreepercentages[!is.na(cagefreepercentages$percent_hens_2),], aes(y=percent_hens_2), colour="brown") +
  geom_text(label="Hens", x=as.Date("2021-01-31"), y=27, color="brown") +
  geom_line(data=cagefreepercentages[!is.na(cagefreepercentages$percent_eggs),], aes(y=percent_eggs), colour="blue") +
  geom_text(label="Eggs", x=as.Date("2021-01-31"), y=23, color="blue") +
  labs(x="",
       y="Percentage of total hens/eggs",
       title="Exponential rise in cage free hens and eggs",
       subtitle="Percentage of eggs produced in cage free housing appear to be a leading indicator of percentage hens",
       caption="US hatching egg-layer hens are distinguished only by breed and their housing systems are classified differently. Also, public data provide no indication of the proportion of hatching eggs that are destined for cage-free systems\nor otherwise. Hence, here total hens or eggs implies table egg-laying hens and table eggs respectively.") +
  theme_linedraw() +
  theme(legend.position = "none")
ggsave("cagefree_hg.jpg")

###############################################################################
#Viz 2
if(!require(scales, quietly=T)) install.packages("scales", repos = "http://cran.us.r-project.org")
library(scales, quietly=T)

eggproduction$observed_month <- as.Date(eggproduction$observed_month)
#Getting the averages of number of eggs and means for both hatching and table
henmeans <- eggproduction |> filter(prod_process=="all") |> group_by(prod_type) |> summarise(mean_val=mean(n_hens))
eggmeans <- eggproduction |> filter(prod_process=="all") |> group_by(prod_type) |> summarise(mean_val=mean(n_eggs))

eggproduction |> filter(prod_process=="all") |>
  arrange(prod_type) |>
  ggplot(aes(x=observed_month, group=prod_type)) +
  geom_line(aes(y=n_hens/1000000, linetype=prod_type), colour="brown") +
  geom_text(label="Table Hens", x=as.Date("2020-12-30"), y=210, color="brown", size=2.5) +
  geom_text(label="Hatching Hens", x=as.Date("2020-12-10"), y=-90, color="brown", size=2.5) +
  geom_line(aes(y=n_eggs/1000000, linetype=prod_type), colour="blue") +
  geom_text(label="Table Eggs", x=as.Date("2020-12-30"), y=7800, color="blue", size=2.5) +
  geom_text(label="Hatching Eggs", x=as.Date("2020-12-10"), y=1050, color="blue", size=2.5) +
  scale_linetype_manual(values = c("dotted","dashed")) +
  labs(x="",
       y="Number in millions",
       title="Total number of hens and eggs for table/hatching",
       caption="") +
  theme_linedraw() +
  #average lines and value labels
  geom_hline(data=eggmeans, aes(yintercept = mean_val/1000000), color="grey", linetype = "twodash") +
  geom_text(data=eggmeans, aes(x=as.Date("2016-8-15"), y=mean_val/1000000, label = paste(round(mean_val/1000000,0),"million")), vjust = 1.3,  color="darkgrey", fontface=2, size=2.5)+
  geom_hline(data=henmeans, aes(yintercept = mean_val/1000000), color="grey", linetype = "twodash") +
  geom_text(data=henmeans, aes(x=as.Date("2016-8-10"), y=mean_val/1000000, label = paste(round(mean_val/1000000,0),"million")), vjust = 1.3,  color="darkgrey", fontface=2, size=2.5)+
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%m-%Y")) +
  scale_y_continuous(labels = label_comma())
ggsave("all_hatchtable.jpg")

###############################################################################
#Viz 3
if(!require(ggalluvial, quietly=T)) install.packages("ggalluvial", repos = "http://cran.us.r-project.org")
library(ggalluvial, quietly=T)

eggproduction |>
  filter(duplicated(cbind(observed_month, prod_type, prod_process)))
# noduplicates, so can work with the latest data

#selecting the latest data only
eggproductionsummary <- eggproduction |>
  select(observed_month, prod_type, prod_process, n_hens, n_eggs) |>
  group_by(prod_type, prod_process) |>
  arrange(desc(observed_month)) |>
  filter(row_number()==1) |>
  ungroup() |>
  select(-observed_month) 

#computing caged from all and cage-free numbers
eggproductionsankey <- eggproductionsummary |>
  add_row(prod_type="table eggs",
          prod_process="caged", 
          n_hens=sum(eggproductionsummary[eggproductionsummary$prod_type=="table eggs" & eggproductionsummary$prod_process=="all","n_hens"])-sum(eggproductionsummary[str_detect(eggproductionsummary$prod_process,"cage-free"),"n_hens"]),
          n_eggs=sum(eggproductionsummary[eggproductionsummary$prod_type=="table eggs" & eggproductionsummary$prod_process=="all","n_eggs"])-sum(eggproductionsummary[str_detect(eggproductionsummary$prod_process,"cage-free"),"n_eggs"])
          ) |>
  filter(!(prod_process=="all" & prod_type=="table eggs"))

#reclassifying production process into housing and management processes
eggproductionsankey <- eggproductionsankey |> 
  mutate(housing=ifelse(str_detect(eggproductionsankey$prod_process,"cage-free"),"Cage-free",ifelse(str_detect(eggproductionsankey$prod_process,"caged"),"Caged","Unclassified")),
         mgmt=ifelse(str_detect(eggproductionsankey$prod_process,"\\(organic\\)"),"Organic",ifelse(str_detect(eggproductionsankey$prod_process,"\\(non-organic\\)"),"Non-organic",ifelse(str_detect(eggproductionsankey$prod_process,"caged"),"Non-organic","Unclassified")))
         ) |>
  select(-prod_process, prod_type, housing, mgmt, n_hens, n_eggs)

eggproductionsankey |>
  arrange(prod_type,housing,desc(mgmt)) |>
  ggplot(aes(axis1=prod_type, axis2=housing, axis3=mgmt, y=n_hens)) +
  geom_alluvium(aes(fill=housing),alpha=0.7, width=1/4, decreasing=TRUE) +
  geom_stratum(width=1/4, decreasing=TRUE) +
  geom_text(stat="stratum", aes(label=after_stat(stratum)), size=5, decreasing=TRUE,
            angle=rep(0,8)) +
  theme_minimal() +
  scale_x_discrete(limits=c("Year","Housing","Management")) +
  labs(x="",
       y="",
       title="\nRelationship between hens housing and management",
       caption="US hatching egg-layer hens are distinguished only by breed and their housing systems are classified differently, not reflected in this data source.\n"
  ) +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks=element_blank(),
        title = element_text(size=28),
        plot.caption = element_text(size=15)
  ) +
  scale_fill_manual(values=c("darkolivegreen4","coral3", "bisque3"))

ggsave("hens_sankey.jpg", width=20, height=13) 

###############################################################################
#Viz 4
if(!require(ggpubr, quietly=T)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
library(ggpubr, quietly=T)

#computing caged data for every month of observations
eggproductiontable <- eggproduction |>
  filter(prod_type=="table eggs" & observed_month <= as.Date("2021-01-31") & observed_month >= as.Date("2016-08-31")) |>
  select(-X, -source, -prod_type) |>
  group_by(observed_month) |>
  group_modify(~ add_row(prod_process="caged (non-organic)", 
                         n_hens=sum(.x[.x$prod_process=="all","n_hens"])-sum(.x[str_detect(.x$prod_process,"cage-free"),"n_hens"]),
                         n_eggs=sum(.x[.x$prod_process=="all","n_eggs"])-sum(.x[str_detect(.x$prod_process,"cage-free"),"n_eggs"]),
                         .x)) |>
  ungroup()

hens <- eggproductiontable |>
  ggplot(aes(x=observed_month, y=n_hens/1000000)) +
  geom_area(aes(fill=fct_reorder(prod_process, n_hens, .desc=TRUE)), alpha=0.8, position="identity") +
  geom_line(aes(group=fct_reorder(prod_process, n_hens, .desc=TRUE)), color="bisque4") +
  scale_fill_manual(values=c("bisque2","coral3","khaki", "darkolivegreen")) +
  geom_segment(x=as.Date("2020-01-13"), xend=as.Date("2021-01-31"), y=285, yend=285, arrow=arrow(length=unit(0.2,"cm"), ends="both"), colour="black") +
  geom_text(label="COVID-19", x=as.Date("2020-08-1"), y=300, color="black", size=4) +
  labs(x="",
       y="Total number (in millions)",
       title="",
       subtitle="",
       fill="Housing & Management",
       caption="") +
  theme_linedraw()
  
eggs <- eggproductiontable |>
  ggplot(aes(x=observed_month, y=n_eggs/1000000)) +
  geom_area(aes(fill=fct_reorder(prod_process, n_eggs, .desc=TRUE)), alpha=0.8, position="identity") +
  geom_line(aes(group=fct_reorder(prod_process, n_eggs, .desc=TRUE)), color="bisque4") +
  scale_fill_manual(values=c("bisque2","coral3","khaki", "darkolivegreen")) +
  geom_segment(x=as.Date("2020-01-13"), xend=as.Date("2021-01-31"), y=7050, yend=7050, arrow=arrow(length=unit(0.2,"cm"), ends="both"), colour="black") +
  geom_text(label="COVID-19", x=as.Date("2020-08-1"), y=7380, color="black", size=4) +
  labs(x="",
       y="",
       title="",
       subtitle="",
       fill="",
       caption="") +
  theme_linedraw()

plot <- ggarrange(hens, eggs, ncol=2, common.legend=TRUE, legend="bottom")
annotate_figure(plot, 
                top=text_grob("\nNotable Observations:\n1. The rise in cage-free hens and eggs produced thereby is associated with increase in non-organic management practices.\n2. Covid-19 cause a big dip in overall number of hens and eggs, primarily from caged hens. Cage-free housing showed no such loss.", 
                              color="black", size=14, hjust=0, x=0.02),
                bottom=text_grob(" "))
ggsave("hens_eggs_division.jpg", width=15, height=10) 

###############################################################################
#Viz 5

eggproductiontable |> 
  ggplot(aes(x=as.factor(month(observed_month)), y=n_eggs/1000000, fill=prod_process)) +
  geom_boxplot(show.legend=F) +
  geom_point(show.legend=F) +
  annotate(geom = "rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.3) +
  facet_wrap(.~factor(prod_process, levels=c("all","caged (non-organic)","cage-free (non-organic)", "cage-free (organic)")), scales="free")+
  scale_fill_manual(values=c("bisque2","khaki","darkolivegreen","coral3")) +
  labs(x="Month",
       y="Number of eggs (in millions)",
       title="Variability in egg production across months",
       subtitle="Februrary shows a dip in egg production",
       caption="") +
  theme_linedraw()
ggsave("eggs_months.jpg", width=15, height=10) 

eggproductiontable |> 
  ggplot(aes(x=as.factor(month(observed_month)), y=n_hens/1000000, fill=prod_process)) +
  geom_boxplot(show.legend=F) +
  geom_point(show.legend=F) +
  annotate(geom = "rect", xmin = 6.5, xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.3) +
  facet_wrap(.~factor(prod_process, levels=c("all","caged (non-organic)","cage-free (non-organic)", "cage-free (organic)")), scales="free")+
  scale_fill_manual(values=c("bisque2","khaki","darkolivegreen","coral3")) +
  labs(x="Month",
       y="Number of hens (in millions)",
       title="Variability in hens across months",
       subtitle="US shows a dip in number of hens in late summer-autumn",
       caption="") +
  theme_linedraw()
ggsave("hens_months.jpg", width=15, height=10) 

#Best, Nemo.
