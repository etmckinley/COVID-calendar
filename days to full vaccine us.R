#Create a vaccine calendar tracking plot
#Created by: Eliot McKinley
#Contact: etmckinley@gmail.com
#Date: February 5 2021

library(tidyverse)
library(zoo)
library(lubridate)
library(cowplot)
library(colorspace)

data=read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/raw_data/vaccine_data_us_state_timeline.csv")


#download.file("https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/COVID_VACCINE_COUNTY_SUMMARY.XLSX",
  #            "./Covid Vaccines/COVID_VACCINE_COUNTY_SUMMARY.XLSX")

#county.data=read_xlsx("./Covid Vaccines/COVID_VACCINE_COUNTY_SUMMARY.XLSX", 
   #                   col_types = c("date", "guess", "numeric",  "numeric", "numeric", "numeric", "numeric", "numeric"))

state_population = 331002651

vaccines=data %>% 
  mutate(DATE=as.Date(date, format = "%m/%d/%Y")) %>% 
  group_by(DATE) %>% 
  summarise(total_doses=sum(doses_admin_total, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(NEW_VACCINE_COUNT = total_doses-lag(total_doses),
         POPULATION = state_population*.75) %>% 
  arrange(DATE) %>% 
  complete(DATE = seq.Date(as.Date("2020-12-20"), as.Date("2021-12-31"), by="day")) %>%
  mutate(doses.left = round(POPULATION)*2 - total_doses,
         rolling.7 = rollmean(x = NEW_VACCINE_COUNT, k=7, align="right", na.pad = T),
         days.to.all= doses.left/rolling.7,
         date.to.all = as.Date(DATE) + days.to.all,
         weekday = wday(DATE, label = T, week_start = 7),
         month = month(DATE, label = T, abbr = F),
         date = yday(DATE),
         week = epiweek(DATE)) %>% 
  filter(DATE >= as.Date("2021-01-01")) %>% 
  mutate(NEW_VACCINE_COUNT = NEW_VACCINE_COUNT/1000000)

vaccines$week[vaccines$month == "January" & vaccines$week == 53] = 0
vaccines = vaccines %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week)) 



p=vaccines %>%  
  ggplot(aes(x = weekday, y = -week, fill = NEW_VACCINE_COUNT)) +
  geom_tile(color = "black")  + 
  geom_text(aes(label = day(DATE)), 
            color= "black", 
            size = 5,  
            alpha = 0.1, 
            family = "Oswald", 
            fontface= "bold") +
  geom_text(aes(label = format(date.to.all, 
                               format = "%b %d\n%Y"), 
            color = if_else(NEW_VACCINE_COUNT >  ceiling(max(vaccines$NEW_VACCINE_COUNT, na.rm=T))*.66, "white", "black")), 
            size = 3, 
            family = "Oswald") +
  
  scale_color_manual(values=c("black", "white"), 
                     guide= FALSE)+
  scale_fill_continuous_sequential(palette = "BluYl",
                                   na.value = "transparent", 
                                   limits = c(0, ceiling(max(vaccines$NEW_VACCINE_COUNT, na.rm=T))),
                                   breaks = seq(0, ceiling(max(vaccines$NEW_VACCINE_COUNT, na.rm = TRUE)), .5))+
  theme(text=element_text(family = "Oswald", 
                          size = 30),
        aspect.ratio = 1/2,
        plot.subtitle = element_text(size=14),
        axis.text=element_text(size=10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(size=11),
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, 
                                    size=1),
        legend.direction = "horizontal",
        legend.position=c(.85, 1.12),
        legend.key.width = unit(.75, "in"),
        legend.key.height = unit(.3, "in"),
        legend.text = element_text(size=9),
        legend.background = element_rect(fill="transparent",
                                         color="transparent")) +
  
  facet_wrap(~month, nrow = 3, ncol = 4, scales = "free") +  
  labs(title = "USA COVID-19 Vaccine Projection Calendar",
       subtitle = "Daily projected date when all adults in the United States will be vaccinated*",
       fill="",
       caption = "@etmckinley Data: Centers for Civic Impact")

pout=ggdraw(p)+
  geom_curve(aes(x=.06, xend=.18, y=.9, yend=.83),
               size=1,
               curvature=0.3,
               arrow = arrow(length = unit(0.01, "npc")))+
  draw_text(x=.73, y=.955, text = "Daily Vaccine Doses\nAdminstered (millions)", family="Oswald", hjust=1, size=14)+
  draw_text(x=.01, y=.025, text = "*Based on 7 day average vaccination rate; assumes two doses per person", family="Oswald", hjust=0, size=10.5)+
  ggsave(paste0("./Covid Vaccines/Covid vaccine calendar USA ", Sys.Date(), ".png"), width=16, height=9)
