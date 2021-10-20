library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-10-19')
data.frame(tuesdata$pumpkins)->pumpkin
glimpse(pumpkin)
summary(pumpkin)


pumpkin[!grepl("Entries.", pumpkin$country),]->pumpkin1

pumpkin1%>%
  filter(place!="EXH")%>%
  filter(place!="DMG")%>%
  mutate(weight_lbs =parse_number(weight_lbs))%>%
  separate(id,into = c("Year","Type"),sep = "-")%>%
  mutate(Year=as.numeric(Year))%>%
  mutate(Type= recode(Type, "L"="Long Gourd (length in inches)", "F"="Field Pumpkin (weight in pounds)","W"="Giant Watermelon (weight in pounds)",
                      "P"="Giant Pumpkin (weight in pounds)","S"="Giant Squash (weight in pounds)","T"="Tomato (weight in pounds)")) %>%
    filter(country!="Unknown country")%>%
  drop_na(country)%>%
  select(Year, Type, country,weight_lbs)%>%
  group_by(Year)->p2

#filter for top pumpkins by size
p2%>%group_by(Type)%>%
  arrange(desc(weight_lbs),.by_group = TRUE)%>%
  slice_head(n = 100)->p5

#filter for top pumkins by country
p2%>%group_by(country)%>%
  arrange(desc(weight_lbs),.by_group = TRUE)%>%
  slice_head(n = 100)->p4


#jitterplot by type
p5%>%
  ggplot(aes(Year,weight_lbs,colour=as.factor(Year)))+
  geom_jitter(alpha=0.6, size=1)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000,2500,3000))+
  scale_colour_manual(values=c("#F9C10E", "#FA4113","#BFDA7A","#5E32BA","#27C424","#BF200E",
                               "#F9804D","#F5D913","#FFF093"))+
  facet_wrap(~Type)+
  theme(plot.background = element_rect(fill="black"),
        panel.background=element_rect(fill="black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face="bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face="bold"),
        panel.grid.minor.x = element_line(colour="#C0C0C0",linetype = "dotted"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="THE GREAT PUMPKIN WEIGHOFF",
       subtitle = "The data visualization belows displays the top 100 pumpkins in each type, based on size, according to the Great Pumpkin Council",
       caption="Data from BigPumpkins.com | Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))->pumpkinplot2


#jitterplot for country
p4%>%
  ggplot(aes(Year,weight_lbs,colour=as.factor(Type)))+
  geom_jitter(alpha=0.6, size=1)+
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000,2500,3000))+
  scale_colour_manual(values=c("#F9C10E", "#FA4113","#BFDA7A","#5E32BA","#27C424","#BF200E"))+
  facet_wrap(~country)+
  theme(plot.background = element_rect(fill="black"),
        panel.background=element_rect(fill="black"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face="bold"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face="bold"),
        panel.grid.minor.x = element_line(colour="#C0C0C0",linetype = "dotted"),
        panel.grid.major = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.background = element_rect("black"),
        legend.key = element_rect(fill="black"),
        legend.text = element_text(colour = "white", face="bold"))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="THE GREAT PUMPKIN WEIGHOFF",
       subtitle = "The data visualization belows displays the top 100 pumpkins in each country, based on size, according to the Great Pumpkin Council",
       caption="Data from BigPumpkins.com | Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))+
  guides(color = guide_legend(override.aes = list(size = 4)))->pumpkinplot1

#save the plots
ggsave("pumpkin.png",pumpkinplot1,width=18,height=9.5)
ggsave("pumpkin1.png",pumpkinplot2,width=12,height=6)
