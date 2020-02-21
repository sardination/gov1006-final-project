################################################################################
#### FIGURES ###################################################################
################################################################################

### FIGURE 2
summ.map <- merge(summ, map_data("state"), by.x="name", by.y="region")
summ.map <- arrange(summ.map, group, order)

map.yrs <- c(1940, 1975, 2010)

## Economic
setwd(paste0(rep.dir, "/pdf"))
pdf("maps_mass_econ.pdf", height=1.5, width=6.5)
summ.map %>%
  filter(Year %in% map.yrs) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=MassEcon00_mean)) +
  facet_wrap(~Year, ncol=3) +
  geom_polygon(colour="black", size=rel(.1)) +
  coord_map("polyconic") +
  viridis::scale_fill_viridis(direction=1) +
  theme_clean() +
  guides(fill=FALSE)
dev.off()

## Social
setwd(paste0(rep.dir, "/pdf"))
pdf("maps_mass_social.pdf", height=1.5, width=6.5)
summ.map %>%
  filter(Year %in% map.yrs) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=MassSocial00_mean)) +
  facet_wrap(~Year, ncol=3) +
  geom_polygon(colour="black", size=rel(.1)) +
  coord_map("polyconic") +
  viridis::scale_fill_viridis(direction=1) +
  theme_clean() +
  guides(fill=FALSE)
dev.off()

### FIGURE 3

## Economic
setwd(paste0(rep.dir, "/pdf"))
pdf("xs_respons_econ_by_era_south.pdf", width=7.5, height=4)
summ %>%
  group_by(Pre72, StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, PolicyEcon00_mean, label=StPO,
             color=South11, lty=South11)) +
  facet_wrap(~Years, scales="free") +
  geom_smooth(method=lm) +
  geom_text(show.legend=FALSE, alpha=2/3) +
  scale_colour_manual(values = c("black", "grey40")) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism", y="Government Liberalism", color=NULL, lty=NULL) +
  ggtitle("Economic Policies")
dev.off()

## Social
setwd(paste0(rep.dir, "/pdf"))
pdf("xs_respons_social_by_era_south.pdf", width=7.5, height=4)
summ %>%
  group_by(Pre72, StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, PolicySocial00_mean, label=StPO,
             color=South11, lty=South11)) +
  facet_wrap(~Years, scales="free") +
  geom_smooth(method=lm) +
  geom_text(show.legend=FALSE, alpha=2/3) +
  scale_colour_manual(values = c("black", "grey40")) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism", y="Government Liberalism", color=NULL, lty=NULL) +
  ggtitle("Social Policies") 
dev.off()


### MORE PLOTS
summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, PolicySocial00_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism",
       y="Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Social Issues") 

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, PolicyEcon00_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Liberalism",
       y="Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Economic Issues")

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicySocial00_mean = mean(PolicySocial00_mean, na.rm=TRUE),
            DemControl = mean(DemControl, na.rm=TRUE)) %>%
  ggplot(aes(x=DemControl, PolicySocial00_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Democratic Control",
       y="Social Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Social Issues") 

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            PolicyEcon00_mean = mean(PolicyEcon00_mean, na.rm=TRUE),
            DemControl = mean(DemControl, na.rm=TRUE)) %>%
  ggplot(aes(x=DemControl, PolicyEcon00_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Democratic Control",
       y="Economic Policy Liberalism",
       color=NULL, lty=NULL) +
  ggtitle("Economic Issues")

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemPID2Party_mean = mean(DemPID2Party_mean, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, DemPID2Party_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Social Liberalism",
       y="Democratic % of Major-Party Identifiers",
       color=NULL, lty=NULL) +
  ggtitle("Social Liberalism and Democratic Partisanship") 

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemPID2Party_mean = mean(DemPID2Party_mean, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, DemPID2Party_mean, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Economic Liberalism",
       y="Democratic % of Major-Party Identifiers",
       color=NULL, lty=NULL) +
  ggtitle("Economic Liberalism and Democratic Partisanship") 

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemControl = mean(DemControl, na.rm=TRUE),
            MassSocial00_mean = mean(MassSocial00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassSocial00_mean, DemControl, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free_x", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Social Liberalism",
       y="Democratic Control of State Government",
       color=NULL, lty=NULL) +
  ggtitle("Social Liberalism and Democratic Control") 

summ %>%
  group_by(## Pre72, 
           StPO, South11) %>%
  summarise(Years = paste0(min(NC(Year)), "-", max(NC(Year))),
            DemControl = mean(DemControl, na.rm=TRUE),
            MassEcon00_mean = mean(MassEcon00_mean, na.rm=TRUE)) %>%
  ggplot(aes(x=MassEcon00_mean, DemControl, label=StPO## ,
             ## color=South11, lty=South11
             )) +
  facet_wrap(~Years, scales="free_x", nrow=1) +
  geom_text(show.legend=FALSE) +
  geom_smooth(method=lm) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  labs(x="Mass Economic Liberalism",
       y="Democratic Control of State Government",
       color=NULL, lty=NULL) +
  ggtitle("Economic Liberalism and Democratic Control") 

summ %>%
  ungroup %>%
  filter(!is.na(DemControl)) %>%
  mutate(Control = cut(DemControl, breaks=2,
                       labels=c("Republican", "Democratic"))) %>% 
  ggplot(aes(x=NC(Year), y=PolicyEcon_mean, group=StPO, color=Control)) +
  theme_bw() +
  geom_line(alpha=1/2) +
  geom_smooth(aes(group=Control)) +
  labs(x="Year", y="Government Liberalism") +
  ggtitle("Economic Policies") +
  theme(plot.title = element_text(hjust = 0.5)) 

summ %>%
  ungroup %>%
  filter(!is.na(DemControl)) %>%
  mutate(Control = cut(DemControl, breaks=2,
                       labels=c("Republican", "Democratic"))) %>% 
  ggplot(aes(x=NC(Year), y=PolicySocial_mean, group=StPO, color=Control)) +
  theme_bw() +
  geom_line(alpha=1/2) +
  geom_smooth(aes(group=Control)) +
  labs(x="Year", y="Government Liberalism") +
  ggtitle("Social Policies") +
  theme(plot.title = element_text(hjust = 0.5)) 
