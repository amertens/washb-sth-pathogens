

###Load in data
rm(list=ls())
source(here::here("0-config.R"))

wbb_prot <- read.csv(here("osf data/washb-bangladesh-protozoa-public.csv"))
wbb_enrol <- read.csv(here("osf data/washb-bangladesh-enrol-public.csv"))
wbb_tr <- read.csv(here("osf data/washb-bangladesh-tr-public.csv"))
wbb_diar <- read.csv(here("osf data/washb-bangladesh-diar-public.csv"))
wbb_sth <- read.csv(here("osf data/washb-bangladesh-sth-public.csv"))


wbk_tr <- read.csv(here("osf data/washb-kenya-tr-public.csv"))
wbk_diar <- read.csv(here("osf data/washb-kenya-diar-public.csv")) %>% filter(time==2)
wbk_sth <- read_dta(here("osf data/parasites_kenya_public_ca20171215.dta")) %>% filter(!is.na(sth_coinf)|!is.na(giardia_yn))

#Clean WBB datasets
wbb_diar <- wbb_diar %>% filter(svy==2) %>% subset(., select =c("dataid","childid","sex", "agedays","diar7d","clusterid"))
wbb_sth <- wbb_sth %>% rename(childid=personid) %>% subset(., select =c("dataid","childid","logalepg","loghwepg","logttepg","al","tt","hw","sth","dirtfloor_hh","dirtfloor_lat","geophagia","dw","shoes","sac","lat","scoop"))
wbb_prot <- wbb_prot %>% rename(childid=personid)  %>% subset(., select =c("dataid","childid","posgi","poseh","poscr","posprot","posmult","ctgi","cteh","ctcr"))
  
#Merge WBB datasets
dim(wbb_diar)
dim(wbb_sth)
dim(wbb_prot)
wbb <- left_join(wbb_diar, wbb_sth, by=c("dataid","childid"))
wbb <- left_join(wbb, wbb_prot, by=c("dataid","childid"))
wbb <- wbb %>% filter(!is.na(sth)|!is.na(posprot))
wbb <- left_join(wbb, wbb_tr, by=c("clusterid"))
wbb <- left_join(wbb, wbb_enrol, by=c("dataid","clusterid","block"))
dim(wbb)
head(wbb)




#Should be around 7094 based on number of kids with diarrhea at year 2 and number of STH samples

table(wbb$tr)

#Clean WBB datasets
head(wbb_diar)
head(wbb_sth)
colnames(wbb_diar)
colnames(wbb_sth)

d <- anti_join(wbb_diar, wbb_sth, by=c("dataid","childid")) 
d2 <- anti_join( wbb_sth, wbb_diar, by=c("dataid","childid")) %>% filter(!is.na(sth))

dim(d)
dim(d2)
head(d)
head(d2)

table(d$childid)
table(d2$childid)

unique(d$dataid)[1:20]
unique(d2$dataid)[1:20]

d[d$dataid=="28002",]
d2[d2$dataid=="28002",]


wbk_diar <- wbk_diar %>%                                         # "HHS")) %>% 
  rename(diar7d=diarr7)
 wbk_sth <- wbk_sth %>% rename(childid=childidr2)


#Merge WBK datasets
dim(wbk_diar)
dim(wbk_sth)
wbk <- left_join(wbk_diar, wbk_sth, by=c("childid")) 
dim(wbk)

# WBB: 9,964 endline diarrhea measurements, 10,011 sth/giardia measurements, 5,373 children with both diarrhea and STH
# WBK: 7,770 endline diarrhea measurements, 9,077 sth/giardia measurements, 5,272  children with both diarrhea and STH

table(wbk$tr)

d <- anti_join(wbk_diar, wbk_sth, by=c("childid")) 
d2 <- anti_join( wbk_sth, wbk_diar, by=c("childid")) %>% filter(!is.na(sth_coinf)|!is.na(giardia_yn))

dim(d)
dim(d2)
head(d)
head(d2)

table(d$targetchild)
unique(d$hhid)[1:20]
unique(d2$hhidr2)[1:20]

d[d$hhid=="20062730",]
d2[d2$hhidr2=="20062730",]
d2$childid[d2$hhidr2=="20062730"]
2003053720

d[d$hhid=="20093030",]
d2[d2$hhidr2=="20093030",]
d2$childid[d2$hhidr2=="20093030"]
2006083720

#Note: seems like mostly either non-merging HHID's
#or merging HHID but different childid's 
#because of what seems like different target children
