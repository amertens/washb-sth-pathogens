

###Load in data
rm(list=ls())
source(here::here("0-config.R"))

wbb_prot <- read.csv(here("osf data/washb-bangladesh-protozoa-public.csv"))
wbb_enrol <- read.csv(here("osf data/washb-bangladesh-enrol-public.csv"))
wbb_tr <- read.csv(here("osf data/washb-bangladesh-tr-public.csv"))
wbb_diar <- read.csv(here("osf data/washb-bangladesh-diar-public.csv"))
wbb_sth <- read.csv(here("osf data/washb-bangladesh-sth-public.csv"))


wbk_tr <- read.csv(here("osf data/washb-kenya-tr-public.csv"))
wbk_diar <- read.csv(here("osf data/washb-kenya-diar-public.csv"))
wbk_sth <- read_dta(here("osf data/parasites_kenya_public_ca20171215.dta"))

#Clean WBB datasets
wbb_diar <- wbb_diar %>% filter(svy==2) %>% subset(., select =c("dataid","childid","sex", "agedays","diar7d","clusterid"))
wbb_sth <- wbb_sth %>% rename(childid=personid) %>% subset(., select =c("dataid","childid","logalepg","loghwepg","logttepg","al","tt","hw","sth","dirtfloor_hh","dirtfloor_lat","geophagia","dw","shoes","sac","lat","scoop"))
wbb_prot <- wbb_prot %>% rename(childid=personid)   %>% subset(., select =c("dataid","childid","posgi","poseh","poscr","posprot","posmult","ctgi","cteh","ctcr"))
  
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


#Clean WBK datasets
head(wbk_diar)
head(wbk_sth)
colnames(wbk_diar)
colnames(wbk_sth)



wbk_diar <- wbk_diar %>% subset(., select =c("childid","clusterid","aged","sex","tr","diarr7",
                                             "Ncomp", "cow", "goat", "chicken",
                                             "dog","mother_age","mother_edu","water_time","roof","walls",
                                             "floor","elec","radio","tv","mobilephone","clock",
                                             "bicycle","motorcycle","stove","cooker","car","u18",
                                             "HHS")) %>% 
  rename(diar7d=diarr7)
wbk_sth <- wbk_sth %>% subset(., select =c("childidr2","deworm6m","soilw",
                                              "wearing_shoes","sameday_defecation", "asca_epg","asca_intensity","asca_intensity_cat",
                                             "tric_epg","tric_intensity","tric_intensity_cat", "hook_epg","hook_intensity",
                                             "hook_intensity_cat", "ascaris_yn","trichuris_yn","hook_yn","sth_yn",
                                             "giardia_yn","sth_coinf","sth_giar_coinf")) %>% rename(childid=childidr2)


#Merge WBK datasets
dim(wbk_diar)
dim(wbk_sth)
wbk <- left_join(wbk_diar, wbk_sth, by=c("childid"))
dim(wbk)


#Clean combined datasets
wbb$posgi[wbb$posgi==9] <- NA
wbb$poseh[wbb$poseh==9] <- NA
wbb$poscr[wbb$poscr==9] <- NA
wbb$posprot[wbb$posprot==9] <- NA

wbb$ctgi[wbb$ctgi==99] <- NA
wbb$cteh[wbb$cteh==99] <- NA
wbb$ctcr[wbb$ctcr==99] <- NA


#Save results
saveRDS(wbb, here("data/clean_wbb.rds"))
saveRDS(wbk, here("data/clean_wbk.rds"))

