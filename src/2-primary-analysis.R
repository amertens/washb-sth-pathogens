


###Load in data
rm(list=ls())
source(here::here("0-config.R"))

wbb <- readRDS(here("data/clean_wbb.rds"))
wbk <- readRDS(here("data/clean_wbk.rds"))
colnames(wbb)
colnames(wbk)

prop.table(table(wbb$sth,wbb$diar7d),1)
prop.table(table(wbk$sth_giar_coinf,wbk$diar7d),1)



#exposures
wbk_Xvars_bin <- c("qpcr_Ascaris","qpcr_Trichuris", "qpcr_Necator",     
                   "qpcr_Ancylostoma","qpcr_Strongyloides",
                   "ascaris_yn", "trichuris_yn", "hook_yn", "sth_yn", "giardia_yn", "sth_coinf", "sth_giar_coinf")
wbk_Xvars_cont <- c("asca_intensity",  "tric_intensity", "hook_intensity")
                    

wbb_Xvars_bin <- c("qpcr.positive.Ac",  "qpcr.positive.Ad",  "qpcr.positive.Al" ,
                   "qpcr.positive.IAC", "qpcr.positive.Na",  "qpcr.positive.Ss",  "qpcr.positive.Tt", 
                   "qpcr.positive.Hw",  "qpcr.positive.Sth", 
                   "al",             
"tt", "hw", "sth", "posgi", "poseh", "poscr", "posprot",        
"posmult"   )

wbb_Xvars_cont <- c("qpcr.CTmean.Ac","qpcr.CTmean.Ad",
                    "qpcr.CTmean.Al","qpcr.CTmean.IAC","qpcr.CTmean.Na","qpcr.CTmean.Ss",
                    "qpcr.CTmean.Tt",
                    "logalepg", "loghwepg", "logttepg","ctgi", "cteh", "ctcr")

for(i in wbk_Xvars_bin){print(summary(wbk[[i]]))}
for(i in wbk_Xvars_cont){print(summary(wbk[[i]]))}
for(i in wbb_Xvars_bin){print(summary(wbb[[i]]))}
for(i in wbb_Xvars_cont){print(summary(wbb[[i]]))}

#-------------------------------------------
# Unadjusted
#-------------------------------------------

wbb_bin_unadj <- NULL
for(i in wbb_Xvars_bin){
  res <- washb_sth_glm(wbb, Ws=NULL, outcome="diar7d", exposure=i, family="binomial")
  wbb_bin_unadj <- bind_rows(wbb_bin_unadj, res)
}
wbb_bin_unadj

wbk_bin_unadj <- NULL
for(i in wbk_Xvars_bin){
  res <- washb_sth_glm(wbk, Ws=NULL, outcome="diar7d", exposure=i, family="binomial")
  wbk_bin_unadj <- bind_rows(wbk_bin_unadj, res)
}
wbk_bin_unadj

wbb_cont_unadj <- NULL
for(i in wbb_Xvars_cont){
  res <- washb_sth_glm(wbb, Ws=NULL, outcome="diar7d", exposure=i, family="gaussian")
  wbb_cont_unadj <- bind_rows(wbb_cont_unadj, res)
}
wbb_cont_unadj

wbk_cont_unadj <- NULL
for(i in wbk_Xvars_cont){
  res <- washb_sth_glm(wbk, Ws=NULL, outcome="diar7d", exposure=i, family="gaussian")
  wbk_cont_unadj <- bind_rows(wbk_cont_unadj, res)
}
wbk_cont_unadj

#-------------------------------------------
# Adjusted
#-------------------------------------------

colnames(wbb)
colnames(wbk)


wbb_Wvars <- c(
"sex", "agedays", "dirtfloor_hh",      "geophagia",     "tr",               "Nhh",            
"Nlt18",           "momage",          "momheight",       "momedu",         
     "dadagri",         "landacre",              
"hfiacat",         "tubewell",        "watmin",          "storewat",        "treatwat",       
"odmen",           "odwom",           "odch85",         "odch38",          "odchu3",         
"latown",          "latslab",         "latseal",         "latfeces",        "potty",          
   "roof",            "walls",           "floor",   "elec",            "asset_radio",     "asset_tvbw",      "asset_tvcol",     "asset_refrig",   
"asset_bike",      "asset_moto",      "asset_sewmach",   "asset_phone",     "asset_tv",       
"asset_wardrobe",  "asset_table",     "asset_chair",     "asset_clock",     "asset_khat",     
"asset_chouki",    "asset_mobile")

wbk_Wvars <- c(
   "aged",              "sex",               "tr",               
      "Ncomp",             "cow",               "goat",              "chicken",          
"dog",               "mother_age",        "mother_edu",        "water_time",        "roof",             
"walls",             "floor",             "elec",              "radio",             "tv",               
"mobilephone",       "clock",             "bicycle",           "motorcycle",        "stove",            
"cooker",            "car",               "u8",               "HHS",               "dewormm",         
"soilw",             "wearing_shoes",     "sameday_defecation")


wbb_bin_adj <- NULL
for(i in wbb_Xvars_bin){
  res <- washb_sth_glm(wbb, Ws=wbb_Wvars, outcome="diar7d", exposure=i, family="binomial")
  wbb_bin_adj <- bind_rows(wbb_bin_adj, res)
}
wbb_bin_adj

wbk_bin_adj <- NULL
for(i in wbk_Xvars_bin){
  res <- washb_sth_glm(wbk, Ws=wbk_Wvars, outcome="diar7d", exposure=i, family="binomial")
  wbk_bin_adj <- bind_rows(wbk_bin_adj, res)
}
wbk_bin_adj

wbb_cont_adj <- NULL
for(i in wbb_Xvars_cont){
  res <- washb_sth_glm(wbb, Ws=wbb_Wvars, outcome="diar7d", exposure=i, family="gaussian")
  wbb_cont_adj <- bind_rows(wbb_cont_adj, res)
}
wbb_cont_adj

wbk_cont_adj <- NULL
for(i in wbk_Xvars_cont){
  res <- washb_sth_glm(wbk, Ws=wbk_Wvars, outcome="diar7d", exposure=i, family="gaussian")
  wbk_cont_adj <- bind_rows(wbk_cont_adj, res)
}
wbk_cont_adj



save(
  wbb_bin_unadj, wbk_bin_unadj, wbb_cont_unadj, wbk_cont_unadj,
  wbb_bin_adj, wbk_bin_adj, wbb_cont_adj, wbk_cont_adj,
  file=here("results/results.Rdata"))
