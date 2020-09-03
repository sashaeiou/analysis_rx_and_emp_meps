library(dplyr)
library(survey)
#library(vardpoor)
#library(convey)
library(srvyr)
library(Hmisc)
install.packages("devtools")
library(devtools)
install_github("e-mitchell/meps_r_pkg/MEPS")
library(expss)
library(htmlTable)
library(sjlabelled)

rx2017 <- sasxport.get("~/Dropbox (GoodRx)/Goodrx Research (1)/Data - Source/AHRQ - MEPS/2017/h197a.ssp") #310,487 rows 
rx2017$dupersid <- as.character(rx2017$dupersid)

#conlink
clink2017 <-  sasxport.get("~/Dropbox (GoodRx)/GoodRx Research (1)/Data - Source/AHRQ - MEPS/2017/h197if1.ssp")


#con file
con2017 <- sasxport.get("~/Dropbox (GoodRx)/GoodRx Research (1)/Data - Source/AHRQ - MEPS/2017/h199.ssp")
#clink2017$evntidx <- as.numeric(as.character(clink2017$evntidx))

#hc data
hh2017 <- sasxport.get("~/Dropbox (GoodRx)/GoodRx Research (1)/Data - Source/AHRQ - MEPS/2017/h201.ssp") #31,880 rows 
hh <- hh2017 %>% select(dupersid, insurc17, age31x, age42x, age53x, age17x, agelast, empst31h, empst42h, empst53h, #add marriage status variables 
                        begrfm31, begrfy31, begrfm42, begrfy42, begrfm53, begrfy53, endrfm17, endrfy17, endrfm31, endrfy31, endrfm42, endrfy42, endrfm53, endrfy53,
                        marry31x, marry42x, marry53x
)

hh2017$dupersid <- as.character(hh2017$dupersid)
hh$dupersid <- as.character(hh$dupersid)

rx17.plus.hh <- rx2017 %>% left_join(hh, by='dupersid') #310,487 rows


options(survey.lonely.psu='adjust')

mepsdsgn <- svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt17f,
  data = rx17.plus.hh,
  nest = TRUE)  

df <- rx17.plus.hh
# drop 65-plus and under-18
df$u65 <- ifelse(df$age17x > 64 | df$age31x > 64 | df$age42x > 64 | df$age53x >64 , 0, 1)
df <- df %>% filter(u65 == 1)

df$u18 <- ifelse(df$age17x < 18 | df$age31x < 18 | df$age42x < 18 | df$age53x < 18, 1, 0)
df <- df %>% filter(u18 == 0) 

#############################################################################################################################################
############################################### next step: associate each rx event with an employment status ############################################### 

# round 3 for PANEL 22 or round 1 for PANEL 21: 8/16 - 3/17. 
# round 4 for PANEL 21 or round 2 for PANEL 21: 3/17 - 7/17
# round 5 for PANEL 21 or round 3 for PANEL 21: 7/17 - 12/17

# each drug should be assigned an employment status at the time it was picked up
# filter by round
z <- rx17.plus.hh %>% select(dupersid, empst31h, empst42h, empst53h, purchrd, rxdrgnam, rxname, rxndc, rxquanty, rxform, rxdaysup, rxsf17x, rxrecidx, drugidx, panel.y)

#create variable: emp_at_rx_fill 
test <- rx17.plus.hh %>% mutate(emp_at_rx_fill = ifelse(panel == 22 & purchrd == 1,empst31h,
                                                        ifelse(panel == 22 & purchrd == 2, empst42h,
                                                               ifelse(panel == 22 & purchrd == 3, empst53h,
                                                                      ifelse(panel == 21 & purchrd == 3, empst31h,
                                                                             ifelse(panel == 21 & purchrd == 4, empst42h,
                                                                                    ifelse(panel == 21 & purchrd == 5, empst53h, 99999)))))))



#create variable: marital_status_rx_fill 
test <- test %>% mutate(ms_at_rx_fill = ifelse(panel == 22 & purchrd == 1,marry31x,
                                                        ifelse(panel == 22 & purchrd == 2, marry42x,
                                                               ifelse(panel == 22 & purchrd == 3, marry53x,
                                                                      ifelse(panel == 21 & purchrd == 3, marry31x,
                                                                             ifelse(panel == 21 & purchrd == 4, marry42x,
                                                                                    ifelse(panel == 21 & purchrd == 5, marry53x, 99999)))))))



#### get total # of conditions for each person
cons.pp <- con2017 %>%
  group_by(dupersid) %>% #add marriage status variable 
  summarise(
    perwt17f = mean(perwt17f),
    n_conditions = n())

#merge w full file via conlink
cons.pp$dupersid <- as.character(cons.pp$dupersid)

dat <- test %>% left_join((cons.pp %>% select(dupersid, n_conditions)))


#merge with full file

######### for each person, calcuate # of prescriptions using rx2017 file

t <- dat %>%
  group_by(dupersid, emp_at_rx_fill, ms_at_rx_fill) %>% #add marriage status variable 
  summarise(
    perwt17f = mean(perwt17f),
    n_prescriptions = n(),
    mean_cost_pp = mean(rxxp17x),
    total_exp_pp = sum(rxxp17x))


t <- t %>% select(dupersid, n_prescriptions, mean_cost_pp, total_exp_pp)

### link back to rx17.plus.hh df 
dat <- dat %>% left_join(t, by="dupersid") 

### employment variables:
# -1 = NA
# 1 = Employed
# 2 = Job to return to 
# 34 = Not employed

################################################################################################v
########################## tables ##########################
########################################################################################################

# table 1. 
t <- dat %>% filter((insurc17 == 3))
t %>% 
  group_by(emp_at_rx_fill) %>%
  dplyr::summarize(rx_n = weighted.mean(n_prescriptions, perwt17f, na.rm=TRUE),
                   cost_per = weighted.mean(mean_cost_pp, perwt17f, na.rm=TRUE),
                   exp_total = weighted.mean(total_exp_pp, perwt17f, na.rm=TRUE))

################################################################################################v

test2 <- df %>%
  group_by(emp_at_rx_fill) %>%
  dplyr::summarize(rx_n = weighted.mean(n_prescriptions, perwt17f, na.rm=TRUE),
                   cost_per = weighted.mean(mean_cost_pp, perwt17f, na.rm=TRUE),
                   exp_total = weighted.mean(total_exp_pp, perwt17f, na.rm=TRUE))

test <- z %>%
  group_by(insurc17) %>%
  dplyr::summarize(rx_n = weighted.mean(n_prescriptions, perwt17f, na.rm=TRUE),
                   cost_per = weighted.mean(mean_cost_pp, perwt17f, na.rm=TRUE),
                   exp_total = weighted.mean(total_exp_pp, perwt17f, na.rm=TRUE))



