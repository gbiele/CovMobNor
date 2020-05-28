
# data google: https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?
# data apple: https://www.apple.com/covid19/mobility/applemobilitytrends-2020-05-24.csv
# weather data: https://klimaservicesenter.no/observations/

library(data.table)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(rstanarm)
library(brms)
options(mc.cores = 4)
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

theme_set(
  theme_base() + 
    theme(axis.ticks.length = unit(2,"pt"),
          axis.title = element_text(size = 11,
                                    margin = margin(0,0,0,0)),
          axis.text = element_text(size = 10,
                                   margin = margin(0,0,0,0)),
          strip.text.x = element_text(size = 11,
                                      margin = margin(0,0,0,0)),
          plot.background = element_rect(linetype = "blank")))


NMI = fread("NMI.csv") %>%
  .[, start_date := as.Date(start_date, format = "%d.%m.%Y")] %>%
  .[, stop_date := as.Date(stop_date, format = "%d.%m.%Y")] %>%
  .[is.na(stop_date), stop_date := as.Date("2020-05-31")]
NMIl = 
  NMI %>% melt(id.vars = "intervention",
               value.name = "date") %>%
  .[, y := as.numeric(factor(intervention))]

NMIp = 
  ggplot(NMIl, aes(x = date, y = y, label = intervention, group = intervention)) + 
  geom_line() + 
  geom_text(data = NMIl[variable == "stop_date"], nudge_y = .25) + 
  xlim(as.Date("2020-03-10"),as.Date("2020-06-05"))


holidays = fread("helligdagskalender-4.csv") %>%
  .[år == 2020] %>%
  .[, date := as.Date(dato, format = "%d.%m.%Y")] 


weather = 
  rbind(fread("Weather1.csv"),
        fread("Weather2.csv"),
        fread("Weather3.csv")) %>%
  .[Navn != "Data er gyldig per 26.05.2020 (CC BY 4.0), Meteorologisk institutt (MET)"] %>%
  .[Navn == "Oslo - Blindern", region := "Oslo"] %>%
  .[Navn == "Arendal Lufthavn", region := "Agder"] %>%
  .[Navn == "Kristiansand - Sømskleiva", region := "Agder"] %>%
  .[Navn == "Hamar Ii", region := "Innlandet"] %>%
  .[Navn == "Ålesund Iv", region := "Møre og Romsdal"] %>%
  .[Navn == "Bodø Vi", region := "Nordland"] %>%
  .[Navn == "Stavanger - Våland", region := "Rogaland"] %>%
  .[Navn == "Tromsø", region := "Troms og Finnmark"] %>%
  .[Navn == "Trondheim - Risvollan", region := "Trøndelag"] %>%
  .[Navn == "Gjerpen - Århus", region := "Vestfold og Telemark"] %>%
  .[Navn == "Bergen - Sandsli", region := "Vestland"] %>%
  .[Navn == "Drammen - Berskog", region := "Viken"] %>%
  .[, c("Navn","Stasjon") := NULL] %>%
  setnames(c("Minimumstemperatur (døgn)",
             "Middeltemperatur (døgn)",
             "Maksimumstemperatur (døgn)",
             "Tid(norsk normaltid)"),
           c("temp_min","temp_avg","temp_max","date")) %>%
  .[, date := as.Date(date, format = "%d.%m.%Y")] %>%
  .[, temp_avg := sub("-","",temp_avg)] %>%
  .[, temp_max := sub("-","",temp_max)] %>%
  .[, temp_min := sub("-","",temp_min)] %>%
  .[, temp_avg := as.numeric(sub(",",".",temp_avg))] %>%
  .[, temp_max := as.numeric(sub(",",".",temp_max))] %>%
  .[, temp_min := as.numeric(sub(",",".",temp_min))] %>%
  .[, list(temp_avg = mean(temp_avg, na.rm = T),
           temp_min = mean(temp_min, na.rm = T),
           temp_max = mean(temp_max, na.rm = T)),
    by = c("date","region")] %>%
  .[is.na(temp_avg), temp_avg := (temp_min + temp_max)/2 ]


goo = fread("Global_Mobility_Report.csv") %>% 
  .[country_region_code == "NO"] %>%
  .[, c("country_region_code","country_region","sub_region_2") := NULL] %>% 
  melt(id.vars = c("date","sub_region_1"), value.name = "y") %>%
  .[, variable := gsub("_percent_change_from_baseline","",variable)] %>% 
  .[sub_region_1 == "", sub_region_1 := "Norway"] %>%
  .[, date := as.Date(date)] %>%
  setnames(c("sub_region_1","variable"),c("region","location")) %>%
  .[, src := "google"]


apl = fread("applemobilitytrends-2020-05-24.csv")  %>%
  .[country == "Norway"] %>%
  .[, c("geo_type","alternative_name","country","sub-region") := NULL] %>%
  melt(id.vars = c("region","transportation_type"),value.name = "y") %>%
  .[, variable := as.Date(as.character(variable),format = "%Y-%m-%d")] %>% 
  setnames(c("transportation_type","variable"),c("location","date")) %>% 
  .[, y := y -100] %>%
  .[, src := "apple"]

dt = 
  rbind(goo,apl) %>%
  .[, weekday := factor(weekdays(date),
                        level = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))] %>%
  .[, weekend := weekday %in% c("Saturday","Sunday")] %>%
  .[, holiday := ifelse(date %in% holidays$date, T, F) ] %>%
  .[, src := factor(src,levels = c("google","apple"))] %>%
  merge(weather, by = c("region","date"), all.x = T, all.y = F) %>%
  .[, open.kindergarten := ifelse(date >= NMI[intervention == "CloseKindergarten",stop_date] & 
                                    date < NMI[intervention == "CloseSchool",stop_date],1, 0)] %>%
  .[, open.KGschool := ifelse(date >= NMI[intervention == "CloseSchool",stop_date],1,0)]  %>%
  .[, open.group := ifelse(date >= NMI[intervention == "ProhibitGroups5_20",stop_date],1,0)] %>%
  .[, lock.down := ifelse(date >= as.Date("2020-03-12"),1,0)] %>%
  .[, lock.down.smooth := 0]  %>%
  .[date > as.Date("2020-03-11") & date < (as.Date("2020-03-11")+8), lock.down.smooth := scale(1:7), by = c("region","location")]



dtx = dt[region != "Norway" & location == "transit_stations" & !is.na(y)] 
ggplot(dtx, aes(x = date, y = y)) +
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = as.Date("2020-03-12"), col = "red") + 
  geom_line(stat = "identity") +
  facet_wrap(~region)

dtx[, date := scale(date)]

hlmfit = stan_glmer(y ~ lock.down + open.kindergarten + open.KGschool + open.group + poly(lock.down.smooth,2) + weekday + holiday + date + temp_avg +
                      (lock.down + open.kindergarten + open.KGschool + open.group + poly(lock.down.smooth,2) + weekday + holiday + date + temp_avg  | region),
                    data = data.frame(dtx))

pp = 
  dtx[,c("date","region")] %>%
  cbind(t(posterior_predict(hlmfit))) %>%
  melt(id.vars = c("date","region"), variable.name = "iter", value.name = "est") %>%
  .[,iter := as.numeric(gsub("V","",iter))]

ggplot(pp[iter < 50], aes(x = date, y = est, group = iter)) +
  geom_line(stat = "identity", alpha = .05, colour = "blue") +
  facet_wrap(~region) + 
  geom_line(data = dtx, aes(x = date, y = y, group = NA), color = "red")


dtm = 
  dtx[,all.vars(hlmfit$formula)[-1],with = F] %>% 
  .[, weekday := "Thursday"] %>%
  .[, holiday := F]  %>%
  .[, temp_avg := mean(dtx$temp_avg)]
ppm = 
  dtm[,c("date","region")] %>%
  cbind(t(posterior_predict(bhlmfit, newdata = data.frame(dtm)))) %>%
  melt(id.vars = c("date","region"), variable.name = "iter", value.name = "est") %>%
  .[,iter := as.numeric(gsub("V","",iter))]
ggplot(ppm[iter < 25], aes(x = date, y = est, group = iter)) +
  geom_line(stat = "identity", alpha = .1) +
  facet_wrap(~region) + 
  geom_line(data = dtx, aes(x = date, y = y, group = NA), color = "red")


dtr = 
  dt[region == "Oslo" & location == "transit_stations"] %>%
  .[, date := scale(date)]

ggplot(dtr, aes(x = date, y = y)) +
  geom_vline(xintercept = dt[holiday == T,date], col = adjustcolor("blue",alpha = .5))+ 
  geom_vline(xintercept = dt[weekday == "Saturday",date], col = "grey", lty = 2) + 
  geom_vline(xintercept = as.Date("2020-03-16"), col = "red") + 
  geom_line(stat = "identity") +
  geom_line(data = NMIl, aes(x = date, y = y, label = intervention, group = intervention)) + 
  geom_text(data = NMIl[variable == "stop_date"], nudge_y = .25,
            aes(x = date, y = y, label = intervention, group = intervention)) + 
  xlim(as.Date("2020-02-20"),as.Date("2020-06-05"))


lmfit = 
  lm( y ~ lock.down + open.kindergarten + open.KGschool + open.group + weekday + holiday + date + temp_avg,
      data = dtr)
plot(dtr$date,dtr$y,"l")
points(dtr$date,predict(lmfit), col = "red")

slmfit = 
  stan_glm( y ~ lock.down + open.kindergarten + open.KGschool + open.group + weekday + holiday + date + temp_avg,
            family = gaussian(),
            data = data.frame(dtr))

blmfit = 
  brm( y ~ lock.down + open.kindergarten + open.KGschool + open.group + weekday + holiday + date + temp_avg,
       family = gaussian(),
       data = data.frame(dtr))

blmsfit = 
  brm( y ~ lock.down + open.kindergarten + open.KGschool + open.group + weekday + holiday + date + temp_avg,
       family = student(),
       data = data.frame(dtr))


ggplot(dt[region == "Oslo"], aes(x = date, y = y, color = location, lty = src)) +
  geom_vline(xintercept = dt[holiday == T,date], col = adjustcolor("blue",alpha = .5))+ 
  geom_vline(xintercept = dt[weekday == "Saturday",date], col = "grey", lty = 2) + 
  geom_vline(xintercept = as.Date("2020-03-12"), col = "red") + 
  geom_line(stat = "identity") +
  facet_wrap(~location)


ggplot(goo[location != "parks" & region == "Norway"], aes(x = value, fill = weekend) ) +
  geom_histogram() +  
  facet_wrap(~location)


ggplot(dt[ location == "workplaces" & region != "Norway"], aes(x = temp_avg, y = value)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~freeday)


summary(lm(value ~ -1 + temp_avg + scale(date) + weekday, dt[location == "parks"]))