library(pdftools)
library(tidyverse)
library(gridExtra)
dslabs::ds_theme_set()
library(stringr)
library(lubridate)

filename <- "https://github.com/c2-d2/pr_mort_official/raw/master/data/RD-Mortality-Report_2015-18-180531.pdf"
txt <- pdf_text(filename)
dat <- lapply(seq_along(txt), function(i){
  s <- str_replace_all(txt[i], "\\*.*", "") %>%
    str_replace_all("Fuente: Registro Demográfico - División de Calidad y Estadísticas Vitales", "") %>%
    str_replace_all("Y(201\\d)\\*?", "\\1") %>%
    str_replace("SEP", "9") %>%
    str_replace("OCT", "10") %>%
    str_replace("NOV", "11") %>%
    str_replace("DEC", "12") %>%
    str_replace("JAN", "1") %>%
    str_replace("FEB", "2") %>%
    str_replace("MAR", "3") %>%
    str_replace("APR", "4") %>%
    str_replace("MAY", "5") %>%
    str_replace("JUN", "6") %>%
    str_replace("JUL", "7") %>%
    str_replace("AGO", "8") %>%
    str_replace("Total", "@") 
  
  tmp <- str_split(s, "\n") %>% .[[1]] %>% str_trim %>% str_split_fixed("\\s+", 50) %>% .[,1:5] %>% as_tibble()
  colnames(tmp) <- tmp[2,]
  tmp <- tmp[-(1:2),]
  j <- which(tmp[,1]=="@")
  if(colnames(tmp)[1]=="2") { ## deal with february 29
    k <- which(tmp==29)
    the_number <- unlist(tmp[k,-1])
    the_number <- the_number[the_number!=""]
    tmp[k, colnames(tmp)!="2016" & colnames(tmp)!="2"] <- 0
    tmp[k, "2016"] <- the_number
  }
  tmp <- tmp %>% slice(1:(j-1)) %>% mutate_all(funs(as.numeric)) %>%
    filter(!is.na(`2015`) & !is.na(`2016`) & !is.na(`2017`)  & !is.na(`2017`))
  tmp <- mutate(tmp, month = as.numeric(names(tmp)[1]))
  names(tmp)[1] <- "day"
  tmp <- tmp[,c(6,1,2,3,4,5)]
  ones <- which(tmp$day==1) ##1 2 3 4 appears due to graph... let's take it out
  if(length(ones)>1) tmp <- tmp[-ones[-1],]
  if(any(diff(tmp$day)!=1)) stop(i) ## check days are ordered
  ##check if negative. this means a black was left and the diff between 2016 and 0 was used!
  tmp[tmp<0] <- NA
  gather(tmp, year, deaths, 3:6, convert = TRUE) 
})
official <- do.call(rbind, dat) %>% 
  arrange(year, month, day) %>% 
  filter(!(month==2 & day==29 & year != 2016))
## add date
official <-official %>% mutate(date = ymd(paste(year, month, day,"-")))

head(official)

official %>% filter(year >= 2017 & deaths > 0) %>%
  ggplot(aes(date, deaths)) + 
  geom_point(alpha = 0.5) + 
  geom_vline(xintercept = make_date(2017,09,20), lty=2, col="grey") +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Datos sin procesar para 2017 y 2018")

dslabs::ds_theme_set()

tmp <- bind_rows(data.frame(date = make_date(year = 2010:2017, month = 7, day = 2), 
                            pop = population_by_year$pop),
                 data.frame(date = make_date(year=c(2017,2017,2017,2017,2018,2018), 
                                             month=c(9,10,11,12,1,2), 
                                             day = c(19,15,15,15,15,15)),
                            pop = c(3337000, 3237000, 3202000, 3200000, 3223000, 3278000)))
tmp <- approx(tmp$date, tmp$pop, xout=official$date, rule = 2)
predicted_pop <- data.frame(date = tmp$x, pop = tmp$y)
p1 <- qplot(date, pop, data=predicted_pop, ylab = "population", geom = "line")