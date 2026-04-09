
getwd()


# Küp alma fonksiyonu
{
kup <- function(x) 
  return(x^3)
}
print(kup(5)) 

# Geri sayım fonksiyonu
gerisay <- function(den) {
  print(den)
  while(den != 0) {
    Sys.sleep(1)
    den <- den - 1
    print(den)
  }
}
gerisay(5)

# environment temizleme (gerektiğinde)
# rm(list=ls())


# YTS verisini internetten çekme
veri = read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")


View(veri)       # tablo görünümünde inceleme
head(veri)       # ilk 6 satır
dim(veri)        # boyutlar: satır x sütun
nrow(veri)       # satır sayısı
ncol(veri)       # sütun sayısı
names(veri)      # sütun isimleri

# yardım alma
?dim
help("dim")


library(dplyr)

# sütun yeniden adlandırma
veri_rename = rename(veri, year = YEAR)
names(veri_rename)

# ikinci bir yeniden adlandırma
veri_rename2 = rename(veri_rename, Year = year)

# orijinale dönüş
veri_rename_orijinal = rename(veri_rename2, YEAR = Year)



library(tidyverse)

data(mtcars)
View(mtcars)

# tibble formatına çevirme
veri22 = as_tibble(mtcars)
head(veri22)

# $ ile sütuna erişim
veri22$carb

# select ile sütun seçimi
select(veri22, mpg)
select(veri22, mpg, cyl)

# filter ile satır filtreleme
filter(veri22, mpg > 20 | mpg < 14)

# iç içe select ve filter
select(filter(veri22, mpg > 20 & cyl == 4), cyl, hp)

# pipe operatörü ile zincirleme işlem
veri22_piped = veri22 %>%
  filter(mpg > 20 & cyl == 4) %>%
  select(cyl, hp)

veri22_piped

# mutate ile yeni sütun ekleme
veri22_mut = mutate(veri22, newcol = wt / 2.2)

# mutate + ifelse ile kategorik değişken oluşturma
veri22_mut2 = mutate(veri22,
                     disp_cat = ifelse(disp <= 200, "Low",
                                       ifelse(disp <= 400, "Medium", "High")))

head(veri22_mut2$disp_cat)

# arrange ile sıralama
arrange(veri22, desc(mpg))
arrange(veri22, mpg, desc(hp))

# sütun ismi değiştirme
colnames(veri22)[1:3] = c("MPG", "CYL", "DISP")
colnames(veri22)

# indeksleme
veri22[,1]  # tüm satırlar, 1. sütun
veri22[1,]  # 1. satır, tüm sütunlar


mean(veri22$MPG)
median(veri22$CYL)
quantile(veri22$CYL)



# iris verisi ile basit grafik 
data(iris)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~ Species)


library(readr)
# write_csv(veri_rename2, path = "YouthTobacco_newNames.csv")

