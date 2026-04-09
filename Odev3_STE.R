
# gerekli paketlerin yüklenmesi
library(MASS)
library(dplyr)
library(ggplot2)
library(corrplot)
library(zoo)
library(caTools)


# BÖLÜM i: ÜRETİLEN VERİ ile EDA


# tekrarlanabilirlik için seed belirleme
set.seed(123)
n <- 100  # gözlem sayısı

x1 <- rnorm(n, mean = 10, sd = 2)   # bağımsız değişken 1
x2 <- rnorm(n, mean = 5,  sd = 1)   # bağımsız değişken 2
x3 <- rnorm(n, mean = 3,  sd = 0.5) # bağımsız değişken 3
x4 <- rnorm(n, mean = 7,  sd = 1.5) # bağımsız değişken 4
x5 <- rnorm(n, mean = 15, sd = 3)   # bağımsız değişken 5

# bağımlı değişken: doğrusal ilişki + hata payı
y <- 3 + 2*x1 - 1.5*x2 + 0.5*x3 + 1.8*x4 - 1.2*x5 + rnorm(n, mean = 0, sd = 2)

# veri çerçevesi oluşturma
data <- data.frame(y, x1, x2, x3, x4, x5)

# ilk 6 satırı görüntüleme
head(data)


# faktör ve sayısal dönüşüm kontrolü
x1_f_factor  <- as.factor(x1)
x2_f_numeric <- as.numeric(x2)

str(x1_f_factor)   # faktör olduğu görülmeli
str(x2_f_numeric)  # sayısal olduğu görülmeli

#eksik veri tespiti

# üretilen veri temiz olduğu için NA kontrolü
is_na_x1 <- is.na(x1)
is_na_x2 <- is.na(x2)

# ortalama ile doldurma örneği
mean_x1 <- mean(x1, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1), mean_x1, x1)

# LOCF yöntemi
# na.locf(): eksik değeri bir önceki gözlemle doldurur
vec <- c(1, NA, NA, 4, NA, 6, NA, 8, NA)
vec_filled <- na.locf(vec)
print(vec_filled)

# medyan ile doldurma örneği
median_y <- median(y, na.rm = TRUE)
y_filled <- ifelse(is.na(y), median_y, y)

#aykırı değer tespiti

# kutu grafiği: aykırı değerleri görsel olarak inceleme
boxplot(data)

# Z-puanı yöntemi: |z| > 3 olanlar aykırı
z_scores <- scale(data$y)
outliers_z <- abs(z_scores) > 3 #abs mutlak değer fonksiyonu
cat("Z-puanı yöntemi ile aykırı değer sayısı:", sum(outliers_z), "\n")

# IQR yöntemi
Q1 <- quantile(data$y, 0.25)  # birinci çeyreklik
Q3 <- quantile(data$y, 0.75)  # üçüncü çeyreklik
IQR <- Q3 - Q1                 # çeyrekler arası aralık

lower_bound <- Q1 - 1.5 * IQR  # alt sınır
upper_bound <- Q3 + 1.5 * IQR  # üst sınır

outliers_iqr <- data$y < lower_bound | data$y > upper_bound
cat("IQR yöntemi ile aykırı değer sayısı:", sum(outliers_iqr), "\n")



#dağılım keşfi

# histogram
hist(data$y)

# kutu grafiği
boxplot(data$y, main = "y — Boxplot", col = "steelblue")

# Q-Q plot
qqnorm(data$y, main = "Q-Q Plot — y")
qqline(data$y, col = "red")

# özet istatistikler
summary(data)


# değişkenler arası korelasyon katsayısı
correlation <- cor(data$x1, data$y)
print(correlation) 

# tüm değişkenlerin korelasyon matrisi
correlation_matrix <- cor(data.frame(x1, x2, x3, y))
print(correlation_matrix)

# korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")


# değişken bazlı saçılım grafikleri

# grafikleri yan yana yerleştirme
par(mfrow = c(1, 3))

plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y",
     col = "blue", pch = 16)

plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y",
     col = "red", pch = 16)

plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y",
     col = "green", pch = 16)

par(mfrow = c(1, 1))  # grafik düzenini sıfırlama

#Standartlaştırma

# scale() ile z-puanı standartlaştırması (ortalama=0, std=1)
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized  <- scale(y)

summary(x1_standardized)  # mean ≈ 0 olmalı
summary(y_standardized)


# multicollinearity

# çoklu doğrusal regresyon
model <- lm(y ~ x1 + x2 + x3)
summary(model)  # katsayılar, R-kare, F-istatistiği


# %70 eğitim, %30 test
split <- sample.split(y, SplitRatio = 0.7)
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data  <- subset(data.frame(x1, x2, x3, y), split == FALSE)

#verinin boyutlarını kontrol etme
dim(train_data)  
dim(test_data)  



# BÖLÜM ii: İNTERNETTEN ÇEKİLEN VERİ ile EDA


# veriyi çekme

veri = read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")



View(veri)       # tablo görünümünde inceleme
head(veri)       # ilk 6 satır
dim(veri)        # boyutlar
nrow(veri)       # satır sayısı
ncol(veri)       # sütun sayısı
names(veri)      # sütun isimleri
str(veri)        # değişken türleri

# sütun yeniden adlandırma 

library(dplyr)

veri_rename = rename(veri, year = YEAR)
names(veri_rename)

#özet istatistikler

summary(veri_rename)

#eksik veri analizi

# toplam eksik değer sayısı
sum(is.na(veri_rename))

# sütun bazında eksik değer sayısı
colSums(is.na(veri_rename))

# numerik sütunları filtreleme

# numerik ve NA içermeyen sütunları seçen özel fonksiyon
is_numeric_no_na <- function(col) {
  is_numeric <- is.numeric(col)
  no_na      <- !anyNA(col)
  return(is_numeric & no_na)
}

veri_num <- Filter(is_numeric_no_na, veri_rename)

#dağılım görselleştirme

# Data_Value sütununun histogramı
hist(veri_rename$Data_Value,
     main   = "Data_Value Histogramı (YTS)",
     xlab   = "Data_Value",
     col    = "darkorange",
     border = "white",
     breaks = 30)

# kutu grafiği: aykırı değer kontrolü
boxplot(veri_rename$Data_Value,
        main = "Data_Value — Boxplot (YTS)",
        col  = "darkorange")

#yıla göre ortalama değer grafiği

# yıllık ortalamayı hesapla
yillik_ort <- veri_rename %>%
  group_by(year) %>%
  summarise(ort_deger = mean(Data_Value, na.rm = TRUE),
            .groups = "drop")

# ggplot2 ile çizgi grafiği
ggplot(yillik_ort, aes(x = year, y = ort_deger)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "tomato", size = 2) +
  labs(title = "Yıla Göre Ortalama Tütün Kullanım Değeri (YTS)",
       x = "Yıl",
       y = "Ortalama Data_Value") +
  theme_minimal()

# korelasyon analizi (numerik sütunlar)

corr_mat <- cor(veri_num)
corrplot(corr_mat, method = "color")

# aykırı değer tespiti (IQR) 

Q1  <- quantile(veri_rename$Data_Value, 0.25, na.rm = TRUE)
Q3  <- quantile(veri_rename$Data_Value, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- veri_rename$Data_Value < lower_bound |
  veri_rename$Data_Value > upper_bound

cat("IQR yöntemi ile aykırı değer sayısı:", sum(outliers, na.rm = TRUE), "\n")


