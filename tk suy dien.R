library(tidyverse)
#phần này để tạo tập main , ở file mẫu là phần tiền xlsl
main_GPU <- Data[c("Memory_Bandwidth", "Memory_Speed", "L2_Cache", 
                   "Memory_Bus", "Shader", "Dedicated", "Manufacturer")]


# Bước 1: Khai báo các cột cần làm sạch
columns_to_clean <- c("L2_Cache", "Memory_Bandwidth", "Memory_Bus", "Memory_Speed")

# Bước 2: Hàm xóa đơn vị khỏi dữ liệu (giữ lại chỉ số và chuyển sang numeric)
remove_units <- function(column) {
  cleaned_column <- gsub("[^0-9.]", "", column)  # Xóa ký tự không phải số
  cleaned_column <- as.numeric(cleaned_column)   # Chuyển thành số
  return(cleaned_column)
}

# Bước 3: Áp dụng hàm cho các cột đã chọn trong main_GPU
main_GPU[columns_to_clean] <- lapply(main_GPU[columns_to_clean], remove_units)

# Bước 4: Kiểm tra kết quả
head(main_GPU)

dim(main_GPU)
###################################################################################

#5.1

# Loc du lieu, lay nha san xuat Nvidia
library(dplyr)

nvidia_data <- main_GPU %>% filter(Manufacturer == "Nvidia")


# Ve QQ-Plot e kiem tra truc quan
qqnorm(nvidia_data$Memory_Bandwidth, main = "QQ-Plot of Memory Bandwidth (NVIDIA)")
qqline(nvidia_data$Memory_Bandwidth, col = "red", lwd = 2)
      # =>suy ra Memory_Bandwidth có tuân theo pp chuuan ko


# Kiem tra phan phoi chuan voi Shapiro-Wilk Test
shapiro_result <- shapiro.test(nvidia_data$Memory_Bandwidth)$p.value
print(shapiro_result)

# Tinh toan ket qua kiem dinh t-test
t_statistic = t.test(nvidia_data$Memory_Bandwidth, mu = 150, alternative = "greater")$
  statistic
t_statistic

# Xac dinh mien bac bo Z_alpha voi muc y nghia 5%
print(qnorm(0.95))

################################################################################

#5.2

GPUnodedicated <- subset(main_GPU, main_GPU$Dedicated == "No")  # Nhóm GPU không dedicated (tích hợp)
GPUdedicated   <- subset(main_GPU, main_GPU$Dedicated == "Yes") # Nhóm GPU dedicated (card rời)

n1 <- length(GPUnodedicated$Memory_Bandwidth)     # Kích thước mẫu 
xtb1 <- mean(GPUnodedicated$Memory_Bandwidth)     # Giá trị tb băng thông bộ nhớ
s1 <- sd(GPUnodedicated$Memory_Bandwidth)         # d

n2 <-length(GPUdedicated$Memory_Bandwidth)
xtb2 <- mean(GPUdedicated$Memory_Bandwidth)
s2 <- sd(GPUdedicated$Memory_Bandwidth)

# kt pp chuan
#
qqnorm(GPUnodedicated$Memory_Bandwidth,
       main = "Q-Q Plot for Memory Bandwidth (No Dedicated)",
       col = "navy",
       pch = 16)
qqline(GPUnodedicated$Memory_Bandwidth, col = "orange",lwd = 2)

#
qqnorm(GPUdedicated$Memory_Bandwidth,
       main = "Q-Q Plot for Memory Bandwidth(Yes Dedicated)",
       col = "navy",
       pch = 16)
qqline(GPUdedicated$Memory_Bandwidth, col = "orange",lwd = 2)

# shapiro
shapiro.test(GPUnodedicated$Memory_Bandwidth)
shapiro.test(GPUdedicated$Memory_Bandwidth)


# gt kiem dinh
Zo = (xtb1-xtb2)/sqrt(s1^2/n1 + s2^2/n2)
 print(Zo)
 
# mien bb
 qnorm(p=0.05,lower.tail = FALSE)

 ###################################################################
 #5.3
 
 #Kiem tra dieu kien ppc
 
 Manufacturer<-as.factor(main_GPU$Manufacturer) #bien doc lap can kiem tra
 Memory_Bandwidth<-main_GPU$Memory_Bandwidth #bien phu thuoc
 
 #Kiem tra ppc bang Shapiro wilk test
 
 library(nortest)
 av_residual<-rstandard(aov(Memory_Bandwidth~Manufacturer))
 shapiro.test(av_residual)
 
 #lay mau
 frequency_AMD <- main_GPU$Memory_Bandwidth[grep("AMD", main_GPU$Manufacturer, 
                                                 ignore.case = TRUE)]
 
 frequency_ATI <- main_GPU$Memory_Bandwidth[grep("ATI", main_GPU$Manufacturer, 
                                                 ignore.case = TRUE)]
 
 frequency_Intel <- main_GPU$Memory_Bandwidth[grep("Intel", main_GPU$Manufacturer, 
                                                   ignore.case = TRUE)]
 
 frequency_Nvidia <- main_GPU$Memory_Bandwidth[grep("Nvidia", main_GPU$Manufacturer, 
                                                    ignore.case = TRUE)]

 # loai bỏ diem ngoai lai
 # Hàm loại bỏ outlier theo IQR
 remove_outliers <- function(x) {
   Q1 <- quantile(x, 0.25)
   Q3 <- quantile(x, 0.75)
   IQR_value <- IQR(x)
   lower_limit <- Q1 - 1.5 * IQR_value
   upper_limit <- Q3 + 1.5 * IQR_value
   return(x[x >= lower_limit & x <= upper_limit])
 }
 
 # Áp dụng loại bỏ outlier cho từng hãng
 frequency_AMD <- remove_outliers(frequency_AMD)
 frequency_ATI <- remove_outliers(frequency_ATI)
 frequency_Intel <- remove_outliers(frequency_Intel)
 frequency_Nvidia <- remove_outliers(frequency_Nvidia)
 
# 
mau_gop <- c(frequency_AMD, frequency_ATI, frequency_Intel, frequency_Nvidia)

thong_ke <- data.frame(
  
Frequency = c("frequency_AMD", "frequency_ATI", 
              "frequency_Intel", "frequency_Nvidia"),
Trung_binh = c(mean(frequency_AMD), mean(frequency_ATI), 
               mean(frequency_Intel), mean(frequency_Nvidia)),
Phuong_sai = c(sd(frequency_AMD), sd(frequency_ATI), 
               sd(frequency_Intel), sd(frequency_Nvidia)),
Kich_thuoc_mau = c(length(frequency_AMD), length(frequency_ATI), 
                   length(frequency_Intel), length(frequency_Nvidia))
 )


#mien bb
 qf(1- 0.05, 4- 1,length(frequency_AMD) 
                  + length(frequency_ATI) 
                  + length(frequency_Intel)
                  +length(frequency_Nvidia)- 3)

 # kiem dinh
 
 # tao bang kd ANOVA
 df_anova <- data.frame(
   Gia_tri = c(frequency_AMD, frequency_ATI, frequency_Intel, frequency_Nvidia),
   Frequency = rep(c("AMD", "ATI", "Intel", "Nvidia"),
                   times = c(length(frequency_AMD),
                             length(frequency_ATI),
                             length(frequency_Intel),
                             length(frequency_Nvidia)))
 )
 
 # kd ANOVA
 ket_qua <- aov(Gia_tri ~ Frequency, data = df_anova)
 
 # In kq kđ  ANOVA
 print(summary(ket_qua))
 
 # neu Ano co y nghia, dung turkey de so tung cap nhom
 TukeyHSD(ket_qua)

 ################################################################### 
 
 # 5.4
 model_1 <- lm(Memory_Bandwidth ~ Memory_Speed + L2_Cache + Memory_Bus + Shader + 
                 Dedicated + Manufacturer, data = main_GPU)
 summary(model_1)
 
 # bo shader
 model_2 <- lm(Memory_Bandwidth ~ Memory_Speed + L2_Cache + Memory_Bus + 
                 Dedicated + Manufacturer, data = main_GPU)
 summary(model_2)
 
 # do thi kt gia dinh 
 par(mfrow = c(2, 2))
 plot(model_2)
 
 # chi tiet tung bieu do
 par(mfrow = c(1, 1))
 #bđ 1
 plot(model_2, which = 1)  # Residuals vs Fitted
 
 
#bđ 2
 plot(model_2, which = 2)  #  Q-Q residuals
 
 # sharpio wilk test phan du mo hinh 2
 model_residuals <- residuals(model_2) #Dua phan du cua mo hinh vao mo bien khac
 shapiro.test(model_residuals) #Thuc hien phep thu len bien
 
 #bđ 3
 plot(model_2, which = 3)  # Scale-Location
 
 #bđ 4
 plot(model_2, which = 5)  # Residuals vs Leverage

 #loai bo diem vuot qua cook's distance
 
 cooks_distance <- cooks.distance(model_2)
 #Tao nguong loai bo
  threshold <- 4 / nrow(main_GPU) 
  #Xac dinh cac diem co anh huong cao
high_influence_points <- which(cooks_distance > threshold)

#Loai bo cac diem co anh huong cao va tao lai mo hinh
data_filtered <- main_GPU[-high_influence_points, ]
filltered_model_2 <- lm(Memory_Bandwidth ~ Memory_Speed + L2_Cache + Memory_Bus +
                             Manufacturer, data = data_filtered) 
plot(filltered_model_2) 
 
# cong da tuyen
library(car)
vif(model_2)
