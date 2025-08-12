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

#5.1 bt 1 mau  

# Loc du lieu, lay nha san xuat Nvidia
library(dplyr)

nvidia_data <- main_GPU %>% filter(Manufacturer == "Nvidia")


# Ve QQ-Plot e kiem tra truc quan
qqnorm(nvidia_data$Memory_Bandwidth, main = "QQ-Plot of Memory Bandwidth (NVIDIA)")
qqline(nvidia_data$Memory_Bandwidth, col = "red", lwd = 2)
      # =>suy ra Memory_Bandwidth có tuân theo pp chuuan ko

#option 2
qqnorm(nvidia_data, main = "QQ-Plot")
qqline(nvidia_data, col = "red", lwd = 2)



# Kiem tra phan phoi chuan voi Shapiro-Wilk Test
shapiro_result <- shapiro.test(nvidia_data$Memory_Bandwidth)$p.value
print(shapiro_result)  # so voi 0.05 de ket luan

# Tinh toan ket qua kiem dinh t-test
t_statistic = t.test(nvidia_data$Memory_Bandwidth, mu = 150, alternative = "greater")$
  statistic
t_statistic    

# slide chuong 5, trang 319  Z_qs  (ko pp chuẩn)

# Xac dinh mien bac bo Z_alpha voi muc y nghia 5% 
print(qnorm(0.95))    #(z-alpah; +vc)

################################################################################

#5.2 bt 2 mau

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


# mien bb
 qnorm(p=0.05,lower.tail = FALSE)

 
 #t_test   
 t_result <- t.test(GPUnodedicated$Memory_Bandwidth,
                    GPUdedicated$Memory_Bandwidth,
                    var.equal = FALSE,      # Không giả định phương sai bằng nhau
                    alternative = "two.sided",
                    conf.level = 0.95)
 
 print(t_result)
 
 ###################################################################
 #5.3   anova 1 yeu to
 
 #Kiem tra dieu kien ppc
 
 Manufacturer<-as.factor(main_GPU$Manufacturer) #bien doc lap can kiem tra
 Memory_Bandwidth<-main_GPU$Memory_Bandwidth #bien phu thuoc
 
 #Kiem tra ppc bang Shapiro wilk test
 
 library(nortest)
 av_residual<-rstandard(aov(Memory_Bandwidth~Manufacturer))
 shapiro.test(av_residual)  #so voi 0.05
 
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
   upper_limit <- Q3 + 1.5 * IQR_value   #slide chuong 4, trang 247
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
#ct slide chương 4 trang 245

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
 # HỒI QUY TUYẾN TÍNH ĐƠN
 
 model_speed <- lm(Memory_Bandwidth ~ Memory_Speed, data = main_GPU)
 summary(model_speed)
 
 
 # Vẽ biểu đồ scatter plot + đường hồi quy
 plot(main_GPU$Memory_Speed, main_GPU$Memory_Bandwidth,
      main = "Hồi quy tuyến tính đơn:\n Memory_Bandwidth ~ Memory_Speed",
      xlab = "Memory Speed (MHz)",
      ylab = "Memory Bandwidth (GB/s)",
      pch = 16, col = "blue")
 
 # Thêm đường hồi quy vào biểu đồ
 abline(model_speed, col = "red", lwd = 2)

 
 #loai bỏ outliner
 
 # Huấn luyện mô hình ban đầu
 model_speed <- lm(Memory_Bandwidth ~ Memory_Speed, data = main_GPU)
 
 # Tính Cook's distance
 cooksD <- cooks.distance(model_speed)
 
 # Thiết lập ngưỡng loại bỏ: 4/n là ngưỡng phổ biến
 threshold <- 4 / nrow(main_GPU)
 
 # Lấy các điểm có ảnh hưởng lớn
 influential_points <- which(cooksD > threshold)
 
 # Xóa các điểm này khỏi dữ liệu
 main_GPU_cleaned <- main_GPU[-influential_points, ]
 
 # Huấn luyện lại mô hình sau khi loại bỏ
 model_cleaned <- lm(Memory_Bandwidth ~ Memory_Speed, data = main_GPU_cleaned)
 
 # Tóm tắt mô hình mới
 summary(model_cleaned)
 
 # Vẽ mô hình hồi quy sau khi làm sạch dữ liệu
 plot(main_GPU_cleaned$Memory_Speed, main_GPU_cleaned$Memory_Bandwidth,
      main = "Hồi quy tuyến tính đơn sau khi loại bỏ outlier:\nMemory_Bandwidth ~ Memory_Speed",
      xlab = "Memory Speed (MHz)",
      ylab = "Memory Bandwidth (GB/s)",
      pch = 20, col = "blue")
 
 # Thêm đường hồi quy
 abline(model_cleaned, col = "red", lwd = 2)

 