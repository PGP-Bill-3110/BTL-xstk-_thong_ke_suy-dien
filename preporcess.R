install.packages("questionr")
library(questionr)



All_GPUs <- read.csv("C:/Users/M S I/Downloads/btl xstk 243/All_GPUs.csv", 
                     na.strings = c("-", "", "N/A", "\nUnknown Release Date "))

Data <- All_GPUs[, c("Manufacturer", "Release_Date", "Process", "Direct_X", "Open_GL", 
                     "Shader", "Integrated", "L2_Cache", "Memory", "Memory_Bandwidth", 
                     "Memory_Bus", "Memory_Speed", "Memory_Type", "ROPs", "TMUs", 
                     "Pixel_Rate", "Texture_Rate", "Dedicated")]

cat("Number of variables:", ncol(Data),"\n")
head(Data, 10)


missing_rate <- freq.na(Data)
missing_rate
####################################################################

# Tính tỷ lệ NA của từng cột (%)
missing_rate <- colMeans(is.na(Data)) * 100

# Lọc các cột có tỷ lệ thiếu < 10%
selected_columns <- names(missing_rate)[missing_rate < 10]

# Giữ lại các cột thỏa điều kiện trong Data
Data <- Data[, selected_columns]

# Xóa các dòng có giá trị NA còn sót lại
Data <- na.omit(Data)

cat("Number of variables:", ncol(Data), "\n")
cat("Number of observations:", nrow(Data), "\n")


#############################################################

# Xu ly cac bien Memory_Bandwidth, Memory_Speed, Memory_Bus
cols_to_rm_unit <- c("Memory_Bandwidth", "Memory_Speed", "Memory_Bus")

# Tao ham thuc hien xoa don vi cac bien
remove_units <- function(column) {
  # Su dung gsub de xoa don vi
  cleaned_column <- gsub("[^0-9.]", "", column)
  # chuyen doi ket qua ve kieu numeric
  cleaned_column <- as.numeric(cleaned_column)
  
  return(cleaned_column)
}

# Ap dung ham cho cac bien can xu ly
Data[cols_to_rm_unit] <- lapply(Data[cols_to_rm_unit], remove_units)

# Can xu ly rieng L2_Cache vi co 2 kieu du lieu, vi du: 2304KB(x2), 512KB
library(stringr)

Data$L2_Cache <- ifelse(
  str_detect(Data$L2_Cache, "x"),
  as.integer(substr(Data$L2_Cache, 1, nchar(Data$L2_Cache) - 6)),
  as.integer(substr(Data$L2_Cache, 1, nchar(Data$L2_Cache) - 2))
)

# Lay gia tri nam cua Release_Date
Data$Release_Date <- as.integer(substr(Data$Release_Date,
                                       nchar(Data$Release_Date)-4,
                                       nchar(Data$Release_Date)-1))

cat("Number of variables:", ncol(Data), "\n")
cat("Number of observations:", nrow(Data), "\n")


##################################################################################
main_GPU <- Data[c("Memory_Bandwidth", "Memory_Speed", "L2_Cache", 
                   "Memory_Bus", "Shader", "Dedicated", "Manufacturer")]


