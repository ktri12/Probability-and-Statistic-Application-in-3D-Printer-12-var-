# 1. Đọc file data.csv (xử lý luôn ký tự ẩn BOM trên macOS nếu có)
df <- read.csv("data.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# Kiểm tra lại xem có đúng 12 cột chưa
if (ncol(df) != 12) {
  df <- read.csv("data.csv", sep = ";", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
}

# 2. Tạo 2 biến mới
df$extrusion_volume <- df$layer_height * df$wall_thickness * df$infill_density / 100
df$temp_speed       <- df$nozzle_temperature / df$print_speed

# 3. Sắp xếp lại cột để 2 biến mới đứng trước nhóm biến kết quả
result_cols <- c("roughness", "tension_strenght", "elongation")
other_cols  <- setdiff(names(df), result_cols)
df <- df[, c(other_cols, result_cols)]

# 4. Xuất ra file newdata.csv
write.csv(df, "newdata.csv", row.names = FALSE)
cat("Thành công! Đã tạo newdata.csv với", nrow(df), "dòng và", ncol(df), "cột.\n")
