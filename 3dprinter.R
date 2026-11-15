# =====================================================================
# 3D PRINTER DATASET FOR MECHANICAL ENGINEERS (chuyển từ 3dprinter.ipynb)
# Mục tiêu: phân loại vật liệu in (ABS = 0, PLA = 1) từ các thông số in
# Phiên bản Base R thuần: Không phụ thuộc ggplot2 / readr / dplyr / caret
# =====================================================================

# Tạo thư mục thư viện cá nhân nếu chưa có và gán làm mặc định
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) {
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
  .libPaths(c(user_lib, .libPaths()))
} else {
  user_lib <- .libPaths()[1]
}

# ---- 1. Import Packages ----
use_pkg <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org", lib = user_lib)
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

use_pkg(c(
  "scatterplot3d",  # scatter 3D (mpl_toolkits.mplot3d)
  "class",          # KNN        (sklearn.neighbors.KNeighborsClassifier)
  "nnet"            # mạng nơ-ron (thay thế keras/TensorFlow)
))


# ---- 2. Cấu hình môi trường & thiết bị đồ họa ----
if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode" &&
    requireNamespace("httpgd", quietly = TRUE)) {
  try(httpgd::hgd(), silent = TRUE)
}

script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", args[grep("^--file=", args)])
    if (length(f) > 0) dirname(normalizePath(f)) else getwd()
  }
}, error = function(e) getwd())
if (nzchar(script_dir) && dir.exists(script_dir)) setwd(script_dir)

# Tìm file dữ liệu newdata.csv (hoặc data.csv) bằng đường dẫn tương đối
data_path <- c("newdata.csv", "data/newdata.csv", "../newdata.csv", "data.csv")
data_path <- data_path[file.exists(data_path)][1]
if (is.na(data_path)) {
  if (interactive()) {
    data_path <- file.choose()
  } else {
    stop("Không tìm thấy newdata.csv hoặc data.csv. Hãy đặt file cạnh script này.")
  }
}


# ---- 3. Import Data ----
first_line <- readLines(data_path, n = 1)
delim <- ifelse(grepl(";", first_line), ";", ",")
data <- read.csv(data_path, sep = delim, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# Tương đương data.info()
str(data)
cat("Số dòng:", nrow(data), "- Số cột:", ncol(data), "\n")
print(colSums(is.na(data)))   # kiểm tra giá trị thiếu


# ---- 4. Nhân các cột cho dễ hiểu ----
data$layer_height <- data$layer_height * 100
data$elongation   <- data$elongation   * 100

print(head(data))


# ---- 5. Mã hóa biến phân loại ----
data$material <- ifelse(tolower(data$material) == "abs", 0L, 1L)
data$infill_pattern <- ifelse(tolower(data$infill_pattern) == "grid", 0L, 1L)

print(head(data))


# ---- 6. Tách biến đầu vào và nhãn ----
y_data <- data$material
x_data <- data[, setdiff(names(data), "material"), drop = FALSE]

absm <- subset(data, material == 0)   # nhóm ABS
pla  <- subset(data, material == 1)   # nhóm PLA

print(head(absm))


# ---- 7. Trực quan hóa dữ liệu (Base R) ----

# 7.1 Fan Speed vs Tension Strength
plot(data$fan_speed, data$tension_strenght,
     col = ifelse(data$material == 0, "red", "green4"),
     pch = 19, cex = 1.2,
     xlab = "Fan Speed", ylab = "Tension Strength",
     main = "Fan Speed vs Tension Strength")
legend("topright", legend = c("ABS", "PLA"),
       col = c("red", "green4"), pch = 19, bty = "n")

# 7.2 Layer Height vs Roughness
plot(data$layer_height, data$roughness,
     col = ifelse(data$material == 0, "blue", "hotpink"),
     pch = 19, cex = 1.2,
     xlab = "Layer Height", ylab = "Roughness",
     main = "Layer Height vs Roughness")
legend("topleft", legend = c("ABS", "PLA"),
       col = c("blue", "hotpink"), pch = 19, bty = "n")

# 7.3 Biểu đồ 3D: Infill Density - Wall Thickness - Tension Strength
scatterplot3d(
  x = data$infill_density,
  y = data$wall_thickness,
  z = data$tension_strenght,
  color = "red", pch = 16,
  xlab = "Infill Density",
  ylab = "Wall Thickness",
  zlab = "Tension Strenght",
  main = "Infill Density - Wall Thickness - Tension Strength"
)


# ---- 8. Chuẩn hóa dữ liệu (Min-Max) & chia Train/Test ----
min_vals <- sapply(x_data, min)
max_vals <- sapply(x_data, max)
range_vals <- ifelse(max_vals - min_vals == 0, 1, max_vals - min_vals)

minmax_norm <- function(df) {
  as.data.frame(sweep(sweep(as.matrix(df), 2, min_vals, "-"),
                      2, range_vals, "/"))
}
x_norm <- minmax_norm(x_data)

set.seed(1)
# Chia stratified 70% train / 30% test bằng Base R (không cần caret)
idx_0 <- which(y_data == 0)
idx_1 <- which(y_data == 1)
train_idx <- sort(c(
  sample(idx_0, size = floor(0.7 * length(idx_0))),
  sample(idx_1, size = floor(0.7 * length(idx_1)))
))

x_train <- x_norm[train_idx, ]
x_test  <- x_norm[-train_idx, ]
y_train <- y_data[train_idx]
y_test  <- y_data[-train_idx]


# ---- 9. K-Nearest Neighbors ----
knn_pred <- knn(train = x_train, test = x_test, cl = factor(y_train), k = 3)
cat(sprintf(" %d nn score: %.4f\n", 3, mean(as.character(knn_pred) == y_test)))

score_list <- numeric(0)
for (k in 1:14) {
  pred_k <- knn(train = x_train, test = x_test, cl = factor(y_train), k = k)
  acc <- mean(as.character(pred_k) == y_test)
  score_list <- c(score_list, acc)
  cat(sprintf(" %d nn score: %.4f\n", k, acc))
}

plot(1:14, score_list, type = "b", col = "steelblue", lwd = 2, pch = 19,
     xaxt = "n", xlab = "k values", ylab = "accuracy",
     main = "KNN: độ chính xác theo k")
axis(1, at = 1:14)


# ---- 10. Mạng nơ-ron (Neural Network) ----
n_features <- ncol(x_data)
nn_backend <- "nnet"
model <- NULL

if (requireNamespace("keras3", quietly = TRUE)) {
  model <- tryCatch({
    suppressPackageStartupMessages(library(keras3))
    m <- keras_model_sequential(input_shape = n_features) |>
      layer_dense(32) |>
      layer_batch_normalization(axis = -1) |>
      layer_activation("relu") |>
      layer_dropout(0.25) |>
      layer_dense(64) |>
      layer_activation("relu") |>
      layer_dropout(0.25) |>
      layer_dense(16) |>
      layer_activation("softmax")

    m |> compile(optimizer = "adam",
                 loss = "sparse_categorical_crossentropy",
                 metrics = "accuracy")

    history <- m |> fit(as.matrix(x_data), y_data,
                        epochs = 500, batch_size = 32,
                        validation_split = 0.20, verbose = 0)
    print(plot(history))
    nn_backend <<- "keras3"
    m
  }, error = function(e) {
    message("Không dùng được keras3/TensorFlow (", conditionMessage(e),
            ") -> chuyển sang nnet.")
    NULL
  })
}

if (is.null(model)) {
  set.seed(1)
  model <- nnet(x = as.matrix(x_norm), y = y_data,
                size = 32, decay = 1e-3, maxit = 500, trace = FALSE)
  train_acc <- mean(as.integer(predict(model, as.matrix(x_norm)) > 0.5) == y_data)
  cat(sprintf("[nnet] Độ chính xác trên toàn bộ dữ liệu: %.4f\n", train_acc))
}
cat("Backend mạng nơ-ron đang dùng:", nn_backend, "\n")


# ---- 11. Dự đoán mẫu mới ----
a1  <- 4     # layer_height*100
a2  <- 5     # wall_thickness
a3  <- 60    # infill_density
a4  <- 0     # infill_pattern
a5  <- 232   # nozzle_temperature
a6  <- 74    # bed_temperature
a7  <- 90    # print_speed
a8  <- 100   # fan_speed
a9  <- 150   # roughness
a10 <- 30    # tension_strenght
a11 <- 200   # elongation*100

tahmin <- data.frame(
  layer_height = a1, wall_thickness = a2, infill_density = a3,
  infill_pattern = a4, nozzle_temperature = a5, bed_temperature = a6,
  print_speed = a7, fan_speed = a8, roughness = a9,
  tension_strenght = a10, elongation = a11
)

# Tự động bổ sung 2 biến mới nếu dữ liệu đầu vào là bản 14 biến
if ("extrusion_volume" %in% colnames(x_data)) {
  tahmin$extrusion_volume <- (a1 / 100) * a2 * (a3 / 100)
}
if ("temp_speed" %in% colnames(x_data)) {
  tahmin$temp_speed <- a5 / a7
}

tahmin <- tahmin[, colnames(x_data), drop = FALSE]

if (nn_backend == "keras3") {
  prob <- predict(model, as.matrix(tahmin), verbose = 0)
  pred_class <- which.max(prob[1, ]) - 1
} else {
  p <- predict(model, as.matrix(minmax_norm(tahmin)))
  pred_class <- as.integer(p > 0.5)
}

print(pred_class)
if (pred_class == 0) {
  print("Material is ABS")
} else {
  print("Material is PLA.")
}
