# =====================================================================
# 3D PRINTER DATASET FOR MECHANICAL ENGINEERS (chuyển từ 3dprinter.ipynb)
# Mục tiêu: phân loại vật liệu in (ABS = 0, PLA = 1) từ các thông số in
# Dữ liệu: newdata.csv (14 biến, 50 mẫu thử)
# Phiên bản Base R thuần: Không cần ggplot2 / patchwork / readr / dplyr / caret
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
  "nnet"            # mạng nơ-ron dự phòng nếu không có keras/TensorFlow
))


# ---- 2. Cấu hình môi trường & thiết bị đồ họa ----
if (interactive() &&
    !isTRUE(getOption("vsc.use_httpgd")) &&
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

# Tìm file dữ liệu newdata.csv bằng đường dẫn tương đối
data_path <- c("newdata.csv", "data/newdata.csv", "../newdata.csv")
data_path <- data_path[file.exists(data_path)][1]
if (is.na(data_path)) {
  if (interactive()) {
    data_path <- file.choose()
  } else {
    stop("Không tìm thấy newdata.csv. Hãy đặt file cạnh script này.")
  }
}


# ---- 3. Import Data (Dùng Base R, không cần readr) ----
first_line <- readLines(data_path, n = 1)
delim <- ifelse(grepl(";", first_line), ";", ",")
data <- read.csv(data_path, sep = delim, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# Tương đương data.info()
str(data)
cat("Số dòng:", nrow(data), "- Số cột:", ncol(data), "\n")
print(colSums(is.na(data)))

# Kiểm tra đúng 14 biến và 50 mẫu thử
stopifnot(
  "Dữ liệu phải có đúng 14 biến" = ncol(data) == 14,
  "Dữ liệu phải có đúng 50 mẫu thử" = nrow(data) == 50
)


# ---- 4. Nhân các cột cho dễ hiểu ----
data$layer_height <- data$layer_height * 100
data$elongation   <- data$elongation   * 100

print(head(data))


# ---- 5. Mã hóa biến phân loại ----
data$material <- ifelse(tolower(data$material) == "abs", 0L, 1L)              # abs = 0, pla = 1
data$infill_pattern <- ifelse(tolower(data$infill_pattern) == "grid", 0L, 1L)  # grid = 0, honeycomb = 1

print(head(data))


# ---- 6. Tách biến đầu vào và nhãn (Dùng Base R, không cần dplyr) ----
y_data <- data$material
x_data <- data[, setdiff(names(data), "material"), drop = FALSE]   # 13 biến đầu vào

absm <- subset(data, material == 0)   # nhóm ABS
pla  <- subset(data, material == 1)   # nhóm PLA

print(head(absm))


# ---- 7. Trực quan hóa dữ liệu (Base R) ----
pt_cols <- ifelse(data$material == 0, "red", "green4")

# Hàm hỗ trợ vẽ scatter + đường hồi quy tuyến tính + khoảng tin cậy 95%
plot_scatter_lm <- function(x, y, xlab, ylab, main, sub_txt = NULL) {
  plot(x, y, col = adjustcolor(pt_cols, alpha.f = 0.8), pch = 19, cex = 1.2,
       xlab = xlab, ylab = ylab, main = main)
  if (!is.null(sub_txt)) mtext(sub_txt, side = 3, line = 0.3, cex = 0.85, col = "grey30")
  fit <- lm(y ~ x)
  x_seq <- seq(min(x), max(x), length.out = 100)
  pred  <- predict(fit, newdata = data.frame(x = x_seq), interval = "confidence")
  polygon(c(x_seq, rev(x_seq)), c(pred[, "lwr"], rev(pred[, "upr"])),
          col = adjustcolor("grey70", alpha.f = 0.4), border = NA)
  lines(x_seq, pred[, "fit"], lwd = 2, col = "black")
  points(x, y, col = adjustcolor(pt_cols, alpha.f = 0.85), pch = 19, cex = 1.2)
  legend("topleft", legend = c("ABS", "PLA"), col = c("red", "green4"), pch = 19, bty = "n")
}

# 7.1 Fan Speed vs Tension Strength
plot(data$fan_speed, data$tension_strenght,
     col = adjustcolor(pt_cols, alpha.f = 0.6), pch = 19, cex = 1.3,
     xlab = "Fan Speed", ylab = "Tension Strength",
     main = "Fan Speed vs Tension Strength")
legend("topright", legend = c("ABS", "PLA"), col = c("red", "green4"), pch = 19, bty = "n")

# 7.2 Layer Height vs Roughness
plot(data$layer_height, data$roughness,
     col = ifelse(data$material == 0, "blue", "hotpink"), pch = 19, cex = 1.3,
     xlab = "Layer Height", ylab = "Roughness",
     main = "Layer Height vs Roughness")
legend("topleft", legend = c("ABS", "PLA"), col = c("blue", "hotpink"), pch = 19, bty = "n")

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

# 7.4 Extrusion Volume vs Tension Strength
r_ext <- cor.test(data$extrusion_volume, data$tension_strenght)
plot_scatter_lm(
  data$extrusion_volume, data$tension_strenght,
  xlab = "Extrusion Volume", ylab = "Tension Strength",
  main = "Extrusion Volume vs Tension Strength",
  sub_txt = sprintf("Pearson r = %.3f (p = %.3g)", r_ext$estimate, r_ext$p.value)
)

# 7.5 Temp Speed Ratio vs Elongation
r_ts <- cor.test(data$temp_speed, data$elongation)
plot_scatter_lm(
  data$temp_speed, data$elongation,
  xlab = "Temp Speed Ratio (nozzle temp / print speed)",
  ylab = "Elongation (x100)",
  main = "Temp Speed Ratio vs Elongation",
  sub_txt = sprintf("Pearson r = %.3f (p = %.3g)", r_ts$estimate, r_ts$p.value)
)


# ---- 8. Heatmap tương quan của 14 biến (Base R) ----
data_num <- data[, sapply(data, is.numeric)]
stopifnot("data_num phải có 14 biến số" = ncol(data_num) == 14)

corr <- cor(data_num)
n_var <- ncol(corr)
pal <- colorRampPalette(c("#3B4CC0", "#DDDDDD", "#B40426"))(201)

op <- par(mar = c(8, 8, 3, 4))
corr_plot <- t(corr[n_var:1, ])
image(1:n_var, 1:n_var, corr_plot, col = pal, zlim = c(-1, 1),
      axes = FALSE, xlab = "", ylab = "",
      main = "Matrix of relations between variables (Correlation Heatmap)")
axis(1, at = 1:n_var, labels = colnames(corr), las = 2, cex.axis = 0.75)
axis(2, at = 1:n_var, labels = rev(colnames(corr)), las = 2, cex.axis = 0.75)
abline(h = 0.5:(n_var + 0.5), v = 0.5:(n_var + 0.5), col = "white", lwd = 1)

for (i in 1:n_var) {
  for (j in 1:n_var) {
    val <- corr_plot[i, j]
    text(i, j, sprintf("%.2f", val), cex = 0.6,
         col = ifelse(abs(val) > 0.6, "white", "black"))
  }
}
par(op)


# ---- 9. Histogram + KDE của 14 biến trong 1 khung (Base R) ----
mat_layout <- matrix(c(
  1, 1, 2, 2, 3, 3, 4, 4,
  5, 5, 6, 6, 7, 7, 8, 8,
  9, 9, 10, 10, 11, 11, 12, 12,
  0, 0, 13, 13, 14, 14, 0, 0
), nrow = 4, byrow = TRUE)

layout(mat_layout)
op <- par(mar = c(2.2, 2.2, 2.2, 1), oma = c(0, 0, 2.5, 0))

for (col in names(data_num)) {
  x <- data_num[[col]]
  breaks <- seq(min(x), max(x), length.out = 11)
  bw <- breaks[2] - breaks[1]
  hist(x, breaks = breaks, col = "skyblue", border = "white",
       main = col, xlab = "", ylab = "", cex.main = 0.9, cex.axis = 0.75)
  dens <- density(x)
  lines(dens$x, dens$y * length(x) * bw, col = "steelblue", lwd = 1.5)
}
mtext("Distributions of 14 variables (Histogram + KDE)", outer = TRUE, cex = 1.1, font = 2)
par(op)
layout(1)


# ---- 10. Hồi quy tuyến tính đa biến (Multiple Linear Regression) ----
outcome_vars   <- c("roughness", "tension_strenght", "elongation")
predictor_vars <- setdiff(names(data_num), outcome_vars)
mlr_df <- data_num

# Hệ số phóng đại phương sai (VIF)
calc_vif <- function(df, vars) {
  sapply(vars, function(v) {
    r2 <- summary(lm(reformulate(setdiff(vars, v), response = v), data = df))$r.squared
    1 / (1 - r2)
  })
}
cat("\nVIF of independent variables (VIF > 5-10 is a notable case of multicollinearity):\n")
print(round(sort(calc_vif(mlr_df, predictor_vars), decreasing = TRUE), 2))

plot_mlr_base <- function(df, target, focus, target_label, focus_label) {
  fit <- lm(reformulate(predictor_vars, response = target), data = df)
  sm  <- summary(fit)
  cat("\n=====", target_label, "~ Processing variables =====\n")
  print(sm)

  fitted_v <- fitted(fit)
  resid_v  <- resid(fit)
  actual_v <- df[[target]]

  op <- par(mfrow = c(2, 2), mar = c(4, 4.5, 3, 1.5), oma = c(0, 0, 2.5, 0))

  # (A) Actual vs. Predicted
  plot(fitted_v, actual_v, col = adjustcolor(pt_cols, alpha.f = 0.8), pch = 19,
       xlab = "Predicted values", ylab = "Actual values",
       main = "Actual vs. Predicted", cex.main = 0.95)
  abline(0, 1, lty = 2, col = "grey40")
  mtext(sprintf("R² = %.3f | Adj. R² = %.3f | RMSE = %.2f",
                sm$r.squared, sm$adj.r.squared, sqrt(mean(resid_v^2))),
        side = 3, line = 0.2, cex = 0.72, col = "grey30")
  legend("topleft", legend = c("ABS", "PLA"), col = c("red", "green4"), pch = 19, bty = "n", cex = 0.8)

  # (B) Standardized coefficients (95% CI) - Lọc bỏ hệ số NA do đa cộng tuyến (fan_speed)
  z_df  <- as.data.frame(scale(df[, c(target, predictor_vars)]))
  z_fit <- lm(reformulate(predictor_vars, response = target), data = z_df)
  ci    <- confint(z_fit)
  valid_terms <- rownames(ci)[rownames(ci) != "(Intercept)" & !is.na(coef(z_fit)[rownames(ci)])]
  est   <- unname(coef(z_fit)[valid_terms])
  ord   <- order(est)
  terms <- valid_terms[ord]
  est   <- est[ord]
  lo    <- ci[terms, 1]
  hi    <- ci[terms, 2]
  c_cols <- ifelse(terms == focus, "#d62728", "grey35")

  par(mar = c(4, 7.5, 3, 1))
  plot(est, seq_along(terms), xlim = range(c(lo, hi), na.rm = TRUE), yaxt = "n",
       pch = 19, col = c_cols, xlab = "Standardized β", ylab = "",
       main = "Standardized coefficients (95% CI)", cex.main = 0.95)
  abline(v = 0, lty = 2, col = "grey50")
  segments(lo, seq_along(terms), hi, seq_along(terms), col = c_cols, lwd = 1.8)
  axis(2, at = seq_along(terms), labels = gsub("_", " ", terms), las = 2, cex.axis = 0.72)
  par(mar = c(4, 4.5, 3, 1.5))

  # (C) Partial effect (Added-variable plot)
  others <- setdiff(predictor_vars, focus)
  rx <- resid(lm(reformulate(others, response = focus),  data = df))
  ry <- resid(lm(reformulate(others, response = target), data = df))
  plot(rx, ry, col = adjustcolor(pt_cols, alpha.f = 0.8), pch = 19,
       xlab = paste0(focus_label, " | others"),
       ylab = paste0(target_label, " | others"),
       main = paste("Partial effect:", focus_label), cex.main = 0.95)
  abline(lm(ry ~ rx), lwd = 1.8, col = "black")
  mtext(sprintf("β = %.3f, p = %.3g", coef(fit)[focus], sm$coefficients[focus, 4]),
        side = 3, line = 0.2, cex = 0.72, col = "grey30")

  # (D) Residuals vs. Predicted
  plot(fitted_v, resid_v, col = adjustcolor(pt_cols, alpha.f = 0.8), pch = 19,
       xlab = "Predicted values", ylab = "Residuals",
       main = "Residuals vs. Predicted", cex.main = 0.95)
  abline(h = 0, lty = 2, col = "grey40")
  lines(lowess(fitted_v, resid_v), col = "grey30", lwd = 1.5)

  mtext(paste("Multivariate Linear Regression:", target_label), outer = TRUE, cex = 1.1, font = 2)
  par(op)
  invisible(fit)
}

# 10.1 Tension Strength, biến trọng tâm: Extrusion Volume
fit_tension <- plot_mlr_base(mlr_df, "tension_strenght", "extrusion_volume",
                             "Tension Strength", "Extrusion Volume")

# 10.2 Elongation, biến trọng tâm: Temp Speed Ratio
fit_elong <- plot_mlr_base(mlr_df, "elongation", "temp_speed",
                           "Elongation", "Temp Speed Ratio")


# ---- 11. Chuẩn hóa dữ liệu (Min-Max) & chia Train/Test ----
min_vals <- sapply(x_data, min)
max_vals <- sapply(x_data, max)
range_vals <- ifelse(max_vals - min_vals == 0, 1, max_vals - min_vals)

minmax_norm <- function(df) {
  as.data.frame(sweep(sweep(as.matrix(df), 2, min_vals, "-"),
                      2, range_vals, "/"))
}
x_norm <- minmax_norm(x_data)

set.seed(1)
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


# ---- 12. K-Nearest Neighbors ----
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
     main = "KNN: Accuracy by k")
axis(1, at = 1:14)


# ---- 13. Mạng nơ-ron (Neural Network) ----
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
    message("Unable to use keras3/TensorFlow (", conditionMessage(e),
            ") -> move to nnet.")
    NULL
  })
}

if (is.null(model)) {
  set.seed(1)
  model <- nnet(x = as.matrix(x_norm), y = y_data,
                size = 32, decay = 1e-3, maxit = 500, trace = FALSE)
  train_acc <- mean(as.integer(predict(model, as.matrix(x_norm)) > 0.5) == y_data)
  cat(sprintf("[nnet] Accuracy on the entire dataset: %.4f\n", train_acc))
}
cat("The neural network backend being used:", nn_backend, "\n")


# ---- 14. Dự đoán mẫu mới ----
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

a_ext <- (a1 / 100) * a2 * (a3 / 100)   # extrusion_volume = layer_height * wall * infill/100
a_ts  <- a5 / a7                        # temp_speed = nozzle_temperature / print_speed

tahmin <- data.frame(
  layer_height = a1, wall_thickness = a2, infill_density = a3,
  infill_pattern = a4, nozzle_temperature = a5, bed_temperature = a6,
  print_speed = a7, fan_speed = a8,
  extrusion_volume = a_ext, temp_speed = a_ts,
  roughness = a9, tension_strenght = a10, elongation = a11
)
tahmin <- tahmin[, colnames(x_data)]

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
