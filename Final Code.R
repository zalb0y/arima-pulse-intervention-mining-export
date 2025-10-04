library(stringr)      # Untuk fungsi str_extract() dan str_replace_all()
library(dplyr)        # Untuk pipe, mutate(), select()
library(lubridate)    # Untuk mengedit format tanggal
library(tseries)      # Untuk adf.test()
library(forecast)     # Untuk forecast(), BoxCox.lambda(), dll
library(lmtest)       # Untuk coeftest()
library(ggplot2)
library(gridExtra)

# ==== Preprocessing Data ====
data <- read.csv("data_ekspor.csv")

# Buat ke dalam time series 
data_ts <- ts(data$Nilai_Ekspor, 
              start = c(2018,1), 
              frequency = 12)

# Buat ke dalam time series 
data_ts <- ts(cleaned_data$nilai_ekspor, 
              start = c(min(cleaned_data$year), min(cleaned_data$month)), 
              frequency = 12)

# Tampilkan hasil
head(data_ts)
tail(data_ts)

# Pengecekan data
missing_values <- sum(is.na(data_ts))
print(missing_values)

# Plot time series
plot(data_ts, main = "Nilai Ekspor Tambang", xlab = "Waktu", ylab = "Nilai (Juta USD)")
BoxCox.lambda(data_ts, method = "loglik")

# Transformasi data 
lambda <- -0.05 
full_data_transformed <- (data_ts^lambda - 1)/lambda
BoxCox.lambda(full_data_transformed, method="loglik")

head(full_data_transformed)
tail(full_data_transformed)

# Plot data setelah transformasi
plot(full_data_transformed, main="Data Setelah Transformasi", xlab="Waktu", ylab="Nilai Ekspor (Transformed)")

# Analisis stasioneritas
adf_test <- adf.test(full_data_transformed)
print(adf_test)

# Menstasionerkan data dengan differencing
ts_stationary <- diff(full_data_transformed) 

# Menghapus nilai NA dari data
ts_stationary_clean <- na.omit(ts_stationary)
adf_test2 <- adf.test(ts_stationary_clean)
print(adf_test2)

# Plot data setelah transformasi
plot(ts_stationary_clean, main="Data Setelah Transformasi", xlab="Waktu", ylab="Nilai Ekspor (Transformed)")

# ==== ACF AND PACF PLOT ====

# Set up side-by-side plots
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

# Plot ACF
acf(ts_stationary_clean, 
    main = "ACF Setelah Differencing",
    xlab = "Lag",
    ylab = "ACF",
    col = "darkblue",
    lwd = 2)

# Plot PACF  
pacf(ts_stationary_clean,
     main = "PACF Setelah Differencing", 
     xlab = "Lag",
     ylab = "Partial ACF",
     col = "darkred",
     lwd = 2)

# Reset plot parameters
par(mfrow = c(1, 1))

# ==== Membuat model ARIMA ====
# Membuat daftar model kombinasi berdasarkan informasi ACF dan PACF
# Menggunakan d=1 karena sudah melakukan differencing 1 kali
cat("=== MODEL 1: ARIMA(3,1,0) ===\n")
model1 <- Arima(full_data_transformed, order = c(3,1,0),include.drift = FALSE)

cat("AIC:", round(AIC(model1), 4), "\n")
coeftest(model1)

cat("\n=== MODEL 2: ARIMA(0,1,3) ===\n")
model2 <- Arima(full_data_transformed, order = c(0,1,3),include.drift = FALSE)

cat("AIC:", round(AIC(model2), 4), "\n")
coeftest(model2)

cat("\n=== MODEL 3: ARIMA(3,1,3) ===\n")
model3 <- Arima(full_data_transformed, order = c(3,1,3),include.drift = FALSE)

cat("AIC:", round(AIC(model3), 4), "\n")
coeftest(model3)

# Ringkasan
cat("\n=== RINGKASAN AIC ===\n")
cat("ARIMA(3,1,0):", round(AIC(model1), 4), "\n")
cat("ARIMA(0,1,3):", round(AIC(model2), 4), "\n")
cat("ARIMA(3,1,3):", round(AIC(model3), 4), "\n")

best_model <- model3

# ==== Diagnostik Model ARIMA Terbaik ====

# Uji Ljung-Box pada residual
lb_test <- Box.test(residuals(best_model), 
                    lag = 10,           # Set lag = 10
                    type = "Ljung-Box", 
                    fitdf = 6)          # Model df = 6 (parameter ARIMA)
print(lb_test)

# Uji normalitas residual dengan Kolmogorov-Smirnov
norm_test <- ks.test(residuals(best_model), "pnorm", 
                     mean = mean(residuals(best_model), na.rm = TRUE), 
                     sd = sd(residuals(best_model), na.rm = TRUE))
print(norm_test)

# Simpan model ARIMA sebelum intervensi
cat("\n--- Model ARIMA Sebelum Intervensi ---\n")
summary(best_model)

# Konversi objek ts ke data frame untuk mempersiapkan pembentukan model intervensi
arima_order <- arimaorder(best_model)
cat("Order ARIMA terpilih: (", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")\n")


# ==== Membuat Varibel Intervensi Pulse ====

# Intervensi Pulse: Januari 2022, ordo (b,s,r) = (0,0,0)
# - b=0: Tidak ada delay untuk titik intervensi
# - s=0: Dampak langsung terlihat
# - r=0: Dampak tidak berlanjut (pure pulse)

# Data sudah ditransformasi: full_data_transformed
cat("Panjang data full_data_transformed:", length(full_data_transformed), "\n")
cat("Periode data:", start(full_data_transformed), "hingga", end(full_data_transformed), "\n")

# Tentukan titik intervensi: Januari 2022
# Hitung posisi Januari 2022 dalam time series
start_year_full <- start(full_data_transformed)[1]
start_month_full <- start(full_data_transformed)[2]

# Hitung bulan ke berapa Januari 2022
intervention_year <- 2022
intervention_month <- 1

# Hitung posisi dalam time series
months_from_start <- (intervention_year - start_year_full) * 12 + (intervention_month - start_month_full) + 1
cat("Posisi intervensi Januari 2022 pada observasi ke-", months_from_start, "\n")

# Buat variabel intervensi Pulse dengan ordo (0,0,0)
# Pulse (0,0,0): dampak langsung, tidak ada delay, tidak berlanjut
# Nilai 1 HANYA pada Januari 2022, 0 untuk periode lainnya
n_obs <- length(full_data_transformed)
pulse_intervention <- rep(0, n_obs)

# Set nilai 1 pada Januari 2022
if(months_from_start > 0 && months_from_start <= n_obs) {
  pulse_intervention[months_from_start] <- 1
  cat("Variabel pulse dibuat: 1 pada posisi", months_from_start, "(Januari 2022), 0 lainnya\n")
} else {
  cat("Error: Posisi Januari 2022 di luar rentang data\n")
}

# Tampilkan beberapa nilai di sekitar intervensi untuk verifikasi
if(months_from_start > 2 && months_from_start < n_obs-2) {
  cat("Nilai pulse di sekitar intervensi:\n")
  for(i in (months_from_start-2):(months_from_start+2)) {
    date_check <- start_year_full + (start_month_full + i - 2)/12
    cat("Posisi", i, "- Pulse:", pulse_intervention[i], "\n")
  }
}

# Cek apakah ada nilai 1
cat("Jumlah dummy 1:", sum(pulse_intervention), "\n")
cat("Posisi dummy 1:", which(pulse_intervention == 1), "\n")

# Lihat beberapa nilai di sekitar intervensi
print(pulse_intervention[(months_from_start-2):(months_from_start+2)])

# Buat model ARIMA + Intervensi menggunakan Arima() dengan xreg
cat("\n--- Estimasi Model ARIMA + Pulse Intervention (0,0,0) ---\n")

# Buat matrix variabel eksternal untuk pulse intervention
xreg_pulse <- matrix(pulse_intervention, ncol=1)
colnames(xreg_pulse) <- "Pulse_Jan2022"

# ==== Membuat model ARIMA dengan intervensi ====
# Menggunakan ordo ARIMA yang sudah ditentukan sebelumnya (p,d,q) dari final_model
p <- arima_order[1]
d <- arima_order[2]
q <- arima_order[3]
cat("\n--- Pemodelan ARIMA dengan Intervensi ---\n")
cat("Menggunakan order ARIMA (", p, ",", d, ",", q, ") yang diperoleh sebelumnya\n")
cat("Intervensi Pulse (b=0, s=0, r=0) untuk periode Januari 2022\n\n")

# Estimasi model ARIMA(3,1,0) dengan intervensi pulse
intervention_model <- Arima(full_data_transformed, 
                            order=c(p,d,q), 
                            xreg=xreg_pulse)

# Tampilkan summary model
summary(intervention_model)

# Uji signifikansi parametern")
coeftest(intervention_model)

# ==== Diagnostik residual model intervensi ====

# Uji Ljung-Box pada residual
lb_test <- Box.test(residuals(intervention_model), 
                    lag = 10,           # Set lag = 10
                    type = "Ljung-Box", 
                    fitdf = 7)          # Model df = 7 
print(lb_test)

# Uji normalitas residual
norm_test <- ks.test(residuals(intervention_model), "pnorm", 
                     mean = mean(residuals(intervention_model), na.rm = TRUE), 
                     sd = sd(residuals(intervention_model), na.rm = TRUE))
cat("Uji Normalitas Residual (Kolmogorov-Smirnov):\n")
print(norm_test)

# Plot diagnostik
par(mfrow=c(2,2))

# Plot time series residual
plot(residuals(intervention_model), type="l", 
     main="Residual Model Intervensi", 
     xlab="Observasi", ylab="Residual")
abline(h=0, col="red", lty=2)

# Plot ACF residual
acf(residuals(intervention_model), 
    main="ACF Residual", 
    xlab="Lag", ylab="ACF")

# Histogram residual
hist(residuals(intervention_model), 
     main="Histogram Residual", 
     xlab="Residual", ylab="Frekuensi", 
     prob=TRUE)
lines(density(residuals(intervention_model)), col="blue", lwd=2)


# ==== Perbandingan Model ARIMA dan ARIMA Intervensi berdasarkan AIC ====

# Bandingkan dengan model ARIMA tanpa intervensi
cat("\n--- Perbandingan Model ---\n")

# Model ARIMA(3,1,3) tanpa intervensi
arima_no_intervention <- best_model

aic_without <- AIC(arima_no_intervention)
aic_with <- AIC(intervention_model)

cat("Model ARIMA(3,1,3) tanpa intervensi:\n")
cat("  AIC:", aic_without, "\n")

cat("Model ARIMA(3,1,3) + Pulse Intervensi:\n")
cat("  AIC:", aic_with, "\n")

aic_diff <- aic_without - aic_with
cat("Selisih AIC (tanpa - dengan intervensi):", aic_diff, "\n")


# ==== FITTING MODEL ARIMA DENGAN DATA AKTUAL ====

# Definisikan fungsi transformasi balik Box-Cox
# Rumus transformasi balik Box-Cox untuk lambda=0.02338244
transformasi_balik <- function(x, lambda) {
  # Rumus transformasi balik: y = (lambda*x + 1)^(1/lambda)
  return((lambda*x + 1)^(1/lambda))
}

# Evaluasi Model menggunakan MAPE (Mean Absolute Percentage Error)
cat("\n--- Evaluasi Model dengan MAPE ---\n")

# Mendapatkan nilai fitted dari model (dalam skala transformasi)
fitted_transform <- fitted(arima_no_intervention)

# Transformasi balik fitted values ke skala asli
fitted_original <- transformasi_balik(fitted_transform, lambda)

# Data aktual (skala asli)
actual_original <- as.numeric(data_ts)

# Pastikan panjang data sama
min_length <- min(length(fitted_original), length(actual_original))
fitted_original <- fitted_original[1:min_length]
actual_original <- actual_original[1:min_length]

# Hitung MAPE
mape <- mean(abs((actual_original - fitted_original) / actual_original)) * 100
cat("MAPE Model ARIMA:", round(mape, 2), "%\n")

# ==== FITTING MODEL INTERVENSI DENGAN DATA AKTUAL ====

# Transformasi balik fitted values ke skala asli
fitted_original_intervensi <- transformasi_balik(fitted(intervention_model), lambda)
actual_original <- as.numeric(data_ts)

# Pastikan panjang data sama
min_length <- min(length(fitted_original_intervensi), length(actual_original))
fitted_original_intervensi <- fitted_original_intervensi[1:min_length]
actual_original <- actual_original[1:min_length]

# Hitung MAPE
mape_intervensi <- mean(abs((actual_original - fitted_original_intervensi) / actual_original)) * 100
cat("MAPE Model ARIMA Intervensi:", round(mape_intervensi, 2), "%\n")

# Plot fitting model dengan data aktual
time_index <- time(data_ts)[1:min_length]
plot(time_index, actual_original, 
     type="l", lwd=2, col="black",
     main="Fitting Model ARIMA Intervensi vs Data Aktual",
     xlab="Waktu", ylab="Nilai Ekspor (Juta USD)")

lines(time_index, fitted_original_intervensi, 
      col="red", lwd=2)

# Garis vertikal untuk titik intervensi
abline(v=2022, col="green", lwd=2, lty=3)

# Legend
legend("topleft", 
       legend=c("Data Aktual", 
                paste("Model Fitted (MAPE:", round(mape_intervensi, 1), "%)"),
                "Intervensi Jan 2022"),
       col=c("black", "red", "green"),
       lty=c(1, 1, 3),
       lwd=c(2, 2, 2))

# ==== FITTING MODEL 6 NILAI TERAKHIR - ANGKA ====

# Model ARIMA tanpa intervensi
fitted_transform <- fitted(arima_no_intervention)
fitted_original <- transformasi_balik(fitted_transform, lambda)
actual_original <- as.numeric(data_ts)

# Pastikan panjang data sama
min_length <- min(length(fitted_original), length(actual_original))
fitted_original <- fitted_original[1:min_length]
actual_original <- actual_original[1:min_length]

# Model ARIMA dengan intervensi  
fitted_original_intervensi <- transformasi_balik(fitted(intervention_model), lambda)

# Pastikan panjang data sama untuk model intervensi
min_length_int <- min(length(fitted_original_intervensi), length(actual_original))
fitted_original_intervensi <- fitted_original_intervensi[1:min_length_int]
actual_original_final <- actual_original[1:min_length_int]

# Ambil 6 nilai terakhir
n <- length(actual_original_final)
last_6_indices <- (n-5):n

cat("=== PERBANDINGAN MODEL: 6 NILAI TERAKHIR ===\n\n")

# Header tabel
cat(sprintf("%-4s %-10s %-12s %-15s %-12s %-12s\n", 
            "No", "Actual", "ARIMA", "ARIMA+Interv", "Error_ARIMA", "Error_Interv"))
cat(paste(rep("-", 75), collapse = ""), "\n")

# Data tabel
for (i in 1:6) {
  idx <- last_6_indices[i]
  actual <- actual_original_final[idx]
  arima_fitted <- fitted_original[idx]
  arima_int_fitted <- fitted_original_intervensi[idx]
  error_arima <- actual - arima_fitted
  error_int <- actual - arima_int_fitted
  
  cat(sprintf("%-4d %-10.3f %-12.3f %-15.3f %-12.3f %-12.3f\n", 
              idx, actual, arima_fitted, arima_int_fitted, error_arima, error_int))
}

# Hitung MAPE keseluruhan
mape_arima_total <- mean(abs((actual_original_final - fitted_original[1:min_length_int]) / actual_original_final)) * 100
mape_int_total <- mean(abs((actual_original_final - fitted_original_intervensi) / actual_original_final)) * 100

cat("\n=== EVALUASI AKURASI ===\n")
cat("Metrik\t\t\tARIMA\t\tARIMA+Intervensi\n")
cat("------\t\t\t-----\t\t----------------\n")
cat(sprintf("MAPE keseluruhan:\t%.2f%%\t\t%.2f%%\n", mape_arima_total, mape_int_total))


# Model terbaik
cat("\n=== KESIMPULAN ===\n")
if (mape_int_total < mape_arima_total) {
  cat("Model terbaik: ARIMA dengan Intervensi\n")
  cat(sprintf("Selisih MAPE: %.2f%% lebih rendah\n", mape_arima_total - mape_int_total))
} else {
  cat("Model terbaik: ARIMA tanpa Intervensi\n")
  cat(sprintf("Selisih MAPE: %.2f%% lebih rendah\n", mape_int_total - mape_arima_total))
}

# ==== PREDIKSI MODEL ARIMA + INTERVENSI PULSE ====

cat("\n--- Prediksi dengan Model Intervensi ---\n")

# Persiapan untuk prediksi 6 bulan ke depan
h_forecast <- 3  # Horizon prediksi 6 bulan

# Buat variabel intervensi untuk periode prediksi
# Karena ini prediksi untuk masa depan, tidak ada intervensi pulse baru
# Jadi semua nilai = 0 untuk 3 bulan ke depan
xreg_forecast <- matrix(rep(0, h_forecast), ncol=1)
colnames(xreg_forecast) <- "Pulse_Jan2022"

cat("Membuat prediksi untuk", h_forecast, "bulan ke depan (Apr 2025 - Juni 2025)\n")
cat("Variabel intervensi untuk prediksi: semua bernilai 0 (tidak ada intervensi baru)\n")

# Prediksi menggunakan model intervensi
forecast_result <- forecast(intervention_model, 
                            h = h_forecast,
                            xreg = xreg_forecast)

# Ekstrak nilai prediksi (hanya point forecast, tanpa interval)
forecast_values_transformed <- as.numeric(forecast_result$mean)

cat("\nHasil prediksi (skala transformasi):\n")
print(forecast_values_transformed)

# Transformasi balik ke skala asli
forecast_values_original <- transformasi_balik(forecast_values_transformed, lambda)

cat("\nHasil prediksi (skala asli - Juta USD):\n")
print(round(forecast_values_original, 2))


# ==== VISUALISASI PREDIKSI ====

cat("\n--- Membuat Visualisasi Prediksi ---\n")

# Persiapan data
historical_data <- as.numeric(data_ts)
historical_time <- as.numeric(time(data_ts))

# Buat time sequence untuk prediksi yang TERSAMBUNG
last_time_numeric <- max(historical_time)
forecast_time_numeric <- seq(from = last_time_numeric + 1/12, 
                             by = 1/12, 
                             length.out = length(forecast_values_original))

# Tambahkan titik penghubung
# Gabungkan data historis terakhir dengan prediksi untuk kontinuitas
last_historical_value <- tail(historical_data, 1)
last_historical_time <- tail(historical_time, 1)

# Gabungkan waktu: [waktu_historis_terakhir, waktu_prediksi...]
combined_time <- c(last_historical_time, forecast_time_numeric)
# Gabungkan nilai: [nilai_historis_terakhir, nilai_prediksi...]
combined_values <- c(last_historical_value, forecast_values_original)

cat("Titik penghubung - Waktu:", last_historical_time, "Nilai:", last_historical_value, "\n")
cat("Prediksi dimulai - Waktu:", forecast_time_numeric[1], "Nilai:", forecast_values_original[1], "\n")

# Plot dengan garis yang tersambung
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

# Plot data historis
plot(historical_time, historical_data,
     type = "l", lwd = 2, col = "black",
     main = "Prediksi Nilai Ekspor Tambang dengan Model ARIMA Intervensi",
     xlab = "Waktu", 
     ylab = "Nilai Ekspor (Juta USD)",
     xlim = c(min(historical_time), max(forecast_time_numeric) + 0.1),
     ylim = range(c(historical_data, forecast_values_original), na.rm = TRUE),
     cex.main = 1.2, cex.lab = 1.1)

# Plot prediksi 
lines(combined_time, combined_values, 
      col = "green3", lwd = 3, type = "l")

# Tambahkan titik untuk memperjelas
points(forecast_time_numeric, forecast_values_original, 
       col = "green3", pch = 19, cex = 1.2)

# Garis vertikal untuk batas prediksi
abline(v = max(historical_time), col = "red", lwd = 2, lty = 2)

# Tambahkan grid
grid(col = "lightgray", lty = 3)

# Legend yang diperbaiki
legend("bottomright",
       legend = c("Data Historis", "Prediksi 3 Bulan", "Batas Prediksi"),
       col = c("black", "green3", "red"),
       lty = c(1, 1, 2),
       lwd = c(2, 3, 2),
       cex = 1.0,
       bg = "white")

# Informasi prediksi
last_date <- end(data_ts)
last_year <- last_date[1] 
last_month <- last_date[2]

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

pred_text <- "PREDIKSI 3 BULAN:\n"

for (i in 1:length(forecast_values_original)) {
  pred_month <- last_month + i
  pred_year <- last_year
  
  if (pred_month > 12) {
    pred_year <- pred_year + floor((pred_month - 1) / 12)
    pred_month <- ((pred_month - 1) %% 12) + 1
  }
  
  pred_text <- paste0(pred_text, 
                      month_names[pred_month], " ", pred_year, 
                      ": ", round(forecast_values_original[i], 0), " Juta USD\n")
}

# Text box prediksi
text(min(historical_time) + (max(forecast_time_numeric) - min(historical_time)) * 0.05,
     max(c(historical_data, forecast_values_original)) * 0.85,
     pred_text,
     cex = 0.9,
     adj = c(0, 1),
     bg = "lightyellow",
     col = "black")