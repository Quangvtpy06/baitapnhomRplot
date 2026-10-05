library(quantmod)

tickers <- c("NKG.VN", "HSG.VN", "HPG.VN")
#==================================================
#             Phần A
#==================================================
# Sửa lại định dạng YYYY-MM-DD để R hiểu đúng từ ngày 01/01/2022
begin <- as.Date("2022-01-01") 
end <- Sys.Date()

getSymbols(tickers, src = "yahoo", from = begin, to = end)

# Loại bỏ các dòng bị rỗng/thiếu dữ liệu (NA)
NKG.VN <- na.omit(NKG.VN)
HSG.VN <- na.omit(HSG.VN)
HPG.VN <- na.omit(HPG.VN)

# Kiểm tra lại 6 dòng đầu tiên (lúc này sẽ hiện đúng từ tháng 01/2022)
head(NKG.VN)
head(HSG.VN)
head(HPG.VN)


# 1. Tính log return hằng ngày cho từng mã
ret_NKG <- dailyReturn(Ad(NKG.VN), type = "log")
ret_HSG <- dailyReturn(Ad(HSG.VN), type = "log")
ret_HPG <- dailyReturn(Ad(HPG.VN), type = "log")
# lay khoi luong volume giao dich cho tung ma
vol_NKG <- Vo(NKG.VN)
vol_HSG <- Vo(HSG.VN)
vol_HPG <- Vo(HPG.VN)
# Đổi tên cột cho dễ phân biệt
colnames(ret_NKG) <- "NKG_log_ret"
colnames(ret_HSG) <- "HSG_log_ret"
colnames(ret_HPG) <- "HPG_log_ret"
colnames(vol_NKG) <- "NKG_volume"
colnames(vol_HSG) <- "HSG_volume"
colnames(vol_HPG) <- "HPG_volume"
# 2. Loại bỏ dòng đầu tiên (dòng đầu tiên bị NA do không có ngày trước đó để tính)
ret_NKG <- na.omit(ret_NKG)
ret_HSG <- na.omit(ret_HSG)
ret_HPG <- na.omit(ret_HPG)
vol_NKG <- na.omit(vol_NKG)
vol_HSG <- na.omit(vol_HSG)
vol_HPG <- na.omit(vol_HPG)
# 3. Xem thử 6 dòng đầu log return của từng mã
head(ret_NKG)
head(ret_HSG)
head(ret_HPG)
head(vol_NKG)
head(vol_HSG)
head(vol_HPG)
# 4. (Tùy chọn) Gộp chung cả 3 mã vào 1 bảng để dễ so sánh/vẽ đồ thị
all_log_ret <- merge(ret_NKG, ret_HSG, ret_HPG,vol_NKG,vol_HSG,vol_HPG)
head(all_log_ret, 22)

#=============================================
#               Phần C 
#=============================================
library(ggplot2)
help(ggplot)
library(scales) # Dùng thư viện scales để chuyển đổi số 1e+... sang number
help(scales)
# tao data frame cho 3 ma
all_log_ret_df <- data.frame(date=index(all_log_ret),
                             log_return_NKG=as.numeric(all_log_ret$NKG_log_ret),
                             log_return_HSG=as.numeric(all_log_ret$HSG_log_ret),
                             log_return_HPG=as.numeric(all_log_ret$HPG_log_ret),
                             volume_NKG=as.numeric(all_log_ret$NKG_volume),
                             volume_HSG=as.numeric(all_log_ret$HSG_volume),
                             volume_HPG=as.numeric(all_log_ret$HPG_volume))
str(all_log_ret_df)
all_log_ret_df
help("geom_smooth")
help("geom_hline")
# 6.Vẽ scatter plot giữa returns của 2 mã cổ phiếu với nhau
# so sanh NKG va HSG anh xa volume cua truc x
p1 <- ggplot(all_log_ret_df, aes(x = log_return_NKG, y = log_return_HSG)) +
  geom_point(aes(size=volume_NKG),colour="steelblue") +
  geom_smooth(method="lm",colour="orange") +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng NKG"
  ) +
  labs(
    title = "Scatter 2D so sanh NKG va HSG",
    x = "NKG",
    y = "HSG"
  ) +
  theme_minimal()
p1
ggsave("BieudoscatterlogNKGvsHSG.png",p1)
# so sanh HPG va NKG
p2 <- ggplot(all_log_ret_df, aes(x = log_return_HPG, y = log_return_NKG)) +
  geom_point(aes(size=volume_HPG),colour="lightgreen") +
  geom_smooth(method="lm",colour="darkred") +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng HPG"
  ) +
  labs(
    title = "Scatter 2D so sanh HPG va NKG",
    x = "HPG",
    y = "NKG"
  ) +
  theme_minimal()
p2
ggsave("BieudoscatterlogHPGvsNKG.png",p2)
# so sanh HSG va HPG
p3 <- ggplot(all_log_ret_df, aes(x = log_return_HSG, y = log_return_HPG)) +
  geom_point(aes(size=volume_HSG),colour="gray") +
  geom_smooth(method="lm",colour="yellow") +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng HSG"
  ) +
  labs(
    title = "Scatter 2D so sanh HSG va HPG",
    x = "HSG",
    y = "HPG"
  ) +
  theme_minimal()
p3
ggsave("BieudoscatterlogHPGvsHSG.png",p3)
