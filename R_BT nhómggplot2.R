library(quantmod)

tickers <- c("NKG.VN", "HSG.VN", "HPG.VN")
#==================================================
#             Phần A – Chuẩn bị dữ liệu
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

# 4. (Tùy chọn) Gộp chung cả 3 mã vào 1 bảng để dễ so sánh/vẽ đồ thị
all_log_ret <- merge(ret_NKG, ret_HSG, ret_HPG,vol_NKG,vol_HSG,vol_HPG)
head(all_log_ret, 22)

#=============================================
#       Phần C – Mối quan hệ giữa 2–3 biến
#=============================================
library(ggplot2)
help(ggplot)
library(scales) # Dùng thư viện scales để chuyển đổi số 1e+... sang number
help(scales)
# tao data frame cho 3 ma
all_log_ret_df <- data.frame(Date=index(all_log_ret),
                             log_return_NKG=as.numeric(all_log_ret$NKG_log_ret),
                             log_return_HSG=as.numeric(all_log_ret$HSG_log_ret),
                             log_return_HPG=as.numeric(all_log_ret$HPG_log_ret),
                             total_volume = as.numeric(all_log_ret$NKG.VN.Volume)+
                               as.numeric(all_log_ret$HSG.VN.Volume)+
                               as.numeric(all_log_ret$HPG.VN.Volume))
str(all_log_ret_df)
all_log_ret_df

# 6.Vẽ scatter plot giữa returns của 2 mã cổ phiếu với nhau
# so sanh NKG va HSG
p1 <- ggplot(all_log_ret_df, aes(x = log_return_NKG,
                                 y = log_return_HSG,
                                 color = weekdays(Date))) +
  geom_point(aes(size = total_volume)) +
  geom_smooth(method="lm",colour="orange",show.legend = FALSE) +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng giao dịch"
  ) +
  labs(
    title = "Scatter 2D so sanh NKG va HSG",
    x = "NKG",
    y = "HSG",
    color = "Ngày trong tuần"
  )+
  theme_minimal()
p1
ggsave("BieudoscatterlogNKGvsHSG.png",p1)

# so sanh HPG va NKG
p2 <- ggplot(all_log_ret_df, aes(x = log_return_HPG,
                                 y = log_return_NKG,
                                 color = weekdays(Date))) +
  geom_point(aes(size = total_volume)) +
  geom_smooth(method="lm",colour="darkred",show.legend = FALSE) +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng giao dịch"
  ) +
  labs(
    title = "Scatter 2D so sanh HPG va NKG",
    x = "HPG",
    y = "NKG",
    color = "Ngày trong tuần"
  )+
  theme_minimal()
p2
ggsave("BieudoscatterlogHPGvsNKG.png",p2)

# so sanh HSG va HPG
p3 <- ggplot(all_log_ret_df, aes(x = log_return_HSG,
                                 y = log_return_HPG,
                                 color = weekdays(Date))) +
  geom_point(aes(size = total_volume)) +
  geom_smooth(method="lm",colour="yellow", show.legend = FALSE) +
  scale_size_continuous(
    labels = label_comma(),
    name = "Khối lượng HSG"
  ) +
  labs(
    title = "Scatter 2D so sanh HSG va HPG",
    x = "HSG",
    y = "HPG",
    color = "Ngày trong tuần"
  )+
  theme_minimal()
p3
ggsave("BieudoscatterlogHPGvsHSG.png",p3)

#============================================
# Phần D – Nâng cao: sử dụng facets & themes
#============================================
library(ggplot2)
help("facet_wrap")
library(scales)

str(all_log_ret_df)
# Tạo 3 bảng dữ liệu cho 3 mã 
df_NKG <- data.frame(
  Date = all_log_ret_df$Date,
  ticker = "NKG",
  log_return = all_log_ret_df$log_return_NKG
)
df_HSG <- data.frame(
  Date = all_log_ret_df$Date,
  ticker = "HSG",
  log_return = all_log_ret_df$log_return_HSG
)
df_HPG <- data.frame(
  Date = all_log_ret_df$Date,
  ticker = "HPG",
  log_return = all_log_ret_df$log_return_HPG
)
# Dùng rbind để ghép chồng 3 bảng thành một bảng với 3 cột date,ticker,log_return giống nhau
returns_3_ticker <- rbind(df_NKG,df_HSG,df_HPG)
returns_3_ticker
str(returns_3_ticker)
head(returns_3_ticker)
tail(returns_3_ticker)

# Vẽ biểu đồ returns theo thời gian cho từng mã dùng facet_wrap
help("geom_hline")
help("facet_wrap")
help("scale_x_date")
p4 <- ggplot(returns_3_ticker, aes(x=Date,y=log_return,color=ticker)) +
  geom_line() +
  geom_hline(yintercept = 0,linetype = "dashed", color="brown")+
  facet_wrap(~ticker, ncol=1, scales = "free_y")+   #Chia 3 ô theo ticker (tự mở rộng theo trục y)
  scale_x_date(date_breaks = "3 months",
               date_labels = "%m-%Y",
               expand = expansion(mult = c(0.01, 0.01)) # tach 1% cho 2 ben mep trai va phai
  )+
  scale_color_manual(values = c("NKG" = "steelblue","HSG" = "orange", "HPG" = "forestgreen"))+
  labs(title = "Biến động tỷ suất sinh lợi theo thời gian của 3 mã",
       subtitle = "So sánh chuỗi lợi suất theo tháng của 3 mã",
       x = "Thời gian (tháng)",
       y= "Tỷ suất sinh lợi")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0),  # chỉnh font chữ cho biểu đồ
    strip.text = element_text(face = "bold", size = 11),   # chỉnh font chữ cho tiêu đề 3 mã
    axis.text.x = element_text(angle = 45, hjust=1),   # quay nhãn trục x để không bị đè
    panel.grid.minor = element_blank(),   # xóa lưới biểu đồ
    legend.position = "none"    # Tắt chú thích màu bên phải
  )
p4
ggsave("Bieu do so sanh return cua 3 ma theo thoi gian.png",p4)
