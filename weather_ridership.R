# 대상 구 이름
target_gu <- c("서대문", "중구", "성북")

# 파일 이름 벡터
files <- c("서울날씨2022.csv", "서울날씨2023.csv", "서울날씨2024.csv")

# 필터링하여 리스트로 저장
data_list <- lapply(files, function(x) {
  df <- read.csv(x, fileEncoding = "euc-kr")
  df[df[[2]] %in% target_gu, ]
})

# 병합
final_data <- do.call(rbind, data_list)

# 저장
write.csv(final_data, "서울날씨_병합데이터.csv", row.names = FALSE, fileEncoding = "euc-kr")




# 조건에 맞는 역명
target_stations <- c("신촌", "성신여대입구(돈암)", "종각")

# 파일 벡터
files <- c("서울지하철_승하차인원2022.csv", 
           "서울지하철_승하차인원2023.csv", 
           "서울지하철_승하차인원2024.csv")

# 필터링 후 병합
data_list <- lapply(files, function(x) {
  df <- read.csv(x, fileEncoding = "euc-kr")
  df[df[[5]] %in% target_stations, ]
})

# 최종 병합
final_data <- do.call(rbind, data_list)
write.csv(final_data, "서울지하철_병합데이터.csv", row.names = FALSE, fileEncoding = "euc-kr")

######################################### 

# --- 4. 날씨와 지하철 승하차 데이터 최종 병합하기 (시간대별 결합 버전) ---
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
library(dplyr)
library(lubridate)
library(tidyr)

# 1) 데이터 불러오기
weather <- read.csv(
  "서울날씨_병합데이터.csv",
  fileEncoding   = "euc-kr",
  stringsAsFactors = FALSE
)
subway <- read.csv(
  "서울지하철_병합데이터.csv",
  fileEncoding     = "euc-kr",
  stringsAsFactors = FALSE,
  check.names      = FALSE
)
# 2) 지하철: '06시 이전' + '24시 이후' → '24-06시'
subway <- subway %>%
  mutate(`24-06시` = `06시 이전` + `24시 이후`) %>%
  select(-`06시 이전`, -`24시 이후`)
# 3) 지하철: wide → long
subway_long <- subway %>%
  mutate(날짜 = as.Date(날짜, format = "%Y-%m-%d")) %>%
  pivot_longer(
    cols = c(
      "24-06시", "06-07시", "07-08시", "08-09시", "09-10시", "10-11시",
      "11-12시", "12-13시", "13-14시", "14-15시", "15-16시", "16-17시",
      "17-18시", "18-19시", "19-20시", "20-21시", "21-22시", "22-23시", "23-24시"
    ),
    names_to  = "time_slot",
    values_to = "count"
  )
# 4) 날씨: 시간대별 요약
weather_slot <- weather %>%
  mutate(
    일시 = as.POSIXct(일시, format = "%Y-%m-%d %H:%M", tz = "Asia/Seoul"),
    날짜 = as.Date(일시),
    hour = hour(일시)
  ) %>%
  mutate(
    time_slot = case_when(
      hour <= 5  ~ "24-06시",
      hour == 6  ~ "06-07시",
      hour == 7  ~ "07-08시",
      hour == 8  ~ "08-09시",
      hour == 9  ~ "09-10시",
      hour == 10 ~ "10-11시",
      hour == 11 ~ "11-12시",
      hour == 12 ~ "12-13시",
      hour == 13 ~ "13-14시",
      hour == 14 ~ "14-15시",
      hour == 15 ~ "15-16시",
      hour == 16 ~ "16-17시",
      hour == 17 ~ "17-18시",
      hour == 18 ~ "18-19시",
      hour == 19 ~ "19-20시",
      hour == 20 ~ "20-21시",
      hour == 21 ~ "21-22시",
      hour == 22 ~ "22-23시",
      hour == 23 ~ "23-24시",
      TRUE       ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_slot)) %>%
  group_by(날짜, time_slot) %>%
  summarize(
    avg_temp   = mean(기온, na.rm = TRUE),
    total_rain = sum(강수량, na.rm = TRUE),
    .groups    = "drop"
  )

# 5) 결합
merged_long <- subway_long %>%
  left_join(weather_slot, by = c("날짜", "time_slot"))
# 6) CSV로 저장 (euc-kr 인코딩)
con <- file("날씨_지하철_병합데이터.csv", encoding = "euc-kr")
write.csv(merged_long, con, row.names = FALSE)
close(con)
# 7) 결과 확인 (선택)
cat("결합된 행개수:", nrow(merged_long), "\n")
str(merged_long)
head(merged_long, 10)


# 데이터 전처리 # 분류분석
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")


library(dplyr)
library(lubridate)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)

# 데이터 로드
data <- read.csv("날씨_지하철_병합데이터.csv", fileEncoding="CP949", stringsAsFactors=FALSE, check.names=FALSE)
data <- data %>% mutate(날짜 = as.Date(날짜))

# 일별 승하차 및 기상 요약
# 1) 지하철 일별·역별 총승하차
data_daily <- data %>%
  group_by(날짜, 역명, 승하차구분) %>%
  summarise(합계 = sum(count, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=승하차구분, values_from=합계, values_fill=0) %>%
  mutate(총승하차 = 승차 + 하차)

# 2) 날씨 일별 요약: 평균기온, 일강수량
weather_daily <- data %>%
  distinct(날짜, time_slot, avg_temp, total_rain) %>%
  mutate(시간수 = ifelse(time_slot=="24-06시", 6, 1)) %>%
  group_by(날짜) %>%
  summarise(
    평균기온 = sum(avg_temp * 시간수, na.rm=TRUE)/sum(시간수, na.rm=TRUE),
    일강수량 = sum(total_rain, na.rm=TRUE),
    .groups="drop"
  )

# 3) 결합
data_daily <- left_join(data_daily, weather_daily, by="날짜")

# 분석 대상 역 목록 확인 후 설정
stations <- c("신촌", "성신여대입구(돈암)", "종각")

# 역별 분류분석: 총승하차량이 해당 역 전체 기간 중앙값 초과 여부 예측
for (st in stations) {
  df_st <- filter(data_daily, 역명 == st) %>% filter(!is.na(총승하차), !is.na(평균기온), !is.na(일강수량))
  if (nrow(df_st) < 20) {
    message(st, ": 데이터 부족으로 분류분석 생략")
    next
  }
  # 목표변수 생성: 중앙값 기준 high/low
  med <- median(df_st$총승하차, na.rm=TRUE)
  df_st <- df_st %>%
    mutate(승차범주 = factor(ifelse(총승하차 > med, "High", "Low")))
  
  # 학습/검증 데이터 분리 (70% 학습, 30% 검증)
  set.seed(123)
  train_idx <- createDataPartition(df_st$승차범주, p=0.7, list=FALSE)
  train <- df_st[train_idx, ]
  test  <- df_st[-train_idx, ]
  
  # 1) 로지스틱 회귀
  model_glm <- glm(승차범주 ~ 평균기온 + 일강수량, data=train, family=binomial)
  # 예측 및 평가
  probs <- predict(model_glm, newdata=test, type="response")
  pred_glm <- factor(ifelse(probs > 0.5, "High", "Low"), levels=c("Low","High"))
  cm_glm <- confusionMatrix(pred_glm, test$승차범주)
  cat("=== ", st, " 로지스틱 회귀 ===\n")
  print(summary(model_glm))
  print(cm_glm)
  
  # 2) 의사결정나무
  model_tree <- rpart(승차범주 ~ 평균기온 + 일강수량, data=train, method="class")
  cat("=== ", st, " 의사결정나무 ===\n")
  rpart.plot(model_tree, main=paste0(st, " 결정트리"))
  pred_tree <- predict(model_tree, newdata=test, type="class")
  cm_tree <- confusionMatrix(pred_tree, test$승차범주)
  print(cm_tree)
}



# 다중 선형 회귀분석
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# 데이터 로드
data <- read.csv("날씨_지하철_병합데이터.csv", fileEncoding="CP949", stringsAsFactors=FALSE, check.names=FALSE)
data <- data %>% mutate(날짜 = as.Date(날짜))

# 일별 승하차 및 기상 요약
data_daily <- data %>%
  group_by(날짜, 역명, 승하차구분) %>%
  summarise(합계 = sum(count, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=승하차구분, values_from=합계, values_fill=0) %>%
  mutate(총승하차 = 승차 + 하차)

weather_daily <- data %>%
  distinct(날짜, time_slot, avg_temp, total_rain) %>%
  mutate(시간수 = ifelse(time_slot=="24-06시", 6, 1)) %>%
  group_by(날짜) %>%
  summarise(
    평균기온 = sum(avg_temp * 시간수, na.rm=TRUE)/sum(시간수, na.rm=TRUE),
    일강수량 = sum(total_rain, na.rm=TRUE),
    .groups="drop"
  )

data_daily <- left_join(data_daily, weather_daily, by="날짜")

# 분석 대상 역 설정
stations <- c("신촌", "성신여대입구(돈암)", "종각")

# 회귀분석 함수
run_regression <- function(df, name) {
  df2 <- df %>% filter(!is.na(총승하차), !is.na(평균기온), !is.na(일강수량))
  if (nrow(df2) < 10) {
    message(name, ": 데이터 부족")
    return(NULL)
  }
  model <- lm(총승하차 ~ 평균기온 + 일강수량, data=df2)
  sum_model <- summary(model)
  coefs <- coef(model)
  eq <- paste0("회귀식(", name, ") = ", round(coefs[1],2),
               ifelse(coefs["평균기온"]>=0, " + ", " - "), abs(round(coefs["평균기온"],2)), "*평균기온",
               ifelse(coefs["일강수량"]>=0, " + ", " - "), abs(round(coefs["일강수량"],2)), "*일강수량")
  cat("=== ", name, " 회귀분석 ===\n")
  print(sum_model)
  cat(eq, "\n\n")
  return(model)
}

# 역별 회귀 실행
for (st in stations) {
  df_st <- filter(data_daily, 역명==st)
  model <- run_regression(df_st, st)
  # 필요 시 plot(model) 으로 진단 가능
}

# 전체 3개역 합산 회귀
data_all <- data_daily %>%
  group_by(날짜) %>%
  summarise(일별총승하차 = sum(총승하차, na.rm=TRUE), .groups="drop") %>%
  left_join(weather_daily, by="날짜")

if (nrow(data_all)>=10) {
  model_all <- lm(일별총승하차 ~ 평균기온 + 일강수량, data=data_all)
  print(summary(model_all))
  coefs_all <- coef(model_all)
  eq_all <- paste0("회귀식(전체합산) = ", round(coefs_all[1],2),
                   ifelse(coefs_all["평균기온"]>=0, " + ", " - "), abs(round(coefs_all["평균기온"],2)), "*평균기온",
                   ifelse(coefs_all["일강수량"]>=0, " + ", " - "), abs(round(coefs_all["일강수량"],2)), "*일강수량")
  cat(eq_all, "\n")
}


# 시각화

# 필요한 패키지 설치 및 로드
required_pkgs <- c("dplyr", "ggplot2", "lubridate", "tidyr")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, dependencies=TRUE)
}
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# 데이터 로드 및 요약 (앞과 동일)
data <- read.csv("날씨_지하철_병합데이터.csv", fileEncoding="CP949", stringsAsFactors=FALSE, check.names=FALSE)
data <- data %>% mutate(날짜 = as.Date(날짜))
data_daily <- data %>%
  group_by(날짜, 역명, 승하차구분) %>%
  summarise(합계 = sum(count, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=승하차구분, values_from=합계, values_fill=0) %>%
  mutate(총승하차 = 승차 + 하차)
weather_daily <- data %>%
  distinct(날짜, time_slot, avg_temp, total_rain) %>%
  mutate(시간수=ifelse(time_slot=="24-06시",6,1)) %>%
  group_by(날짜) %>%
  summarise(
    평균기온 = sum(avg_temp*시간수, na.rm=TRUE)/sum(시간수, na.rm=TRUE),
    일강수량 = sum(total_rain, na.rm=TRUE),
    .groups="drop"
  )
data_daily <- left_join(data_daily, weather_daily, by="날짜")

# 대상 역 및 전체합산 준비
stations <- c("신촌", "성신여대입구(돈암)", "종각")
data_sel <- filter(data_daily, 역명 %in% stations)

# 역별 시각화
for (st in stations) {
  df_st <- filter(data_sel, 역명==st) %>% filter(!is.na(평균기온), !is.na(일강수량), !is.na(총승하차))
  if (nrow(df_st)<5) next
  # 평균기온 vs 총승하차
  p1 <- ggplot(df_st, aes(x=평균기온, y=총승하차)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE, color="red") +
    labs(title=paste0(st, ": 평균기온 vs 총승하차"), x="평균기온", y="총승하차") +
    theme_minimal()
  print(p1)
  # 일강수량 vs 총승하차
  p2 <- ggplot(df_st, aes(x=일강수량, y=총승하차)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE, color="red") +
    labs(title=paste0(st, ": 일강수량 vs 총승하차"), x="일강수량", y="총승하차") +
    theme_minimal()
  print(p2)
}

# 전체합산 시각화
data_all <- data_sel %>%
  group_by(날짜) %>%
  summarise(일별총승하차 = sum(총승하차, na.rm=TRUE), .groups="drop") %>%
  left_join(weather_daily, by="날짜")

if (nrow(data_all)>=5) {
  p3 <- ggplot(data_all, aes(x=평균기온, y=일별총승하차)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE, color="blue") +
    labs(title="전체합산: 평균기온 vs 일별총승하차", x="평균기온", y="일별총승하차") +
    theme_minimal()
  print(p3)
  p4 <- ggplot(data_all, aes(x=일강수량, y=일별총승하차)) +
    geom_point(alpha=0.6) +
    geom_smooth(method="lm", se=FALSE, color="blue") +
    labs(title="전체합산: 일강수량 vs 일별총승하차", x="일강수량", y="일별총승하차") +
    theme_minimal()
  print(p4)
}






















