#'---
#'title: "전국 강우기준(Rainfall Threshold)"
#'author: "Kyungtak Kim"
#'date: '2020 5 11 '
#'output: github_document
#'      
#'  
#'---


#+ library, warning=FALSE, message=FALSE
Sys.setenv(Language="En")
library(tidyverse)


#' # 원본 데이터 읽기 / 특성 분석  
#' 
rain_threshold <- read.csv('input/Rainfall_Threshold_200511.csv')
head(rain_threshold, 3)
str(rain_threshold)

#'전체 Data 특성
#'
levels(rain_threshold$BASIN)  #4개권역으로 구분

## [시사점]현 기상청 3시간 호우주의보(60mm), 호우경보(90mm)가 본 연구의
## 경계, 심각의 기준과 유사하게 형성되어 있다. (200512현재) 
(mean_all <- round(sapply(rain_threshold[,5:7], mean), 2)) # 평균
(quan_all <- do.call(cbind.data.frame, lapply(rain_threshold[,5:7], quantile))) # quantile

## 전체 기준에 대한 boxplot을 그려본 것이다.
rain_threshold %>% 
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rainfall_threshold") %>% 
  ggplot(aes(limit, rainfall_threshold))+
  geom_boxplot()+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)



#' 권역별
#' 
tapply(rain_threshold[,5], rain_threshold$BASIN, mean) # 평균 비교
tapply(rain_threshold[,6], rain_threshold$BASIN, mean) # 평균 비교
tapply(rain_threshold[,7], rain_threshold$BASIN, mean) # 평균 비교

## limit2
(limit2 <- do.call(cbind.data.frame, tapply(rain_threshold[,5], rain_threshold$BASIN, quantile)))
(limit3 <- do.call(cbind.data.frame, tapply(rain_threshold[,6], rain_threshold$BASIN, quantile)))
(limit4 <- do.call(cbind.data.frame, tapply(rain_threshold[,7], rain_threshold$BASIN, quantile)))

## 각권역의 주의/경계/심각의 평균을 비교해보면 낙동강이 가장 낮고 한강이 가장 높다.
## 즉, 현재와 비교하면 약간 하향좆정해야한다는 것을 의미한다.(낙동강권역의경우)
## 나머지 한강권역의 경우 연재 기준에 비해 상향조정해도 괜찮다.
rain_threshold %>% 
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  select(-LIMIT1) %>% 
  group_by(BASIN, limit) %>% 
  summarise(n=n(),
            mean=mean(rain_cri),
            lq=quantile(rain_cri, 0.25),
            uq=quantile(rain_cri, 0.75))%>% 
  ggplot(aes(BASIN, mean))+
  geom_point(aes(col=limit), size=3)+
  geom_linerange(aes(ymin=lq, ymax=uq), size=0.5, col="blue", alpha=0.3)+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)

#' 권역별, 주의/경계/심각에 대한 각 지자체의 rainfall_threshold 값의 분포에 대한
#' Boxplot 
rain_threshold %>% 
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  select(-LIMIT1) %>% 
  group_by(BASIN, limit) %>% 
  ggplot(aes(BASIN, rain_cri))+
  geom_boxplot(aes(col=limit))+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)




#' 228개 지자체별 비교
#' 
#+ fig.width=12, fig.height=25
rain_threshold %>% 
  mutate(mean= round((LIMIT3+LIMIT4)/2,2)) %>% 
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  group_by(ADMINNAME)%>% 
  ggplot(aes(reorder(ADMINNAME, mean), rain_cri))+
  geom_boxplot()+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)+
  coord_flip()

##[시사점] 서울특별시를 포함한 도시지역의 현재의 60m-90m/3hr 기준은 
## 피해현상과 비교 (LIMIT3+LIMIT4의 평균)과 비교할때 너무 빨리 발령되는 것이라 할 수 있다.
rain_threshold %>% 
  mutate(mean= round((LIMIT3+LIMIT4)/2,2)) %>%  
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  group_by(ADMINNAME)%>% 
  filter(mean > 90) %>% 
  ggplot(aes(reorder(ADMINNAME, mean), rain_cri))+
  geom_boxplot()+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)+
  coord_flip()

rain_threshold %>% 
  mutate(mean= round((LIMIT3+LIMIT4)/2,2)) %>%  
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  group_by(ADMINNAME) %>% 
  filter(mean <= 90 & mean > 60) %>% 
  ggplot(aes(reorder(ADMINNAME, mean), rain_cri))+
  geom_boxplot()+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)+
  coord_flip()  

## [시사점] 일부 지자체의 경우 현재의 60m-90m/3hr 기준은 
## 피해현상과 비교 (LIMIT3+LIMIT4의 평균)과 비교할때 너무 늦게 발령 되는 것이라 할 수 있다.
## 즉, 강우예측이 정확하다는 가정만 이루어지면
## 이들 지지체의 경우는 현재보고 좀더 빨리 발령할 필요가 있다.(???)
rain_threshold %>% 
  mutate(mean= round((LIMIT3+LIMIT4)/2,2)) %>%  
  pivot_longer(c("LIMIT2","LIMIT3","LIMIT4"), names_to = "limit", values_to = "rain_cri")%>% 
  group_by(ADMINNAME)%>% 
  filter(mean <= 60) %>% 
  ggplot(aes(reorder(ADMINNAME, mean), rain_cri))+
  geom_boxplot()+
  geom_hline(yintercept=60, linetype="dashed", color = "blue", size=0.5)+
  geom_hline(yintercept=90, linetype="dashed", color = "red", size=0.5)+
  coord_flip()


