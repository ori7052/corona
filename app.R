library(shiny)
library(tidyverse)
library(magrittr)
library(sp)
library(rgdal)
library(ggmap)
library(raster)
library(shiny)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(lubridate)
register_google(key = "AIzaSyDlA6dt1tIOZ4f5B3abzsz6HLXW-iegB7U", write = TRUE)
monthlist <- c("201901", "201902", "201903", "201904", "201905", "201906", "201907", "201908", "201909", "201910", "201911", "201912", "202001", "202002", "202003", "202004", "202005", "202006", "202007", "202008", "202009", "202010", "202011", "202012", "202101", "202102")
timelist <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
gmap <- get_googlemap("seoul", zoom = 11, maptype = "roadmap") %>% ggmap()
busstop <- read_csv("data\\busstop.csv")
dong_jwapyo <- read_csv("data\\dong_jwapyo.csv")
ID_upjong <- read_csv("data\\ID_upjong.csv")
dong_upjong <- read_csv("data\\dong_upjong.csv")
corona <- read_csv("data\\월별_코로나_확진자.csv")

get_busIDlist <- function(i,j) {
  IDlist <- read_csv(str_c("data\\", monthlist[i], timelist[j], "ID.csv"))
  return(IDlist)
}

get_busdonglist <- function(i,j) {
  IDlist <- read_csv(str_c("data\\", monthlist[i], timelist[j], ".csv"))
  return(IDlist)
}

get_busIDtime <- function(IDtime, sigantime) {
  if(length(IDtime) == 1) {
    cat(IDtime, sigantime, "\n")
    IDtimelist <- read_csv(str_c("data\\", IDtime, ".csv"))
    IDtimelist <- IDtimelist[,c(1, 2+sigantime, 26+sigantime, 50+sigantime)]
    colnames(IDtimelist) <- c("년월", "승차량", "하차량", "유입량")
    return(IDtimelist)
  } else if(length(IDtime) ==2) {
    cat(IDtime, sigantime, "\n")
    IDtimelist1 <- read_csv(str_c("data\\", IDtime[1], ".csv"))
    IDtimelist1 <- IDtimelist1[,c(1, 2+sigantime, 26+sigantime, 50+sigantime)]
    colnames(IDtimelist1) <- c("년월", "승차량", "하차량", "유입량")
    IDtimelist2 <- read_csv(str_c("data\\", IDtime[2], ".csv"))
    IDtimelist2 <- IDtimelist2[,c(2+sigantime, 26+sigantime, 50+sigantime)]
    colnames(IDtimelist2) <- c("승차량", "하차량", "유입량")
    IDtimelist <- IDtimelist1 %>% mutate(승차량 = 승차량 + IDtimelist2$승차량, 
                                            하차량 = 하차량 + IDtimelist2$하차량, 
                                            유입량 = 유입량 + IDtimelist2$유입량)
    return(IDtimelist)
  }
}

IDdiff <- function(nal1, nal2, sigan) {
  i1 = which(monthlist == nal1)
  i2 = which(monthlist == nal2)
  j = which(timelist == sigan)
  list1 <- get_busIDlist(i1,j)
  list2 <- get_busIDlist(i2,j)
  difflist <- inner_join(list1, list2, by="ID")
  colnames(difflist) <- c("ID", "in1", "out1", "diff1", "in2", "out2", "diff2")
  difflist[is.na(difflist)] <- 0
  difflist %<>% mutate(diff1 = -diff1, diff2 = -diff2)
  difflist %<>% mutate(indiff = in2 - in1, indiffpercentage = indiff/abs(in1+1), 
                       outdiff = out2 - out1, outdiffpercentage = outdiff/abs(out1+1),
                       diff = diff2 - diff1, diffpercentage = diff/abs(diff1+1))
  difflist$diffpercentage[is.nan(difflist$diffpercentage)] <- 0
  difflist$indiffpercentage[is.nan(difflist$indiffpercentage)] <- 0
  difflist$outdiffpercentage[is.nan(difflist$outdiffpercentage)] <- 0
  difflist$diffpercentage[is.infinite(difflist$diffpercentage)] <- 0
  difflist$indiffpercentage[is.infinite(difflist$indiffpercentage)] <- 0
  difflist$outdiffpercentage[is.infinite(difflist$outdiffpercentage)] <- 0
  return(difflist)
}

dongdiff <- function(nal1, nal2, sigan) {
  i1 = which(monthlist == nal1)
  i2 = which(monthlist == nal2)
  j = which(timelist == sigan)
  list1 <- get_busdonglist(i1,j)
  list2 <- get_busdonglist(i2,j)
  difflist <- bind_cols(list1, list2[,2:4])
  colnames(difflist) <- c("dong", "in1", "out1", "diff1", "in2", "out2", "diff2")
  difflist %<>% mutate(diff1 = -diff1, diff2 = -diff2)
  difflist %<>% mutate(indiff = in2 - in1, indiffpercentage = indiff/abs(in1), 
                       outdiff = out2 - out1, outdiffpercentage = outdiff/abs(out1),
                       diff = diff2 - diff1, diffpercentage = diff/abs(diff1))
  return(difflist)
}

find_difflist <- function(nal1, nal2, sigan) {
  difflist <- IDdiff(nal1, nal2, sigan)
  difflist <- left_join(difflist, busstop, by = "ID")
  difflist %<>% mutate(pm = as.factor(diff / abs(diff)))
  difflist <- difflist[!is.na(difflist[,1]),]
  for(i in 1:nrow(difflist)) {
    if(abs(difflist$indiffpercentage[i]) > 1) {
      difflist$indiffpercentage[i] <- difflist$indiffpercentage[i] / abs(difflist$indiffpercentage[i])
    }
    if(abs(difflist$outdiffpercentage[i]) > 1) {
      difflist$outdiffpercentage[i] <- difflist$outdiffpercentage[i] / abs(difflist$outdiffpercentage[i])
    }
    if(abs(difflist$diffpercentage[i]) > 1) {
      difflist$diffpercentage[i] <- difflist$diffpercentage[i] / abs(difflist$diffpercentage[i])
    }
  }
  return(difflist)
}

find_difflistdong <- function(nal1, nal2, sigan) {
  difflist <- dongdiff(nal1, nal2, sigan)
  difflist <- left_join(difflist, dong_jwapyo, by = "dong")
  difflist %<>% mutate(pm = as.factor(diff / abs(diff)))
  difflist <- difflist[!is.na(difflist[,1]),]
  for(i in 1:nrow(difflist)) {
    if(abs(difflist$indiffpercentage[i]) > 1) {
      difflist$indiffpercentage[i] <- difflist$indiffpercentage[i] / abs(difflist$indiffpercentage[i])
    }
    if(abs(difflist$outdiffpercentage[i]) > 1) {
      difflist$outdiffpercentage[i] <- difflist$outdiffpercentage[i] / abs(difflist$outdiffpercentage[i])
    }
    if(abs(difflist$diffpercentage[i]) > 1) {
      difflist$diffpercentage[i] <- difflist$diffpercentage[i] / abs(difflist$diffpercentage[i])
    }
  }
  return(difflist)
}

IDlinear <- function(nal1, nal2, sigan, inout) {
  diffID <- full_join(IDdiff(nal1, nal2, sigan), ID_upjong, by="ID")
  if(inout=="in") {
    target <- diffID$indiffpercentage[-c(431:434)]
  } else if(inout=="out") {
    target <- diffID$outdiffpercentage[-c(431:434)]
  } else {
    target <- diffID$diffpercentage[-c(431:434)]
  }
  arr <- diffID[-c(431:434),-c(1:13)]
  arr[is.na(arr)] <- 0
  colnames(arr) %<>% str_sub(1,-5)
  lmodel <- lm(formula = target ~ 안전상비의약품판매업소 + 산후조리업 + 부속의료기관 + 의원 + 병원 + 의료유사업 + 의료법인 + 응급환자이송업 + 약국 + 치과기공소 + 의료기기판매임대업 + 의료기기수리업 + 안경업 + 가축사육업 + 동물판매업 + 동물장묘업 + 동물용의약품도매상 + 동물용의료용구판매업 + 동물약국 + 동물병원 + 복합영상물제공업 + 게임물제작업 + 게임물배급업 + 종축업 + 사료제조업 + 부화업 + 도축업 + 가축인공수정소 + 관광극장유흥업 + 관광공연장업 + 청소년게임제공업 + 일반게임제공업 + 인터넷컴퓨터게임시설제공업 + 복합유통게임제공업 + 일반유원시설업 + 유원시설업기타 + 시내순환관광업 + 국제회의시설업 + 관광사업자 + 관광유람선업 + 관광궤도업 + 대중문화예술기획업 + 국제회의기획업 + 지방문화원 + 종합휴양업 + 종합유원시설업 + 전통사찰 + 전문휴양업 + 비디오물제작업 + 비디오물시청제공업 + 비디오물소극장업 + 비디오물배급업 + 비디오물감상실업 + 노래연습장업 + 문화예술법인 + 자동차야영장업 + 외국인관광도시민박업 + 숙박업 + 관광펜션업 + 관광숙박업 + 영화상영업 + 영화상영관 + 영화배급업 + 일반여행업 + 국외여행업 + 국내여행업 + 일반야영장업 + 음반물제작업 + 음반물배급업 + 음반.음악영상물제작업 + 음반.음악영상물배급업 + 온라인음악서비스제공업 + 영화제작업 + 영화수입업 + 미용업 + 출판사 + 인쇄사 + 옥외광고업 + 집단급식소 + 위탁급식영업 + 의료기관세탁물처리업 + 세탁업 + 이용업 + 축산가공업 + 축산판매업 + 건강기능식품일반판매업 + 건강기능식품유통전문판매업 + 집단급식소식품판매업 + 식품자동판매기업 + 식품운반업 + 식품소분업 + 식품냉동냉장업 + 식육포장처리업 + 옹기류제조업 + 식품판매업기타 + 식품첨가물제조업 + 식품제조가공업 + 즉석판매제조가공업 + 제과점영업 + 유통전문판매업 + 용기냉동기특정설비 + 단란주점영업 + 축산물운반업 + 축산물보관업 + 식용얼음판매업 + 집유업 + 일반음식점 + 외국인전용유흥음식점업 + 관광유흥음식점업 + 관광식당 + 유흥주점영업 + 휴게음식점 + 전화권유판매업 + 방문판매업 + 다단계판매업체 + 대규모점포 + 통신판매업 + 계량기제조업 + 계량기수입업 + 계량기수리업 + 제재업 + 원목생산업 + 목재수입유통업 + 후원방문판매업체 + 고압가스업 + 계량기증명업 + 일반도시가스업체 + 액화석유가스용품제조업체 + 석유판매업 + 석유및석유대체연료판매업체 + 석연탄제조업 + 가축분뇨배출시설관리업사업장 + 가축분뇨수집운반업 + 지하수정화업체 + 지하수영향조사기관 + 지하수시공업체 + 특정고압가스업 + 전력기술설계업체 + 전력기술감리업체 + 배출가스전문정비사업자확인검사대행자 + 대기오염물질배출시설설치사업장 + 단독정화조오수처리시설설계시공업 + 급수공사대행업 + 건설폐기물처리업 + 건물위생관리업 + 개인하수처리시설관리업사업장 + 환경전문공사업 + 환경관리대행기관 + 저수조청소업 + 쓰레기종량제봉투판매업 + 수질오염원설치시설기타 + 소독업 + 분뇨수집운반업 + 당구장업 + 등록체육시설업 + 골프장 + 골프연습장업 + 환경컨설팅회사 + 환경측정대행업 + 요트장업 + 썰매장업 + 승마장업 + 종합체육시설업 + 스키장 + 수영장업 + 빙상장업 + 무도학원업 + 무도장업 + 담배소매업 + 담배도매업 + 체력단련장업 + 체육도장업 + 상조업 + 민방위대피시설 + 민방위급수시설 + 물류창고업체 + 물류주선업국제 + 목욕장업 + 담배수입판매업체 + 행정사업 + 유료직업소개소 + 무료직업소개소 + 장례지도사교육기관 + 요양보호사교육기관 + 승강기제조및수입업체 + 승강기유지관리업체, 
               data = arr)
  return(stargazer(lmodel, type = "html"))
}

donglinear <- function(nal1, nal2, sigan, inout) {
  diffdong <- full_join(dongdiff(nal1, nal2, sigan), dong_upjong, by="dong")
  if(inout=="in") {
    target <- diffdong$indiffpercentage[-c(431:434)]
  } else if(inout=="out") {
    target <- diffdong$outdiffpercentage[-c(431:434)]
  } else {
    target <- diffdong$diffpercentage[-c(431:434)]
  }
  arr <- diffdong[-c(431:434),-c(1:13)]
  arr[is.na(arr)] <- 0
  colnames(arr) %<>% str_sub(1,-5)
  lmodel <- lm(formula = target ~ 안전상비의약품판매업소 + 산후조리업 + 부속의료기관 + 의원 + 병원 + 의료유사업 + 의료법인 + 응급환자이송업 + 약국 + 치과기공소 + 의료기기판매임대업 + 의료기기수리업 + 안경업 + 가축사육업 + 동물판매업 + 동물장묘업 + 동물용의약품도매상 + 동물용의료용구판매업 + 동물약국 + 동물병원 + 복합영상물제공업 + 게임물제작업 + 게임물배급업 + 종축업 + 사료제조업 + 부화업 + 도축업 + 가축인공수정소 + 관광극장유흥업 + 관광공연장업 + 청소년게임제공업 + 일반게임제공업 + 인터넷컴퓨터게임시설제공업 + 복합유통게임제공업 + 일반유원시설업 + 유원시설업기타 + 시내순환관광업 + 국제회의시설업 + 관광사업자 + 관광유람선업 + 관광궤도업 + 대중문화예술기획업 + 국제회의기획업 + 지방문화원 + 종합휴양업 + 종합유원시설업 + 전통사찰 + 전문휴양업 + 비디오물제작업 + 비디오물시청제공업 + 비디오물소극장업 + 비디오물배급업 + 비디오물감상실업 + 노래연습장업 + 문화예술법인 + 자동차야영장업 + 외국인관광도시민박업 + 숙박업 + 관광펜션업 + 관광숙박업 + 영화상영업 + 영화상영관 + 영화배급업 + 일반여행업 + 국외여행업 + 국내여행업 + 일반야영장업 + 음반물제작업 + 음반물배급업 + 음반.음악영상물제작업 + 음반.음악영상물배급업 + 온라인음악서비스제공업 + 영화제작업 + 영화수입업 + 미용업 + 출판사 + 인쇄사 + 옥외광고업 + 집단급식소 + 위탁급식영업 + 의료기관세탁물처리업 + 세탁업 + 이용업 + 축산가공업 + 축산판매업 + 건강기능식품일반판매업 + 건강기능식품유통전문판매업 + 집단급식소식품판매업 + 식품자동판매기업 + 식품운반업 + 식품소분업 + 식품냉동냉장업 + 식육포장처리업 + 옹기류제조업 + 식품판매업기타 + 식품첨가물제조업 + 식품제조가공업 + 즉석판매제조가공업 + 제과점영업 + 유통전문판매업 + 용기냉동기특정설비 + 단란주점영업 + 축산물운반업 + 축산물보관업 + 식용얼음판매업 + 집유업 + 일반음식점 + 외국인전용유흥음식점업 + 관광유흥음식점업 + 관광식당 + 유흥주점영업 + 휴게음식점 + 전화권유판매업 + 방문판매업 + 다단계판매업체 + 대규모점포 + 통신판매업 + 계량기제조업 + 계량기수입업 + 계량기수리업 + 제재업 + 원목생산업 + 목재수입유통업 + 후원방문판매업체 + 고압가스업 + 계량기증명업 + 일반도시가스업체 + 액화석유가스용품제조업체 + 석유판매업 + 석유및석유대체연료판매업체 + 석연탄제조업 + 가축분뇨배출시설관리업사업장 + 가축분뇨수집운반업 + 지하수정화업체 + 지하수영향조사기관 + 지하수시공업체 + 특정고압가스업 + 전력기술설계업체 + 전력기술감리업체 + 배출가스전문정비사업자확인검사대행자 + 대기오염물질배출시설설치사업장 + 단독정화조오수처리시설설계시공업 + 급수공사대행업 + 건설폐기물처리업 + 건물위생관리업 + 개인하수처리시설관리업사업장 + 환경전문공사업 + 환경관리대행기관 + 저수조청소업 + 쓰레기종량제봉투판매업 + 수질오염원설치시설기타 + 소독업 + 분뇨수집운반업 + 당구장업 + 등록체육시설업 + 골프장 + 골프연습장업 + 환경컨설팅회사 + 환경측정대행업 + 요트장업 + 썰매장업 + 승마장업 + 종합체육시설업 + 스키장 + 수영장업 + 빙상장업 + 무도학원업 + 무도장업 + 담배소매업 + 담배도매업 + 체력단련장업 + 체육도장업 + 상조업 + 민방위대피시설 + 민방위급수시설 + 물류창고업체 + 물류주선업국제 + 목욕장업 + 담배수입판매업체 + 행정사업 + 유료직업소개소 + 무료직업소개소 + 장례지도사교육기관 + 요양보호사교육기관 + 승강기제조및수입업체 + 승강기유지관리업체, 
               data = arr)
  return(stargazer(lmodel, type = "html"))
}

ui <- fluidPage(
  titlePanel("서울시 버스 승하차량 변화 분석"),
  tabsetPanel(type = "tabs",
              tabPanel("공간 차원 분석", sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput(inputId = "inputsigan",
                  label = "원하는 시간을 선택하세요",
                  min = 0,
                  max = 23,
                  value = 22),
      selectInput(inputId = "inputnal1",
                  label = "기준 달을 선택하세요",
                  choices = list("2019년 1월" = "201901","2019년 2월" = "201902",
                                 "2019년 3월" = "201903","2019년 4월" = "201904",
                                 "2019년 5월" = "201905","2019년 6월" = "201906",
                                 "2019년 7월" = "201907","2019년 8월" = "201908",
                                 "2019년 9월" = "201909","2019년 10월" = "201910",
                                 "2019년 11월" = "201911","2019년 12월" = "201912",
                                 "2020년 1월" = "202001","2020년 2월" = "202002",
                                 "2020년 3월" = "202003","2020년 4월" = "202004",
                                 "2020년 5월" = "202005","2020년 6월" = "202006",
                                 "2020년 7월" = "202007","2020년 8월" = "202008",
                                 "2020년 9월" = "202009","2020년 10월" = "202010",
                                 "2020년 11월" = "202011","2020년 12월" = "202012",
                                 "2021년 1월" = "202101","2021년 2월" = "202102"),
                  selected = "202001"),
      selectInput(inputId = "inputnal2",
                  label = "비교 대상 달을 선택하세요",
                  choices = list("2019년 1월" = "201901","2019년 2월" = "201902",
                                 "2019년 3월" = "201903","2019년 4월" = "201904",
                                 "2019년 5월" = "201905","2019년 6월" = "201906",
                                 "2019년 7월" = "201907","2019년 8월" = "201908",
                                 "2019년 9월" = "201909","2019년 10월" = "201910",
                                 "2019년 11월" = "201911","2019년 12월" = "201912",
                                 "2020년 1월" = "202001","2020년 2월" = "202002",
                                 "2020년 3월" = "202003","2020년 4월" = "202004",
                                 "2020년 5월" = "202005","2020년 6월" = "202006",
                                 "2020년 7월" = "202007","2020년 8월" = "202008",
                                 "2020년 9월" = "202009","2020년 10월" = "202010",
                                 "2020년 11월" = "202011","2020년 12월" = "202012",
                                 "2021년 1월" = "202101","2021년 2월" = "202102"),
                  selected = "202101"),
      selectInput(inputId = "inputinout",
                  label = "원하는 분류를 선택하세요",
                  choices = list("승차량분석" = "in",
                                 "하차량분석" = "out",
                                 "유입량분석" = "diff"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("정류장 단위 지도",
                           plotOutput(outputId = "plotmap", width = "1024", height = "768")
                           ),
                 tabPanel("행정동 단위 지도",
                           plotOutput(outputId = "plotmapdong", width = "1024", height = "768")
                         ),
                 tabPanel("정류장 단위 회귀분석",
                          uiOutput("regression")
                         ),
                 tabPanel("행정동 단위 회귀분석",
                          uiOutput("regressiondong")
                 )
                 )
             )
  )
  ),
  tabPanel("시간 차원 분석(정류장 단위)", sidebarLayout(
    sidebarPanel(width = 3,
                 sliderInput(inputId = "inputsigantime",
                             label = "원하는 시간을 선택하세요",
                             min = 0,
                             max = 23,
                             value = 22),
                 selectInput(inputId = "inputIDtime",
                             label = "분석 대상 정류장을 선택하세요",
                             choices = busstop$정류소명)
                 ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("표",
      tableOutput("IDtable")
                           ),
      tabPanel("그래프",
               plotOutput("IDplot")
               )
                 )
             )
    )
  ),
  tabPanel("시간 차원 분석(행정동 단위)", sidebarLayout(
    sidebarPanel(width = 3,
                 sliderInput(inputId = "inputsigantimedong",
                             label = "원하는 시간을 선택하세요",
                             min = 0,
                             max = 23,
                             value = 22),
                 selectInput(inputId = "inputIDtimedong",
                             label = "분석 대상 행정동을 선택하세요",
                             choices = dong_jwapyo$dong)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("표",
                           tableOutput("IDtabledong")
                  ),
                  tabPanel("그래프",
                           plotOutput("IDplotdong")
                  )
      )
    )
  )
  )
  )
  
)


server <- function(input, output) {
  
  sigan <- reactive({
    if(input$inputsigan < 10) {
      return(str_c("0",as.character(input$inputsigan)))
    } else {
      return(as.character(input$inputsigan))
    }
  })
  
  nal1 <- reactive({
    return(input$inputnal1)
  })
  
  nal2 <- reactive({
    return(input$inputnal2)
  })
  
  inout <- reactive({
    return(input$inputinout)
  })
  
  sigantime <- reactive({
    return(input$inputsigantime)
  })
  
  IDtime <- reactive({
    return(input$inputIDtime)
  })
  
  sigantimedong <- reactive({
    return(input$inputsigantimedong)
  })
  
  IDtimedong <- reactive({
    return(input$inputIDtimedong)
  })
  
  output$plotmap <- renderPlot({
    difflist <- find_difflist(nal1(), nal2(), sigan())
    if(inout() == "in") {
      return(gmap + geom_point(data = difflist, size = 3,
                               aes(x = X좌표, y = Y좌표, color = indiffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "승차량변화(%)")) 
    } else if(inout() == "out") {
      return(gmap + geom_point(data = difflist, size = 3,
                               aes(x = X좌표, y = Y좌표, color = outdiffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "하차량변화(%)"))
    } else {
      return(gmap + geom_point(data = difflist, size = 3,
                               aes(x = X좌표, y = Y좌표, color = diffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "유입량변화(%)"))
    }
  })
  
  output$plotmapdong <- renderPlot({
    difflist <- find_difflistdong(nal1(), nal2(), sigan())
    if(inout() == "in") {
      return(gmap + geom_point(data = difflist, size = 10,
                               aes(x = x, y = y, color = indiffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "승차량변화(%)")) 
    } else if(inout() == "out") {
      return(gmap + geom_point(data = difflist, size = 10,
                               aes(x = x, y = y, color = outdiffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "하차량변화(%)"))
    } else {
      return(gmap + geom_point(data = difflist, size = 10,
                               aes(x = x, y = y, color = diffpercentage*100)) +
               scale_colour_gradient2(low = "red" , mid = "white" , high = "blue") +
               labs(color = "유입량변화(%)"))
    }
  })
  
  output$regression <- renderUI({
    return(HTML(IDlinear(nal1(), nal2(), sigan(), inout())))
  })
  
  output$regressiondong <- renderUI({
    return(HTML(donglinear(nal1(), nal2(), sigan(), inout())))
  })
  
  output$IDtable <- renderTable({
    ID <- busstop$ID[which(busstop$정류소명 == IDtime())]
    timebus <- sigantime()
    IDtimelist <- get_busIDtime(ID, timebus)
    return(IDtimelist)
  })
  
  output$IDplot <- renderPlot({
    ID <- busstop$ID[which(busstop$정류소명 == IDtime())]
    timebus <- sigantime()
    IDtimelist <- get_busIDtime(ID, timebus)
    p1 <- ggplot(IDtimelist, aes(x=ym(년월), y=승차량)) +
      geom_line()
    p2 <- ggplot(IDtimelist, aes(x=ym(년월), y=하차량)) +
      geom_line()
    p3 <- ggplot(IDtimelist, aes(x=ym(년월), y=유입량)) +
      geom_line()
    p4 <- ggplot(corona, aes(x=ym(월), y=서울확진자)) +
      geom_line()
    return(grid.arrange(p1, p2, p3, p4, nrow=2))
  })
  
  output$IDtabledong <- renderTable({
    ID <- IDtimedong()
    timebus <- sigantimedong()
    IDtimelist <- get_busIDtime(ID, timebus)
    return(IDtimelist)
  })
  
  output$IDplotdong <- renderPlot({
    ID <- IDtimedong()
    timebus <- sigantimedong()
    IDtimelist <- get_busIDtime(ID, timebus)
    p1 <- ggplot(IDtimelist, aes(x=ym(년월), y=승차량)) +
      geom_line()
    p2 <- ggplot(IDtimelist, aes(x=ym(년월), y=하차량)) +
      geom_line()
    p3 <- ggplot(IDtimelist, aes(x=ym(년월), y=유입량)) +
      geom_line()
    p4 <- ggplot(corona, aes(x=ym(월), y=서울확진자)) +
      geom_line()
    return(grid.arrange(p1, p2, p3, p4, nrow=2))
  })
}

shinyApp(ui = ui, server = server)