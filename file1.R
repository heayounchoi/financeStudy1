# 패키지 설치
pkg = c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)}

# API로 데이터 불러오기 
url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl = read.csv(url.aapl)

head(data.aapl)

# 티커로 데이터 불러오기. auto.assign = 변수명 자동 지정
library(quantmod)
data = getSymbols('AAPL',
                  from = '2000-01-01', to = '2023-02-22',
                  auto.assign = FALSE)

tail(data)

# 배당이 반영된 수정주가로 시계열 그래프 그리기
chart_Series(Ad(data))

# 여러 종목 주가 불러오기
ticker = c('META', 'NVDA') 
getSymbols(ticker)

head(META)
head(NVDA)

# 국내 종목 주가 불러오기 (KS = KOSPI, KQ = KOSDAQ)
getSymbols('005930.KS',
           from = '2000-01-01', to = '2023-02-22')

tail(Ad(`005930.KS`))

# 국내 종목의 경우 수정주가에 오류가 발생하는 경우가 많으니 단순 종가 사용 권장
tail(Cl(`005930.KS`))

# FRED 데이터 불러오기
getSymbols('DGS10', src='FRED')

chart_Series(DGS10)

getSymbols('DEXKOUS', src='FRED')

tail(DEXKOUS)

# 크롤링(GET)
library(rvest)
library(httr)

url = 'https://finance.naver.com/news/news_list.naver?mode=LSS2D&section_id=101&section_id2=258'
data = GET(url)

data_title = data %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_attr('title') 

print(data_title)

# 크롤링(POST)
Sys.setlocale("LC_ALL", "English")

url = 'https://kind.krx.co.kr/disclosure/todaydisclosure.do'
data = POST(url, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'Y',
                selDate = '2023-02-23'
              ))

data = read_html(data) %>%
  html_table() %>%
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")

print(head(data))

# 크롤링 예제
library(stringr)

i = 0 # 코스피(1은 코스닥)
j = 1 # 첫번째 페이지
data = list()

for (i in 0:1) {
  ticker = list()
  
  url = paste0('https://finance.naver.com/sise/',
             'sise_market_sum.naver?sosok=',i,'&page=1')
  down_table = GET(url)

  navi.final = read_html(down_table, encoding = 'EUC-KR') %>%
    html_nodes(., '.pgRR') %>%
    html_nodes(., 'a') %>%
    html_attr(., 'href')

  navi.final = navi.final %>%
    strsplit(., '=') %>%
    unlist() %>%
    tail(., 1) %>%
    as.numeric()

  for (j in 1:navi.final) {

    url = paste0(
      'https://finance.naver.com/sise/',
      'sise_market_sum.naver?sosok=',i,'&page=',j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL", "English")

    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table()
    table = table[[2]]

    Sys.setlocale("LC_ALL", "Korean")

    table[, ncol(table)] = NULL
    table = na.omit(table)

    symbol = read_html(down_table, encoding = 'EUC-KR') %>%
      html_nodes(., '.tltle') %>%
      html_attr(., 'href')

    symbol = sapply(symbol, function(x) {
      str_sub(x, -6, -1) 
    })

    symbol = unique(symbol)

    table$N = symbol

    colnames(table)[1] = '종목코드'
    rownames(table) = NULL

    ticker[[j]] = table

    Sys.sleep(0.5)
  }

  ticker = do.call(rbind, ticker)

  data[[i + 1]] = ticker
}

data = do.call(rbind, data)

