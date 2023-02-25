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

library(httr)
library(rvest)
library(readr)

# 네이버 증권에서 최근 영업일 가져오기
url = 'https://finance.naver.com/sise/sise_deposit.naver'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_0"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

# 한국거래소 산업별 현황 크롤링
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'

gen_otp_data = list(
  mktId = 'STK',
  trdDd = biz_day,
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'

down_sector_KS = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

gen_otp_data = list(
  mktId = 'KSQ', 
  trdDd = biz_day,
  money = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_sector_KQ = POST(down_url, query = list(code = otp),
                      add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

down_sector = rbind(down_sector_KS, down_sector_KQ)

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

# 한국거래소 개별지표 크롤링
gen_otp_url =
  'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'

gen_otp_data = list(
  searchType = '1',
  mktId = 'ALL',
  trdDd = biz_day,
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT03501'
)

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'

down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')

# 한국거래소 산업별 현황, 개별 지표 데이터 합치기
down_sector = read.csv('data/krx_sector.csv', row.names = 1,
                       stringsAsFactors = FALSE)
down_ind = read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)

intersect(names(down_sector), names(down_ind))

setdiff(down_sector[, '종목명'], down_ind[ ,'종목명'])

KOR_ticker = merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE # TRUE = 합집합 반환, FALSE = 교집합 반환
)

KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액']), ]

KOR_ticker[grepl('스팩', KOR_ticker[, '종목명']), '종목명']  

KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) != 0, '종목명']

KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]

rownames(KOR_ticker) = NULL

write.csv(KOR_ticker, 'data/KOR_ticker.csv')

# WICS 기준 섹터정보 크롤링
library(jsonlite)

url = 'https://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20230223&sec_cd=G10'
data = fromJSON(url)

lapply(data, head)

sector_code = data$sector$SEC_CD

data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector = do.call(rbind, data_sector)

write.csv(data_sector, 'data/KOR_sector.csv')

# 개별종목 수정주가 크롤링
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

i = 1
name = KOR_ticker$'종목코드'[i]

price = xts(NA, order.by = Sys.Date())

library(lubridate)

from = (Sys.Date() - years(3)) %>% str_remove_all('-')
to = Sys.Date() %>% str_remove_all('-')

url = paste0('https://api.finance.naver.com/siseJson.naver?symbol=', name,
             '&requestType=1&startTime=', from, '&endTime=', to, '&timeframe=day')

data = GET(url)

data_html = data %>% read_html %>%
  html_text() %>%
  read_csv()

library(timetk)

price = data_html[c(1, 5)]

colnames(price) = (c('Date', 'Price'))

price = na.omit(price)

price$Date = parse_number(price$Date)

price$Date = ymd(price$Date)

price = tk_xts(price, date_var = Date)

write.csv(data.frame(price),
          paste0('data/KOR_price/', name, '_price.csv'))
