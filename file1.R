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
