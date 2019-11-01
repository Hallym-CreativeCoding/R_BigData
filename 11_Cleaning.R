#                                     #
#   데이터 정제                       #
#     - 결측값과 이상값 확인          #
#     - 결측값 제거 및 대체           #
#     - 이상값 제거 및 NA로 변환      #
#                                     #          

## dplyr, ggplot2 패키지 불러오기
library(dplyr)
library(ggplot2)

##--------------------------------------#
## 외부 데이터 읽어오기                ##
##--------------------------------------#

# 외부파일 읽어오기, 문자열은 문자형으로 읽어오기
dfr <- read.csv("na_exam.csv", stringsAsFactors = F)
dfr

# na.strings 옵션 : 특정 문자를 R에서 인식하는 결측값 NA 변경
#                   숫자는 NA  문자는 <NA> 
dfr <- read.csv("na_exam.csv", stringsAsFactors=F, 
                na.strings= c(" ", "", NA))
head(dfr)

# 데이터구조 확인 : 20개 관측값, 7개 변수, 각 변수의 자료형 확인
str(dfr)      


##---------------------------------#
##  결측값 및 이상값 확인         ####
##---------------------------------#

df <- dfr     # 데이터프레임 복사


## 결측값 확인
#   - is.na() 함수 사용하여 결측값 존재여부 확인 (TRUE/FALSE로 표시)
#   - 요약 통계 확인 summary() 


mean(df$math)     # 결측값이 있는 경우 정상적으로 연산되지 않는다.

summary(df)       # 요약통계량을 통해 결측값 확인 가능
summary(as.factor(df$class))

is.na(df)         # TRUE로 표시된 값이 결측값

table(is.na(df))      # 데이터프레임 전체에서 결측값의 갯수 
table(is.na(df$math)) # 특정 열에서 결측값의 갯수 확인
     
#colSums(is.na(df))  # 열별 합계, 합계가 0보다 큰 열 : 결측치 존재


## 이상값 확인
#   - 요약통계 확인
#   - 빈도표, 히스토그램, 상자그림으로 값의 분포 확인 

summary(df)
summary(df$gender)  # 성별빈도가 나오지 않는다. 
  # 확인 방법은?  형변환 필요 


nrow(df)          # df의 행수는 20
table(df$gender)  # 이상값은 확인 가능
table(df$class)   # 결측값은 확인 불가능(결측값 제외된 빈도표)

quantile(df$math)   # 결측값으로 인해 Error 발생
quantile(df$math, na.rm =T) # 결측값 제외하고 함수 실행

# 히스토그램으로 데이터의 분포 확인  
# - hist() : 연속형 변수 즉 숫자인 경우만 사용 가능 
# - qplot() : 연속형이나 범주형(문자나 팩터) 모두 사용가능

hist(df$math)               # math 변수값에 대한 값의 분포
hist(df$math, breaks=20)    # math값의 구간을 20개로 나눔       
hist(df$math, breaks=20, xlim = c(0, 100))   # 축 범위 지정       

hist(df$eng)
hist(df$com)

qplot(df$class)    

boxplot(df$math)          # math 변수값  상자그림으로 확인 
boxplot.stats(df$math)    #이상값 $out


##--------------------------------------------------#
##  결측값  제거                                   ####
##   (1) 결측값이 없는 행만 추출 : filter() 사용    #
##   (2) 여러 변수에 모두 결측값이 없는 행 추출     #
##   (3) 결측값이 있는 모든 행 제거                 # 
##   (*) 함수의 결측값 제외 기능                    #
##--------------------------------------------------#

df <- dfr     # 데이터프레임 복사

## (1) 특정 변수에서 결측값이 없는 행만 추출
#      - dplyr패키지의 filter(), ! 연산자,  %>% 연산자 사용

df_nomiss1 <- df %>% filter(!is.na(math)) 
table(is.na(df_nomiss1$math))

## (2) 여러 변수에 모두 결측값이 없는 행 추출
#      - filter()와 & 연산자 

df_nomiss2 <- df %>% filter( !is.na(math) & !is.na(class) )  
table(is.na(df_nomiss2$class))


## (3) 결측값이 있는 모든 행 제거 
#       - na.omit()  : 분석에 필요한 행까지 손실되는 단점  

df_nomiss3 <- na.omit(df)
table(is.na(df_nomiss3))

## (*) 함수의 결측값 제외 기능 
#   - mean()과 같은 계산함수에서 결측값 제외하고 연산 : na.rm=TRUE 사용 
#   - 모든 함수가 na.rm 옵션을 지원하는 것은 아님

mean(df$eng, na.rm=T)
sum(df$eng, na.rm=T)

##-----------------------------------------------------------#
##  결측값  대체                                             ####
##   - 결측값 제거시 데이터 손실로 분석왜곡 발생 할 수 있음  #
##   - 다른 값으로 대체하여 왜곡 문제 보완                   #
##   - 평균값, 최빈값과 같은 대표값 사용, 예측값 계산        #
##-----------------------------------------------------------# 

df <- dfr     # 데이터프레임 복사
str(df)

table(is.na(df$math))

## 변수 math의 결측값을 평균값으로 대체

# 먼저 math의 평균값 계산
mean(df$math) 
mean_math <- mean(df$math, na.rm=T) 

# math 변수값을 새로운 값으로 덮어쓰기

# ① mutate() 를 이용하는 방법 
df <- df %>% mutate(math=ifelse(is.na(math), mean_math, math))

# ② 기존 변수 이용하는 방법
df$math <- ifelse(is.na(df$math), mean_math, df$math)


##--------------------------------------#
##  이상값  처리                       ####
##    (1) 이상값 제거                   #
##    (2) 결측값 NA 로 변경             #
##--------------------------------------#

df <- dfr     # 데이터프레임 복사

# 이상값 여부 확인하는 3가지 방법
table(df$gender)                  # 또는
summary( as.factor(df$gender) )   # 또는 
df %>%  distinct(df$gender) 


## (1) 이상값 제거 : filter() 이용
df <- df %>% filter( gender=='M' | gender=='F')
table(df$gender)


## (2) 결측값(NA)으로 변경 : ifelse() 이용 

df <- dfr         # 데이터프레임 복사
table(df$gender)

# gender 변수의 값이 M이나 F가 아니면 NA 처리

# ① mutate() 를 이용
df <- df %>% mutate(gender=ifelse(gender=='M' | gender=='F', gender, NA))

# ② 기존 변수 이용
df$gender <- ifelse(df$gender=='M' | df$gender=='F', df$gender, NA)


