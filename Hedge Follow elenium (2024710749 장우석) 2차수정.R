setwd("D:/wooseok/0.Quantitative Applied Economics/2.경제데이터분석 (최재성 교수님)")

library(dplyr)
library(stringr)
library(httr)
library(XML)
library(jsonlite)
library(readxl)
library(writexl)
library(RSelenium)
library(rvest)

# Table 1 (펀드매니져별 핵심 정보)
summary <- res %>% 
  html_table() %>% 
  .[[1]]
summary2<-summary[,2:6]

#Could be sovled this way!
#for (fundname in hedgefund.list) {
#  tab <- fundinfo(fundname)
#  Stack <- rbind(Stack, tab)
#  Sys.sleep(3)
#}


#for (i in 1:length(hedgefund.list)) {
#  tab <- fundinfo(hedgefund.list[i])
#  Stack <- rbind(Stack, tab)
#  Sys.sleep(3)
}

#TOP PEF 이름 및 핵심운용역 이름
fundmanager.list <- c("Warren Buffett", "Carl Icahn", "Michael Burry","George Soros","Bill Ackman","John Paulson","David Tepper", "Daniel Loeb")
hedgefund.list <- c("Berkshire+Hathaway", "Icahn+Enterprises", "Scion+Asset+Management","Soros+Fund+Management","Pershing+Square+Capital+Management","Paulson","Appaloosa", "Third+Point")


rD <- rsDriver(browser="firefox", port=4835L, chromever=NULL)
remDr <- rD$client




fundinfo <- function(hf) {
  
  URL <- str_c("https://hedgefollow.com/funds/", hf)
  remDr$navigate(URL)
  remDr$refresh()
  
    # Get HTML source
    txt <- remDr$getPageSource()[[1]]
    res <- read_html(txt)
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(1)"
  ticker<- res %>% 
      html_nodes(pattern) %>% 
      html_text()
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(2)"
  companyname<- res %>% 
    html_nodes(pattern) %>% 
    html_text() 
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(3)"
  portfoliopercentage <- res %>% 
    html_nodes(pattern) %>% 
    html_text() 
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(4)"
  sharesowned <- res %>% 
    html_nodes(pattern) %>% 
    html_text()

  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(5)"
  valueowned <- res %>% 
    html_nodes(pattern) %>% 
    html_text()
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(8) > div > span.cellSpan.hasTooltip > span"
  changeinshare <- res %>% 
    html_nodes(pattern) %>% 
    html_text()
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(8)"
  averagebuyprice <- res %>% 
    html_nodes(pattern) %>% 
    html_text()
  
  
  pattern <- "#fund_holdings_equities > tbody > tr > td:nth-child(10)"
  valuedate <- res %>% 
    html_nodes(pattern) %>% 
    html_text()
    
  rawsummary <- cbind(ticker, companyname, portfoliopercentage,sharesowned, valueowned,changeinshare,averagebuyprice,valuedate) %>% as.data.frame()
    
   return(rawsummary)
 
  }


Stack2 <- NULL
for (j in 1:8) {
  print(fundmanager.list[j])
  tab <- fundinfo(hedgefund.list[j])
  tab2 <- cbind(fundmanager.list[j], tab)
  Stack2 <- rbind(Stack2, tab2)
  Sys.sleep(3)}

  
  #복수펀드 뽑기 반복문 방법 #1
  #for (i in 1:length(hedgefund.list)) {
  #  tab <- fundinfo(hedgefund.list[i])
  #  Stack <- rbind(Stack, tab)
  #  Sys.sleep(3)

Stack2 %>% slice(-c(43,44,45,46,47,63,94,95,96,147,148,149,150,151,152,161,181,182,226,227,228,229,230,268,269,270,271)

value <- Stack$ownedvalue
Stack3 <- Stack2$valueowned %>% str_remove("M")
Stack3
#개별적으로 도출하기
  return1 <- fundinfo("Berkshire+Hathaway")
  return2 <- fundinfo("Icahn+Enterprises")  
  return3 <- fundinfo("Scion+Asset+ Management")
  return4 <- fundinfo("Soros+Fund+Management")
  return5 <- fundinfo("Pershing+Square+Capital+Management")
  return6 <- fundinfo("Pershing+Square+Capital+Management")
  return7 <- fundinfo("Paulson")
  return8 <- fundinfo("Appaloosa")

  write.csv(Stack,"HedgeFollow.csv")
  write.csv(Stack2,"HedgeFollow2.csv")
  
  
remDr$close()
rD$server$stop()
