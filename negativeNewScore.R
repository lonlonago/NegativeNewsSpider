# 从国搜网、中搜网抓取公司舆论数据,并使用关键负面词汇进行打分

library('rvest')
library('RCurl')


setwd("/users/lon/bonddata")
# 用来获取公司名称和代码
fina_data = read.table("fina2.txt",stringsAsFactors = F,sep = ";",fill = T,fileEncoding = 'utf-8',header = T,encoding = 'utf-8')
#fina_data = fina_data[fina_data$overdue==1,]

#如果已经抓取好了舆论数据，直接 load 就可以
load('resultDat.RData')    # 不用resultDat = 。。。直接 load 即可
load('scoreMatrix.RData')  # 不用resultDat = 。。。直接 load 即可

resultDat2 = list()
scoreMatrix2 = matrix(nrow = dim(fina_data)[1],ncol = 3)

for (i in 1:dim(fina_data)[1]){
  compName = fina_data$S_INFO_COMPNAME[i]
  compCode = fina_data$S_INFO_COMPCODE[i]
  if (compName=='' | is.na(compName)) {
    print('公司名字为空。')
    next()
  }
  if(compCode %in% names(resultDat2)){
    print('已经存在该公司数据。')
    next()
  }
  print(c('正在抓取公司负面数据:',compName))
  
  dat = 'Error?'
  while (startsWith(dat,'Error')) {
    dat=tryCatch( getdata(compName,startTime = '20160101',endTime = '20170101'),error=function(e) {paste("Error",conditionMessage(e),"\n\n")})
    Sys.sleep(2)
    print(dat)
  }
  #dat = getdata(compName)  ,startTime = '20160101',endTime = '20170101'
  riskValue = riskScore(compName, dat)
  scoreMatrix2[i,] = c(compCode,compName,riskValue)
  resultDat2[[compCode]] = dat   # 妈的，列表赋值要这样，不然只有第一个值进去了
  
  save(resultDat2,file = 'resultDat2.RData')
  save(scoreMatrix2,file = 'scoreMatrix2.RData')
}

#把上次没有抓取成功的公司删除，再抓取一遍
temp = which(resultDat=='被阻挡了16次，暂时没有搜索结果')
if(length(temp)>0){  resultDat = resultDat[-temp]   } 
length(resultDat)




myheader=c(
  "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "Accept-Language"="zh-CN,zh;q=0.8,en;q=0.6"
  # ,
  # "Connection"="keep-alive"  ,
  # "Host"="news.chinaso.com",
  # "Accept-Encoding"="gzip, deflate, sdch",
  # "Referer"="http://news.chinaso.com/"
  
)

# 
# pars=list(
#   username="394467238@qq.com",
#   password=" ",
#   lt="LT-765060-sciwUEKe9MAY6A0j0XSimsdAHBNT5J",
#   execution="e6s1",
#   #_eventId="submit",
#   submit="登 录"
# )

myheader2=c(
  "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"
)

# 
# getdata <- function( compName ){
#   
#   if (endsWith(compName,'股份有限公司')) {  #获取公司简称
#     compSName = substr( compName,1,nchar(compName)-6)
#   } else if ( endsWith(compName,'有限公司') ) {
#     compSName = substr( compName,1,nchar(compName)-4)
#   } else {
#     compSName = compName
#   }
#   
#   result = c()
#   n = 1
#   for (j in 1:10){ 
#     url <- paste('http://news.chinaso.com/newssearch.htm?q=',
#                  compSName,'&page=',j,  '&startTime=20150102&endTime=20160101',sep = '')
#     curl <- getCurlHandle()
#     tempP = getURL(url,httpheader=myheader, curl = curl,cookie=cookie) 
#     # (getCurlInfo(curl)$response.code!='200') |
#     while(  (tempP %>% read_html() %>% html_nodes("h1") %>% html_text() %in% c("Unauthorized ...","Web site unauthorized...")) ){
#       print(c(n,'resCode',getCurlInfo(curl)$response.code,'IP',getCurlInfo(curl)$primary.ip,'被阻挡了>换 IP 中'))  
#       # (tempP %>% read_html() %>% html_nodes("title") %>% html_text() == "您访问出错了！")
#       n = n + 1
#       tempProxy = getProxy(n)
#       proxy = tempProxy[2]
#       n = as.numeric( tempProxy[1] )
#       curl <- getCurlHandle(proxy=proxy)  #proxy=proxy,    timeout=5,
#       tempP = getURL(url,httpheader=myheader,  curl = curl,cookie=cookie)
#       
#     }
#     page = tempP %>% read_html()
#     
#     if(length(page %>% html_nodes("div.noresult") %>% html_text())>0){
#       if(substr(page %>% html_nodes("div.noresult") %>% html_text(),1,2) =='抱歉') { 
#         print('该公司在该时间段内没有数据！')  # 没有结果数据也可能导致出错
#         return('no result')
#        }       
#     } 
#     
#     if(j==1){ # 不用每次都去获取结果页数，第一次获取了就好
#       pageInfo = page %>% html_nodes("div.toolTab_xgxwts") %>% html_text()
#       index = regexpr("[0-9]+", pageInfo )              #toolTab_xgxwts
#       resultLength = attr(index,"match.length")
#       pageTotal = as.numeric( substr(pageInfo,index,index+resultLength-1) )
#       nPage = floor(pageTotal/15)  #获取结果页数
#     }
#     
#     resultHeader = page%>% html_nodes("li.reItem") %>% html_nodes("h2")%>%html_text()
#     resultContext = page%>% html_nodes("li.reItem") %>% html_nodes("div.clearfix")%>%html_text(trim=T)
#     dat = paste(resultHeader,'--CUT LINE--',resultContext) #把标题和内容合并
#     result = append(result,dat)
#     Sys.sleep(1)
#     
#     if (j >= nPage){ 
#       return(result)
#     }
#   }
# }
# 




getdata <- function( compName ,startTime="20150102", endTime='20160101'){

  if (endsWith(compName,'股份有限公司')) {  #获取公司简称
    compSName = substr( compName,1,nchar(compName)-6)
  } else if ( endsWith(compName,'有限公司') ) {
    compSName = substr( compName,1,nchar(compName)-4)
  } else if ( endsWith(compName,'公司') ) {
    compSName = substr( compName,1,nchar(compName)-2)
  } else {
    compSName = compName
  }

  result = c()
  stopFlag = 0
  url <- paste('http://news.chinaso.com/newssearch.htm?q=',
               compSName,'&page=1&startTime=',startTime,'&endTime=',endTime,sep = '')
  temp = getURL(url,httpheader=myheader) %>% read_html()
  
  
  if(length( temp %>% html_nodes("div.noresult") %>% html_text() )>0){
    if(substr(temp %>% html_nodes("div.noresult") %>% html_text(),1,2) =='抱歉') { 
      print('该公司在该时间段内没有数据！')  # 没有结果数据也可能导致出错
      return('no result')
    }       
  } 
  
  pageInfo = temp %>% html_nodes("div.toolTab_xgxwts") %>% html_text()
  sleepSec = 34
  count = 1
  while(length(pageInfo)==0 & count<=15) {#& count<=5
    stopFlag = 1
    sleepSec = sleepSec+1
    count = count + 1 # 最多等待15次
    Sys.sleep(sleepSec)
    cat(c('被阻挡了，休息',sleepSec,'秒吧。'))
    
    d = basicHeaderGatherer()
    curl1 <- getCurlHandle(cookiejar="cookiefile.txt",cookiefile='cookiefile.txt')
    
    temp1 = getURL(url,curl = curl1,httpheader=myheader,followlocation = T,headerfunction=d$update) 
    temp = temp1 %>% read_html() #%>% html_nodes("div.toolTab_xgxwts") %>% html_text()
    #print(temp1)
    print(d$value()['status'])
    pageInfo = temp %>% html_nodes("div.toolTab_xgxwts") %>% html_text()
   # rm(curl1)
    #gc()
    
    if(length( temp %>% html_nodes("div.noresult") %>% html_text() )>0){
      if(substr(temp %>% html_nodes("div.noresult") %>% html_text(),1,2) =='抱歉') { 
        print('该公司在该时间段内没有数据！')  # 没有结果数据也可能导致出错
        return('no result')
      }       
    }
  }
  
  if (count==16) {
    #print('采用中搜结果。')
    return('被阻挡了16次，暂时没有搜索结果')
  }
  
  resultHeader = temp%>% html_nodes("li.reItem") %>% html_nodes("h2")%>%html_text()
  resultContext = temp%>% html_nodes("li.reItem") %>% html_nodes("div.clearfix")%>%html_text(trim=T)
  dat = paste(resultHeader,'--CUT LINE--',resultContext) #把标题和内容合并
  result = append(result,dat)
  
  
  index = regexpr("[0-9]+", pageInfo )              #toolTab_xgxwts
  resultLength = attr(index,"match.length")
  pageTotal = as.numeric( substr(pageInfo,index,index+resultLength-1) )
  nPage = floor(pageTotal/15)  #获取结果页数
  if (nPage==0 | nPage==1) {
    print(c('只有1页数据',result))
    return(result)
  }
  if (nPage>10) nPage=10 #最多抓取10页内容

  if(stopFlag==1){
    curl2 <- getCurlHandle(cookiejar="cookiefile.txt",cookiefile='cookiefile.txt')
  } else{
    curl2 = getCurlHandle()
  }
  
  for (j in 2:nPage){
    url <- paste('http://news.chinaso.com/newssearch.htm?q=',
                 compSName,'&page=',j,'startTime=',startTime,'&endTime=',endTime,sep = '')

    page = getURL(url,httpheader=myheader,curl = curl2) %>% read_html()
    resultHeader = page%>% html_nodes("li.reItem") %>% html_nodes("h2")%>%html_text()
    resultContext = page%>% html_nodes("li.reItem") %>% html_nodes("div.clearfix")%>%html_text(trim=T)
    dat = paste(resultHeader,'--CUT LINE--',resultContext) #把标题和内容合并
    result = append(result,dat)
    Sys.sleep(1)
  }
  rm(curl1)
  rm(curl2)
  gc()
  cat(c('有多页数据:',nPage,'页。'))  #,result
  
  result
}

  









#预警评分关键词  第一行是几列，后面的所有的就是几列。。。蛋疼
# keyword = read.table("keyword.txt",stringsAsFactors = F,header = F,sep = ';',fill = T,fileEncoding = 'utf-8',encoding = 'utf-8')
# keyWordCount = rep(0,dim(keyword)[1])
# names(keyWordCount) = paste(keyword[,1],keyword[,2],sep = '')


#使用keyword.txt 
riskScore = function( compName, dat ){
  # compName 公司名字的字符串
  # dat  公司搜索结果的向量集合
  #print(keyWordCount)

  #预警评分关键词  第一行是几列，后面的所有的就是几列。。。蛋疼
  keyword = read.table("keyword.txt",stringsAsFactors = F,header = F,sep = ';',fill = T,fileEncoding = 'utf-8',encoding = 'utf-8')
  
  
  if (endsWith(compName,'股份有限公司')) {  #获取公司简称
    compSName = substr( compName,1,nchar(compName)-6)
  } else if ( endsWith(compName,'有限公司') ) {
    compSName = substr( compName,1,nchar(compName)-4)
  } else {
    compSName = compName
  }
  Score = 0
  RedScore = 0
  YellowScore = 0
  if(length(dat[[1]])==0) {return(0)}
  for(i in 1:length(dat[[1]])){
    keyWordAppear = 0
    if(grepl(compSName,dat[[1]][i])){#确认包含该公司名字
      for(j in 1:dim(keyword)[1]){
       
        
        if(grepl(keyword[j,1],dat[[1]][i]) & grepl(keyword[j,2],dat[[1]][i])) {
          #print(c(keyword[j,1],dat[[1]][i],i,j ))
          keyWordAppear = keyWordAppear + 1   #如果有两个关键词是不是要加2分？ 存在改进空间。。。
          #keyWordCount[paste(keyword[j,1],keyword[j,2],sep = '')] = keyWordCount[paste(keyword[j,1],keyword[j,2],sep = '')] +1
          #print( keyWordCount2[paste(keyword[j,1],keyword[j,2],sep = '')])
        }
      }
    }
    if(keyWordAppear>0) { Score = Score + 1 }
  }
  Score
}



#使用keyword2.txt
riskScore2 = function( compName, dat ){
  # compName 公司名字的字符串
  # dat  公司搜索结果的向量集合
  keyword = read.table("keyword2.txt",stringsAsFactors = F,header = F,sep = ';',fill = T,fileEncoding = 'utf-8',encoding = 'utf-8')
  
  
  if (endsWith(compName,'股份有限公司')) {  #获取公司简称
    compSName = substr( compName,1,nchar(compName)-6)
  } else if ( endsWith(compName,'有限公司') ) {
    compSName = substr( compName,1,nchar(compName)-4)
  } else {
    compSName = compName
  }
  Score = 0
  RedScore = 0
  YellowScore = 0

  
  if(length(dat[[1]])==0) {return(0)}
  for(i in 1:length(dat[[1]])){
    keyWordAppear = 0
    RedFlag = 0
    YellowFlag = 0
    if(grepl(compSName,dat[[1]][i])){#确认包含该公司名字

      for(j in 1:dim(keyword)[1]){
        
        
        if(grepl(keyword[j,2],dat[[1]][i]) & grepl(keyword[j,3],dat[[1]][i])) {
          #print(c(keyword[j,2],dat[[1]][i],i,j ))
          keyWordAppear = keyWordAppear + 1   #如果有两个关键词是不是要加2分？ 存在改进空间。。。
          keyWordCount[paste(keyword[j,2],keyword[j,3],sep = '')] = keyWordCount[paste(keyword[j,2],keyword[j,3],sep = '')] +1
          if(keyword[j,1]=='红') { RedFlag = 1 }
          if(keyword[j,1]=='黄') { YellowFlag = 1 }
          
          
        }
      }
    }
    if(keyWordAppear>0) { Score = Score + 1 }
    if(RedFlag==1) { RedScore = RedScore + 1 }  # 红色预警必加分；  这里的预警颜色有可能是重合的，但是也有可能是不重合
    if(YellowFlag==1 & RedFlag==0) { YellowScore = YellowScore + 1 }  #只要这一条消息有红色关键词,他就是红色的
  }
  
  c(Score,RedScore,YellowScore)
}





#把中文转化为 GBK 编码的 URL 格式
urlCoding = function(indexName){  
  tempCode  = iconv(indexName,to = 'GBK',toRaw = T)
  urlcodeTemp = ''
  for(i1 in 1:length(tempCode[[1]])){
    urlcodeTemp = paste(urlcodeTemp,'%',toupper(tempCode[[1]][i1]),sep = '')
  }
  urlcodeTemp
}


#从中搜网抓取新闻，只抓取2页
getDataChinaso <- function( compName ){ 
  
  if (endsWith(compName,'股份有限公司')) {  #获取公司简称
    compSName = substr( compName,1,nchar(compName)-6)
  } else if ( endsWith(compName,'有限公司') ) {
    compSName = substr( compName,1,nchar(compName)-4)
  } else {
    compSName = compName
  }
  result = c()
  
  url <- paste('http://zixun.zhongsou.com/n?w=',
               urlCoding(compSName),
               #'&utf=2&tm=5&p1=2015-01-01&p2=2015-12-31&b=1',sep = '')
         '&k=&netid=&aid=&y=4&utf=2&v=%D7%CA%D1%B6&sc=&oi=&tm=5&st=&p1=2015-01-01&p2=2015-12-31&lw=',sep = '')
  temp = url %>% read_html() 
  
  pageInfo = as.numeric( temp %>% html_nodes("div.bgnr_s") %>% html_nodes("b")%>%  html_text() )
  sleepSec = 15
  while(length(pageInfo)==0) {
    sleepSec = sleepSec+1
    Sys.sleep(sleepSec)
    print(c('被阻挡了，休息',sleepSec,'秒吧'),quote = F)
    temp = url %>% read_html() #%>% html_nodes("div.toolTab_xgxwts") %>% html_text()
    pageInfo = as.numeric( temp %>% html_nodes("div.bgnr_s") %>% html_nodes("b")%>%  html_text() )
  }
  
  resultHeader = temp%>% html_nodes("li.clearfix") %>% html_nodes("h3.h3-zx")%>%html_text(trim=T)
  resultContext = temp%>% html_nodes("li.clearfix") %>% html_nodes("div.img-info")%>%html_text(trim=T)
  dat = paste(resultHeader,'--CUT LINE--',resultContext) #把标题和内容合并
  result = append(result,dat)
  
  #读取第2页内容
  # url <- paste('http://zixun.zhongsou.com/n?w=',
  #              urlCoding(compSName),
  #              '&utf=2&tm=5&p1=2015-01-01&p2=2015-12-31&b=2',sep = '')
  # temp = url %>% read_html() 
  # resultHeader = temp%>% html_nodes("li.clearfix") %>% html_nodes("h3.h3-zx")%>%html_text(trim=T)
  # resultContext = temp%>% html_nodes("li.clearfix") %>% html_nodes("div.img-info")%>%html_text(trim=T)
  # dat = paste(resultHeader,'--CUT LINE--',resultContext) #把标题和内容合并
  # result = append(result,dat)
  
  return(result)
}


# 获取代理 IP
getProxy = function( index = 1 ){
  # 如果之前没有proxy，或者生成时间超过了60分钟，就重新抓取
  if(is.na(file.info('proxys.txt')$ctime) | (Sys.time() - file.info('proxys.txt')$ctime)>1  ){
    print('重新抓取代理中>>>>>')
    proxys = c()
    proxyUrl = 'http://www.xicidaili.com/nn/1'
    proxyPage=proxyUrl %>% read_html() %>% html_nodes("tr") 
    for(i in 2:length(proxyPage)){
      IP = proxyPage[i] %>% html_nodes("td") %>% .[2] %>% html_text(trim=T)
      port = proxyPage[i] %>% html_nodes("td") %>% .[3] %>% html_text(trim=T)
      proxy = paste(IP,port,sep=':')
      proxys = append(proxys,proxy)
    }
    write.csv(proxys,'proxys.txt')
  } else {
    proxys = read.csv('proxys.txt',stringsAsFactors = F,header = T)[,2]
  }
  
  # 检测该代理是否可用，不可以就换下一个 http://ip.chinaz.com/getip.aspx http://www.xiaopian.com
  proxy = proxys[index]
  curl <- getCurlHandle(proxy=proxy)
  tempTry = tryCatch(getURL('http://www.xiaopian.com',httpheader=myheader2,timeout=2,curl = curl) ,error=function(e) {paste("Error",conditionMessage(e),"\n\n")})
  
  while( startsWith(tempTry,'Error') ) {
    #!url.exists('http://ip.chinaz.com/getip.aspx',httpheader=myheader2, .opts =list(CURLOPT_TIMEOUT=3)  ,curl = curl)
    index = index+1
    proxy = proxys[index]
    curl <- getCurlHandle(proxy=proxy)
    tempTry = tryCatch(getURL('http://www.xiaopian.com',httpheader=myheader2,timeout=2,curl = curl) ,error=function(e) {paste("Error",conditionMessage(e),"\n\n")})
  }
  print(c(index,proxys[index],'is avilable'))
  return(c(index,proxys[index]))
}

#getProxy(7)  # timeout =3  ,CONNECTTIMEOUT=3,

