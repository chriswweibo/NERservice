#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(readxl)
library(text2vec)
library(stringr)
library(magrittr)
library(data.table)
library(dplyr)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

   final <- reactive({
   
    datafile=input$file
     punctuation1="％|？|·|－|—|\\|--|\\{|\\},|，|。|：|（|）|\\(|\\)|%|、|:|\\.|/|;|；|\\*|\\?|√|\\[|\\]|【|】|\\+|＋| |　|<|>|=|＜|≤|≥|＞|×|\n|”|“"
     stopW1="大于|小于|早读|因此|参看|仅有|我科|至|侵至|少数|多数|并|并且|合并|同意|所以|均以|均为|均未|是否|需要|指出|不是|都是|是由|由于|我们|明显|不明显|读片|具体|少许|至少|少量|大量|至多|病人|这是|起源于|源于|此例|本例|科内|全科|集体|讨论|仔细|证据|级别|同时|具有|支持|分别|介于|主要|主要以|多个|证实|与|显示|及|有无|获益|可疑|疑|参阅|明确|进一步行|相应|重新|医师|或|及其|数目|为主|为|均为|鉴于|紧贴|累及|免疫组化|其他|意义|个数|相关|提示|注意|局限于|补做|不易|找到|需行|未查见|查见|以及|生产|产|明确|结果示|结果|原单位|的|市|但未|但|排除|未被|所做|难以|负责|我院|本院|鉴别|可以|可归为|如对|以上|归为|复习|来|提供|根据|病史|距离|距|无法|不能|确定|约|伴|本例|未检测到|检测到|追加|目前|未知|检出|考虑|符合|倾向|位于|均未见|未见到|未发现|发现|未见|可见到|可见|见到|见|另见|另送|建议|以协助诊断|建议行|正在行|正在|呈|病理科|依据|送检|待|请|参考|结合|协助|备注|补充报告|进一步|医院|其中|出现|除外|可能|联系|电话|人民|患者|加做|参见|上述|按照|大小|本次|必要时|情况|补充|主任|医生|阅片|会诊|随访|病例|应该|该|针对"
     combination1="[0-9,一,两,二,三,四,五,六,七,八,九,十,多]+[灶,月,枚,个,根,张,CM,cm,号]{1}|[0-9]{1,}号{0,1}[层,楼]{1}|[0-9]{4}[年,版]{1,2}|第[0-9,一,二,三,四,五,六,七,八,九,十]{1,2}[次,位,组,版]{0,1}|[0-9]{4}[年,版]{1,2}"
    
   withProgress(min=0, max=1, value=1,message = "加载文件",expr={
    NET=read_excel(datafile$datapath,col_names  = input$headerConfirm)
    #NET <- read_excel("E:/pathology/BeiZhong.xls",col_names =  input$headerConfirm)
     })
     
    prep_fun=function(x){
      pattern = paste(punctuation1,stopW1,combination1,input$punctuationM,input$stopWM,input$combinationM,sep="|")
      return(str_replace_all(x,pattern = pattern, replacement = "#")   %>% toupper())
    }
    
    it_train = itoken(NET$检查结论,ids = NET$病理号,preprocessor=prep_fun, tokenizer = char_tokenizer,progressbar =T)
    
    withProgress(min=0, max=1, value=1,message = "切词，计算词频",expr={
    vocab=create_vocabulary(it_train,ngram = c(ngram_min = 1L, ngram_max = input$ngram)) %>% 
      prune_vocabulary(term_count_min=2,doc_count_min =2)%>% 
      subset(grepl("^[且,在,是,经,型,与,及,于,有,行,性,状,示,～,_,-]{1}_|[0-9,也,是,与,不,无,示,～,_,-]{1}$",term)==F) %>%
      subset(grepl("[#,\\,和,以]",term)==F) %>%  # not containing # (stopword and punctuaton),
      subset(grepl("^[-,0-9]+_{1,}",term)==F) %>% # or only composed by digits,-,_
      subset(nchar(term)>1) 
    }
    )
    
    vocab=data.table(vocab)[,prob:=term_count/sum(term_count)][order(-nchar(term))]
    vocabp=data.table(vocab)[,prob:=doc_count/nrow(NET)]
    vocab_compressed=NULL
    vocabORI=nrow(vocab)
    
    withProgress(min=0, max=1,detail = "finishing", value = 0, message = "生成长实体词",expr={
    while (nrow(vocab)>1){
      candTerm=vocab$term[1]
      candCount=vocab$term_count[1]
      result=vocab[term_count<input$rate*candCount & sapply(term,grepl,candTerm)==T,,] 
      vocab=vocab[is.element(term,result$term)==F,,]
      vocab_compressed=c(vocab_compressed,result$term[1])
      incProgress(0.1, detail = paste("finishing", round(1-(nrow(vocab)/vocabORI),digits = 3)*100,"%"))
      setTxtProgressBar(txtProgressBar(min=0,max=1,style = 3),value =1-(nrow(vocab)/vocabORI))
      print(result$term[1])
    }
    })
    vocabp=vocabp[is.element(term,vocab_compressed)==T ,,][order(-nchar(term),term_count)]
    
    vocabp_deleted=NULL
    vocabpORI=nrow(vocabp)
    vocab_cand=vocabp
    
    withProgress(min=0, max=1, detail = "finishing", value = 0, message = "生成独立实体词",expr={
    while (nrow(vocab_cand)>1){
      candTerm=vocab_cand$term[1]
      candCount=vocab_cand$term_count[1]
      pattern=paste0(vocab_cand$term[-1],collapse = "|")
      result=str_replace_all(candTerm,pattern=pattern,replacement = "")
      if (result=="_") {
        vocabp_deleted=c(vocabp_deleted,candTerm)}
      else {
        vocabp_deleted=vocabp_deleted}
      vocab_cand=vocab_cand[-1,]
      incProgress(0.1, detail = paste("finishing", round(1-(nrow(vocab_cand)/vocabpORI),digits = 3)*100,"%"))
      setTxtProgressBar(txtProgressBar(min=0,max=1,style = 3),value =1-(nrow(vocab_cand)/vocabpORI))
      print(candTerm)
    }
    })
    vocabp=vocabp[is.element(term,vocabp_deleted)==F,,][order(-nchar(term),term_count)]
    mutualInfo=function(x){
     cand=vocabp$term[sapply(vocabp$term,grepl,x)==T & vocabp$term!=x]
      
      if (length(cand)==0) {return(0)}
      else {
        table=data.frame(cand,score=rep(0,length(cand)),stringsAsFactors = F)
        for ( i in 1 : length(cand)){
          spl=str_split(x,cand[i])[[1]] %>% str_replace_all("^_|_$","")
          spl=spl[nchar(spl)!=0]
          table[i,2]=vocabp$prob[vocabp$term==x]/prod(vocabp$prob[vocabp$term==spl],vocabp$prob[vocabp$term==cand[i]])
        }
        
        return(min(table$score))
      }
    }
    
    
    vocabp$score=rep(0,nrow(vocabp))
    
    withProgress(min=0, max=1, detail = "finishing", value = 0, message = "计算互信息",expr={
    for ( i in 1:nrow(vocabp) ){
      vocabp$score[i]=mutualInfo(vocabp$term[i])
      incProgress(0.1, detail = paste("finishing", round(i/nrow(vocabp),digits = 3)*100,"%"))
      setTxtProgressBar(txtProgressBar(min=0,max=1,style = 3),value =i/nrow(vocabp))
    }
    })
    
    m=subset(vocabp,((term_count>5 & doc_count>5) | (score>0.6 & term_count>2)))
    m$term=str_replace_all(m$term,pattern = "_",replacement = "")
    m$length=sapply(m$term,nchar)
    m[order(-term_count),]
    
    })
   
   output$result=renderDataTable({
     datatable(final(),filter = "top",options = list(pageLength=20))
   })
  
   output$download<- downloadHandler(
    filename = function() {
      paste(Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(final(), file, row.names = FALSE,fileEncoding="UTF-8")
    }
  )
   
   output$lengthDist=renderEChart({
     cand=count(final(),length)
     eBar(cand,xvar=length,yvar=n,title="实体词长度分布")
   })
   
   output$scoreDist=renderEChart({
     cand=count(final(),length)
     eBar(cand,xvar=length,yvar=n,title="实体词互信息分布")
   })
  
})
