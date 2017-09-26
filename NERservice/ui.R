

library(shiny)
library(DT)
library(recharts)

shinyUI(fluidPage(

  titlePanel("Genowis实体词发现服务"),
  sidebarLayout(
    sidebarPanel(width=3,
       textInput("punctuationM","停用标点符号，异常字符(用|分割)",value="%"),
       textInput("stopWM","停用词语(用|分割)",value="是否"),
       textInput("combinationM","停用搭配(用|分割)",value="[0-9]{1,}%"),
       fileInput("file","上传数据excel文件（单列病理结论）:"),
       checkboxInput("headerConfirm","数据表的第一行为列名。",value=T),
       sliderInput("ngram","n-gram切词最大窗口长度",min=2,max=30,value=2,tick=F,step=1),
       sliderInput("vocPrune","词典过滤最小窗口长度",min=1,max=3,value=2,tick=F,step=1),
       sliderInput("rate","增长强度",min=1,max=1.5,value=1.08,tick=F,step=0.01),
       actionButton("submit","确定")
    ),
    #conditionalPanel(condition="",box(width = 12, h1("The data is loading..."))),
    mainPanel(
      dataTableOutput("result"),
      downloadButton("download", "下载数据"),
      eChartOutput("lengthDist"),
      eChartOutput("scoreDist"),
      eChartOutput("probDist")
      
    )
  )
))
