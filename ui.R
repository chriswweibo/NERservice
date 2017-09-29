library(shiny)
library(DT)
library(rbokeh)

shinyUI(fluidPage(

  titlePanel("Genowis实体词发现服务"),
  column(width=7,
    sidebarPanel(width=3,
                 fileInput("file","上传excel文件"),
                 textInput("colname","解析列名",value="检查结论"),
                 textInput("punctuationM","停用标点符号，异常字符(用|分割)",value="%"),
                 textInput("stopWM","停用词语(用|分割)",value="是否"),
                 textInput("combinationM","停用搭配(用|分割)",value="[0-9]{1,}%"),
                 sliderInput("ngram","n-gram切词最大窗口长度（一般不超过15）",min=2,max=20,value=10,tick=F,step=1),
                 sliderInput("rate","增长强度",min=1,max=1.5,value=1.08,tick=F,step=0.01),
                 actionButton("submit","生成全量切词数据"),
                 br(),
                 h4("过滤条件生成实体词："),
                 #sliderInput("vocPrune1","最小字符串长度",min=1,max=3,value=2,tick=F,step=1),
                 textInput("notBegin","不以其开始，多个用[]包裹,内部英文逗号分割",value="也"),
                 textInput("notEnd","不以其结束，多个用[]包裹,内部英文逗号分割",value="见"),
                 textInput("notContain","不包含，多个用[]包裹,内部英文逗号分割",value="[于,是]"),
                 sliderInput("vocPrune2","最小频度",min=2,max=10,value=2,tick=F,step=1),
                 sliderInput("vocPrune3","最小互信息score",min=0,max=1,value=0.6,tick=F,step=0.01),
                 sliderInput("vocPrune4","最小高权频度",min=3,max=100,value=5,tick=F,step=1),
                 p("筛选模式：(tf>最小高权频度 & df>最小高权频度) | (score>最小互信息score & term_count>最小频度)")
                 
                 ),
    
    mainPanel(width=9,
              titlePanel("全量切词数据"),
              dataTableOutput("resultTotal"),
              downloadButton("downloadTotal", "下载全量数据"),
              rbokehOutput("scoreTotal"),
              rbokehOutput("lengthTotal")
              )
    
),
column(width=5,
       titlePanel("实体词数据"),
       dataTableOutput("resultFiltered"),
       downloadButton("downloadFiltered", "下载实体词数据"),
       rbokehOutput("scoreFiltered"),
       rbokehOutput("lengthFiltered")
)
)
)