# 定义t-test选项卡开始----------------------------------------------------------
fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '数据上传与主要设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_rf",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_rf',
                          label = h4('下载示例数据')),
             
             # 训练集数据比例
             sliderInput('train_per_rf',
                         label = h4('训练集数据比例'),
                         min = 0.1, max = 1,
                         step = 0.1, value = 0.7),
             # ntree
             sliderInput('ntree_rf',
                         label = h4('ntree'),
                         min = 100, max = 9999,
                         step = 100, value = 500),
             # mtry
             sliderInput('mtry_rf',
                         label = h4('mtry'),
                         min = 1, max = 30,
                         step = 1, value = 1),
             
             # 交叉验证
             selectInput('cv_rf',
                         label = h4('交叉验证'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'SFALSE'),
                         selected = 'TRUE'),
             br(),
             # 点击提交
             actionButton('submit_rf',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_rf_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__rf',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 400,
             title = '模型准确率',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('acc_rf_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 450,
             title = '变量重要性',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = 'navy',
             dataTableOutput('variable_rf'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 400,
             title = '预测准确率',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('pre_per_rf')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 450,
             title = '交叉验证',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = 'navy',
             plotOutput('cv_plot')
         )
  ))
