# 异常值检测设置开始------------------------------------------------------------
fluidRow(
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '数据上传与参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_outliers_detect",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_outliers_detect',
                          label = h4('下载示例数据')),
             # 选择是否保存异常值
             selectInput('save_or_not_outliers_detect',
                         label = h4('是保存异常值'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 异常值替换方法
             selectInput('mean_or_not_outliers_detect',
                         label = h4('异常值替换方法'),
                         choices = list('均值替换' = 'mean',
                                        '其他方法' = 'other'),
                         selected = 'mean'),
             # 离群值检测
             selectInput('sd_or_not_outliers_detect',
                         label = h4('离群值检测'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 异常值替换方法
             selectInput('mean_or_not_outliers_detect',
                         label = h4('离群值替换方法'),
                         choices = list('均值替换' = 'mean',
                                        '其他方法' = 'other'),
                         selected = 'mean'),
             
             br(),
             
             # 点击提交
             actionButton('submit_outliers_detect',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             
             br(),
             br(),
             
             # 选择数据下载格式
             selectInput('outliers_detect_filetype',
                         label = h5('数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             
             fluidRow(
               column(width = 2,
                      offset = 3,
                      downloadButton('taboutput_outliers_detect_download',
                                     label = h4('下载结果'),
                                     width = 230)))
         )),
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 425,
             title = '各种检测结果',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_tab_outliers_detect_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 425,
             title = '异常值箱线图',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             plotOutput('box_outliers_detect_view'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 425,
             title = '正态性Q-Q图',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('qqplot_outliers_detect_view')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 425,
             title = '离群值检测图',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #br(),
             textOutput('otherinfo_outliers_detect_view'))
  )
)
# 异常值检测设置开始------------------------------------------------------------
