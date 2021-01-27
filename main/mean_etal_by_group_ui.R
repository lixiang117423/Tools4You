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
             fileInput("data_input_mean_etal_by_group",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_mean_etal_by_group',
                          label = h4('下载示例数据')),
             # 选择是否求和
             selectInput('sum_or_not_mean_etal_by_group',
                         label = h4('是否求和'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 选择是否求均值
             selectInput('mean_or_not_mean_etal_by_group',
                         label = h4('是否求均值'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 选择是否求标准差
             selectInput('sd_or_not_mean_etal_by_group',
                         label = h4('是否求标准差'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 选择是否求标准误
             selectInput('se_or_not_mean_etal_by_group',
                         label = h4('是否求标准误'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             br(),
             
             # 点击提交
             actionButton('submit_mean_etal_by_group',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             
             br(),
             br(),
             
             # 选择数据下载格式
             selectInput('mean_etal_by_group_filetype',
                         label = h5('数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             
             fluidRow(
               column(width = 2,
                      offset = 3,
                      downloadButton('taboutput_mean_etal_by_group_download',
                                     label = h4('下载结果'),
                                     width = 230)))
         )),
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 425,
             title = '求和结果',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_sum_mean_etal_by_group_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 425,
             title = '求均值结果',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             #br(),
             dataTableOutput('taboutput_mean_mean_etal_by_group_view'))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 425,
             title = '标准差结果',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_sd_mean_etal_by_group_view')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 425,
             title = '标准误结果',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #br(),
             dataTableOutput('taboutput_se_mean_etal_by_group_view'))
  )
)