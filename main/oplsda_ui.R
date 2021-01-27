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
             fileInput("data_input_oplsda",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_oplsda',
                          label = h4('下载示例数据')),
             
             # 差异物种展示数量
             textInput('num_oplsda_show',
                       label = h4('展示的差异物质数'),
                       value = '10'),
            
             # 热图数据标准化
             selectInput('oplsda_heatmap_scale',
                         label = h4('热图标准化'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 热图聚类
             selectInput('oplsda_heatmap_cluster',
                         label = h4('热图聚类'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             # 热图是否行聚类
             selectInput('oplsda_stat_res_filetype',
                         label = h4('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             # 图片格式
             selectInput('oplsda_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             # 点击提交
             actionButton('submit_oplsda',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_oplsda_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__oplsda',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 425,
             title = '分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_oplsda_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 425,
             title = 'OPLS-DA图',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = 'navy',
             plotOutput('oplsda_plot')
             )
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 425,
             title = '差异物质棒棒糖图',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('lollipop_plot_oplsda')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 425,
             title = '差异物质热图',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = 'navy',
             plotOutput('heatmap_oplsda')
         )
  ))
