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
             fileInput("data_input_wilcox",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_wilcox',
                          label = h4('下载示例数据')),
             
             # 选择置信区间
             selectInput('select_confidence_interval_wilcox',
                         label = h4('置信区间'),
                         choices = list('0.95' = 0.95,
                                        '0.99' = 0.99),
                         selected = 0.99),
             # 选择单尾or双尾检验
             selectInput('select_oneside_or_twoside_wilcox',
                         label = h4('单尾/双尾'),
                         choices = list('双尾' = "two.sided",
                                        '单尾' = "less")),
             # 选择是否配对数据
             selectInput('select_paired_or_not_wilcox',
                         label = h4('配对数据'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE')),
             # 选择方差齐性
             selectInput('var_equal_wilcox',
                         label = h4('方差相等'),
                         choices = list('否' = 'FALSE',
                                        '是' = 'TRUE'),
                         selected = 'FALSE'),
             
             # 选择误差棒
             selectInput('wilcox_err_bar',
                         label = h4('误差棒数据'),
                         choices = list('标准差' = 'SD',
                                        '标准误' = 'SE'),
                         selected = 'SD'),
             br(),
             # 点击提交
             actionButton('submit_wilcox',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('taboutput_wilcox_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('download_figure__wilcox',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 5,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 400,
             title = '分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_wilcox_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 450,
             title = '下载参数设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = 'navy',
             #"随便打的文本", # 直接插入文本
             #br(),
             selectInput('wilcox_stat_res_filetype',
                         label = h4('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             # 图片格式
             selectInput('wilcox_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             
             # 图片宽度
             sliderInput('wilcox_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('wilcox_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5))
  ),
  
  # 第三列
  column(width = 5,
         # 第一个box用于绘图
         box(width = NULL,
             height = 400,
             title = '绘图结果展示',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('plot_wilcox')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 450,
             title = '绘图参数设置',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = 'navy',
             textInput('wilcox_fig_sig',
                       label = h4('输入显示显著性的分组名称'),
                       value = '如：y'),
             textInput('wilcox_fig_x_axis',
                       label = h4('输入X轴标题')),
             textInput('wilcox_fig_y_axis',
                       label = h4('输入Y轴标题')),
             textInput('wilcox_fig_title',
                       label = h4('输入图片标题'))
         )
  ))
