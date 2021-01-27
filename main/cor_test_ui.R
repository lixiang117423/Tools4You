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
             fileInput("data_input_cor",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_cor',
                          label = h4('下载示例数据')),
             br(),
 
             # 相关性计算方法
             selectInput('method_cor',
                         label = h4('相关性计算方法'),
                         choices = list('pearson' = 'pearson',
                                        'kendall' = 'kendall',
                                        'spearman' = 'spearman'),
                         selected = 'pearson'),
             
             selectInput('cor_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             selectInput('cor_stat_res_filetype',
                         label = h4('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             
             # 图片宽度
             sliderInput('cor_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('cor_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             
             # 点击提交
             actionButton('submit_cor',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             
             
             fluidRow(
               column(width = 2,
                      downloadButton('download_figure__cor',
                                     label = h5('相关性图'),
                                     width = 230)),
               column(width = 2,
                      offset = 4,
                      downloadButton('taboutput_cor_download',
                                     label = h5('下载表格'),
                                     width = 230))
             )
         )),
  
  
  # 第二列
  column(width = 8,
         #offset = 1,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 450,
             title = '相关性图',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             plotOutput('cor_res_plot')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 400,
             title = '相关性表',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             dataTableOutput('cor_res_table'))
  ),
  
  column(width = 2,
         #offset = 1,
         box(width = NULL,
             height = 870,
             title = '绘图相关参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             # 设置绘图样式
             selectInput('select_plot_type_cor',
                         label = h4('选择绘图样式'),
                         choices = list('全部展示' = 1,
                                        '上半部分' = 2,
                                        '下半部分' = 3),
                         selected = 1),
             
             selectInput('select_cir_type_cor',
                         label = h4('选择可视化方法'),
                         choices = list('圆形' = 1,
                                        '方形' = 2,
                                        '椭圆形' = 3,
                                        '饼图' = 4)),
             br(),
             
             selectInput('num_cort',
                         label = h4('是否展示数值'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             selectInput('signif_cort',
                         label = h4('是否展示显著性'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             br(),
             
             textInput('cor_taxt_size',
                       label = h4('文字标签大小')),
             textInput('cor_text_color',
                       label = h4('文字标签颜色')),
             
             br(),
             
             textInput('cor_color_scale_but',
                       label = h4('相关性底色')),
             textInput('cor_color_scale_top',
                       label = h4('相关性顶色'))
             
         )),
  
  
  
  )
