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
             fileInput("data_input_nmds",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_nmds',
                          label = h4('下载示例数据')),
             
             # 是否导出解释度
             textInput('nmds_group_input',
                       label = h4('分组名称(英文逗号分隔)')),
             
             br(),
             br(),
             
             selectInput('nmds_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             
             # 图片宽度
             sliderInput('nmds_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('nmds_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             br(),
             
             # 点击提交
             actionButton('submit_nmds',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             br(),
             
             
             fluidRow(
               column(width = 2,
                      downloadButton('download_figure__nmds',
                                     label = h4('散点图'),
                                     width = 230)),
               column(width = 2,
                      offset = 4,
                      downloadButton('taboutput_nmds_download',
                                     label = h4('拟合度'),
                                     width = 230))
             )
         )),
  
  column(width = 2,
         box(width = NULL,
             height = 870,
             title = '绘图相关参数设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             # 设置绘图样式
             selectInput('select_plot_type_nmds',
                         label = h4('选择绘图样式'),
                         choices = list('椭圆' = 1,
                                        '放射' = 2,
                                        '边框' = 3),
                         selected = 1),
             textInput('select_pc_num',
                       label = h4('输入展示的坐标轴'),
                       value = 'NMDS,NMDS2'),
             sliderInput('nmds_point_size',
                         label = h4('点的大小'),
                         min = 0, max = 10, 
                         step = 0.5, value = 2),
             sliderInput('nmds_point_transparency',
                         label = h4('点的透明度'),
                         min = 0.1, max = 1,
                         step = 0.1,value = 1),
             #textInput('nmds_point_size_input',label = h4('控制点的大小的分组名称')),
             textInput('nmds_point_shape',
                       label = h4('控制点的形状的分组名称')),
             textInput('nmds_point_shape_input',
                       label = h4('输入点的形状'),
                       value = '1,2,3'),
             textInput('nmds_point_color',
                       label = h4('控制点的颜色的分组名称')),
             textInput('nmds_point_color_input',
                       label = h4('输入点的颜色'),
                       value = 'red,blue,black')
         )),
  
  # 第二列
  column(width = 6,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 450,
             title = 'nmds散点图',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             plotOutput('nmds_point')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 400,
             title = '拟合度评估',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             plotOutput('nmds_stone'))
  ),
  
  # 第三列
  column(width = 2,
         # 第一个box用于绘图
         box(width = NULL,
             height = 875,
             title = '其他绘图参数设置',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             textInput('nmds_xaxis',
                       label = h4('输入X轴名称')),
             textInput('nmds_yaxis',
                       label = h4('输入Y轴名称')),
             textInput('nmds_title',
                       label = h4('输入图片标题')),
             selectInput('nmds_legend',
                         label = h4('是否保留图例'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             selectInput('nmds_legend_position',
                         label = h4('图例位置'),
                         choices = list('左' = 'left',
                                        '右' = 'right',
                                        '下' = 'bottom',
                                        '上' = 'top'),
                         selected = 'right'),
             selectInput('nmds_hline',
                         label = h4('是否绘制横线'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             selectInput('nmds_vline',
                         label = h4('是否绘制竖线'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             selectInput('nmds_axis',
                         label = h4('四周坐标轴'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE')
             #selectInput('select_interpretation_or_not_nmds',label = h4('导出主成分解释度'),choices = list('是' = "TRUE",'否' = "FALSE"),selected = 'FALSE')
         )
  )
)


