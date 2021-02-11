warnings('off')

package_list = c(
  'shiny',
  'shinydashboard',
  'multcomp',
  'pgirmess',
  'dplyr',
  'data.table',
  'stringr',
  'xlsx',
  'ggsci',
  'tidyverse',
  'markdown',
  'knitr',
  'reshape2',
  'shinyWidgets',
  'extrafont',
  'Cairo',
  'showtext',
  'plyr',
  'vegan',
  'ggplotify',
  #'ggcor',
  'ggpmisc',
  #'ropls',
  'WGCNA',
  'randomForest',
  'caret',
  'tidymodels'
  
)

for (i in package_list) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i, dependencies = TRUE)
  }
}

library(shiny)
library(shinydashboard)
library(multcomp)
library(pgirmess)
library(dplyr)
library(data.table)
library(stringr)
library(xlsx)
library(ggsci)
library(tidyverse)
library(markdown)
library(knitr)
library(reshape2)
library(shinyWidgets)
library(extrafont)
library(Cairo)
library(showtext)
#loadfonts()
showtext_auto()
#font.add("simsun", "/usr/share/fonts/chinese/simsun.tcc") # 你的中文字体位置

library(plyr)
library(vegan)
library(ggplotify)
library(ggcor)
library(ggpmisc)
library(ropls)
library(WGCNA)
library(randomForest)
library(caret)
library(tidymodels)
library(export)
library(ggprism)











