#' 读取属性选项数据
#'
#' @param conn  连接
#' @param file_name 文件名
#' @param sheet_name 页签表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValueConfig_read()
propValueConfig_read <- function(conn=tsda::conn_rds('cprds'),
                           file_name ='data-raw/data/属性选项配置模板.xlsx',
                           sheet_name ="属性选项配置") {
  res = data_read3(conn=conn,
                         file_name = file_name,
                         sheet_name =sheet_name,
                         table_name = 'rds_mtrl_propValueConfig',
                         table_key = 'FPrdCategoryNumber',
                         table_key2 = 'FPropCategoryNumber',
                         table_key3 = 'FPropNumber',
                         table_caption = '产品大类代码',
                         table_caption2 = '属性类别代码',
                         table_caption3 = '属性选项代码',
                         pageCount=500)

  return(res)
}


#' 属性选项配置查询
#'
#' @param conn 连接
#'
#' @return 返回值
#'
#' @examples
#' propValueConfig_query()
propValueConfig_query <- function(conn=tsda::conn_rds('cprds')) {

  sql <- paste0("SELECT
      [产品大类代码]
      ,[产品大类名称]
      ,[属性类别代码]
      ,[属性类别名称]
      ,[属性选项代码]
      ,[属性选项名称]
      ,[选项显示名称]
      ,[行号]

  FROM  [vw_rds_mtrl_propValueConfig]
  where 是否禁用 =0 order  by 产品大类代码,属性类别代码, 属性选项代码")
  data = tsda::sql_select(conn,sql)
  return(data)

}





#' 属性选项配置查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' propValueConfigServer_query()
propValueConfigServer_query <- function(input,output,session){

  shiny::observeEvent(input$propValueConfig_query_btn,{
    data = propValueConfig_query()
    file_name = paste0('属性选项配置下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'propValueConfig_query_dataview',data = data)
    tsui::run_download_xlsx(id = 'propValueConfig_dl_btn',data = data,filename = file_name)

  })


}




#' 文本选择
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#' @export
#'
#' @examples
#' propValueConfigUIServer_sheet()
propValueConfigUIServer_sheet <- function(input,output,session){

  var_propValueConfig_file <- tsui::var_file('propValueConfig_upload_file')

  output$propValueConfig_sheets_placeholder <- shiny::renderUI({

    file_name = var_propValueConfig_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'propValueConfig_sheets_lc1',label = '请选择一下属性选项配置所在页签',
                                  choiceNames = sheetNames,choiceValues = sheetNames,selected = sheetNames[[1]])
    }

    res


  })


}





#' 处理内容
#'
#' @param input 输入
#' @param output 输出
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
propValueConfigServer_read <- function(input,output,session) {
  var_file_name  = tsui::var_file('propValueConfig_upload_file')
  var_sheet_name =tsui::var_ListChoose1('propValueConfig_sheets_lc1')
  shiny::observeEvent(input$propValueConfig_upload_btn,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = propValueConfig_read(file_name = file_name ,sheet_name = sheet_name )
    print(data)

    ncount =nrow(data)
    if(ncount >0){
      tsui::pop_notice('上传成功')
    }else{
      tsui::pop_notice('上传失败')
    }






  })

}




#' 产品大娄的处理函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValueConfigServer()
propValueConfigServer <- function(input,output,session){

  propValueConfigServer_query(input,output,session)
  propValueConfigUIServer_sheet(input,output,session)
  propValueConfigServer_read(input,output,session)



}



