#' 读取属性类别数据
#'
#' @param file_name 文件名
#' @param sheet_name 页签表
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propCategoryConfig_read()
propCategoryConfig_read <- function(conn_cfg ='config/conn_cfg.R'
                                    ,
                              file_name ='data-raw/data/属性类别配置模板.xlsx',
                              sheet_name ="属性类别配置") {
  res <- data_read2(conn_cfg = conn_cfg,
                                         file_name =file_name,
                                         sheet_name =sheet_name,
                                         table_name = 'rds_mtrl_propCategoryConfig',
                                         table_key = 'FPrdCategoryNumber',
                                         table_key2 = 'FPropCategoryNumber',
                                         table_caption = '产品大类代码',
                                         table_caption2 = '物料属性类别代码',
                                         pageCount=500)
  return(res)
}


#' 属性类别配置查询
#'
#' @param conn_cfg 连接配置
#'
#' @return 返回值
#'
#' @examples
#' propCategoryConfig_query()
propCategoryConfig_query <- function(conn_cfg ='config/conn_cfg.R') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)


  sql <- paste0("SELECT
      [产品大类代码]
      ,[产品大类名称]
      ,[物料属性类别代码]
      ,[物料属性类别]
      ,[行号]
  FROM  [vw_rds_mtrl_propCategoryConfig]
  where 是否禁用 =0 order  by 产品大类代码,物料属性类别代码 ")
  data = tsda::sql_select(conn,sql)
  tsda::conn_close(conn)

  return(data)

}





#' 属性类别配置查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' propCategoryConfigServer_query()
propCategoryConfigServer_query <- function(input,output,session){

  shiny::observeEvent(input$propCategoryConfig_query_btn,{
    data = propCategoryConfig_query()
    file_name = paste0('属性类别配置下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'propCategoryConfig_query_dataview',data = data)
    tsui::run_download_xlsx(id = 'propCategoryConfig_dl_btn',data = data,filename = file_name)

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
#' propCategoryConfigUIServer_sheet()
propCategoryConfigUIServer_sheet <- function(input,output,session){

  var_propCategoryConfig_file <- tsui::var_file('propCategoryConfig_upload_file')

  output$propCategoryConfig_sheets_placeholder <- shiny::renderUI({

    file_name = var_propCategoryConfig_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'propCategoryConfig_sheets_lc1',label = '请选择一下属性类别配置所在页签',
                                  choiceNames = sheetNames,choiceValues = sheetNames,selected = sheetNames[[1]])
    }

    res


  })


}





#' 处理内容
#'
#' @param input 输入
#' @param output 输出
#' @param conn_cfg  连接配置
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
propCategoryConfigServer_read <- function(input,output,session,conn_cfg ='config/conn_cfg.R') {
  var_file_name  = tsui::var_file('propCategoryConfig_upload_file')
  var_sheet_name =tsui::var_ListChoose1('propCategoryConfig_sheets_lc1')
  shiny::observeEvent(input$propCategoryConfig_upload_btn,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = propCategoryConfig_read(conn_cfg = conn_cfg,file_name = file_name ,sheet_name = sheet_name )
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
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propCategoryConfigServer()
propCategoryConfigServer <- function(input,output,session,conn){

  propCategoryConfigServer_query(input,output,session)
  propCategoryConfigUIServer_sheet(input,output,session)
  propCategoryConfigServer_read(input,output,session,conn = conn)



}



