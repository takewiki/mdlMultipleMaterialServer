#' 读取属性选项数据
#'
#' @param file_name 文件名
#' @param sheet_name 页签表
#' @param conn_cfg 连接文件设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValue_read()
propValue_read <- function(conn_cfg ='config/conn_cfg.R'
                           ,
                              file_name ='data-raw/data/属性选项模板.xlsx',
                              sheet_name ="属性选项") {
  res <- data_read2(conn_cfg = conn_cfg,
                    file_name =file_name,
                    sheet_name =sheet_name,
                    table_name = 'rds_mtrl_propValue',
                    table_key = 'FPropCategoryNumber',
                    table_key2 = 'FPropNumber',
                    table_caption = '属性类别代码',
                    table_caption2 = '属性代码',
                    pageCount=500)

  return(res)
}


#' 属性选项查询
#'
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#'
#' @examples
#' propValue_query()
propValue_query <- function(conn_cfg ='config/conn_cfg.R'

) {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)


  sql <- paste0("SELECT
      [属性类别代码]
      ,[属性类别名称]
      ,[属性代码]
      ,[属性名称]
      ,[行号]
  FROM [vw_rds_mtrl_propValue]
  where 是否禁用 =0 order  by 属性类别代码, 属性代码")
  data = tsda::sql_select(conn,sql)
  tsda::conn_close(conn)

  return(data)

}


#' 属性选项查询
#'
#' @param FPrdCategoryNumber 产品大类代码
#' @param FPropCategoryNumber 产品属性代码
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValue_queryDetail()
propValue_queryDetail <- function(conn_cfg = 'config/conn_cfg.R'
                                  ,
                                  FPrdCategoryNumber ='1.1.1.001',
                                  FPropCategoryNumber ='AF'
                                  ) {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)


  sql <- paste0("SELECT
      FPropNumber
      ,FPropName

  FROM rds_mtrl_propValueConfig
  where FPrdCategoryNumber ='",FPrdCategoryNumber,"' and FPropCategoryNumber ='",FPropCategoryNumber,"'   and FDeleted = 0
  order by FSeq")

  data = tsda::sql_select(conn,sql)
  ncount =nrow(data)
  if(ncount >0){
    res = tsdo::vect_as_list(data$FPropNumber)
    names(res) <- data$FPropName
  }else
  {
    res= list('')
    names(res) <- '无选项,请联系管理员'
  }
  tsda::conn_close(conn)

  return(res)

}


#' 返回选项值
#'
#' @param FPropNumber 选项代码
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValue_queryByNumber()
propValue_queryByNumber <- function(conn_cfg = 'config/conn_cfg.R'
                                    ,FPropNumber ='C000009') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  if(FPropNumber == ''){
    res =''
  }else{
    sql <- paste0("  select  FPropName from  rds_mtrl_propValue
  where FPropNumber ='",FPropNumber,"'")
    data = tsda::sql_select(conn,sql)
    ncount = nrow(data)
    if(ncount >0){
      res = data$FPropName[1]
    }else{
      res =''
    }
  }
tsda::conn_close(conn)


  return(res)

}




#' 属性选项查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' propValueServer_query()
propValueServer_query <- function(input,output,session){

  shiny::observeEvent(input$propValue_query_btn,{
    data = propValue_query()
    file_name = paste0('属性选项下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'propValue_query_dataview',data = data)
    tsui::run_download_xlsx(id = 'propValue_dl_btn',data = data,filename = file_name)

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
#' propValueUIServer_sheet()
propValueUIServer_sheet <- function(input,output,session){

  var_propValue_file <- tsui::var_file('propValue_upload_file')

  output$propValue_sheets_placeholder <- shiny::renderUI({

    file_name = var_propValue_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'propValue_sheets_lc1',label = '请选择一下属性选项所在页签',
                                  choiceNames = sheetNames,choiceValues = sheetNames,selected = sheetNames[[1]])
    }

    res


  })


}





#' 处理内容
#'
#' @param input 输入
#' @param output 输出
#' @param conn_cfg 连接设置
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
propValueServer_read <- function(input,output,session,conn_cfg) {


  var_file_name  = tsui::var_file('propValue_upload_file')
  var_sheet_name =tsui::var_ListChoose1('propValue_sheets_lc1')
  shiny::observeEvent(input$propValue_upload_btn,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = propValue_read(conn_cfg = conn_cfg,file_name = file_name ,sheet_name = sheet_name )
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
#' @param conn_cfg 连接设置
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propValueServer()
propValueServer <- function(input,output,session,conn_cfg){

  propValueServer_query(input,output,session)
  propValueUIServer_sheet(input,output,session)
  propValueServer_read(input,output,session,conn_cfg = conn_cfg)



}



