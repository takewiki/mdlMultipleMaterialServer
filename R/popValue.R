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
#' propValue_read()
propValue_read <- function(conn=tsda::conn_rds('cprds'),
                              file_name ='data-raw/data/属性选项模板.xlsx',
                              sheet_name ="属性选项") {
  res <- data_read2(conn=conn,
                    file_name =file_name,
                    sheet_name =sheet_name,
                    table_name = 'rds_mtrl_propValue',
                    table_key = 'FPropCategoryNumber',
                    table_key2 = 'FPropNumber',
                    table_caption = '属性类别代码',
                    table_caption2 = '属性代码',
                    pageCount=500)
  return(res)
  return(res)
}


#' 属性选项查询
#'
#' @param conn 连接
#'
#' @return 返回值
#'
#' @examples
#' propValue_query()
propValue_query <- function(conn=tsda::conn_rds('cprds')) {

  sql <- paste0("SELECT
      [属性类别代码]
      ,[属性类别名称]
      ,[属性代码]
      ,[属性名称]
      ,[行号]
  FROM [vw_rds_mtrl_propValue]
  where 是否禁用 =0 order  by 属性类别代码, 属性代码")
  data = tsda::sql_select(conn,sql)
  return(data)

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
#' @param session 会议
#'
#' @return 返回值
#' @export
#'
#' @examples
#' file_deal()
propValueServer_read <- function(input,output,session) {
  var_file_name  = tsui::var_file('propValue_upload_file')
  var_sheet_name =tsui::var_ListChoose1('propValue_sheets_lc1')
  shiny::observeEvent(input$propValue_upload_btn,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = propValue_read(file_name = file_name ,sheet_name = sheet_name )
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
#' propValueServer()
propValueServer <- function(input,output,session){

  propValueServer_query(input,output,session)
  propValueUIServer_sheet(input,output,session)
  propValueServer_read(input,output,session)



}



