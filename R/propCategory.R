#' 读取属性类别数据
#'
#' @param conn  连接
#' @param file_name 文件名
#' @param sheet_name 页签表
#'
#' @return 返回值
#' @export
#'
#' @examples
#' propCategory_read()
propCategory_read <- function(conn=tsda::conn_rds('cprds'),
                             file_name ='data-raw/data/属性类别模板.xlsx',
                             sheet_name ="属性类别") {
  res <- data_read(conn=conn,
                   file_name =file_name,
                   sheet_name =sheet_name,
                   table_name = 'rds_mtrl_propCategory',
                   table_key = 'FPropCategoryNumber',
                   table_caption = '属性类型代码',
                   pageCount=500 )
  return(res)
}


#' 属性类别查询
#'
#' @param conn 连接
#'
#' @return 返回值
#'
#' @examples
#' propCategory_query()
propCategory_query <- function(conn=tsda::conn_rds('cprds')) {

  sql <- paste0("SELECT
      [属性类型代码]
      ,[属性类别名称]
  FROM  [vw_rds_mtrl_propCategory]
  where 是否禁用 =0")
  data = tsda::sql_select(conn,sql)
  return(data)

}





#' 属性类别查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#'@import tsdo
#' @return 返回值
#'
#' @examples
#' propCategoryServer_query()
propCategoryServer_query <- function(input,output,session){

  shiny::observeEvent(input$propCategory_query_btn,{
    data = propCategory_query()
    file_name = paste0('属性类别下载_',getDate(),".xlsx")
    tsui::run_dataTable2(id = 'propCategory_query_dataview',data = data)
    tsui::run_download_xlsx(id = 'propCategory_dl_btn',data = data,filename = file_name)

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
#' propCategoryUIServer_sheet()
propCategoryUIServer_sheet <- function(input,output,session){

  var_propCategory_file <- tsui::var_file('propCategory_upload_file')

  output$propCategory_sheets_placeholder <- shiny::renderUI({

    file_name = var_propCategory_file()

    if (is.null(file_name)){
      res = NULL
    }else{
      sheetNames =  tsdo::vect_as_list(  openxlsx::getSheetNames(file=file_name))
      res = tsui::mdl_ListChoose1(id = 'propCategory_sheets_lc1',label = '请选择一下属性类别所在页签',
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
propCategoryServer_read <- function(input,output,session) {
  var_file_name  = tsui::var_file('propCategory_upload_file')
  var_sheet_name =tsui::var_ListChoose1('propCategory_sheets_lc1')
  shiny::observeEvent(input$propCategory_upload_btn,{
    file_name = var_file_name()
    print(file_name)
    sheet_name = var_sheet_name()
    print(sheet_name)
    data = propCategory_read(file_name = file_name ,sheet_name = sheet_name )
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
#' propCategoryServer()
propCategoryServer <- function(input,output,session){

  propCategoryServer_query(input,output,session)
  propCategoryUIServer_sheet(input,output,session)
  propCategoryServer_read(input,output,session)



}



