#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' materialGeneratorServer()
materialGeneratorServer <- function(input,output,session) {

  var_mg_text <- tsui::var_text('mg_text')
  var_mg_date <- tsui::var_date('mg_date')
  observeEvent(input$mg_test,{

    print(var_mg_text())
    print(var_mg_date())


  })
}
