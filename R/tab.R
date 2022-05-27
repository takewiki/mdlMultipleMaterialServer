#' 标签选择的内容
#'
#' @param f 函数名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' tabPanel_chooser()
tabPanel_chooser <- function(f) {
  res = switch(f,
         propValueConfigUI = mdlMultipleMaterialUI::propValueConfigUI(),
         propValueUI = mdlMultipleMaterialUI::propValueUI(),
         propCategoryConfigUI = mdlMultipleMaterialUI::propCategoryConfigUI(),
         propCategoryUI = mdlMultipleMaterialUI::propCategoryUI(),
         prdGroupUI = mdlMultipleMaterialUI::prdGroupUI(),
         prdCategoryUI = mdlMultipleMaterialUI::prdCategoryUI(),
         prdSelUI = mdlMultipleMaterialUI::prdSelUI()
         )
  return(res)

}

#' 动态标签的加载
#'
#' @param input 输入
#' @param output 输出
#' @param app_id 会话IP
#' @param session 会话
#' @param role_name 角色
#' @param conn_cfg 会话连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' tabPanel_initial_Server()
tabPanel_initial_Server <- function(input,output,session,conn_cfg,app_id='cpdms', role_name ='admin') {
  conn = tsda::conn_rds('rdbe')
  sql = paste0("   select FTabSetId,FTabUIName,FTargetName,FPosition from t_md_tabPanelRight
   where FappId ='",app_id,"' and Fpermissions ='",role_name,"' and Fshow = 1 and Fid ='mdlMultipleMaterialCom'
   order by Findex desc ")
  data = tsda::sql_select(conn,sql)
  print(data)
  print(role_name)
  ncount = nrow(data)
  if(ncount >0){
    for (i in 1:ncount) {
      FTabSetId = data$FTabSetId[i]
      FTabUIName  =data$FTabUIName[i]
      FTargetName = data$FTargetName[i]
      FPosition = data$FPosition[i]
      shiny::insertTab(inputId = FTabSetId
                       ,tabPanel_chooser(FTabUIName)
                       ,target = FTargetName,
                       position =FPosition)
    }
  }


}
