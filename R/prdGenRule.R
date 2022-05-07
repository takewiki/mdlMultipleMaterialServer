#' 获取最大号ID
#'
#' @param conn_cfg 连接配置
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_maxId()
prdGenRule_maxId <- function(conn_cfg ='config/conn_cfg.R') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  res = tsda::db_maxId(conn = conn,FTableName = 'rds_mtrl_prdGenRule')
  tsda::conn_close(conn)

  return(res)

}


#' 是否最新
#'
#' @param conn_cfg  连接配置
#' @param FPrdGroupNumber  分组代码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_isNew()
prdGenRule_isNew <- function(conn_cfg ='config/conn_cfg.R'
                             ,FPrdGroupNumber ='1.1.1.001.001') {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  sql <- paste0(" select * from rds_mtrl_prdGenRule
where FPrdGroupNumber ='",FPrdGroupNumber,"'")
  data <- tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount >0){
    res <- FALSE
  }else{
    res<- TRUE
  }
  tsda::conn_close(conn)

  return(res)

}


#' 获取下一个代码
#'
#' @param FPrdGroupNumber 物料分组代码
#' @param conn_cfg 连接文件
#' @param FNumberLength 长度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_getNextNumber()
prdGenRule_getNextNumber <- function(conn_cfg = 'config/conn_cfg.R'
                                     ,FPrdGroupNumber ='1.1.1.001.001',FNumberLength=6) {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  flag = prdGenRule_isNew(conn_cfg = conn_cfg,FPrdGroupNumber =FPrdGroupNumber )
  if(flag){
    FInterId = prdGenRule_maxId(conn_cfg = conn_cfg)
    FNextNumber  = 1
    data = data.frame(FInterId,FPrdGroupNumber,FNextNumber,FNumberLength,stringsAsFactors = F)
    tsda::db_writeTable(conn = conn,table_name = 'rds_mtrl_prdGenRule',r_object = data,append = TRUE)
    res = FNextNumber
  }else{
    sql <- paste0(" select FNextNumber from rds_mtrl_prdGenRule
where FPrdGroupNumber ='",FPrdGroupNumber,"'")
    data <- tsda::sql_select(conn,sql)
    res = data$FNextNumber[1]

  }
  tsda::conn_close(conn)

  return(res)
}


#' 设置下一个代码
#'
#' @param FPrdGroupNumber 物料分组代码
#' @param FNextNumber 设置下一个值
#' @param conn_cfg 连接设置
#'
#' @return 返回值
#' @export
#'
#' @examples
#'
#' prdGenRule_SetNextNumber()
prdGenRule_SetNextNumber <- function(conn_cfg ='config/conn_cfg.R'
                                     ,FPrdGroupNumber ='1.1.1.001.001',FNextNumber=1) {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  flag = !prdGenRule_isNew(conn_cfg = conn_cfg ,FPrdGroupNumber =FPrdGroupNumber )
  if(flag){
    sql <- paste0(" update a set  FNextNumber = ",FNextNumber,"  from rds_mtrl_prdGenRule a
where FPrdGroupNumber ='",FPrdGroupNumber,"'")
    data <- tsda::sql_update(conn,sql)
    res <- TRUE


  }else{
    res <- FALSE
  }
  tsda::conn_close(conn)

  return(res)
}


#' 获取下一个代码
#'
#' @param FPrdGroupNumber 物料分组代码
#' @param FNumberLength  长度
#' @param conn_cfg 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_autoNumber()
prdGenRule_autoNumber <- function(conn_cfg ='config/conn_cfg.R'
                                  ,
                                  FPrdGroupNumber ='1.1.1.001.001',FNumberLength=6) {
  conn_info = tsda::conn_config(config_file = conn_cfg)
conn = tsda::conn_open(conn_config_info = conn_info)

  FNumber = prdGenRule_getNextNumber(conn_cfg = conn_cfg,FPrdGroupNumber = FPrdGroupNumber,FNumberLength = FNumberLength)
  FNextNumber = FNumber +1
  prdGenRule_SetNextNumber(conn_cfg = conn_cfg,FPrdGroupNumber = FPrdGroupNumber,FNextNumber =FNextNumber )
  res = tsdo::str_rep(x =FNumber,len = FNumberLength )
  tsda::conn_close(conn)

  return(res)
}

