#' 获取最大号ID
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_maxId()
prdGenRule_maxId <- function(conn=tsda::conn_rds('cprds')) {
  res = tsda::db_maxId(conn = conn,FTableName = 'rds_mtrl_prdGenRule')
  return(res)

}


#' 是否最新
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_isNew()
prdGenRule_isNew <- function(conn=tsda::conn_rds('cprds'),FPrdGroupNumber ='1.1.1.001.001') {
  sql <- paste0(" select * from rds_mtrl_prdGenRule
where FPrdGroupNumber ='",FPrdGroupNumber,"'")
  data <- tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount >0){
    res <- FALSE
  }else{
    res<- TRUE
  }
  return(res)

}


#' 获取下一个代码
#'
#' @param conn 连接
#' @param FPrdGroupNumber 物料分组代码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_getNextNumber()
prdGenRule_getNextNumber <- function(conn=tsda::conn_rds('cprds'),FPrdGroupNumber ='1.1.1.001.001',FNumberLength=6) {
  flag = prdGenRule_isNew(conn = conn,FPrdGroupNumber =FPrdGroupNumber )
  if(flag){
    FInterId = prdGenRule_maxId(conn = conn)
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
  return(res)
}


#' 设置下一个代码
#'
#' @param conn 连接
#' @param FPrdGroupNumber 物料分组代码
#' @param FNextNumber 设置下一个值
#'
#' @return 返回值
#' @export
#'
#' @examples
#'
#' prdGenRule_SetNextNumber()
prdGenRule_SetNextNumber <- function(conn=tsda::conn_rds('cprds'),FPrdGroupNumber ='1.1.1.001.001',FNextNumber=1) {
  flag = !prdGenRule_isNew(conn = conn,FPrdGroupNumber =FPrdGroupNumber )
  if(flag){
    sql <- paste0(" update a set  FNextNumber = ",FNextNumber,"  from rds_mtrl_prdGenRule a
where FPrdGroupNumber ='",FPrdGroupNumber,"'")
    data <- tsda::sql_update(conn,sql)
    res <- TRUE


  }else{
    res <- FALSE
  }
  return(res)
}


#' 获取下一个代码
#'
#' @param conn 连接
#' @param FPrdGroupNumber 物料分组代码
#' @param FNumberLength  长度
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGenRule_autoNumber()
prdGenRule_autoNumber <- function(conn=tsda::conn_rds('cprds'),FPrdGroupNumber ='1.1.1.001.001',FNumberLength=6) {
  FNumber = prdGenRule_getNextNumber(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FNumberLength = FNumberLength)
  FNextNumber = FNumber +1
  prdGenRule_SetNextNumber(conn = conn,FPrdGroupNumber = FPrdGroupNumber,FNextNumber =FNextNumber )
  res = tsdo::str_rep(x =FNumber,len = FNumberLength )
  return(res)
}

