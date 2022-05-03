#' 生成动态数据
#'
#' @param input 输入
#' @param output 输出
#' @param session  返回值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGen_prdCategory_reactiveData()
prdGen_prdCategory_reactiveData <- function(input,output,session) {

  res <- shiny::reactive({
    var_prdGen_prdCategory_placeholder <- tsui::var_ListChoose1('prdGen_parentCategory_lc1')
    FParentPrdCategory =var_prdGen_prdCategory_placeholder()
    print(FParentPrdCategory)
    res <- prdCategory_queryByParent(FParentPrdCategory=FParentPrdCategory)
    return(res)

  })
  return(res)

}
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
prdGenServer_prdCategory <- function(input,output,session) {



  output$prdGen_prdCategory_dt <- DT::renderDataTable(prdGen_prdCategory_reactiveData(input,output,session)(),selection = 'single')


}



#' 生成动态数据
#'
#' @param input 输入
#' @param output 输出
#' @param session  返回值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' prdGen_prdGroup_reactiveData()
prdGen_prdGroup_reactiveData <- function(input,output,session) {

  res  <- reactive({

    shiny::validate(
      need(length(input$prdGen_prdCategory_dt_rows_selected) > 0, "请从产品大类中选中任意一行")
    )
    data_detail <- prdGen_prdCategory_reactiveData(input,output,session)()
    FPrdCategoryFNumber  <- data_detail[as.integer(input$prdGen_prdCategory_dt_rows_selected), '产品大类代码']
    print(FPrdCategoryFNumber)


    data_detail1 <- prdGroup_queryDetail(FPrdCategoryFNumber=FPrdCategoryFNumber)
    ncount <- nrow(data_detail1)


    return(data_detail1)

  })
  return(res)

}

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
prdGenServer_prdGroup <- function(input,output,session) {



  output$prdGen_prdGroup_dt <- DT::renderDataTable(prdGen_prdGroup_reactiveData(input,output,session)(),selection = 'single')


}





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
prdGenServer <- function(input,output,session) {
  # prdGenServer_prdCategory(input,output,session)
  # prdGenServer_prdGroup(input,output,session)

  prdCategoryData <- shiny::eventReactive(input$prdGen_confirm_btn,{
    var_prdGen_prdCategory_placeholder <- tsui::var_ListChoose1('prdGen_parentCategory_lc1')
    FParentPrdCategory =var_prdGen_prdCategory_placeholder()
    print(FParentPrdCategory)
    res <- prdCategory_queryByParent(FParentPrdCategory=FParentPrdCategory)
    return(res)

  })
  shiny::observeEvent(input$prdGen_confirm_btn,{
    output$prdGen_prdCategory_dt <- DT::renderDataTable(prdCategoryData(),selection = 'single')

  })

  prdGroupData  <- reactive({

    shiny::validate(
      need(length(input$prdGen_prdCategory_dt_rows_selected) > 0, "请从产品大类中选中任意一行")
    )
    data_detail <- prdCategoryData()
    FPrdCategoryFNumber  <- data_detail[as.integer(input$prdGen_prdCategory_dt_rows_selected), '产品大类代码']
    print(FPrdCategoryFNumber)


    res <- prdGroup_queryDetail(FPrdCategoryFNumber=FPrdCategoryFNumber)



    return(res)

  })
  prdGroupSelected  <- reactive({

    shiny::validate(
      need(length(input$prdGen_prdGroup_dt_rows_selected) > 0, "请从产品分组中选中任意一行")
    )
    data_detail <- prdGroupData()
    res  <- data_detail[as.integer(input$prdGen_prdGroup_dt_rows_selected), '产品分组代码']



    return(res)

  })

  propCategoryData  <- reactive({

    shiny::validate(
      need(length(input$prdGen_prdCategory_dt_rows_selected) > 0, "请从产品大类中选中任意一行")
    )
    data_detail <- prdCategoryData()
    FPrdCategoryFNumber  <- data_detail[as.integer(input$prdGen_prdCategory_dt_rows_selected), '产品大类代码']
    print(FPrdCategoryFNumber)


    res <- propCategory_queryDetail(FPrdCategoryNumber=FPrdCategoryFNumber)



    return(res)

  })
  output$prdGen_prdGroup_dt <- DT::renderDataTable(prdGroupData(),selection = 'single')
  #deal with selector,the most dificulty part for this module.
  output$prdGen_propSelector_placeholder <-shiny::renderUI({
    data = propCategoryData()
    ncount = nrow(data)
    if(ncount>0){
      res = lapply(1:ncount, function(i){
        FPrdCategoryNumber = data$FPrdCategoryNumber[i]
        FPrdCategoryName = data$FPrdCategoryName[i]
        FPropCategoryNumber = data$FPropCategoryNumber[i]
        FPropCategoryName =   data$FPropCategoryName[i]
        choices = propValue_queryDetail(FPrdCategoryNumber =FPrdCategoryNumber,FPropCategoryNumber =FPropCategoryNumber  )
        shiny::selectizeInput(inputId = FPropCategoryNumber,label = FPropCategoryName,choices = choices)



      })
    }


  })
  #show the propSelector
  shiny::observeEvent(input$prdGen_propSelector_btn,{

    output$prdGen_propSelector_print <- shiny::renderPrint({
      data = propCategoryData()
      ncount = nrow(data)
      if(ncount>0){
        res = lapply(1:ncount, function(i){
          FPrdCategoryNumber = data$FPrdCategoryNumber[i]
          FPrdCategoryName = data$FPrdCategoryName[i]
          FPropCategoryNumber = data$FPropCategoryNumber[i]
          FPropCategoryName =   data$FPropCategoryName[i]
          FPropValueNumber = input[[FPropCategoryNumber]]
          FPropValueName = propValue_queryByNumber(FPropNumber=FPropValueNumber)
          FPrdGroupNumber =prdGroupSelected()
          info = paste0("  产品大类:",FPrdCategoryNumber,"/",FPrdCategoryName," 产品分组:",FPrdGroupNumber,"  属性类别:",FPropCategoryNumber,"/",FPropCategoryName,"  属性选项:",FPropValueNumber,"/",FPropValueName)

          print(info)



        })
      }





    })


  })




}
