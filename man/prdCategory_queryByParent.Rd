% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prdCategory.R
\name{prdCategory_queryByParent}
\alias{prdCategory_queryByParent}
\title{按上级组进行查询产品大类}
\usage{
prdCategory_queryByParent(
  conn_cfg = "config/conn_cfg.R",
  FParentPrdCategory = "实验耗材"
)
}
\arguments{
\item{conn_cfg}{连接配置}

\item{FParentPrdCategory}{上级组}
}
\value{
返回值
}
\description{
按上级组进行查询产品大类
}
\examples{
prdCategory_queryByParent()
}
