% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta.R
\name{meta_upload}
\alias{meta_upload}
\title{上传元数据}
\usage{
meta_upload(
  conn = tsda::conn_rds("metadata"),
  file_name = "data-raw/data/rds_mtrl_productCategory.xlsx"
)
}
\arguments{
\item{file_name}{数据表}
}
\value{
返回值
}
\description{
上传元数据
}
\examples{
meta_upload()
}
