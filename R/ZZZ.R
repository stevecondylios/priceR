.onLoad <- function(libname, pkgname){

  .cm <<- cachem::cache_disk(dir="memoise_cache",max_size = Inf)
  m_fromJSON <<- memoise::memoise(fromJSON, cache=.cm)

  #assign(".cm", cachem::cache_disk(dir="memoise",max_size = Inf) , envir = parent.env(environment()))
}
