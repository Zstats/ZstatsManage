#' 获取OSS配置
#'
#' @description 获取预设的OSS配置信息（无需用户配置）
#'
#' @return 包含配置信息的列表
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' config <- getOSSConfig()
#' print(config$base_url)
#' }
getOSSConfig <- function() {
  # 预设的固定OSS配置
  base_url <- "https://zstats-rpackages.oss-cn-hangzhou.aliyuncs.com/"
  
  # 设置缓存目录
  cache_dir <- file.path(Sys.getenv("HOME"), ".zstatsManage")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 默认超时时间
  timeout <- 30
  
  list(
    base_url = base_url,
    cache_dir = cache_dir,
    timeout = timeout
  )
}

#' 读取OSS上的JSON配置文件
#'
#' @description 从OSS读取JSON格式的配置文件，支持缓存
#'
#' @param file_path 相对于OSS根目录的文件路径
#' @param use_cache 是否使用缓存，默认TRUE
#' @param cache_hours 缓存有效时间（小时），默认24小时
#'
#' @return 解析后的JSON数据
#' @keywords internal
readOSSJson <- function(file_path, use_cache = TRUE, cache_hours = 24) {
  config <- getOSSConfig()
  url <- paste0(config$base_url, file_path)
  
  # 缓存文件路径
  cache_file <- file.path(config$cache_dir, gsub("/", "_", file_path))
  
  # 检查缓存
  if (use_cache && file.exists(cache_file)) {
    cache_info <- file.info(cache_file)
    cache_age_hours <- as.numeric(difftime(Sys.time(), cache_info$mtime, units = "hours"))
    
    if (cache_age_hours < cache_hours) {
      message("使用缓存数据: ", file_path)
      return(jsonlite::fromJSON(cache_file, simplifyVector = FALSE))
    }
  }
  
  # 从OSS读取
  message("从OSS读取: ", file_path)
  
  tryCatch({
    response <- httr::GET(url, httr::timeout(config$timeout))
    httr::stop_for_status(response)
    
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    
    # 保存到缓存
    if (use_cache) {
      writeLines(content, cache_file)
    }
    
    jsonlite::fromJSON(content, simplifyVector = FALSE)
  }, error = function(e) {
    # 如果网络错误且有缓存，使用缓存
    if (use_cache && file.exists(cache_file)) {
      warning("网络错误，使用过期缓存: ", e$message)
      return(jsonlite::fromJSON(cache_file, simplifyVector = FALSE))
    }
    stop("无法读取OSS文件 '", file_path, "': ", e$message, "\n",
         "请检查网络连接或联系管理员")
  })
}

#' 清理缓存
#'
#' @description 清理本地缓存文件
#'
#' @param older_than 清理多少小时前的缓存，默认清理所有
#'
#' @return 清理的文件数量
#' @export
#'
#' @examples
#' \dontrun{
#' # 清理所有缓存
#' clearCache()
#' 
#' # 清理24小时前的缓存
#' clearCache(older_than = 24)
#' }
clearCache <- function(older_than = 0) {
  config <- getOSSConfig()
  
  if (!dir.exists(config$cache_dir)) {
    message("缓存目录不存在")
    return(invisible(0))
  }
  
  cache_files <- list.files(config$cache_dir, full.names = TRUE)
  
  if (length(cache_files) == 0) {
    message("没有缓存文件")
    return(invisible(0))
  }
  
  if (older_than > 0) {
    # 只清理旧文件
    file_info <- file.info(cache_files)
    age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
    cache_files <- cache_files[age_hours > older_than]
  }
  
  if (length(cache_files) > 0) {
    unlink(cache_files)
    message("已清理 ", length(cache_files), " 个缓存文件")
  } else {
    message("没有需要清理的缓存文件")
  }
  
  invisible(length(cache_files))
}

#' 测试OSS连接
#'
#' @description 测试到OSS服务器的连接是否正常
#'
#' @return 连接成功返回TRUE，失败返回FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' if (testOSSConnection()) {
#'   message("OSS连接正常")
#' } else {
#'   message("OSS连接失败")
#' }
#' }
testOSSConnection <- function() {
  config <- getOSSConfig()
  
  message("测试OSS连接: ", config$base_url)
  
  tryCatch({
    # 尝试读取包列表来测试连接
    response <- httr::GET(
      paste0(config$base_url, "metadata/packages.json"),
      httr::timeout(config$timeout)
    )
    
    if (httr::status_code(response) == 200) {
      message("OSS连接测试成功")
      return(TRUE)
    } else {
      message("OSS连接测试失败，HTTP状态码: ", httr::status_code(response))
      return(FALSE)
    }
  }, error = function(e) {
    message("OSS连接测试失败: ", e$message)
    return(FALSE)
  })
} 