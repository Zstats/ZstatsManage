#' 列出可用的包
#'
#' @description 列出OSS上所有可用的R包及其信息
#'
#' @param pattern 包名过滤模式（正则表达式），默认NULL显示所有包
#' @param tags 标签过滤，只显示包含指定标签的包
#' @param show_versions 是否显示所有版本信息，默认FALSE只显示最新版本
#'
#' @return 包信息的数据框
#' @export
#'
#' @examples
#' \dontrun{
#' # 列出所有包
#' packages <- listAvailablePackages()
#' 
#' # 按名称过滤
#' packages <- listAvailablePackages(pattern = "^data")
#' 
#' # 按标签过滤
#' packages <- listAvailablePackages(tags = "statistics")
#' }
listAvailablePackages <- function(pattern = NULL, tags = NULL, show_versions = FALSE) {
  # 获取包列表
  packages_data <- readOSSJson("metadata/packages.json")
  packages <- packages_data$packages
  
  # 名称过滤
  if (!is.null(pattern)) {
    matches <- grep(pattern, sapply(packages, function(x) x$name), perl = TRUE)
    packages <- packages[matches]
  }
  
  # 标签过滤
  if (!is.null(tags)) {
    packages <- Filter(function(pkg) {
      pkg_tags <- pkg$tags
      if (is.null(pkg_tags)) return(FALSE)
      any(tags %in% pkg_tags)
    }, packages)
  }
  
  if (length(packages) == 0) {
    message("没有找到符合条件的包")
    return(data.frame())
  }
  
  # 构建结果数据框
  if (show_versions) {
    # 显示所有版本
    results <- do.call(rbind, lapply(packages, function(pkg) {
      versions_df <- do.call(rbind, lapply(pkg$versions, function(v) {
        data.frame(
          package = pkg$name,
          version = v$version,
          release_date = v$release_date,
          size_mb = round(v$size / 1024 / 1024, 2),
          description = ifelse(
            !is.null(pkg$description_cn),
            pkg$description_cn,
            pkg$description
          ),
          author = pkg$author,
          license = pkg$license,
          stringsAsFactors = FALSE
        )
      }))
      versions_df
    }))
  } else {
    # 只显示最新版本
    results <- do.call(rbind, lapply(packages, function(pkg) {
      latest_version <- pkg$versions[[1]]  # 假设第一个是最新版本
      data.frame(
        package = pkg$name,
        version = pkg$current_version,
        release_date = latest_version$release_date,
        size_mb = round(latest_version$size / 1024 / 1024, 2),
        description = ifelse(
          !is.null(pkg$description_cn),
          pkg$description_cn,
          pkg$description
        ),
        author = pkg$author,
        license = pkg$license,
        tags = paste(pkg$tags, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # 输出摘要信息
  message("找到 ", length(packages), " 个包")
  if (!show_versions) {
    message("使用 show_versions = TRUE 查看所有版本")
  }
  
  return(results)
}

#' 获取包的详细信息
#'
#' @description 获取指定包的详细信息，包括所有版本、依赖等
#'
#' @param package_name 包名
#'
#' @return 包含详细信息的列表
#' @export
#'
#' @examples
#' \dontrun{
#' info <- getPackageInfo("package1")
#' print(info$description)
#' print(info$dependencies)
#' }
getPackageInfo <- function(package_name) {
  # 获取包列表
  packages <- readOSSJson("metadata/packages.json")$packages
  
  # 查找包
  pkg_info <- NULL
  for (pkg in packages) {
    if (pkg$name == package_name) {
      pkg_info <- pkg
      break
    }
  }
  
  if (is.null(pkg_info)) {
    stop("未找到包: ", package_name)
  }
  
  # 检查是否已安装
  is_installed <- package_name %in% rownames(installed.packages())
  installed_version <- if (is_installed) as.character(packageVersion(package_name)) else NA
  
  # 构建详细信息
  info <- list(
    name = pkg_info$name,
    current_version = pkg_info$current_version,
    description = pkg_info$description,
    description_cn = pkg_info$description_cn,
    author = pkg_info$author,
    license = pkg_info$license,
    homepage = pkg_info$homepage,
    dependencies = pkg_info$dependencies,
    tags = pkg_info$tags,
    is_installed = is_installed,
    installed_version = installed_version,
    update_available = if (is_installed) {
      compareVersion(installed_version, pkg_info$current_version) < 0
    } else {
      FALSE
    },
    versions = pkg_info$versions,
    total_versions = length(pkg_info$versions)
  )
  
  # 打印信息
  cat("\n包信息: ", info$name, "\n")
  cat("====================\n")
  cat("最新版本: ", info$current_version, "\n")
  cat("描述: ", ifelse(!is.null(info$description_cn), info$description_cn, info$description), "\n")
  cat("作者: ", info$author, "\n")
  cat("许可证: ", info$license, "\n")
  if (!is.null(info$homepage)) {
    cat("主页: ", info$homepage, "\n")
  }
  cat("标签: ", paste(info$tags, collapse = ", "), "\n")
  cat("\n安装状态:\n")
  if (info$is_installed) {
    cat("  已安装版本: ", info$installed_version, "\n")
    if (info$update_available) {
      cat("  有更新可用!\n")
    } else {
      cat("  已是最新版本\n")
    }
  } else {
    cat("  未安装\n")
  }
  cat("\n依赖包:\n")
  for (dep in info$dependencies) {
    cat("  - ", dep, "\n")
  }
  cat("\n可用版本: ", info$total_versions, " 个\n")
  
  invisible(info)
}

#' 搜索包
#'
#' @description 根据关键词搜索包（在名称、描述、标签中搜索）
#'
#' @param keyword 搜索关键词
#' @param fields 搜索字段，可选 "name", "description", "tags", "all"
#'
#' @return 匹配的包信息数据框
#' @export
#'
#' @examples
#' \dontrun{
#' # 搜索包含"data"的包
#' results <- searchPackages("data")
#' 
#' # 只在描述中搜索
#' results <- searchPackages("analysis", fields = "description")
#' }
searchPackages <- function(keyword, fields = "all") {
  # 获取包列表
  packages <- readOSSJson("metadata/packages.json")$packages
  
  # 转换为小写进行不区分大小写的搜索
  keyword_lower <- tolower(keyword)
  
  # 搜索函数
  search_in_package <- function(pkg) {
    if (fields == "all" || fields == "name") {
      if (grepl(keyword_lower, tolower(pkg$name))) return(TRUE)
    }
    
    if (fields == "all" || fields == "description") {
      if (!is.null(pkg$description) && grepl(keyword_lower, tolower(pkg$description))) return(TRUE)
      if (!is.null(pkg$description_cn) && grepl(keyword_lower, tolower(pkg$description_cn))) return(TRUE)
    }
    
    if (fields == "all" || fields == "tags") {
      if (!is.null(pkg$tags)) {
        tags_lower <- tolower(paste(pkg$tags, collapse = " "))
        if (grepl(keyword_lower, tags_lower)) return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  # 过滤包
  matched_packages <- Filter(search_in_package, packages)
  
  if (length(matched_packages) == 0) {
    message("没有找到匹配 '", keyword, "' 的包")
    return(data.frame())
  }
  
  # 构建结果
  results <- do.call(rbind, lapply(matched_packages, function(pkg) {
    data.frame(
      package = pkg$name,
      version = pkg$current_version,
      description = ifelse(
        !is.null(pkg$description_cn),
        pkg$description_cn,
        pkg$description
      ),
      tags = paste(pkg$tags, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  
  message("找到 ", nrow(results), " 个匹配的包")
  return(results)
}

#' 获取包的标签列表
#'
#' @description 获取所有包使用的标签及其频率
#'
#' @return 标签频率数据框
#' @export
#'
#' @examples
#' \dontrun{
#' tags <- getPackageTags()
#' print(tags)
#' }
getPackageTags <- function() {
  # 获取包列表
  packages <- readOSSJson("metadata/packages.json")$packages
  
  # 收集所有标签
  all_tags <- unlist(lapply(packages, function(pkg) pkg$tags))
  
  if (length(all_tags) == 0) {
    message("没有找到任何标签")
    return(data.frame())
  }
  
  # 计算频率
  tag_freq <- as.data.frame(table(all_tags))
  names(tag_freq) <- c("tag", "count")
  tag_freq <- tag_freq[order(tag_freq$count, decreasing = TRUE), ]
  
  message("找到 ", nrow(tag_freq), " 个不同的标签")
  return(tag_freq)
} 