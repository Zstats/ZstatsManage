#' 检查包更新
#'
#' @description 检查本地已安装的包是否有新版本可用
#'
#' @param packages 要检查的包名向量，默认NULL检查所有已安装包
#' @param show_all 是否显示所有包（包括已是最新版本的），默认FALSE只显示需要更新的
#'
#' @return 返回一个数据框，包含包名、当前版本、最新版本等信息
#' @export
#'
#' @examples
#' \dontrun{
#' # 检查所有包
#' updates <- checkPackageUpdates()
#' 
#' # 检查特定包
#' updates <- checkPackageUpdates(c("package1", "package2"))
#' }
checkPackageUpdates <- function(packages = NULL, show_all = FALSE) {
  # 获取OSS上的包信息
  oss_packages <- readOSSJson("metadata/packages.json")$packages
  
  # 创建包名到信息的映射
  oss_map <- setNames(oss_packages, sapply(oss_packages, function(x) x$name))
  
  # 获取要检查的包列表
  if (is.null(packages)) {
    # 获取所有已安装的包
    installed_packages <- as.data.frame(installed.packages()[, c("Package", "Version")])
    packages <- installed_packages$Package
  } else {
    # 验证包是否已安装
    installed_packages <- as.data.frame(installed.packages()[, c("Package", "Version")])
    not_installed <- packages[!packages %in% installed_packages$Package]
    if (length(not_installed) > 0) {
      warning("以下包未安装: ", paste(not_installed, collapse = ", "))
    }
    packages <- packages[packages %in% installed_packages$Package]
  }
  
  # 检查每个包
  results <- data.frame(
    package = character(),
    current_version = character(),
    latest_version = character(),
    update_available = logical(),
    description = character(),
    stringsAsFactors = FALSE
  )
  
  for (pkg in packages) {
    if (pkg %in% names(oss_map)) {
      current_ver <- as.character(packageVersion(pkg))
      latest_ver <- oss_map[[pkg]]$current_version
      
      # 比较版本
      update_available <- compareVersion(current_ver, latest_ver) < 0
      
      if (show_all || update_available) {
        results <- rbind(results, data.frame(
          package = pkg,
          current_version = current_ver,
          latest_version = latest_ver,
          update_available = update_available,
          description = ifelse(
            !is.null(oss_map[[pkg]]$description_cn),
            oss_map[[pkg]]$description_cn,
            oss_map[[pkg]]$description
          ),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # 输出信息
  if (nrow(results) > 0) {
    updates_count <- sum(results$update_available)
    if (updates_count > 0) {
      message("发现 ", updates_count, " 个包有更新可用")
    } else {
      message("所有包都是最新版本")
    }
  } else {
    message("没有找到需要检查的包")
  }
  
  return(results)
}

#' 比较版本号
#'
#' @description 比较两个版本号的大小
#'
#' @param v1 版本号1
#' @param v2 版本号2
#'
#' @return -1 如果v1 < v2, 0 如果v1 = v2, 1 如果v1 > v2
#' @keywords internal
compareVersion <- function(v1, v2) {
  v1_parts <- as.numeric(strsplit(v1, "\\.")[[1]])
  v2_parts <- as.numeric(strsplit(v2, "\\.")[[1]])
  
  # 补齐长度
  max_len <- max(length(v1_parts), length(v2_parts))
  v1_parts <- c(v1_parts, rep(0, max_len - length(v1_parts)))
  v2_parts <- c(v2_parts, rep(0, max_len - length(v2_parts)))
  
  # 逐位比较
  for (i in seq_along(v1_parts)) {
    if (v1_parts[i] < v2_parts[i]) return(-1)
    if (v1_parts[i] > v2_parts[i]) return(1)
  }
  
  return(0)
}

#' 获取包的更新历史
#'
#' @description 获取指定包的版本更新历史
#'
#' @param package_name 包名
#' @param n_versions 显示最近几个版本，默认5
#'
#' @return 版本历史数据框
#' @export
#'
#' @examples
#' \dontrun{
#' history <- getUpdateHistory("package1")
#' }
getUpdateHistory <- function(package_name, n_versions = 5) {
  # 获取包信息
  oss_packages <- readOSSJson("metadata/packages.json")$packages
  
  # 查找包
  pkg_info <- NULL
  for (pkg in oss_packages) {
    if (pkg$name == package_name) {
      pkg_info <- pkg
      break
    }
  }
  
  if (is.null(pkg_info)) {
    stop("未找到包: ", package_name)
  }
  
  # 获取版本历史
  versions <- pkg_info$versions
  if (length(versions) > n_versions) {
    versions <- versions[1:n_versions]
  }
  
  # 转换为数据框
  history <- do.call(rbind, lapply(versions, function(v) {
    data.frame(
      version = v$version,
      release_date = v$release_date,
      size_mb = round(v$size / 1024 / 1024, 2),
      changes = v$changes,
      stringsAsFactors = FALSE
    )
  }))
  
  message("包 '", package_name, "' 的更新历史:")
  return(history)
}

#' 批量更新包
#'
#' @description 批量更新需要更新的包
#'
#' @param packages 要更新的包名向量，默认NULL更新所有可更新的包
#' @param ask 是否在更新前询问确认，默认TRUE
#'
#' @return 更新结果的数据框
#' @export
#'
#' @examples
#' \dontrun{
#' # 更新所有包
#' updatePackages()
#' 
#' # 更新特定包
#' updatePackages(c("package1", "package2"))
#' }
updatePackages <- function(packages = NULL, ask = TRUE) {
  # 检查更新
  updates <- checkPackageUpdates(packages, show_all = FALSE)
  
  if (nrow(updates) == 0) {
    message("没有需要更新的包")
    return(invisible(NULL))
  }
  
  # 显示待更新列表
  message("\n待更新的包:")
  print(updates[, c("package", "current_version", "latest_version")])
  
  # 询问确认
  if (ask) {
    response <- readline("是否继续更新？(y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("更新已取消")
      return(invisible(NULL))
    }
  }
  
  # 执行更新
  results <- data.frame(
    package = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(updates))) {
    pkg <- updates$package[i]
    message("\n正在更新 ", pkg, " ...")
    
    tryCatch({
      installFromOSS(pkg)
      results <- rbind(results, data.frame(
        package = pkg,
        status = "成功",
        message = paste("已更新到版本", updates$latest_version[i]),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      results <- rbind(results, data.frame(
        package = pkg,
        status = "失败",
        message = e$message,
        stringsAsFactors = FALSE
      ))
    })
  }
  
  # 显示结果摘要
  message("\n更新完成:")
  success_count <- sum(results$status == "成功")
  fail_count <- sum(results$status == "失败")
  message("成功: ", success_count, " 个包")
  if (fail_count > 0) {
    message("失败: ", fail_count, " 个包")
  }
  
  return(results)
} 