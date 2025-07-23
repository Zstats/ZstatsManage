#' 从OSS安装包
#'
#' @description 从阿里云OSS下载并安装指定的R包
#'
#' @param package_name 包名
#' @param version 要安装的版本，默认NULL安装最新版本
#' @param dependencies 是否安装依赖包，默认TRUE
#' @param upgrade 是否升级依赖包，可选 "never", "ask", "always"
#' @param force 是否强制重新安装（即使已是最新版本），默认FALSE
#' @param verify_checksum 是否验证文件校验和，默认TRUE
#'
#' @return 安装成功返回TRUE，失败返回FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' # 安装最新版本
#' installFromOSS("package1")
#' 
#' # 安装特定版本
#' installFromOSS("package1", version = "1.0.0")
#' 
#' # 强制重新安装
#' installFromOSS("package1", force = TRUE)
#' }
installFromOSS <- function(package_name, 
                          version = NULL, 
                          dependencies = TRUE,
                          upgrade = "never",
                          force = FALSE,
                          verify_checksum = TRUE) {
  
  # 获取包信息
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
  
  # 确定要安装的版本
  if (is.null(version)) {
    version <- pkg_info$current_version
    version_info <- pkg_info$versions[[1]]
  } else {
    # 查找指定版本
    version_info <- NULL
    for (v in pkg_info$versions) {
      if (v$version == version) {
        version_info <- v
        break
      }
    }
    if (is.null(version_info)) {
      stop("未找到版本: ", version, "\n可用版本: ", 
           paste(sapply(pkg_info$versions, function(x) x$version), collapse = ", "))
    }
  }
  
  # 检查是否已安装
  is_installed <- package_name %in% rownames(installed.packages())
  if (is_installed && !force) {
    installed_version <- as.character(packageVersion(package_name))
    if (compareVersion(installed_version, version) >= 0) {
      message("包 '", package_name, "' 已安装版本 ", installed_version, 
              " (>= ", version, ")，使用 force = TRUE 强制重新安装")
      return(invisible(TRUE))
    }
  }
  
  # 构建下载URL
  config <- getOSSConfig()
  download_url <- paste0(config$base_url, version_info$file)
  
  # 创建临时文件
  temp_file <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(temp_file), add = TRUE)
  
  message("正在下载包 '", package_name, "' 版本 ", version, " ...")
  message("从: ", download_url)
  
  # 下载文件
  tryCatch({
    response <- httr::GET(
      download_url, 
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::progress(),
      httr::timeout(config$timeout * 10)  # 下载使用更长的超时时间
    )
    httr::stop_for_status(response)
  }, error = function(e) {
    stop("下载失败: ", e$message)
  })
  
  # 验证校验和
  if (verify_checksum && !is.null(version_info$sha256)) {
    message("验证文件完整性...")
    file_hash <- digest::digest(temp_file, algo = "sha256", file = TRUE)
    if (file_hash != version_info$sha256) {
      # 如果SHA256不匹配，也尝试MD5校验
      if (!is.null(version_info$md5)) {
        file_md5 <- digest::digest(temp_file, algo = "md5", file = TRUE)
        if (file_md5 != version_info$md5) {
          stop("文件校验和不匹配（SHA256和MD5都不匹配）！文件可能已损坏。")
        } else {
          message("MD5校验和验证通过（SHA256不匹配但MD5匹配）")
        }
      } else {
        stop("文件SHA256校验和不匹配！文件可能已损坏。")
      }
    } else {
      message("SHA256校验和验证通过")
    }
  }
  
  # 安装包
  message("正在安装包...")
  
  # 处理依赖
  if (dependencies) {
    # 检查依赖是否满足
    deps <- pkg_info$dependencies
    if (!is.null(deps) && length(deps) > 0) {
      message("检查依赖包: ", paste(deps, collapse = ", "))
    }
  }
  
  # 执行安装
  tryCatch({
    install.packages(
      temp_file,
      repos = NULL,
      type = "source",
      dependencies = dependencies,
      upgrade = upgrade,
      quiet = FALSE
    )
    
    # 验证安装
    if (package_name %in% rownames(installed.packages())) {
      new_version <- as.character(packageVersion(package_name))
      message("\n成功安装 '", package_name, "' 版本 ", new_version)
      return(invisible(TRUE))
    } else {
      stop("安装似乎失败了，包未找到")
    }
  }, error = function(e) {
    stop("安装失败: ", e$message)
  })
}

#' 批量安装包
#'
#' @description 批量安装多个包
#'
#' @param packages 包名向量
#' @param ask 是否在安装前询问确认，默认TRUE
#' @param stop_on_error 遇到错误是否停止，默认FALSE继续安装其他包
#'
#' @return 安装结果的数据框
#' @export
#'
#' @examples
#' \dontrun{
#' # 安装多个包
#' results <- installPackages(c("package1", "package2", "package3"))
#' }
installPackages <- function(packages, ask = TRUE, stop_on_error = FALSE) {
  if (length(packages) == 0) {
    stop("请指定要安装的包")
  }
  
  # 获取包信息
  all_packages <- readOSSJson("metadata/packages.json")$packages
  package_map <- setNames(all_packages, sapply(all_packages, function(x) x$name))
  
  # 验证包是否存在
  not_found <- packages[!packages %in% names(package_map)]
  if (length(not_found) > 0) {
    warning("以下包未找到: ", paste(not_found, collapse = ", "))
    packages <- packages[packages %in% names(package_map)]
  }
  
  if (length(packages) == 0) {
    stop("没有有效的包可以安装")
  }
  
  # 显示待安装列表
  message("\n待安装的包:")
  install_info <- data.frame(
    package = packages,
    version = sapply(packages, function(p) package_map[[p]]$current_version),
    size_mb = sapply(packages, function(p) {
      round(package_map[[p]]$versions[[1]]$size / 1024 / 1024, 2)
    }),
    stringsAsFactors = FALSE
  )
  print(install_info)
  
  # 计算总大小
  total_size <- sum(install_info$size_mb)
  message("\n总下载大小: ", round(total_size, 2), " MB")
  
  # 询问确认
  if (ask) {
    response <- readline("是否继续安装？(y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("安装已取消")
      return(invisible(NULL))
    }
  }
  
  # 执行安装
  results <- data.frame(
    package = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  
  for (pkg in packages) {
    message("\n正在处理 ", pkg, " ...")
    
    tryCatch({
      installFromOSS(pkg)
      results <- rbind(results, data.frame(
        package = pkg,
        status = "成功",
        message = paste("已安装版本", package_map[[pkg]]$current_version),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      results <- rbind(results, data.frame(
        package = pkg,
        status = "失败", 
        message = e$message,
        stringsAsFactors = FALSE
      ))
      if (stop_on_error) {
        stop("安装中止: ", e$message)
      }
    })
  }
  
  # 显示结果摘要
  message("\n\n安装完成:")
  success_count <- sum(results$status == "成功")
  fail_count <- sum(results$status == "失败")
  message("成功: ", success_count, " 个包")
  if (fail_count > 0) {
    message("失败: ", fail_count, " 个包")
    message("\n失败的包:")
    failed <- results[results$status == "失败", ]
    for (i in seq_len(nrow(failed))) {
      message("  - ", failed$package[i], ": ", failed$message[i])
    }
  }
  
  return(results)
}

#' 下载包但不安装
#'
#' @description 从OSS下载包文件到本地目录，但不安装
#'
#' @param package_name 包名
#' @param version 版本号，默认NULL下载最新版本
#' @param dest_dir 目标目录，默认当前工作目录
#' @param verify_checksum 是否验证校验和，默认TRUE
#'
#' @return 下载的文件路径
#' @export
#'
#' @examples
#' \dontrun{
#' # 下载最新版本
#' file_path <- downloadPackage("package1")
#' 
#' # 下载到指定目录
#' file_path <- downloadPackage("package1", dest_dir = "~/Downloads")
#' }
downloadPackage <- function(package_name, 
                           version = NULL, 
                           dest_dir = ".",
                           verify_checksum = TRUE) {
  
  # 获取包信息
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
  
  # 确定版本
  if (is.null(version)) {
    version <- pkg_info$current_version
    version_info <- pkg_info$versions[[1]]
  } else {
    # 查找指定版本
    version_info <- NULL
    for (v in pkg_info$versions) {
      if (v$version == version) {
        version_info <- v
        break
      }
    }
    if (is.null(version_info)) {
      stop("未找到版本: ", version)
    }
  }
  
  # 构建文件名和路径
  file_name <- basename(version_info$file)
  dest_file <- file.path(dest_dir, file_name)
  
  # 检查目标目录
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # 构建下载URL
  config <- getOSSConfig()
  download_url <- paste0(config$base_url, version_info$file)
  
  message("正在下载包 '", package_name, "' 版本 ", version, " ...")
  message("从: ", download_url)
  message("到: ", dest_file)
  
  # 下载文件
  tryCatch({
    response <- httr::GET(
      download_url,
      httr::write_disk(dest_file, overwrite = TRUE),
      httr::progress(),
      httr::timeout(config$timeout * 10)
    )
    httr::stop_for_status(response)
  }, error = function(e) {
    if (file.exists(dest_file)) unlink(dest_file)
    stop("下载失败: ", e$message)
  })
  
  # 验证校验和
  if (verify_checksum && !is.null(version_info$sha256)) {
    message("验证文件完整性...")
    file_hash <- digest::digest(dest_file, algo = "sha256", file = TRUE)
    if (file_hash != version_info$sha256) {
      # 如果SHA256不匹配，也尝试MD5校验
      if (!is.null(version_info$md5)) {
        file_md5 <- digest::digest(dest_file, algo = "md5", file = TRUE)
        if (file_md5 != version_info$md5) {
          unlink(dest_file)
          stop("文件校验和不匹配（SHA256和MD5都不匹配）！文件可能已损坏。")
        } else {
          message("MD5校验和验证通过（SHA256不匹配但MD5匹配）")
        }
      } else {
        unlink(dest_file)
        stop("文件SHA256校验和不匹配！文件可能已损坏。")
      }
    } else {
      message("SHA256校验和验证通过")
    }
  }
  
  # 显示文件信息
  file_size_mb <- round(file.info(dest_file)$size / 1024 / 1024, 2)
  message("\n下载完成!")
  message("文件: ", dest_file)
  message("大小: ", file_size_mb, " MB")
  
  return(invisible(dest_file))
} 