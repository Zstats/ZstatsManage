#' 检测文件格式
#'
#' @description 检测文件是zip还是tar.gz格式
#'
#' @param file_path 文件路径
#'
#' @return "zip"或"tar.gz"
#' @keywords internal
detectFileFormat <- function(file_path) {
  # 读取文件头部字节
  con <- file(file_path, "rb")
  on.exit(close(con))
  
  # 读取前4个字节
  header <- readBin(con, "raw", n = 4)
  
  if (length(header) >= 4) {
    # zip文件的魔数：50 4B 03 04 (PK..)
    if (header[1] == as.raw(0x50) && header[2] == as.raw(0x4B) && 
        header[3] == as.raw(0x03) && header[4] == as.raw(0x04)) {
      return("zip")
    }
    # tar.gz文件通常以1F 8B开头（gzip魔数）
    if (header[1] == as.raw(0x1F) && header[2] == as.raw(0x8B)) {
      return("tar.gz")
    }
  }
  
  # 如果无法确定，根据文件扩展名判断
  if (grepl("\\.zip$", file_path, ignore.case = TRUE)) {
    return("zip")
  } else if (grepl("\\.tar\\.gz$", file_path, ignore.case = TRUE)) {
    return("tar.gz")
  }
  
  return("unknown")
}

#' 解析版本要求
#'
#' @description 解析版本要求字符串（如">= 1.0.0", "== 2.1.0"等）
#'
#' @param version_req 版本要求字符串
#'
#' @return 包含操作符和版本号的列表
#' @keywords internal
parseVersionRequirement <- function(version_req) {
  # 移除空格
  version_req <- gsub("\\s+", "", version_req)
  
  # 匹配操作符和版本号
  if (grepl("^>=", version_req)) {
    operator <- ">="
    version <- sub("^>=", "", version_req)
  } else if (grepl("^<=", version_req)) {
    operator <- "<="
    version <- sub("^<=", "", version_req)
  } else if (grepl("^==", version_req)) {
    operator <- "=="
    version <- sub("^==", "", version_req)
  } else if (grepl("^>", version_req)) {
    operator <- ">"
    version <- sub("^>", "", version_req)
  } else if (grepl("^<", version_req)) {
    operator <- "<"
    version <- sub("^<", "", version_req)
  } else {
    # 如果没有操作符，默认为 >=
    operator <- ">="
    version <- version_req
  }
  
  list(operator = operator, version = version)
}

#' 检查版本是否满足要求
#'
#' @description 检查当前版本是否满足版本要求
#'
#' @param current_version 当前版本
#' @param version_req 版本要求字符串
#'
#' @return 逻辑值，满足要求返回TRUE
#' @keywords internal
checkVersionRequirement <- function(current_version, version_req) {
  parsed <- parseVersionRequirement(version_req)
  comp_result <- compareVersion(current_version, parsed$version)
  
  switch(parsed$operator,
    ">=" = comp_result >= 0,
    "<=" = comp_result <= 0,
    "==" = comp_result == 0,
    ">" = comp_result > 0,
    "<" = comp_result < 0,
    TRUE  # 默认返回TRUE
  )
}

#' 检查R版本要求
#'
#' @description 检查当前R版本是否满足要求
#'
#' @param r_version_req R版本要求字符串（如"R (>= 4.0.0)"）
#'
#' @return 逻辑值，满足要求返回TRUE
#' @keywords internal
checkRVersion <- function(r_version_req) {
  # 提取版本要求
  version_match <- regmatches(r_version_req, regexpr("[>=<]+\\s*[0-9\\.]+", r_version_req))
  if (length(version_match) == 0) {
    warning("无法解析R版本要求: ", r_version_req)
    return(TRUE)
  }
  
  current_r_version <- paste0(R.Version()$major, ".", R.Version()$minor)
  return(checkVersionRequirement(current_r_version, version_match))
}

#' 检查CRAN包依赖
#'
#' @description 检查CRAN包依赖是否满足，并返回需要安装的包
#'
#' @param cran_deps CRAN包依赖列表
#' @param check_only 是否只检查不安装，默认FALSE
#'
#' @return 包含检查结果的数据框
#' @keywords internal
checkCranDependencies <- function(cran_deps, check_only = FALSE) {
  if (length(cran_deps) == 0) {
    return(data.frame(
      package = character(),
      required = logical(),
      installed = logical(),
      version_ok = logical(),
      current_version = character(),
      required_version = character(),
      action = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  results <- data.frame(
    package = character(),
    required = logical(),
    installed = logical(),
    version_ok = logical(),
    current_version = character(),
    required_version = character(),
    action = character(),
    stringsAsFactors = FALSE
  )
  
  installed_packages <- installed.packages()
  
  for (dep in cran_deps) {
    pkg_name <- dep$name
    version_req <- dep$version
    required <- dep$required
    
    is_installed <- pkg_name %in% rownames(installed_packages)
    current_version <- if (is_installed) as.character(packageVersion(pkg_name)) else ""
    version_ok <- if (is_installed) checkVersionRequirement(current_version, version_req) else FALSE
    
    action <- "none"
    if (!is_installed && required) {
      action <- "install"
    } else if (is_installed && !version_ok && required) {
      action <- "update"
    } else if (!is_installed && !required) {
      action <- "optional"
    }
    
    results <- rbind(results, data.frame(
      package = pkg_name,
      required = required,
      installed = is_installed,
      version_ok = version_ok,
      current_version = current_version,
      required_version = version_req,
      action = action,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' 检查OSS包依赖
#'
#' @description 检查OSS包依赖是否满足，并返回需要安装的包
#'
#' @param oss_deps OSS包依赖列表
#' @param all_oss_packages 所有OSS包信息
#' @param check_only 是否只检查不安装，默认FALSE
#'
#' @return 包含检查结果的数据框
#' @keywords internal
checkOssDependencies <- function(oss_deps, all_oss_packages, check_only = FALSE) {
  if (length(oss_deps) == 0) {
    return(data.frame(
      package = character(),
      required = logical(),
      installed = logical(),
      version_ok = logical(),
      current_version = character(),
      required_version = character(),
      available_version = character(),
      action = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  results <- data.frame(
    package = character(),
    required = logical(),
    installed = logical(),
    version_ok = logical(),
    current_version = character(),
    required_version = character(),
    available_version = character(),
    action = character(),
    stringsAsFactors = FALSE
  )
  
  installed_packages <- installed.packages()
  oss_package_map <- setNames(all_oss_packages, sapply(all_oss_packages, function(x) x$name))
  
  for (dep in oss_deps) {
    pkg_name <- dep$name
    version_req <- dep$version
    required <- dep$required
    
    is_installed <- pkg_name %in% rownames(installed_packages)
    current_version <- if (is_installed) as.character(packageVersion(pkg_name)) else ""
    
    # 检查OSS上是否有这个包
    available_version <- if (pkg_name %in% names(oss_package_map)) {
      oss_package_map[[pkg_name]]$current_version
    } else {
      ""
    }
    
    version_ok <- if (is_installed && available_version != "") {
      checkVersionRequirement(current_version, version_req)
    } else {
      FALSE
    }
    
    action <- "none"
    if (!is_installed && required && available_version != "") {
      action <- "install"
    } else if (is_installed && !version_ok && required && available_version != "") {
      action <- "update"
    } else if (!is_installed && !required && available_version != "") {
      action <- "optional"
    } else if (available_version == "") {
      action <- "unavailable"
    }
    
    results <- rbind(results, data.frame(
      package = pkg_name,
      required = required,
      installed = is_installed,
      version_ok = version_ok,
      current_version = current_version,
      required_version = version_req,
      available_version = available_version,
      action = action,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' 安装CRAN依赖包
#'
#' @description 安装必需的CRAN包依赖
#'
#' @param cran_deps_check CRAN依赖检查结果
#' @param install_optional 是否安装可选依赖，默认FALSE
#'
#' @return 安装结果列表
#' @keywords internal
installCranDependencies <- function(cran_deps_check, install_optional = FALSE) {
  # 筛选需要安装的包
  to_install <- cran_deps_check[
    cran_deps_check$action %in% c("install", "update") |
    (cran_deps_check$action == "optional" & install_optional), 
  ]
  
  if (nrow(to_install) == 0) {
    message("  所有CRAN依赖都已满足")
    return(list(success = TRUE, installed = character()))
  }
  
  message("  需要安装的CRAN包: ", paste(to_install$package, collapse = ", "))
  
  installed_packages <- character()
  failed_packages <- character()
  
  for (i in seq_len(nrow(to_install))) {
    pkg <- to_install$package[i]
    action <- to_install$action[i]
    
    message("    ", action, " CRAN包: ", pkg)
    
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      installed_packages <- c(installed_packages, pkg)
      message("      成功")
    }, error = function(e) {
      failed_packages <- c(failed_packages, pkg)
      message("      失败: ", e$message)
    })
  }
  
  list(
    success = length(failed_packages) == 0,
    installed = installed_packages,
    failed = failed_packages
  )
}

#' 安装OSS依赖包
#'
#' @description 按照依赖顺序安装OSS包依赖
#'
#' @param oss_deps_check OSS依赖检查结果
#' @param install_optional 是否安装可选依赖，默认FALSE
#'
#' @return 安装结果列表
#' @keywords internal
installOssDependencies <- function(oss_deps_check, install_optional = FALSE) {
  # 筛选需要安装的包
  to_install <- oss_deps_check[
    oss_deps_check$action %in% c("install", "update") |
    (oss_deps_check$action == "optional" & install_optional), 
  ]
  
  if (nrow(to_install) == 0) {
    message("  所有OSS依赖都已满足")
    return(list(success = TRUE, installed = character()))
  }
  
  message("  需要安装的OSS包: ", paste(to_install$package, collapse = ", "))
  
  installed_packages <- character()
  failed_packages <- character()
  
  for (i in seq_len(nrow(to_install))) {
    pkg <- to_install$package[i]
    action <- to_install$action[i]
    
    message("    ", action, " OSS包: ", pkg)
    
    tryCatch({
      # 递归调用installFromOSS，但设置check_dependencies=FALSE避免无限循环
      installFromOSS(pkg, dependencies = FALSE, force = (action == "update"))
      installed_packages <- c(installed_packages, pkg)
      message("      成功")
    }, error = function(e) {
      failed_packages <- c(failed_packages, pkg)
      message("      失败: ", e$message)
    })
  }
  
  list(
    success = length(failed_packages) == 0,
    installed = installed_packages,
    failed = failed_packages
  )
}

#' 检查和安装包依赖
#'
#' @description 全面检查并安装包的所有依赖
#'
#' @param pkg_info 包信息
#' @param all_oss_packages 所有OSS包信息
#' @param install_optional 是否安装可选依赖，默认FALSE
#' @param check_only 是否只检查不安装，默认FALSE
#'
#' @return 依赖检查和安装结果
#' @keywords internal
checkAndInstallDependencies <- function(pkg_info, all_oss_packages, 
                                       install_optional = FALSE, check_only = FALSE) {
  message("检查包 '", pkg_info$name, "' 的依赖...")
  
  deps <- pkg_info$dependencies
  
  # 1. 检查R版本
  if (!is.null(deps$r_version)) {
    message("  检查R版本要求: ", deps$r_version)
    if (!checkRVersion(deps$r_version)) {
      stop("R版本不满足要求: ", deps$r_version, 
           "，当前版本: ", R.Version()$version.string)
    }
  }
  
  results <- list(
    r_version_ok = TRUE,
    cran_deps = NULL,
    oss_deps = NULL,
    install_results = list()
  )
  
  # 2. 检查CRAN包依赖
  if (!is.null(deps$cran_packages) && length(deps$cran_packages) > 0) {
    message("  检查CRAN包依赖...")
    results$cran_deps <- checkCranDependencies(deps$cran_packages, check_only)
    
    if (!check_only) {
      cran_install_result <- installCranDependencies(results$cran_deps, install_optional)
      results$install_results$cran <- cran_install_result
    }
  }
  
  # 3. 检查OSS包依赖
  if (!is.null(deps$oss_packages) && length(deps$oss_packages) > 0) {
    message("  检查OSS包依赖...")
    results$oss_deps <- checkOssDependencies(deps$oss_packages, all_oss_packages, check_only)
    
    if (!check_only) {
      oss_install_result <- installOssDependencies(results$oss_deps, install_optional)
      results$install_results$oss <- oss_install_result
    }
  }
  
  # 4. 检查系统依赖（目前只是警告）
  if (!is.null(deps$system_dependencies) && length(deps$system_dependencies) > 0) {
    message("  检查系统依赖...")
    for (sys_dep in deps$system_dependencies) {
      if (sys_dep$required) {
        message("    需要系统依赖: ", sys_dep$name, " - ", sys_dep$description)
        if (!is.null(sys_dep$install_command)) {
          message("      安装命令: ", sys_dep$install_command)
        }
      }
    }
  }
  
  return(results)
}

#' 通过直接解压安装包
#'
#' @description 直接解压zip文件到R库目录进行安装
#'
#' @param zip_file zip文件路径
#' @param package_name 包名
#'
#' @return 安装成功返回TRUE，失败返回FALSE
#' @keywords internal
installByExtraction <- function(zip_file, package_name) {
  # 获取R库路径
  lib_paths <- .libPaths()
  target_lib <- lib_paths[1]  # 使用第一个可写的库路径
  
  # 检查库路径是否可写
  if (!file.access(target_lib, mode = 2) == 0) {
    stop("R库目录不可写: ", target_lib)
  }
  
  # 创建临时目录用于解压
  temp_dir <- tempfile("pkg_direct_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  tryCatch({
    message("  解压zip文件到临时目录...")
    # 解压zip文件
    utils::unzip(zip_file, exdir = temp_dir)
    
    # 查找包目录
    extracted_items <- list.files(temp_dir, full.names = TRUE)
    package_dir <- NULL
    
    # 优先查找与包名匹配的目录
    for (item in extracted_items) {
      if (dir.exists(item)) {
        item_name <- basename(item)
        if (item_name == package_name && file.exists(file.path(item, "DESCRIPTION"))) {
          package_dir <- item
          break
        }
      }
    }
    
    # 如果没找到匹配的，查找任何包含DESCRIPTION的目录
    if (is.null(package_dir)) {
      for (item in extracted_items) {
        if (dir.exists(item) && file.exists(file.path(item, "DESCRIPTION"))) {
          package_dir <- item
          break
        }
      }
    }
    
    if (is.null(package_dir)) {
      stop("未找到有效的R包目录（缺少DESCRIPTION文件）")
    }
    
    message("  找到包目录: ", basename(package_dir))
    
    # 目标安装路径
    install_path <- file.path(target_lib, package_name)
    
    # 如果包已存在，先删除
    if (dir.exists(install_path)) {
      message("  删除现有包目录...")
      unlink(install_path, recursive = TRUE)
      # 等待文件系统操作完成
      Sys.sleep(0.5)
    }
    
    message("  复制包文件到R库目录...")
    # 复制包目录到R库
    success <- file.copy(package_dir, target_lib, recursive = TRUE)
    
    if (!success) {
      stop("复制包文件失败")
    }
    
    # 确保目录名正确
    copied_dir <- file.path(target_lib, basename(package_dir))
    if (basename(package_dir) != package_name && dir.exists(copied_dir)) {
      message("  重命名包目录为正确名称...")
      if (dir.exists(install_path)) {
        unlink(install_path, recursive = TRUE)
        Sys.sleep(0.5)
      }
      success_rename <- file.rename(copied_dir, install_path)
      if (!success_rename) {
        warning("重命名失败，但包可能已成功安装在: ", copied_dir)
      }
    }
    
    # 验证安装结果
    final_path <- if (dir.exists(install_path)) install_path else copied_dir
    
    if (dir.exists(final_path) && file.exists(file.path(final_path, "DESCRIPTION"))) {
      message("  包已成功安装到: ", final_path)
      return(TRUE)
    } else {
      stop("安装验证失败，目标目录不存在或无效")
    }
    
  }, error = function(e) {
    message("直接解压安装失败: ", e$message)
    return(FALSE)
  })
}

#' 转换zip包为tar.gz格式
#'
#' @description 将zip格式的R包转换为tar.gz格式以便安装
#'
#' @param zip_file zip文件路径
#' @param output_file 输出的tar.gz文件路径
#'
#' @return 转换成功返回TRUE，失败返回FALSE
#' @keywords internal
convertZipToTarGz <- function(zip_file, output_file) {
  # 创建临时目录用于解压
  temp_dir <- tempfile("pkg_extract_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  tryCatch({
    message("  解压zip文件...")
    # 解压zip文件
    utils::unzip(zip_file, exdir = temp_dir)
    
    # 获取解压后的内容
    extracted_items <- list.files(temp_dir, full.names = TRUE)
    
    if (length(extracted_items) == 0) {
      stop("zip文件为空")
    }
    
    # 查找R包目录
    package_dir <- NULL
    if (length(extracted_items) == 1 && dir.exists(extracted_items[1])) {
      # 如果只有一个目录，检查是否为R包目录
      candidate_dir <- extracted_items[1]
      if (file.exists(file.path(candidate_dir, "DESCRIPTION"))) {
        package_dir <- candidate_dir
      }
    }
    
    # 如果没找到明显的包目录，在所有内容中查找DESCRIPTION文件
    if (is.null(package_dir)) {
      for (item in extracted_items) {
        if (dir.exists(item) && file.exists(file.path(item, "DESCRIPTION"))) {
          package_dir <- item
          break
        }
      }
    }
    
    if (is.null(package_dir)) {
      stop("未找到有效的R包目录（缺少DESCRIPTION文件）")
    }
    
    message("  找到R包目录: ", basename(package_dir))
    
    # 获取包目录名
    pkg_name <- basename(package_dir)
    
    # 切换到包目录的父目录
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dirname(package_dir))
    
    message("  创建tar.gz文件...")
    
    # 在Windows下使用特殊的tar处理
    if (.Platform$OS.type == "windows") {
      # 尝试使用系统的tar命令
      tar_cmd <- "tar"
      if (nzchar(Sys.which("tar"))) {
        tar_cmd <- Sys.which("tar")
      } else if (file.exists("C:/Windows/System32/tar.exe")) {
        tar_cmd <- "C:/Windows/System32/tar.exe"
      }
      
      # 构建tar命令
      cmd <- sprintf('"%s" -czf "%s" "%s"', 
                     tar_cmd, 
                     normalizePath(output_file, mustWork = FALSE), 
                     basename(package_dir))
      
      message("  执行命令: ", cmd)
      result <- system(cmd, intern = FALSE)
      
      if (result == 0 && file.exists(output_file)) {
        message("  tar.gz文件创建成功")
        return(TRUE)
      } else {
        message("  系统tar命令失败，尝试R内置tar...")
      }
    }
    
    # 使用R内置的tar函数
    tar_result <- utils::tar(
      tarfile = output_file, 
      files = basename(package_dir),
      compression = "gzip",
      tar = "internal"  # 强制使用内置tar
    )
    
    if (tar_result == 0 && file.exists(output_file)) {
      message("  tar.gz文件创建成功")
      return(TRUE)
    } else {
      message("  tar.gz文件创建失败")
      return(FALSE)
    }
    
  }, error = function(e) {
    message("zip转tar.gz失败: ", e$message)
    return(FALSE)
  })
}

#' 从OSS安装包
#'
#' @description 从阿里云OSS下载并安装指定的R包，支持完整的依赖管理
#'
#' @param package_name 包名
#' @param version 要安装的版本，默认NULL安装最新版本
#' @param dependencies 是否安装依赖包，默认TRUE
#' @param upgrade 是否升级依赖包，可选 "never", "ask", "always"
#' @param force 是否强制重新安装（即使已是最新版本），默认FALSE
#' @param verify_checksum 是否验证文件校验和，默认TRUE
#' @param install_optional 是否安装可选依赖，默认FALSE
#' @param check_dependencies 是否检查依赖（内部递归调用时设为FALSE），默认TRUE
#'
#' @return 安装成功返回TRUE，失败返回FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' # 安装最新版本（包含依赖）
#' installFromOSS("ZGBD")
#' 
#' # 安装特定版本
#' installFromOSS("ZGBD", version = "1.0.0")
#' 
#' # 强制重新安装
#' installFromOSS("ZGBD", force = TRUE)
#' 
#' # 安装包含可选依赖
#' installFromOSS("ZGBD", install_optional = TRUE)
#' 
#' # 仅安装包本身，不检查依赖
#' installFromOSS("ZGBD", dependencies = FALSE)
#' }
installFromOSS <- function(package_name, 
                          version = NULL, 
                          dependencies = TRUE,
                          upgrade = "never",
                          force = FALSE,
                          verify_checksum = TRUE,
                          install_optional = FALSE,
                          check_dependencies = TRUE) {
  
  # 获取包信息
  all_oss_data <- readOSSJson("metadata/packages.json")
  packages <- all_oss_data$packages
  
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
  
  # 检查和安装依赖（如果启用）
  dependency_results <- NULL
  if (check_dependencies && dependencies) {
    dependency_results <- checkAndInstallDependencies(
      pkg_info, 
      packages, 
      install_optional = install_optional,
      check_only = FALSE
    )
    
    # 检查依赖安装是否成功
    if (!is.null(dependency_results$install_results$cran) && 
        !dependency_results$install_results$cran$success) {
      stop("CRAN依赖安装失败: ", 
           paste(dependency_results$install_results$cran$failed, collapse = ", "))
    }
    
    if (!is.null(dependency_results$install_results$oss) && 
        !dependency_results$install_results$oss$success) {
      stop("OSS依赖安装失败: ", 
           paste(dependency_results$install_results$oss$failed, collapse = ", "))
    }
  }
  
  # 构建下载URL
  config <- getOSSConfig()
  download_url <- paste0(config$base_url, version_info$file)
  
  # 创建临时文件 - 先用通用扩展名
  temp_file <- tempfile(fileext = ".tmp")
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
  
  # 检测文件格式并处理
  file_format <- detectFileFormat(temp_file)
  message("检测到文件格式: ", file_format)
  
  # 准备用于安装的文件
  install_file <- temp_file
  
  if (file_format == "zip") {
    message("检测到zip格式，作为二进制包处理...")
    # 对于zip格式，标记为二进制包处理
    install_file <- temp_file
  } else if (file_format == "unknown") {
    warning("无法确定文件格式，尝试直接安装...")
  }
  
  # 安装包
  message("正在安装包...")
  
  # 这里已经在前面的checkAndInstallDependencies中处理了依赖，无需重复处理
  
  # 执行安装
  tryCatch({
    # 根据文件格式选择安装方式
    if (file_format == "zip") {
      # 对于zip格式（二进制包），优先使用直接解压安装
      message("尝试直接解压安装...")
      if (installByExtraction(temp_file, package_name)) {
        # 验证安装
        if (package_name %in% rownames(installed.packages())) {
          new_version <- as.character(packageVersion(package_name))
          message("\n成功通过解压安装 '", package_name, "' 版本 ", new_version)
          return(invisible(TRUE))
        }
      } else {
        stop("直接解压安装失败")
      }
    } else {
      # 源码包安装
      message("作为源码包安装...")
      
      # 确保文件扩展名正确
      if (!grepl("\\.tar\\.gz$", install_file)) {
        # 如果文件扩展名不是.tar.gz，创建一个正确命名的副本
        correct_name_file <- tempfile(fileext = ".tar.gz")
        file.copy(install_file, correct_name_file)
        on.exit(unlink(correct_name_file), add = TRUE)
        install_file <- correct_name_file
        message("创建正确命名的文件副本: ", basename(install_file))
      }
      
      # 设置安装选项
      old_options <- options()
      on.exit(options(old_options), add = TRUE)
      
      # 在Windows下设置特殊选项
      if (.Platform$OS.type == "windows") {
        options(
          repos = NULL,
          install.packages.compile.from.source = "always"
        )
      }
      
      install.packages(
        install_file,
        repos = NULL,
        type = "source",
        dependencies = dependencies,
        upgrade = upgrade,
        quiet = FALSE,
        INSTALL_opts = c("--no-lock", "--no-test-load")
      )
      
      # 等待一小段时间让安装完成
      Sys.sleep(1)
      
      # 验证安装
      if (package_name %in% rownames(installed.packages())) {
        new_version <- as.character(packageVersion(package_name))
        message("\n成功安装 '", package_name, "' 版本 ", new_version)
        return(invisible(TRUE))
      } else {
        stop("安装似乎失败了，包未找到")
      }
    }
  }, error = function(e) {
    # 如果主要安装方法失败，尝试备用方法
    if (file_format == "zip") {
      message("\n直接解压安装失败，尝试转换为tar.gz格式...")
      tryCatch({
        # 创建临时tar.gz文件
        tar_gz_file <- tempfile(fileext = ".tar.gz")
        on.exit(unlink(tar_gz_file), add = TRUE)
        
        if (convertZipToTarGz(temp_file, tar_gz_file)) {
          message("尝试源码包安装...")
          install.packages(
            tar_gz_file,
            repos = NULL,
            type = "source",
            dependencies = dependencies,
            upgrade = upgrade,
            quiet = FALSE,
            INSTALL_opts = c("--no-lock", "--no-test-load")
          )
          
          # 验证安装
          if (package_name %in% rownames(installed.packages())) {
            new_version <- as.character(packageVersion(package_name))
            message("\n成功通过源码包安装 '", package_name, "' 版本 ", new_version)
            return(invisible(TRUE))
          }
        }
        
        # 最后尝试二进制包安装
        message("尝试二进制包安装...")
        install.packages(
          temp_file,
          repos = NULL,
          type = "win.binary",
          dependencies = dependencies,
          quiet = FALSE
        )
        
        # 验证安装
        if (package_name %in% rownames(installed.packages())) {
          new_version <- as.character(packageVersion(package_name))
          message("\n成功通过二进制包安装 '", package_name, "' 版本 ", new_version)
          return(invisible(TRUE))
        }
        
      }, error = function(e2) {
        message("所有备用安装方法都失败: ", e2$message)
      })
    }
    
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