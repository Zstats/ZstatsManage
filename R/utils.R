#' 生成文件校验和
#'
#' @description 为指定文件生成SHA256和MD5校验和
#'
#' @param file_path 文件路径
#'
#' @return 包含校验和信息的列表
#' @export
#'
#' @examples
#' \dontrun{
#' checksums <- generateChecksums("path/to/package.tar.gz")
#' print(checksums$sha256)
#' print(checksums$md5)
#' }
generateChecksums <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("文件不存在: ", file_path)
  }
  
  message("正在计算文件校验和: ", basename(file_path))
  
  # 计算SHA256和MD5
  sha256 <- digest::digest(file_path, algo = "sha256", file = TRUE)
  md5 <- digest::digest(file_path, algo = "md5", file = TRUE)
  
  # 获取文件大小
  file_info <- file.info(file_path)
  file_size <- file_info$size
  
  result <- list(
    file = file_path,
    sha256 = sha256,
    md5 = md5,
    size = file_size,
    modified_time = file_info$mtime
  )
  
  message("SHA256: ", sha256)
  message("MD5: ", md5)
  message("大小: ", round(file_size / 1024 / 1024, 2), " MB")
  
  return(result)
}

#' 生成包元数据
#'
#' @description 扫描包目录并生成完整的packages.json元数据文件
#'
#' @param packages_dir 包文件目录路径
#' @param output_file 输出的JSON文件路径，默认为"metadata/packages.json"
#' @param base_info 仓库基本信息列表，包含name、description、version
#'
#' @return 生成的元数据列表
#' @export
#'
#' @examples
#' \dontrun{
#' # 扫描packages目录生成元数据
#' metadata <- generatePackageMetadata("packages/")
#' 
#' # 自定义仓库信息
#' base_info <- list(
#'   name = "我的R包仓库",
#'   description = "私有R包仓库",
#'   version = "1.0.0"
#' )
#' metadata <- generatePackageMetadata("packages/", base_info = base_info)
#' }
generatePackageMetadata <- function(packages_dir, 
                                  output_file = "metadata/packages.json",
                                  base_info = NULL) {
  
  if (!dir.exists(packages_dir)) {
    stop("包目录不存在: ", packages_dir)
  }
  
  # 默认仓库信息
  if (is.null(base_info)) {
    base_info <- list(
      name = "zstats私有包仓库",
      description = "zstats团队私有R包仓库",
      version = "1.0.0"
    )
  }
  
  message("扫描包目录: ", packages_dir)
  
  # 获取所有包子目录
  package_dirs <- list.dirs(packages_dir, full.names = TRUE, recursive = FALSE)
  package_dirs <- package_dirs[basename(package_dirs) != ""]
  
  if (length(package_dirs) == 0) {
    stop("未找到任何包目录")
  }
  
  packages_metadata <- list()
  
  # 遍历每个包目录
  for (pkg_dir in package_dirs) {
    pkg_name <- basename(pkg_dir)
    message("\n处理包: ", pkg_name)
    
    # 获取包的所有版本文件
    tar_files <- list.files(pkg_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
    
    if (length(tar_files) == 0) {
      warning("包 '", pkg_name, "' 目录下没有找到.tar.gz文件")
      next
    }
    
    # 解析版本信息
    versions_info <- list()
    for (tar_file in tar_files) {
      # 从文件名提取版本号
      filename <- basename(tar_file)
      # 假设格式为 packagename_version.tar.gz
      version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tar\\.gz$", filename))
      if (length(version_match) == 0) {
        warning("无法从文件名解析版本: ", filename)
        next
      }
      
      version <- sub("^_", "", sub("\\.tar\\.gz$", "", version_match))
      
      # 生成校验和
      checksums <- generateChecksums(tar_file)
      
      # 构建相对路径
      relative_path <- file.path("packages", pkg_name, filename)
      
      # 获取修改时间作为发布日期
      release_date <- as.character(as.Date(checksums$modified_time))
      
      version_info <- list(
        version = version,
        release_date = release_date,
        file = relative_path,
        sha256 = checksums$sha256,
        md5 = checksums$md5,
        size = checksums$size,
        changes = "版本更新", # 默认值，可后续手动编辑
        breaking_changes = FALSE
      )
      
      versions_info[[length(versions_info) + 1]] <- version_info
    }
    
    if (length(versions_info) == 0) {
      next
    }
    
    # 按版本排序（最新的在前）
    version_numbers <- sapply(versions_info, function(v) v$version)
    sorted_indices <- order(sapply(version_numbers, function(v) {
      parts <- as.numeric(strsplit(v, "\\.")[[1]])
      # 简单的版本比较，可以改进
      sum(parts * c(1000000, 1000, 1))
    }), decreasing = TRUE)
    
    versions_info <- versions_info[sorted_indices]
    current_version <- versions_info[[1]]$version
    
    # 构建包元数据
    pkg_metadata <- list(
      name = pkg_name,
      current_version = current_version,
      description = paste("R包", pkg_name, "的描述"), # 默认描述
      description_cn = paste("R包", pkg_name, "的中文描述"),
      author = "包作者", # 默认作者
      dependencies = c("R (>= 4.0.0)"), # 默认依赖
      versions = versions_info,
      tags = c("r-package"), # 默认标签
      license = "MIT",
      homepage = paste0("https://github.com/zstats/", pkg_name)
    )
    
    packages_metadata[[length(packages_metadata) + 1]] <- pkg_metadata
  }
  
  # 构建完整的元数据
  full_metadata <- list(
    last_updated = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    repository_info = base_info,
    packages = packages_metadata
  )
  
  # 创建输出目录
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 写入JSON文件
  json_content <- jsonlite::toJSON(full_metadata, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, output_file, useBytes = TRUE)
  
  message("\n元数据文件已生成: ", output_file)
  message("包含 ", length(packages_metadata), " 个包")
  
  return(full_metadata)
}

#' 更新单个包的元数据
#'
#' @description 在现有的packages.json中更新或添加单个包的信息
#'
#' @param package_name 包名
#' @param package_file 包文件路径(.tar.gz)
#' @param metadata_file packages.json文件路径
#' @param package_info 包的详细信息列表（可选）
#'
#' @return 更新后的元数据
#' @export
#'
#' @examples
#' \dontrun{
#' # 添加新包版本
#' updatePackageMetadata("mypackage", "packages/mypackage/mypackage_1.2.0.tar.gz")
#' 
#' # 带详细信息
#' info <- list(
#'   description = "我的包描述",
#'   author = "作者名",
#'   license = "GPL-3"
#' )
#' updatePackageMetadata("mypackage", "packages/mypackage/mypackage_1.2.0.tar.gz", 
#'                      package_info = info)
#' }
updatePackageMetadata <- function(package_name, 
                                package_file,
                                metadata_file = "metadata/packages.json",
                                package_info = NULL) {
  
  if (!file.exists(package_file)) {
    stop("包文件不存在: ", package_file)
  }
  
  # 读取现有元数据
  if (file.exists(metadata_file)) {
    metadata <- jsonlite::fromJSON(metadata_file)
  } else {
    # 创建新的元数据结构
    metadata <- list(
      last_updated = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      repository_info = list(
        name = "zstats私有包仓库",
        description = "zstats团队私有R包仓库",
        version = "1.0.0"
      ),
      packages = list()
    )
  }
  
  # 生成新版本的校验和
  checksums <- generateChecksums(package_file)
  
  # 从文件名提取版本
  filename <- basename(package_file)
  version_match <- regmatches(filename, regexpr("_([0-9\\.\\-]+)\\.tar\\.gz$", filename))
  version <- sub("^_", "", sub("\\.tar\\.gz$", "", version_match))
  
  # 构建相对路径
  relative_path <- file.path("packages", package_name, filename)
  
  # 新版本信息
  new_version <- list(
    version = version,
    release_date = as.character(as.Date(checksums$modified_time)),
    file = relative_path,
    sha256 = checksums$sha256,
    md5 = checksums$md5,
    size = checksums$size,
    changes = "版本更新",
    breaking_changes = FALSE
  )
  
  # 查找现有包
  pkg_index <- which(sapply(metadata$packages, function(p) p$name == package_name))
  
  if (length(pkg_index) > 0) {
    # 更新现有包
    pkg <- metadata$packages[[pkg_index]]
    
    # 添加新版本到版本列表开头
    pkg$versions <- append(list(new_version), pkg$versions, after = 0)
    pkg$current_version <- version
    
    # 更新包信息
    if (!is.null(package_info)) {
      for (key in names(package_info)) {
        pkg[[key]] <- package_info[[key]]
      }
    }
    
    metadata$packages[[pkg_index]] <- pkg
  } else {
    # 添加新包
    new_package <- list(
      name = package_name,
      current_version = version,
      description = ifelse(is.null(package_info$description), 
                          paste("R包", package_name, "的描述"), 
                          package_info$description),
      description_cn = ifelse(is.null(package_info$description_cn),
                            paste("R包", package_name, "的中文描述"),
                            package_info$description_cn),
      author = ifelse(is.null(package_info$author), "包作者", package_info$author),
      dependencies = ifelse(is.null(package_info$dependencies), 
                           list(c("R (>= 4.0.0)")), 
                           list(package_info$dependencies)),
      versions = list(new_version),
      tags = ifelse(is.null(package_info$tags), 
                    list(c("r-package")), 
                    list(package_info$tags)),
      license = ifelse(is.null(package_info$license), "MIT", package_info$license),
      homepage = ifelse(is.null(package_info$homepage),
                       paste0("https://github.com/zstats/", package_name),
                       package_info$homepage)
    )
    
    metadata$packages <- append(metadata$packages, list(new_package))
  }
  
  # 更新时间戳
  metadata$last_updated <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # 写入文件
  output_dir <- dirname(metadata_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  json_content <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, metadata_file, useBytes = TRUE)
  
  message("已更新包 '", package_name, "' 版本 ", version, " 的元数据")
  
  return(metadata)
} 