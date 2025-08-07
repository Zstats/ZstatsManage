# zstatsManage

一个基于阿里云OSS的R包管理和更新工具，用于管理私有R包的分发和更新。**开箱即用，无需配置！**


## 安装

```r
# 从GitHub安装
if (!require("remotes")) install.packages("remotes")
remotes::install_github("yourusername/zstatsManage")
```

## 快速开始

### 1. 开箱即用 - 无需配置

```r
library(zstatsManage)
# 安装后直接使用，无需任何配置步骤！

# 测试连接
testOSSConnection()
```

### 2. 浏览可用包

```r
# 列出所有可用的包
packages <- listAvailablePackages()

# 搜索特定的包
results <- searchPackages("data analysis")

# 获取包的详细信息
info <- getPackageInfo("package1")
```

### 3. 检查更新

```r
# 检查所有已安装包的更新
updates <- checkPackageUpdates()

# 检查特定包的更新
updates <- checkPackageUpdates(c("package1", "package2"))
```

### 4. 安装和更新包

```r
# 安装单个包（自动处理所有依赖）
installFromOSS("ZGBD")

# 安装特定版本
installFromOSS("ZGBD", version = "1.0.0")

# 安装包含可选依赖
installFromOSS("ZGBD", install_optional = TRUE)

# 仅安装包本身，不处理依赖
installFromOSS("ZGBD", dependencies = FALSE)

# 批量安装
installPackages(c("package1", "package2", "package3"))

# 更新所有可更新的包（含依赖变更检测）
updatePackages()

# 更新包含可选依赖
updatePackages(install_optional = TRUE)
```



## 主要函数

### 包浏览
- `listAvailablePackages()`: 列出所有可用包
- `getPackageInfo()`: 获取包的详细信息
- `searchPackages()`: 搜索包
- `getPackageTags()`: 获取所有标签

### 版本管理
- `checkPackageUpdates()`: 检查包更新（含依赖变更检测）
- `getUpdateHistory()`: 获取包的版本历史
- `updatePackages()`: 批量更新包（自动处理依赖变更）

### 安装功能
- `installFromOSS()`: 从OSS安装包（含智能依赖管理）
- `installPackages()`: 批量安装包
- `downloadPackage()`: 下载包文件但不安装

### 依赖管理
- `checkAndInstallDependencies()`: 检查和安装包依赖（内部函数）
- `checkVersionRequirement()`: 版本要求验证（内部函数）
- `installCranDependencies()`: 安装CRAN依赖（内部函数）
- `installOssDependencies()`: 安装OSS依赖（内部函数）



## 贡献

欢迎提交Issue和Pull Request！

## 联系方式

- 项目主页: https://github.com/zstats/zstatsManage
- Bug报告: https://github.com/zstats/zstatsManage/issues 