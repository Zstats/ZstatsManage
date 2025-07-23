# ZstatsManage
Manage packages 
安装

# 从GitHub安装
if (!require("remotes")) install.packages("remotes")
remotes::install_github("yourusername/zstatsManage")

快速开始

1. 开箱即用 - 无需配置

library(zstatsManage)
# 安装后直接使用，无需任何配置步骤！

# 测试连接
testOSSConnection()

2. 浏览可用包

# 列出所有可用的包
packages <- listAvailablePackages()

# 搜索特定的包
results <- searchPackages("data analysis")

# 获取包的详细信息
info <- getPackageInfo("package1")

3. 检查更新

# 检查所有已安装包的更新
updates <- checkPackageUpdates()

# 检查特定包的更新
updates <- checkPackageUpdates(c("package1", "package2"))

4. 安装和更新包

# 安装单个包
installFromOSS("package1")

# 安装特定版本
installFromOSS("package1", version = "1.0.0")

# 批量安装
installPackages(c("package1", "package2", "package3"))

# 更新所有可更新的包
updatePackages()

主要函数

包浏览

listAvailablePackages(): 列出所有可用包

getPackageInfo(): 获取包的详细信息

searchPackages(): 搜索包

getPackageTags(): 获取所有标签

版本管理

checkPackageUpdates(): 检查包更新

getUpdateHistory(): 获取包的版本历史

updatePackages(): 批量更新包

安装功能

installFromOSS(): 从OSS安装包

installPackages(): 批量安装包

downloadPackage(): 下载包文件但不安装

