# zstatsManage

一个基于阿里云OSS的R包管理和更新工具，用于管理私有R包的分发和更新。**开箱即用，无需配置！**

## 功能特性

- 📦 **包管理**: 从阿里云OSS下载和安装R包
- 🔄 **版本检测**: 检查本地包是否有新版本可用
- 🎯 **智能依赖管理**: 自动检测和安装所有依赖包（CRAN包、OSS包、系统依赖）
- 📊 **依赖变更检测**: 更新时自动检测新增、移除或更新的依赖
- 📋 **包浏览**: 列出所有可用的包及其详细信息
- 🔍 **智能搜索**: 根据包名、描述、标签搜索包
- ✅ **完整性验证**: 支持SHA256和MD5双重校验和验证
- 💾 **缓存机制**: 智能缓存减少网络请求
- 🌐 **离线支持**: 网络故障时使用缓存数据
- 🛠️ **开发工具**: 提供包元数据生成和校验和计算工具
- 🔧 **二进制包支持**: 优化的zip格式二进制包安装

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

### 5. 智能依赖管理

系统会自动检测并处理三种类型的依赖：

```r
# 自动依赖处理示例输出：
# 检查包 'ZGBD' 的依赖...
#   检查R版本要求: R (>= 4.0.0)
#   检查CRAN包依赖...
#   需要安装的CRAN包: dplyr, httr
#     install CRAN包: dplyr ✓
#     install CRAN包: httr ✓
#   检查OSS包依赖...
#   需要安装的OSS包: zstatsBase
#     install OSS包: zstatsBase ✓
#   检查系统依赖...
#     需要系统依赖: libcurl - HTTP客户端库
# 正在下载包 'ZGBD'...
# 成功通过解压安装 'ZGBD' 版本 0.0.1
```

### 6. 依赖变更检测

更新包时自动检测依赖变更：

```r
# 检查更新时显示依赖变更
updates <- checkPackageUpdates()
# 发现 1 个包有更新可用
# 其中 1 个包有新的依赖需要安装
#   - ZGBD: 新增: tidyr (cran), zstatsUtils (oss)

# 更新时显示变更详情
updatePackages()
# 注意：以下包在更新时会有依赖变更：
#   ZGBD: 新增: tidyr (cran); 更新: dplyr (cran)
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

### 系统工具
- `testOSSConnection()`: 测试OSS连接
- `clearCache()`: 清理本地缓存

### 开发者工具
- `generateChecksums()`: 生成文件校验和
- `generatePackageMetadata()`: 批量生成包元数据
- `updatePackageMetadata()`: 更新单个包元数据（支持依赖变更检测）
- `compareDependencyChanges()`: 比较依赖变更（内部函数）
- `createDefaultDependencies()`: 创建默认依赖结构（内部函数）

## OSS配置说明

本工具使用预设的OSS配置，**用户无需手动配置任何OSS参数**：

- **OSS地址**: `https://zstats-packages.oss-cn-hangzhou.aliyuncs.com/`
- **权限**: 公共读取
- **结构**: 统一的元数据文件 `metadata/packages.json`

OSS目录结构：

```
zstats-packages/
├── packages/              # R包文件目录
│   └── package1/
│       └── package1_1.0.0.tar.gz
└── metadata/              # 元数据目录
    └── packages.json      # 统一配置文件（包含所有信息）
```

## 开发者指南

### 发布新包到OSS

1. **上传包文件**到OSS相应目录
2. **生成元数据**（自动计算校验和）:
   ```r
   # 为单个包生成校验和
   generateChecksums("packages/mypackage/mypackage_1.0.0.tar.gz")
   
   # 批量生成整个目录的元数据
   generatePackageMetadata("packages/")
   
   # 更新单个包的元数据
   updatePackageMetadata("mypackage", "packages/mypackage/mypackage_1.0.0.tar.gz")
   ```

3. **自定义包信息和依赖**（可选）:
   ```r
   package_info <- list(
     description = "我的R包描述",
     description_cn = "我的R包中文描述",
     author = "作者名",
     maintainer = "author@example.com",
     license = "",
     tags = c("statistics", "data-analysis"),
     homepage = "https://github.com/myuser/mypackage",
     dependencies = list(
       r_version = "R (>= 4.0.0)",
       cran_packages = list(
         list(name = "dplyr", version = ">= 1.0.0", required = TRUE, description = "数据操作"),
         list(name = "ggplot2", version = ">= 3.3.0", required = FALSE, description = "数据可视化")
       ),
       oss_packages = list(
         list(name = "zstatsBase", version = ">= 1.0.0", required = TRUE, description = "基础功能包")
       ),
       system_dependencies = list(
         list(name = "libcurl", platform = "linux", required = TRUE, 
              description = "HTTP客户端库", install_command = "apt-get install libcurl4-openssl-dev")
       )
     )
   )
   
   updatePackageMetadata("mypackage", 
                        "packages/mypackage/mypackage_1.0.0.zip",  # 支持zip格式
                        package_info = package_info)
   ```

### packages.json文件格式（新版本支持完整依赖管理）

```json
{
  "last_updated": "2025-07-25T07:22:51Z",
  "repository_info": {
    "name": "zstats私有包仓库",
    "description": "zstats团队私有R包仓库",
    "version": "1.0.0"
  },
  "packages": [
    {
      "name": "ZGBD",
      "current_version": "0.0.1",
      "description": "zstats数据处理包",
      "description_cn": "zstats数据处理包，提供基础GBD等数据分析功能",
      "author": "zstats团队",
      "maintainer": "zstats@example.com",
      "dependencies": {
        "r_version": "R (>= 4.0.0)",
        "oss_packages": [
          {
            "name": "zstatsBase",
            "version": ">= 1.0.0",
            "required": true,
            "description": "zstats基础功能包"
          }
        ],
        "cran_packages": [
          {
            "name": "jsonlite",
            "version": ">= 1.7.0",
            "required": true,
            "description": "JSON数据处理"
          },
          {
            "name": "dplyr",
            "version": ">= 1.0.0",
            "required": true,
            "description": "数据操作"
          },
          {
            "name": "ggplot2",
            "version": ">= 3.3.0",
            "required": false,
            "description": "数据可视化"
          }
        ],
        "system_dependencies": [
          {
            "name": "libcurl",
            "platform": "linux",
            "required": true,
            "description": "HTTP客户端库",
            "install_command": "apt-get install libcurl4-openssl-dev"
          }
        ]
      },
      "versions": [
        {
          "version": "0.0.1",
          "release_date": "2025-07-25",
          "file": "packages/ZGBD/ZGBD_0.0.1.zip",
          "sha256": "80880d96d30746665e3268c9ce0964416a1b336a1df6258e09e6c85e786da623",
          "md5": "c9766ed658a72e57ffb89db13cd31bbf",
          "size": 61820075,
          "changes": "初始版本发布",
          "breaking_changes": false,
          "dependency_changes": {
            "added": [
              {
                "type": "cran",
                "name": "dplyr",
                "version": ">= 1.0.0"
              }
            ],
            "removed": [],
            "updated": []
          }
        }
      ],
      "tags": ["finance", "data-analysis", "bonds"],
      "license": "",
      "homepage": "https://github.com/zstats/ZGBD",
      "repository": "https://github.com/zstats/ZGBD.git",
      "install_priority": 1
    }
  ]
}
```

## 高级用法

### 缓存管理

```r
# 清理所有缓存
clearCache()

# 清理24小时前的缓存
clearCache(older_than = 24)
```

### 离线模式

当网络不可用时，包会自动使用缓存的数据：

```r
# 即使网络断开，如果有缓存仍可浏览包列表
packages <- listAvailablePackages()
```

### 按标签筛选

```r
# 查看所有标签
tags <- getPackageTags()

# 按标签筛选包
stats_packages <- listAvailablePackages(tags = "statistics")
```

### 连接测试

```r
# 测试OSS连接状态
if (testOSSConnection()) {
  message("连接正常，可以正常使用")
} else {
  message("连接异常，将使用缓存数据")
}
```

## 故障排除

### 常见问题

1. **网络连接错误**
   - 使用 `testOSSConnection()` 测试连接
   - 检查网络连接是否正常
   - 包会自动使用缓存数据

2. **校验和不匹配**
   - 文件可能在传输中损坏
   - 包会同时验证SHA256和MD5
   - 尝试清理缓存后重新下载：`clearCache()`

3. **找不到包**
   - 使用 `listAvailablePackages()` 查看所有可用包
   - 检查包名拼写是否正确
   - 使用 `searchPackages()` 搜索相关包

## 设计优势

1. **零配置**: 安装即用，无需设置OSS地址
2. **智能依赖管理**: 自动检测、解析和安装所有类型依赖
3. **依赖变更追踪**: 精确跟踪每个版本的依赖变更
4. **多格式支持**: 支持tar.gz源码包和zip二进制包
5. **简化结构**: 单一配置文件，信息集中管理
6. **自动工具**: 校验和和元数据自动生成，减少人工错误
7. **双重校验**: SHA256和MD5双重校验确保文件完整性
8. **智能缓存**: 离线也能正常使用基本功能
9. **版本要求验证**: 支持复杂的版本要求表达式（>=, <=, ==等）
10. **递归依赖处理**: 自动处理依赖包的依赖，避免循环依赖

## 贡献

欢迎提交Issue和Pull Request！

## 联系方式

- 项目主页: https://github.com/zstats/zstatsManage
- Bug报告: https://github.com/zstats/zstatsManage/issues 