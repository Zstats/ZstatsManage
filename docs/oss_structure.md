# 阿里云OSS文件夹结构设计

## OSS Bucket 结构

```
your-bucket-name/
├── packages/                    # R包存储目录
│   ├── package1/
│   │   ├── package1_1.0.0.tar.gz
│   │   ├── package1_1.0.1.tar.gz
│   │   └── package1_1.1.0.tar.gz
│   ├── package2/
│   │   ├── package2_0.1.0.tar.gz
│   │   └── package2_0.2.0.tar.gz
│   └── .../
├── metadata/                    # 元数据目录
│   └── packages.json           # 统一配置文件（包含所有信息）
└── docs/                       # 文档目录
    ├── README.md
    └── package_docs/
        ├── package1.md
        └── package2.md
```

## 配置文件格式

### packages.json（统一配置文件）

```json
{
  "last_updated": "2024-01-01T00:00:00Z",
  "repository_info": {
    "name": "zstats私有包仓库", 
    "description": "zstats团队私有R包仓库",
    "version": "1.0.0"
  },
  "packages": [
    {
      "name": "package1",
      "current_version": "1.1.0",
      "description": "包的简要描述",
      "description_cn": "包的中文描述",
      "author": "作者名",
      "dependencies": ["R (>= 4.0.0)", "jsonlite"],
      "versions": [
        {
          "version": "1.1.0",
          "release_date": "2024-01-01",
          "file": "packages/package1/package1_1.1.0.tar.gz",
          "sha256": "文件SHA256校验和",
          "md5": "文件MD5校验和", 
          "size": 1024000,
          "changes": "更新内容说明",
          "breaking_changes": false
        },
        {
          "version": "1.0.1",
          "release_date": "2023-12-01",
          "file": "packages/package1/package1_1.0.1.tar.gz",
          "sha256": "文件SHA256校验和",
          "md5": "文件MD5校验和",
          "size": 1020000,
          "changes": "修复bug",
          "breaking_changes": false
        }
      ],
      "tags": ["data-analysis", "statistics"],
      "license": "MIT",
      "homepage": "https://github.com/user/package1"
    }
  ]
}
```

## OSS访问配置

### 固定配置
- OSS URL: `https://zstats-packages.oss-cn-hangzhou.aliyuncs.com/`
- Bucket权限：公共读
- 无需用户配置，包内预设固定地址

### 访问示例
```r
# 所有配置都是预设的，用户无需配置
# 获取包列表配置
packages_url <- "https://zstats-packages.oss-cn-hangzhou.aliyuncs.com/metadata/packages.json"

# 下载特定包
package_url <- "https://zstats-packages.oss-cn-hangzhou.aliyuncs.com/packages/package1/package1_1.1.0.tar.gz"
```

## 校验和生成

使用新增的工具函数自动生成：

```r
# 为包文件生成校验和
generateChecksums("path/to/package.tar.gz")

# 批量为目录下所有包生成校验和
generatePackageMetadata("packages/")
```

## 更新流程

1. 开发者将新版本R包上传到对应目录
2. 使用工具函数生成校验和：`generatePackageMetadata("packages/")`
3. 更新 metadata/packages.json（工具函数自动更新）
4. 客户端自动获取更新信息（无需配置）

## 优势

1. **简化配置**：只有一个配置文件，信息集中管理
2. **无需配置**：用户安装即用，不需要设置OSS地址
3. **自动生成**：校验和和元数据自动生成，减少人工错误
4. **版本集中**：包的所有版本信息都在同一个文件中，便于管理 