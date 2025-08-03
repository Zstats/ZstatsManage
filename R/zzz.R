.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "欢迎使用 zstatsManage ", 
    utils::packageVersion("zstatsManage"),
    "\n",
    "zstats私有R包管理工具 - 开箱即用，无需配置",
    "\n",
    "使用 listAvailablePackages() 查看可用的包"
  )
} 