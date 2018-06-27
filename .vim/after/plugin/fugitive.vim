" force autoloading in newer versions by calling a nonexistent func
sil! let s:autoload_fugitive = fugitive#Autoload()
au! fugitive_remove
