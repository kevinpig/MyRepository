@echo on
@rem 删除VC编译生成的中间文件

for /r . %%a in (.) do del /q "%%a\*.obj" "%%a\*.pdb" "%%a\*.idb" "%%a\*.ncb" "%%a\*.pch"

@echo Mission Complete

