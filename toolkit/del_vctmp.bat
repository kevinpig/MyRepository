@echo on
@rem ɾ��VC�������ɵ��м��ļ�

for /r . %%a in (.) do del /q "%%a\*.obj" "%%a\*.pdb" "%%a\*.idb" "%%a\*.ncb" "%%a\*.pch"

@echo Mission Complete

