---
description: "Собрать TransGUI (Lazarus/FPC) в units\\transgui.exe."
agent: transgui-build
---
Собери TransGUI. Аргументы: $ARGUMENTS

Сборка выполняется только этим агентом; main-сессия не запускает `lazbuild` сама. Спроси, нужна ли сборка, если не ясно. Закрой блокирующие экземпляры. Собери `transgui.lpi` через `lazbuild`, проверь `units\transgui.exe`, синхронизируй `units\lang\`. Не редактируй исходники, не делай commit/push.
