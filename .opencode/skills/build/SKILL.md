---
name: build
description: "Сборка TransGUI (Lazarus/FPC): make_icons_lrs, lazbuild transgui.lpi, units\\transgui.exe, lang. Use when building TransGUI."
---

# Сборка TransGUI

Проект: `C:\Users\SERGEYBO\Documents\Projects\transgui`. Прочитай `AGENTS.md`
(раздел TransGUI build).

## Правила

- После изменений кода спроси, нужна ли сборка. Не собирай молча.
- Не убивай запущенный `transgui`. Если `units\transgui.exe` заблокирован,
  переименуй его или используй правила проекта; закрывай приложение только с
  явного согласия пользователя.
- Не редактируй исходники и не делай commit/push.

## Порядок

1. Перед сборкой перегенерируй встроенные ресурсы из `icons\icons.ini`:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File 'C:\Users\SERGEYBO\Documents\Projects\transgui\make_icons_lrs.ps1'
```

Скрипт создаёт `icons.lrs` (иконки из `icons\`) и перегенерирует `main.lrs` из
`main.lfm` через временную копию (IDE может держать `main.lfm`). Оба `.lrs` —
генерируемые, git-ignored, встраиваются в exe. Папка `icons` — только исходники,
не деплоить.

2. Собери проект:

```powershell
& 'C:\Lazarus\lazbuild.exe' 'C:\Users\SERGEYBO\Documents\Projects\transgui\transgui.lpi'
```

Выход: `units\transgui.exe`.

3. Переводы: держи `units\lang\` в синхроне с `lang\` (29 файлов).

4. После правки другого `.lfm` (кроме `main.lfm`) перегенерируй его ресурс:

```powershell
& 'C:\Lazarus\tools\lazres.exe' daemonoptions.lrs daemonoptions.lfm
```

5. Проверка запуска с одноразовым home:

```powershell
.\units\transgui.exe --home=C:\Windows\TEMP\opencode\transgui-test
```

## Отчёт

Команда, результат, путь к exe, предупреждения. Не запускай приложение без просьбы.
