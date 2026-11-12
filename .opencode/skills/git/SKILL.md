---
name: git
description: "Git TransGUI: ветка master, remotes origin/upstream, правила коммита, генерируемые файлы. Use when committing, pushing or inspecting git history in transgui."
---

# Git — TransGUI

Репозиторий: `C:\Users\SERGEYBO\Documents\Projects\transgui`.

## Remotes и ветка

- `origin` = https://github.com/SNBogdanov/transgui (форк).
- `upstream` = https://github.com/transmission-remote-gui/transgui.git.
- Рабочая ветка: `master` (upstream `origin/master`).

## Жёсткие правила

- Коммит и пуш — только по явной просьбе и только через субагент `git-commit`
  (`/commit`). Main-сессия не делает `git add/commit/push` сама.
- Перед коммитом: `git status --short`, `git diff`, `git log --oneline -10`.
- Не делать force, amend, `--no-verify`; не менять git config.
- Не коммитить генерируемые файлы и артефакты: `units/`, `*.lrs` (включая
  `icons.lrs`, `main.lrs`), `*.res`, `*.exe`, `backup/`, `*.bak`, логи.
- Папка `icons/` — это исходники (PNG + `icons.ini`), её нужно коммитить.
  В корне проекта PNG быть не должно.
- Не коммитить служебные `.opencode/` и `AGENTS.md`, если пользователь явно не
  попросил включать их в репозиторий.
- Не коммитить секреты и учётные данные (`transgui.ini` с паролями и т.п.).

## Сообщения коммитов

- Кратко, в стиле последних коммитов (`git log --oneline -10`).
- Тело — только «почему», без пересказа диффа.

## Push

- Push — только в текущую ветку/upstream, без force.
- При отказе (non-fast-forward, auth, VPN) — не force, сообщи причину и что
  осталось.
