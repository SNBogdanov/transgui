# Build and deployment rules

## Workflow

- After code changes, ask whether a build is needed. Do not build silently.
- **Build/deploy only via subagents** `transgui-build` / `transgui-deploy`.
  The main session does not run `lazbuild`/copy commands itself: it asks for targets,
  invokes the agent, and reads the report.
- **Pass all targets of the current stage at once** (e.g. local + `samsung-pc`); the
  agent handles them in one invocation; the main session must not deploy between targets.
- Before building, close running instances that lock the build output.
- For TransGUI deployment, first check whether `transgui` is running (`Get-Process transgui`). A running `transgui.exe` does not have to be closed: rename the locked executable to `transgui.exe.<timestamp>` and copy the new build into place. The running process continues using the old file and the new version applies after restart. Close the application only if the rename itself fails.
- Before deployment, determine which targets need updating. Compare the source executable SHA-256 and language files with each target. If a target is already up to date, skip it: do not copy and do not create a backup for it.
- Only create a `transgui.exe.<timestamp>` backup when that target actually needs a new executable.
- After a successful deployment and verification, delete old `transgui.exe.*` backup files. If a backup is still locked by the old running process, leave it and delete it after the application restarts.
- After a successful build, ask whether deployment is needed and which targets to use.
- Never store passwords, tokens, or private keys in this file. Read them from environment variables or a secret store.
- Record deployments in the final response: what was stopped, what was copied, and the verification result.

## Localization

- `ResTranslator` strips trailing `.`, `,`, and `:` from the source string before lookup (`FWordDelims` with `wdIgnoreTrailing`) and keeps them in the output.
  - Store translation keys without those trailing characters.
  - Never end a translation value with `.`, `,`, `:`, `?`, `!`, or a trailing ellipsis; keep the punctuation in the source string only.
  - Example: source `Topic:` -> key `Topic` -> value `Топик`, the output is `Топик:`.
  - Menu captions with `...` use the base key, e.g. source `Add labels...` -> key `Add labels` -> value `Добавить метки`.

## Window sizes

- Remember user-resized dimensions for resizable windows and restore them on next open.
- Remember user-resized column widths for resizable grids and restore them on next open.

## TransGUI build

- Project: `transgui.lpi` (Lazarus/FPC).
- Build command:
  ```powershell
  & 'C:\Lazarus\lazbuild.exe' 'C:\Users\SERGEYBO\Documents\Projects\transgui\transgui.lpi'
  ```
- Before building, regenerate the embedded resources from `icons\icons.ini`:
  ```powershell
  powershell -NoProfile -ExecutionPolicy Bypass -File 'C:\Users\SERGEYBO\Documents\Projects\transgui\make_icons_lrs.ps1'
  ```
  The script generates `icons.lrs` and regenerates `main.lrs` from `main.lfm`
  (through an unlocked temporary copy). Both are git-ignored and embedded into the
  exe. The `icons` folder is source only; do not deploy it.
- Output: `units\transgui.exe`.
- Translation source: `lang\`. Keep the runtime copy in `units\lang\` in sync when running the build output locally.
- Regenerate a form resource after editing its `.lfm`:
  ```powershell
  & 'C:\Lazarus\tools\lazres.exe' daemonoptions.lrs daemonoptions.lfm
  ```
- Verify a build by starting the executable with a disposable home:
  ```powershell
  .\units\transgui.exe --home=C:\Windows\TEMP\opencode\transgui-test
  ```

## TransGUI deployment (Windows)

### Local host

Target: `C:\Program Files\Transmission Remote GUI`

1. Compare `units\transgui.exe` SHA-256 and `lang\*` with the target. If both are already identical, skip this target completely: do not copy and do not create a backup.
2. If the target executable is locked by a running instance, rename it to `transgui.exe.<timestamp>`; close the application only if renaming fails.
3. Copy `units\transgui.exe` to `C:\Program Files\Transmission Remote GUI\transgui.exe`.
4. Copy `lang\*` to `C:\Program Files\Transmission Remote GUI\lang\`.
5. Verify SHA-256 of the source and installed executables, and compare the language files.
6. Delete old `transgui.exe.*` backup files; if one is locked, delete it after the application restarts.
7. Do not start the application unless the user asks.

### Remote Windows host: samsung-pc

Target: `\\samsung-pc\c$\Program Files\Transmission Remote GUI`

1. Compare `units\transgui.exe` SHA-256 and `lang\*` with the target. If both are already identical, skip this target completely: do not copy and do not create a backup.
2. If the target executable is locked by a running instance on `samsung-pc`, rename it to `transgui.exe.<timestamp>`; close the application only if renaming fails.
3. Copy `units\transgui.exe` to `\\samsung-pc\c$\Program Files\Transmission Remote GUI\transgui.exe`.
4. Copy `lang\*` to `\\samsung-pc\c$\Program Files\Transmission Remote GUI\lang\`.
5. Verify SHA-256 of the source and remote executables, and compare the language files.
6. Delete old `transgui.exe.*` backup files; if one is locked, delete it after the application restarts.
7. Do not start the application unless the user asks.

## Transmission daemon build

Project: `C:\Users\SERGEYBO\Documents\Projects\Transmission`.

Build three artifact types:

1. Windows
2. Linux
3. QNAP

Windows uses the CMake presets in that project:

```powershell
cmake --preset Release
cmake --build --preset Release --target transmission-daemon
```

The Windows binary is typically under `build\Release\daemon\Release\transmission-daemon.exe`.

Linux and QNAP artifacts are built for the daemon host architecture. Use the Transmission project's build documentation and confirm the target before building. The deployable artifact is `transmission-daemon`.

After the build, ask whether to deploy and to which of the targets below.

## Transmission daemon deployment

### 1. transmission server

SSH alias: `transmission` (configured in the user's SSH config).

1. Stop the daemon and watchdog services.
2. Back up the current binary with a replacement timestamp, then copy the new `transmission-daemon` to `/usr/local/bin/transmission-daemon`.
3. Start the daemon and watchdog services.

Expected pattern:

```bash
ts=$(date +%Y%m%d-%H%M%S)
systemctl stop transmission-daemon watchdog
cp -p /usr/local/bin/transmission-daemon "/usr/local/bin/transmission-daemon.$ts"
# copy the new binary to /usr/local/bin/transmission-daemon
systemctl start transmission-daemon watchdog
```

### 2. QNAP server

1. Copy the artifact from the `transmission` server to the QNAP server.
2. Stop the daemon:
   ```sh
   /etc/init.d/transmission.sh stop
   ```
3. Back up the current binary with a replacement timestamp, then copy the new binary to:
   ```text
   /share/MD0_DATA/.qpkg/Transmission/bin/transmission-daemon
   ```
4. Start the daemon:
   ```sh
   /etc/init.d/transmission.sh start
   ```

The QNAP host address and SSH credentials are not stored here. Confirm them before deployment.

### 3. Local Windows host: sergeybo02

1. Stop the service:
   ```powershell
   sc stop Transmission
   ```
2. Back up `C:\Program Files\Transmission\transmission-daemon.exe` with a replacement timestamp, then copy the new binary into that path.
3. Start the service:
   ```powershell
   sc start Transmission
   ```

### 4. Remote Windows host: samsung-pc

Use PsExec with credentials supplied by the operator. Do not put the password in this file.

```text
psexec \\samsung-pc -nobanner -priority <priority> -u <account> -p <password> CMD /C <command>
```

1. Stop the service:
   ```powershell
   sc stop Transmission
   ```
2. Back up `C:\Program Files\Transmission\transmission-daemon.exe` with a replacement timestamp, then copy the new binary into that path.
3. Start the service:
   ```powershell
   sc start Transmission
   ```

## Access status

- `transmission`: SSH alias works (checked 2026-10-20). `transmission-daemon` is active; `watchdog` is inactive. Revalidate before deploying.
- QNAP: no host alias or address was found in the workspace. The operator must provide the host and credentials.
- `sergeybo02`: local host; service `Transmission` is running; service operations require administrator rights.
- `samsung-pc`: PsExec account exists; DNS resolution was not confirmed from this host. Keep the password in a secret store or environment variable.
