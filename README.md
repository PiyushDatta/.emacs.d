# emacs_config
my emacs config

Git clone my configuration to your new `~/.emacs.d/` :
```
git clone https://github.com/PiyushDatta/.emacs.d.git ~/.emacs.d
```

```
For windows: git clone https://github.com/PiyushDatta/.emacs.d.git C:/Users/<user>/AppData/Roaming/.emacs.d
```

# Dependancies
- ripgrep (rg) for projectile-ripgrep
- clang for cpp code

If you are on windows:
- Visual Studio 2015 or 2017 for irony (when building server)
- Perl for executing markdown (no markdown file needed, just perl)

# Commands

M-x:
```
ESC-x
```

C-x
```
Ctrl-x
```
Alt/Command (mac) -x
```
Alt-x
OR
Command-x (on mac)
```

Delete other windows:
```
M-x delete-other-windows
```

Open treemacs:
```
C-x t t
```

Switch between windows:
```
C-x o
```

Close current windows:
```
C-x 0
```

Split window into 3 vertical windows
```
C-x 3
```

Change buffers
```
C-x -> (right/left arrow key)
```

Eval buffer
```
M-x eval-buffer
```

Check the key sequence for keybinds
```
C-h k [key sequence] (e.g C-h k C-m)
```

Evaluate elisp
```
M-: 
(ESC + :)
```

Find directory of user's init file
```
M-: user-init-file
```

For mac users, to show hidden files ('.' files)
```
Command-Shift-. (period)
```

For shell
```
M-x shell
```

Use previous command in shell
```
M-p
```

Kill process in shell
```
C-c C-c
```

Projectile find file
```
s-p f
OR
C-c p f
```

Find key binding for command
```
C-h w <command-name>
```

Find command for key binding
```
C-h k <key-sequence>
```

Get detailed information about command
```
C-h f <function-name>
```

Get detailed information about bound variable
```
C-h v <variable-name>
```

