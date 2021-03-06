# scale-scope
View 3D models and animation sequences interactively.

![Screenshot](screenshot.png)

## Build
Have [Stack](https://docs.haskellstack.org/) installed.

```bash
stack build
stack install
```

It will take a while.

## Usage
This project includes two executables.

`scale-pack` packs an OBJ file (or a sequence of them) into a custom format.

`scale-scope` visualizes the resulting file using SDL2 and OpenGL3.

### Packing files
Call the `scale-pack` program with the input OBJ files as positional arguments
and the output filename given to the `-o` option.
Example:

```bash
scale-pack data/pyramid.obj -o data/pyramid.ssc
```

Multiple OBJ files can be packed together as an animation sequence.
See `scale-pack -h` for advanced options.

### Viewing files
Launch the `scale-scope` with the output of `scale-pack` given as an argument.
Example:

```bash
scale-scope data/rex-walk-20hz-lvl0.ssc
```

The program's working directory must include the `shader/` directory.
While the program is running, it also accepts files to display via
drag&drop onto its window.

Interactive commands (default):

| Key               | Action                |
| ----------------- | --------------------- |
| LMB drag          | Rotate view           |
| Scroll / MMB drag | Zoom view             |
| Q / Escape        | Exit                  |
| PgUp PgDown       | Move view up/down     |
| P / Space         | Toggle pause          |
| J K               | Step back/forward     |
| U I               | Playback rate up/down |
| Backspace         | Playback rate reset   |
| O                 | Reverse playback      |
| F5                | Reload shaders        |
| F11               | Toggle fullscreen     |

(LMB, MMB, RMB stand for left/middle/right mouse button)

If `inputmap.yaml` exists in the working directory, this file is parsed
and replaces the default mappings.
For possible keys, see `src/ReadScancode.hs`, and
for possible actions, see `src/Action.hs`.
