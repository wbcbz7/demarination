# demarination - an Amiga 64k  - source release

source code for [demarination](https://demozoo.org/productions/342184/) Amiga OCS 64k intro, first released at Revision 2024.

i'm not an amiga guru yet, but this one could be useful as an example if you want to make an amiga prod without diving much into deep assembly stuff :)

## building instructions

get Visual Studio Code + [vscode-amiga-debug](https://github.com/BartmanAbyss/vscode-amiga-debug), set it up for Amiga 500 config (make sure to locate a Kickstart 1.3 ROM! (in VSCode > Preferences > Settings > Amiga C/C++ Compile,Debug > amiga.rom-paths.A500)), switch to project directory and press F5 to compile and run :)

if you want to build an ADF image, head to `out` folder, fixup tool paths in `make_release.bat` and run it.

## known bugs

* there is a memory corruption bug which may crash Amiga after several intro runs.



--wbcbz7 17.o7.2o24