ocamlopt.opt -w p -I +labltk -o cxa-ppm.opt.exe labltk.cmxa unix.cmxa str.cmxa types.ml divers.ml huffman.ml destructive.ml cxa_image.ml ppm_image.ml interface.ml -cclib "-nocygpath -L C:\MinGW\lib"

ocamlopt.opt -w p -o decompression.opt.exe unix.cmxa str.cmxa types.ml divers.ml huffman.ml destructive.ml cxa_image.ml ppm_image.ml decompression.ml -cclib "-nocygpath -L C:\MinGW\lib"
ocamlopt.opt -w p -o compression.opt.exe unix.cmxa str.cmxa types.ml divers.ml huffman.ml destructive.ml cxa_image.ml ppm_image.ml compression.ml -cclib "-nocygpath -L C:\MinGW\lib"

ocamlopt.opt -w p -o degrader.opt.exe unix.cmxa str.cmxa types.ml divers.ml huffman.ml destructive.ml cxa_image.ml ppm_image.ml degrader.ml -cclib "-nocygpath -L C:\MinGW\lib"

del *.cmx
del *.cmi
del *.o
del *.cmo